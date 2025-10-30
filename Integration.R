# app.R — Tourist Explorer (Parking + Pedestrian + Theme Filter w/ working radius)

options(shiny.launch.browser = TRUE)

# --- libraries ---
library(shiny)
library(bslib)
library(sf)
library(dplyr)
library(readr)
library(stringr)
library(scales)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(DT)
library(ggplot2)
library(readxl)
library(shinythemes)
library(janitor)
library(tidyr)
library(geosphere)
library(forcats)
library(lubridate)
library(purrr)
library(shinydashboard)

# null-coalescing helper
`%||%` <- function(a, b) if (!is.null(a)) a else b

# -------------------------
# 1) LOCAL FILE PATHS
# -------------------------
landmarks_path      <- "./landmark.xlsx"
ped_counts_path     <- "./pedestrian-counting-system-monthly-counts-per-hour.xlsx"
sensor_csv_local    <- "./pedestrian-counting-system-sensor-locations.csv"

# -------------------------
# 2) LANDMARKS (Excel source for analytics views + dropdowns)
# -------------------------
landmarks_raw <- read_excel(landmarks_path)

landmarks_excel <- landmarks_raw %>%
  rename(
    Theme        = `Theme`,
    SubTheme     = `Sub Theme`,
    Name         = `Feature Name`,
    Coordinates  = `Co-ordinates`
  ) %>%
  separate(
    Coordinates,
    into = c("Latitude", "Longitude"),
    sep = ",\\s*",
    remove = FALSE
  ) %>%
  mutate(
    Latitude  = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  ) %>%
  filter(!is.na(Latitude), !is.na(Longitude))

# -------------------------
# 3) PEDESTRIAN COUNTS
# -------------------------
ped_raw <- read_excel(ped_counts_path)

ped_by_sensor <- ped_raw %>%
  rename(
    Sensor_Name = Sensor_Name,
    Total       = Total_of_Directions
  ) %>%
  group_by(Sensor_Name) %>%
  summarise(
    Avg_Count = mean(Total, na.rm = TRUE),
    .groups   = "drop"
  )

# -------------------------
# 4) SENSOR LOCATIONS (semicolon csv)
# -------------------------
sensor_locations <- read_delim(sensor_csv_local, delim = ";", show_col_types = FALSE)

sensors_geo <- sensor_locations %>%
  clean_names() %>%
  rename(
    Location_ID  = location_id,
    Sensor_Name  = sensor_name,
    Latitude     = latitude,
    Longitude    = longitude
  ) %>%
  select(Location_ID, Sensor_Name, Latitude, Longitude) %>%
  distinct()

# -------------------------
# 5) JOIN PEDESTRIAN COUNTS TO GEO
# -------------------------
ped_geo <- ped_by_sensor %>%
  left_join(sensors_geo, by = "Sensor_Name") %>%
  filter(!is.na(Latitude), !is.na(Longitude))

# -------------------------
# 6) LANDMARK POPULARITY (nearest pedestrian sensor)
# -------------------------
landmark_popularity <- landmarks_excel %>%
  rowwise() %>%
  mutate(
    nearest_sensor = {
      distances <- distHaversine(
        cbind(ped_geo$Longitude, ped_geo$Latitude),
        c(Longitude, Latitude)
      )
      idx <- which.min(distances)
      ped_geo$Sensor_Name[idx]
    },
    nearest_count = {
      distances <- distHaversine(
        cbind(ped_geo$Longitude, ped_geo$Latitude),
        c(Longitude, Latitude)
      )
      idx <- which.min(distances)
      ped_geo$Avg_Count[idx]
    }
  ) %>%
  ungroup() %>%
  distinct(Name, .keep_all = TRUE) %>%
  arrange(desc(nearest_count))

# -------------------------
# 7) SAFE READERS / HELPERS FOR SPATIAL
# -------------------------
read_sf_cached <- function(url, local_path = NULL) {
  tryCatch({
    if (!is.null(local_path) && file.exists(local_path)) {
      return(sf::st_read(local_path, quiet = TRUE))
    }
    sf::st_read(url, quiet = TRUE)
  }, error = function(e) {
    message("SF read failed: ", conditionMessage(e))
    return(sf::st_sf())
  })
}

norm_col <- function(df, candidates, new) {
  hit <- intersect(names(df), candidates)
  if (length(hit) >= 1) {
    df %>% dplyr::rename(!!new := dplyr::all_of(hit[1]))
  } else df
}

# -------------------------
# 8) DATA ENDPOINTS
# -------------------------
URL_BOUNDARY <- "https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/municipal-boundary/exports/geojson?limit=-1"
URL_LANDMARK <- "https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/landmarks-and-places-of-interest-including-schools-theatres-health-services-spor/exports/geojson?limit=-1"
URL_BAYS     <- "https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/on-street-parking-bays/exports/geojson?limit=-1"
URL_ZONES_TO_SEGMENTS <- "https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/parking-zones-linked-to-street-segments/exports/csv?limit=-1"
URL_SIGN_PLATES       <- "https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/sign-plates-located-in-each-parking-zone/exports/csv?limit=-1"

# -------------------------
# 9) LOAD SPATIAL LAYERS
# -------------------------
mel_boundary <- read_sf_cached(URL_BOUNDARY)
if (nrow(mel_boundary) > 0) {
  mel_boundary <- mel_boundary |>
    sf::st_make_valid() |>
    sf::st_transform(4326)
}

# full CoM landmarks (sf)
landmarks_city <- read_sf_cached(URL_LANDMARK)

if (nrow(landmarks_city) > 0) {
  # Dynamically find the best match for name and theme-related columns
  col_names <- names(landmarks_city)
  name_col  <- col_names[str_detect(tolower(col_names), "name|title|feature")][1]
  theme_col <- col_names[str_detect(tolower(col_names), "theme|category|type|class|function|primary_theme|sector")][1]
  
  landmarks_city <- landmarks_city %>%
    mutate(
      name  = if (!is.na(name_col)) as.character(.data[[name_col]]) else "Unknown",
      theme = if (!is.na(theme_col)) as.character(.data[[theme_col]]) else "Unknown"
    ) %>%
    mutate(
      name = ifelse(is.na(name) | nchar(trimws(name)) == 0, "Unknown", name),
      theme = ifelse(is.na(theme) | nchar(trimws(theme)) == 0, "Unknown", theme)
    ) %>%
    mutate(theme = stringr::str_to_title(theme)) %>%
    janitor::clean_names() %>%
    sf::st_make_valid() %>%
    sf::st_transform(4326)
}

# Guarantee we have a theme column
if (!"theme" %in% names(landmarks_city)) {
  landmarks_city$theme <- "Unknown"
}

# guarantee we have theme column
if (!"theme" %in% names(landmarks_city)) {
  fallback_theme <-
    landmarks_city$category %||%
    landmarks_city$Category %||%
    landmarks_city$type %||%
    landmarks_city$Type %||%
    rep("Unknown", nrow(landmarks_city))
  landmarks_city$theme <- fallback_theme
} else {
  landmarks_city$theme[is.na(landmarks_city$theme)] <- "Unknown"
}

bays <- read_sf_cached(URL_BAYS)
if (nrow(bays) > 0) {
  bays <- bays |>
    sf::st_make_valid() |>
    sf::st_transform(4326)
}

bays <- if (nrow(bays) > 0) {
  norm_col(
    bays,
    c("bay_id","bayid","bayidentifier","id","BAY_ID","kerbsideid"),
    "bay_id"
  )
} else bays

if (nrow(bays) > 0 && "bay_id" %in% names(bays)) {
  bays <- bays |> dplyr::mutate(bay_id = as.character(bay_id))
}
if (nrow(bays) > 0 && "roadsegmentid" %in% names(bays)) {
  bays <- bays |> dplyr::mutate(roadsegmentid = as.character(roadsegmentid))
}

# -------------------------
# 9.5) LOAD PUBLIC TRANSPORT DATA
# -------------------------
cat("Loading public transport data...\n")

boundary <- tryCatch({
  st_read("./municipal-boundary.shp", quiet = TRUE)
}, error = function(e) st_sf())

stops <- tryCatch({
  st_read("./public_transport_stops.geojson", quiet = TRUE)
}, error = function(e) st_sf())

stations <- tryCatch({
  read.csv("./annual_metropolitan_train_station_entries_fy_2024_2025.csv")
}, error = function(e) data.frame())

lines <- tryCatch({
  st_read("./public_transport_lines.geojson", quiet = TRUE)
}, error = function(e) st_sf())

# --- make sure the transport data has consistent columns like in your friend's app ---

if (nrow(stops) > 0) {
  stops <- cbind(stops, st_coordinates(stops))
  # try to standardize column names if possible
  # we'll look for plausible columns:
  nm <- names(stops)
  # id / name / mode guesses
  id_col   <- nm[str_detect(tolower(nm), "stop_id|id$")][1]
  name_col <- nm[str_detect(tolower(nm), "stop_name|name")][1]
  mode_col <- nm[str_detect(tolower(nm), "mode|transport|type")][1]
  # rename safely if found
  if (!is.na(id_col))   stops <- rename(stops, id   = !!id_col)
  if (!is.na(name_col)) stops <- rename(stops, name = !!name_col)
  if (!is.na(mode_col)) stops <- rename(stops, mode = !!mode_col)
  # coords from st_coordinates() became columns "X" "Y" above
}

if (nrow(stations) > 0) {
  # stations CSV likely already has Stop_lat / Stop_long etc.
  stations <- st_as_sf(stations, coords = c("Stop_long", "Stop_lat"), crs = 4326, remove = FALSE)
  nm2 <- names(stations)
  # try to match your friend's names
  year_col    <- nm2[str_detect(tolower(nm2), "year")][1]
  id_col2     <- nm2[str_detect(tolower(nm2), "station_id|id$")][1]
  name_col2   <- nm2[str_detect(tolower(nm2), "station|name")][1]
  annual_col  <- nm2[str_detect(tolower(nm2), "annual")][1]
  if (!is.na(year_col))   stations <- rename(stations, year = !!year_col)
  if (!is.na(id_col2))    stations <- rename(stations, id = !!id_col2)
  if (!is.na(name_col2))  stations <- rename(stations, station = !!name_col2)
  if (!is.na(annual_col)) stations <- rename(stations, annual = !!annual_col)
  # keep Stop_lat / Stop_long as given in CSV
  if (!("Stop_lat" %in% names(stations))) stations$Stop_lat <- stations$geometry[[2]]
  if (!("Stop_long" %in% names(stations))) stations$Stop_long <- stations$geometry[[1]]
}

if (nrow(lines) > 0) {
  nm3 <- names(lines)
  dest_col   <- nm3[str_detect(tolower(nm3), "destination")][1]
  short_col  <- nm3[str_detect(tolower(nm3), "short")][1]
  long_col   <- nm3[str_detect(tolower(nm3), "long")][1]
  mode_col3  <- nm3[str_detect(tolower(nm3), "mode|type")][1]
  if (!is.na(dest_col))  lines <- rename(lines, destination = !!dest_col)
  if (!is.na(short_col)) lines <- rename(lines, shortname = !!short_col)
  if (!is.na(long_col))  lines <- rename(lines, longname = !!long_col)
  if (!is.na(mode_col3)) lines <- rename(lines, mode = !!mode_col3)
}

# filter to boundary if possible
if (nrow(boundary) > 0) {
  if (nrow(stops) > 0)    stops    <- tryCatch(st_filter(stops,    boundary, .predicate = st_within),    error=function(e) stops)
  if (nrow(stations) > 0) stations <- tryCatch(st_filter(stations, boundary, .predicate = st_within),    error=function(e) stations)
  if (nrow(lines) > 0)    lines    <- tryCatch(st_filter(lines,    boundary, .predicate = st_intersects), error=function(e) lines)
}

# keep only tram/bus for stops, and supported modes for lines
if (nrow(stops) > 0 && "mode" %in% names(stops)) {
  stops <- stops %>% filter(mode %in% c("METRO TRAM", "METRO BUS", "METRO TRAIN"))
}

if (nrow(lines) > 0 && "mode" %in% names(lines)) {
  lines <- lines %>% filter(mode %in% c("METRO TRAIN", "METRO TRAM", "METRO BUS"))
}

# Assign icons/colors
mapcolour <- setNames(c("blue", "green", "orange"),
                      c("METRO TRAIN", "METRO TRAM", "METRO BUS"))
mapicon <- setNames(c("subway", "train", "bus"),
                    c("METRO TRAIN", "METRO TRAM", "METRO BUS"))

if (nrow(stops) > 0) {
  stops$colour <- if ("mode" %in% names(stops)) unname(mapcolour[stops$mode]) else "gray"
  stops$icon   <- if ("mode" %in% names(stops)) unname(mapicon[stops$mode])   else "info"
}
if (nrow(stations) > 0) {
  stations$colour <- "blue"
  stations$icon   <- "subway"
}
if (nrow(lines) > 0) {
  if ("mode" %in% names(lines)) {
    lines$colour <- unname(mapcolour[lines$mode])
  } else {
    lines$colour <- "blue"
  }
}

# Metric CRS for proximity search
if (nrow(stops) > 0)    stops_m    <- st_transform(stops, 3857)
if (nrow(stations) > 0) stations_m <- st_transform(stations, 3857)

# -------------------------
# 10) ZONE-BASED DATA
# -------------------------
cat("Loading zone-to-segments mapping...\n")
zones_to_segments <- tryCatch({
  readr::read_delim(URL_ZONES_TO_SEGMENTS, delim = ";", show_col_types = FALSE)
}, error = function(e) {
  message("Zones-to-segments CSV read failed: ", conditionMessage(e))
  data.frame()
})

cat("Loading sign plates data...\n")
sign_plates <- tryCatch({
  readr::read_delim(URL_SIGN_PLATES, delim = ";", show_col_types = FALSE)
}, error = function(e) {
  message("Sign plates CSV read failed: ", conditionMessage(e))
  data.frame()
})

if (nrow(zones_to_segments) > 0) {
  zones_to_segments <- zones_to_segments |>
    norm_col(c("segment_id", "segmentid", "roadsegmentid", "road_segment_id"), "segment_id") |>
    norm_col(c("parkingzone", "parking_zone", "zone_id", "zoneid"), "zone_id") |>
    dplyr::mutate(
      segment_id = as.character(segment_id),
      zone_id    = as.character(zone_id)
    )
  cat("Loaded", nrow(zones_to_segments), "zone-to-segment mappings\n")
}

if (nrow(sign_plates) > 0) {
  sign_plates <- sign_plates |>
    norm_col(c("parkingzone", "parking_zone", "zone_id", "zoneid"), "zone_id") |>
    dplyr::mutate(zone_id = as.character(zone_id))
  cat("Loaded", nrow(sign_plates), "sign plate records\n")
}

# -------------------------
# 11) AGGREGATE BAYS -> SEGMENTS
# -------------------------
cat("PHASE 2: Aggregating bays to road segments...\n")
segment_locations <- NULL
if (nrow(bays) > 0 && "roadsegmentid" %in% names(bays)) {
  segment_locations <- bays |>
    dplyr::group_by(roadsegmentid) |>
    dplyr::summarise(
      num_bays = dplyr::n(),
      segment_geometry = sf::st_centroid(sf::st_union(geometry)),
      .groups = "drop"
    ) |>
    dplyr::rename(segment_id = roadsegmentid) |>
    sf::st_as_sf(sf_column_name = "segment_geometry")
  
  cat("Created", nrow(segment_locations), "segment centroids\n")
}

# -------------------------
# 12) MAP SEGMENTS -> ZONES
# -------------------------
cat("PHASE 3: Mapping segments to parking zones...\n")
zone_locations <- NULL
if (!is.null(segment_locations) &&
    nrow(segment_locations) > 0 &&
    nrow(zones_to_segments) > 0) {
  
  segment_zone_map <- sf::st_drop_geometry(segment_locations) |>
    dplyr::select(segment_id, num_bays) |>
    dplyr::left_join(
      zones_to_segments,
      by = "segment_id",
      relationship = "many-to-many"
    ) |>
    dplyr::filter(!is.na(zone_id))
  
  cat("Matched", nrow(segment_zone_map), "segment-zone pairs\n")
  
  zone_locations <- segment_zone_map |>
    dplyr::left_join(
      segment_locations |> dplyr::select(segment_id, segment_geometry),
      by = "segment_id"
    ) |>
    sf::st_as_sf(sf_column_name = "segment_geometry") |>
    dplyr::group_by(zone_id) |>
    dplyr::summarise(
      segment_count = dplyr::n(),
      zone_geometry  = sf::st_centroid(sf::st_union(segment_geometry)),
      .groups        = "drop"
    ) |>
    sf::st_as_sf(sf_column_name = "zone_geometry")
  
  cat("Created", nrow(zone_locations), "zone locations\n")
}

# -------------------------
# 13) UNMATCHED SEGMENTS
# -------------------------
cat("PHASE 3.5: Processing unmatched segments (no zone data)...\n")
unmatched_segments <- NULL
if (!is.null(segment_locations) && nrow(segment_locations) > 0) {
  
  matched_segment_ids <- character(0)
  if (!is.null(zone_locations) &&
      nrow(zone_locations) > 0 &&
      nrow(zones_to_segments) > 0) {
    matched_segment_ids <- unique(zones_to_segments$segment_id)
  }
  
  unmatched_segments <- segment_locations |>
    dplyr::filter(!segment_id %in% matched_segment_ids) |>
    dplyr::mutate(
      zone_id                   = paste0("NO_ZONE_", segment_id),
      restriction_days          = "N/A",
      time_restrictions_start   = "N/A",
      time_restrictions_finish  = "N/A",
      restriction_display       = "Zone ID not found. This area may not require a parking ticket. Please check the nearby sign plate for specific restrictions."
    ) |>
    dplyr::rename(zone_geometry = segment_geometry) |>
    sf::st_as_sf(sf_column_name = "zone_geometry")
  
  cat("Found", nrow(unmatched_segments), "unmatched segments (no zone data)\n")
}

# -------------------------
# 14) POPUP HELPERS
# -------------------------
create_zone_popup_table <- function(zone_id_val, sign_data, lat = NULL, lon = NULL) {
  zone_signs <- sign_data |> dplyr::filter(zone_id == zone_id_val)
  
  if (nrow(zone_signs) == 0) {
    return(paste0("<b>Zone ID:</b> ", zone_id_val,
                  "<br><i>No restriction data available</i>"))
  }
  
  zone_signs <- zone_signs |>
    dplyr::mutate(
      day_order = ifelse(
        grepl("Mon|Tue|Wed|Thu|Fri|M-F|Mon-Fri",
              restriction_days,
              ignore.case = TRUE),
        0, 1
      ),
      time_sort = as.character(time_restrictions_start)
    ) |>
    dplyr::arrange(day_order, time_sort) |>
    dplyr::select(-day_order, -time_sort)
  
  html <- paste0("<b>Zone ID:</b> ", zone_id_val, "<br>")
  if (!is.null(lat) && !is.null(lon)) {
    html <- paste0(
      html,
      "<b>Location:</b> ",
      lat, ", ", lon, "<br>"
    )
  }
  html <- paste0(html, "<b>Restrictions:</b><br>")
  html <- paste0(
    html,
    "<table style='border-collapse: collapse; width: 100%; font-size: 11px;'>",
    "<tr style='background-color: #f0f0f0; font-weight: bold;'>",
    "<th style='border: 1px solid #ddd; padding: 4px;'>Days</th>",
    "<th style='border: 1px solid #ddd; padding: 4px;'>Start</th>",
    "<th style='border: 1px solid #ddd; padding: 4px;'>Finish</th>",
    "<th style='border: 1px solid #ddd; padding: 4px;'>Type</th>",
    "</tr>"
  )
  
  for (i in 1:nrow(zone_signs)) {
    row <- zone_signs[i, ]
    html <- paste0(
      html,
      "<tr>",
      "<td style='border: 1px solid #ddd; padding: 4px;'>",
      row$restriction_days, "</td>",
      "<td style='border: 1px solid #ddd; padding: 4px;'>",
      row$time_restrictions_start, "</td>",
      "<td style='border: 1px solid #ddd; padding: 4px;'>",
      row$time_restrictions_finish, "</td>",
      "<td style='border: 1px solid #ddd; padding: 4px;'>",
      row$restriction_display, "</td>",
      "</tr>"
    )
  }
  
  html <- paste0(html, "</table>")
  return(html)
}

create_zone_popups_with_coords <- function(zones_data, sign_data) {
  popup_list <- vector("character", nrow(zones_data))
  for (i in 1:nrow(zones_data)) {
    coords <- tryCatch({
      sf::st_coordinates(sf::st_geometry(zones_data[i, ]))
    }, error = function(e) { NULL })
    
    if (!is.null(coords) && nrow(coords) > 0) {
      lat <- round(coords[1, "Y"], 6)
      lon <- round(coords[1, "X"], 6)
      popup_list[i] <- create_zone_popup_table(
        zones_data$zone_id[i], sign_data, lat, lon
      )
    } else {
      popup_list[i] <- create_zone_popup_table(
        zones_data$zone_id[i], sign_data, NULL, NULL
      )
    }
  }
  zones_data$popup_html <- popup_list
  return(zones_data)
}

create_unmatched_popups <- function(unmatched_data) {
  popup_list <- vector("character", nrow(unmatched_data))
  for (i in 1:nrow(unmatched_data)) {
    coords <- tryCatch({
      sf::st_coordinates(sf::st_geometry(unmatched_data[i, ]))
    }, error = function(e) { NULL })
    
    segment_id <- gsub("NO_ZONE_", "", unmatched_data$zone_id[i])
    
    if (!is.null(coords) && nrow(coords) > 0) {
      lat <- round(coords[1, "Y"], 6)
      lon <- round(coords[1, "X"], 6)
      popup_list[i] <- paste0(
        "<b>Segment ID:</b> ", segment_id, "<br>",
        "<b>Location:</b> ", lat, ", ", lon, "<br>",
        "<b>Restrictions:</b><br>",
        "Zone ID not found. This area may not require a parking ticket. ",
        "Please check the nearby sign plate for specific restrictions."
      )
    } else {
      popup_list[i] <- paste0(
        "<b>Segment ID:</b> ", segment_id, "<br>",
        "<b>Restrictions:</b><br>",
        "Zone ID not found. This area may not require a parking ticket. ",
        "Please check the nearby sign plate for specific restrictions."
      )
    }
  }
  unmatched_data$popup_html <- popup_list
  return(unmatched_data)
}

# -------------------------
# 15) PREPARE zones_display
# -------------------------
zones_display <- NULL
if (!is.null(zone_locations) && nrow(zone_locations) > 0) {
  zones_display <- zone_locations |>
    dplyr::mutate(has_zone = TRUE)
  
  cat("Prepared", nrow(zones_display), "zones for display with table popups\n")
}

if (!is.null(unmatched_segments) && nrow(unmatched_segments) > 0) {
  unmatched_segments <- unmatched_segments |>
    dplyr::mutate(has_zone = FALSE)
  
  if (!is.null(zones_display) && nrow(zones_display) > 0) {
    zones_display <- dplyr::bind_rows(zones_display, unmatched_segments)
    cat("Combined:", nrow(zones_display),
        "total markers (zones + unmatched segments)\n")
  } else {
    zones_display <- unmatched_segments
    cat("No zones found, using", nrow(zones_display),
        "unmatched segments only\n")
  }
}

# Load and preprocess data
load_crime_data <- function() {
  file_path <- "./Data_Tables_LGA_Recorded_Offences_Year_Ending_June_2025.xlsx"
  
  table01 <- read_excel(file_path, sheet = "Table 01") %>%
    filter(`Local Government Area` == "Melbourne")
  table02 <- read_excel(file_path, sheet = "Table 02") %>%
    filter(`Local Government Area` == "Melbourne")
  table03 <- read_excel(file_path, sheet = "Table 03") %>%
    filter(`Local Government Area` == "Melbourne")
  table04 <- read_excel(file_path, sheet = "Table 04") %>%
    filter(`Local Government Area` == "Melbourne")
  table05 <- read_excel(file_path, sheet = "Table 05") %>%
    filter(`Local Government Area` == "Melbourne")
  table06 <- read_excel(file_path, sheet = "Table 06") %>%
    filter(`Local Government Area` == "Melbourne")
  
  list(
    table01 = table01,
    table02 = table02,
    table03 = table03,
    table04 = table04,
    table05 = table05,
    table06 = table06
  )
}

crime_data <- load_crime_data()

# -------------------------
# 16) UI
# -------------------------
ui <- navbarPage(
  title = "Tourist Explorer",
  theme = bslib::bs_theme(bootswatch = "flatly"),
  header = tags$head(
    tags$style(HTML("
      .irs-grid-text { font-size: 10px; }
      .irs-grid-text:last-child { margin-left: -10px; }
      .leaflet-control-layers-expanded {
        max-height: 200px;
        overflow-y: auto;
      }
    "))
  ),
  
  tabPanel(
    "Parking and Public Transport near Landmarks",
    sidebarLayout(
      sidebarPanel(
        h4("Select Landmarks for Parking and Public Transport Search"),
        helpText("1) Pick a theme, 2) optional: choose specific landmarks"),
        
        # Theme filter FIRST
        selectInput(
          "theme_filter",
          "Filter by Landmark Theme:",
          choices  = c("All", sort(unique(landmark_popularity$Theme))),
          selected = "All"
        ),
        
        # Landmarks dropdown (updated dynamically per theme)
        selectizeInput(
          "lm_name",
          "Landmark(s)",
          choices = sort(unique(landmark_popularity$Name)),
          options = list(
            placeholder = "Type to search...",
            plugins = list('remove_button')
          ),
          multiple = TRUE
        ),
        
        sliderInput(
          "radius_m", "Search radius (meters)",
          min = 100, max = 1000, value = 300, step = 50,
          ticks = TRUE, sep = ""
        ),
        
        hr(),
        
        tags$details(
          tags$summary(
            tags$strong("Parking Abbreviations Guide"),
            style = "cursor: pointer; color: #2c3e50;"
          ),
          tags$div(
            style = "margin-top: 10px; font-size: 12px; line-height: 1.6;",
            tags$table(
              style = "width: 100%; border-collapse: collapse;",
              tags$tbody(
                tags$tr(
                  tags$td(tags$strong("FP1P"),
                          style = "padding: 4px 8px; color: #27ae60;"),
                  tags$td("Free Parking 1 hour",
                          style = "padding: 4px 8px;")
                ),
                tags$tr(
                  tags$td(tags$strong("MP2P"),
                          style = "padding: 4px 8px; color: #e74c3c;"),
                  tags$td("Meter Parking 2 hours",
                          style = "padding: 4px 8px;")
                ),
                tags$tr(
                  tags$td(tags$strong("LZ30"),
                          style = "padding: 4px 8px; color: #f39c12;"),
                  tags$td("Loading Zone 30 min",
                          style = "padding: 4px 8px;")
                ),
                tags$tr(
                  tags$td(tags$strong("QP"),
                          style = "padding: 4px 8px; color: #3498db;"),
                  tags$td("Quarter Parking (15 min)",
                          style = "padding: 4px 8px;")
                ),
                tags$tr(
                  tags$td(tags$strong("DP"),
                          style = "padding: 4px 8px; color: #9b59b6;"),
                  tags$td("Disabled Parking",
                          style = "padding: 4px 8px;")
                ),
                tags$tr(
                  tags$td(tags$strong("HP"),
                          style = "padding: 4px 8px; color: #16a085;"),
                  tags$td("Hospital/Health Permit",
                          style = "padding: 4px 8px;")
                ),
                tags$tr(
                  tags$td(tags$strong("PP"),
                          style = "padding: 4px 8px; color: #34495e;"),
                  tags$td("Permit Parking",
                          style = "padding: 4px 8px;")
                ),
                tags$tr(
                  tags$td(tags$strong("SP"),
                          style = "padding: 4px 8px; color: #7f8c8d;"),
                  tags$td("Special/Short-term Parking",
                          style = "padding: 4px 8px;")
                )
              )
            )
          )
        ),
        
        hr(),
        
        h4("View Mode:"),
        tabsetPanel(
          id = "view_mode",
          tabPanel("Ranking View", value = "ranking"),
          tabPanel("Trend View",   value = "trend")
        ),
        
        width = 3
      ),
      
      mainPanel(
        leafletOutput("map_parking", height = 560),
        br(),
        uiOutput("info_summary"),
        
        conditionalPanel(
          "input.view_mode == 'ranking'",
          plotlyOutput("popularity_plot", height = "600px")
        ),
        conditionalPanel(
          "input.view_mode == 'trend'",
          plotlyOutput("trend_plot", height = "600px")
        ),
        
        # JS helpers that toggle layer checkboxes.
        tags$script(HTML("
          Shiny.addCustomMessageHandler('uncheckLayers', function(message) {
            setTimeout(function() {
              var layerControl = document.querySelector('.leaflet-control-layers');
              if (layerControl) {
                var checkboxes = layerControl.querySelectorAll('input[type=\"checkbox\"]');
                checkboxes.forEach(function(checkbox) {
                  var label = checkbox.nextSibling;
                  if (label && label.textContent) {
                    var labelText = label.textContent.trim();
                    if (labelText === 'All Parking Zones' ||
                        labelText === 'Pedestrian Heatmap' ) {
                      checkbox.checked = false;
                    }
                  }
                });
              }
            }, 100);
          });

          Shiny.addCustomMessageHandler('recheckLayers', function(message) {
            setTimeout(function() {
              var layerControl = document.querySelector('.leaflet-control-layers');
              if (layerControl) {
                var checkboxes = layerControl.querySelectorAll('input[type=\"checkbox\"]');
                checkboxes.forEach(function(checkbox) {
                  var label = checkbox.nextSibling;
                  if (label && label.textContent) {
                    var labelText = label.textContent.trim();
                    if (labelText === 'All Parking Zones' ||
                        labelText === 'Pedestrian Heatmap') {
                      checkbox.checked = true;
                    }
                  }
                });
              }
            }, 100);
          });
        "))
      )
    )
  ),
  # ==========================================================
  # TAB 2: Melbourne Crime Statistics
  # ==========================================================
  # ==========================================================
  # TAB 2: Melbourne Crime Statistics (with permanent left sidebar)
  # ==========================================================
  tabPanel(
    "Melbourne Crime",
    fluidPage(
      titlePanel("Melbourne Crime Statistics - Year Ending June 2025"),
      fluidRow(
        column(
          width = 3,
          wellPanel(
            h4("Sub-Tabs"),
            tags$ul(
              style = "list-style-type:none; padding-left:0;",
              tags$li(actionLink("nav_overview", "Overview")),
              tags$li(actionLink("nav_categories", "Offence Categories")),
              tags$li(actionLink("nav_suburbs", "Suburbs Analysis")),
              tags$li(actionLink("nav_locations", "Location Types")),
              tags$li(actionLink("nav_investigation", "Investigation Status")),
              tags$li(actionLink("nav_drugs", "Drug Offences")),
              tags$li(actionLink("nav_tables", "Data Tables"))
            )
          )
        ),
        column(
          width = 9,
          uiOutput("crime_main_panel")
        )
      )
    )
  )
)

# -------------------------
# 17) SERVER
# -------------------------
server <- function(input, output, session) {
  
  # ---- notify if critical data failed
  observe({
    if (nrow(landmarks_city) == 0 || nrow(bays) == 0) {
      showNotification(
        "Data failed to load. Please check network or replace URLs with local files.",
        type = "error",
        duration = NULL
      )
    }
  })
  
  # ---- dependent dropdown: theme -> landmark list
  observeEvent(input$theme_filter, {
    if (input$theme_filter == "All") {
      ch <- sort(unique(landmark_popularity$Name))
    } else {
      ch <- sort(unique(
        landmark_popularity$Name[
          landmark_popularity$Theme == input$theme_filter
        ]
      ))
    }
    updateSelectizeInput(
      session,
      "lm_name",
      choices = ch,
      selected = character(0),
      server = TRUE
    )
  }, ignoreInit = TRUE)
  
  # ---- reactive: sf of THEME landmarks for base display
  theme_landmarks_sf <- reactive({
    if (nrow(landmarks_city) == 0) return(landmarks_city)
    
    if (input$theme_filter == "All") {
      lm_sf <- landmarks_city
    } else {
      lm_sf <- landmarks_city %>%
        filter(grepl(input$theme_filter, theme, ignore.case = TRUE))
    }
    lm_sf
  })
  
  # ---- reactive: sf of SELECTED landmarks (subset of theme_landmarks_sf)
  selected_landmarks_sf <- reactive({
    lm_sf <- theme_landmarks_sf()
    if (nrow(lm_sf) == 0) return(lm_sf)
    if (length(input$lm_name) > 0) {
      lm_sf <- lm_sf %>% filter(name %in% input$lm_name)
    } else {
      lm_sf <- lm_sf[0, ]  # empty sf if none selected
    }
    lm_sf
  })
  
  # ---- buffer around selected landmarks
  combined_buffer <- reactive({
    sel_lm <- selected_landmarks_sf()
    req(nrow(sel_lm) > 0)
    
    lm_proj <- tryCatch({
      sf::st_transform(sel_lm, 7899) # metric CRS in Melbourne
    }, error = function(e) {
      message("EPSG:7899 not available, fallback 3857")
      sf::st_transform(sel_lm, 3857)
    })
    
    buf <- sf::st_buffer(lm_proj, dist = input$radius_m)
    
    if (nrow(buf) > 1) {
      buf <- sf::st_union(buf) |> sf::st_sf()
    }
    
    sf::st_transform(buf, 4326)
  })
  
  # ---- zones within buffer
  zones_in_radius <- reactive({
    sel <- selected_landmarks_sf()
    req(nrow(sel) > 0)
    buf <- combined_buffer()
    req(nrow(buf) > 0)
    
    if (!is.null(zones_display) && nrow(zones_display) > 0) {
      intersects <- sf::st_intersects(zones_display, buf, sparse = FALSE)
      zones_filtered <- zones_display[as.vector(intersects), ]
      return(zones_filtered)
    } else {
      zones_display[0, , drop = FALSE]
    }
  })
  
  # ---- INITIAL MAP
  output$map_parking <- renderLeaflet({
    map <- leaflet(options = leafletOptions(minZoom = 11)) %>%
      addProviderTiles(providers$CartoDB.Positron)
    
    # boundary / view
    if (nrow(mel_boundary) > 0) {
      bb <- sf::st_bbox(mel_boundary)
      map <- fitBounds(
        map,
        lng1 = as.numeric(bb["xmin"]),
        lat1 = as.numeric(bb["ymin"]),
        lng2 = as.numeric(bb["xmax"]),
        lat2 = as.numeric(bb["ymax"])
      )
      map <- addPolygons(
        map, data = mel_boundary,
        weight = 2, color = "#222",
        fill = FALSE, group = "Boundary"
      )
    } else {
      map <- setView(map, lng = 144.9631, lat = -37.8136, zoom = 13)
    }
    
    # heatmap
    if (nrow(ped_geo) > 0) {
      map <- addHeatmap(
        map,
        data = ped_geo,
        lng = ~Longitude, lat = ~Latitude,
        intensity = ~Avg_Count,
        blur = 25, max = 0.9, radius = 15,
        group = "Pedestrian Heatmap"
      )
    }
    
    # THEME Landmarks layer (we start with "All")
    base_lm <- theme_landmarks_sf()
    if (nrow(base_lm) > 0) {
      map <- addCircleMarkers(
        map,
        data = base_lm,
        radius = 4,
        stroke = TRUE,
        weight = 1,
        color = "#003366",
        fillColor = "#0000AA",
        fillOpacity = 0.6,
        label = ~name,
        popup = ~paste0("<b>", name, "</b><br>Theme: ", theme),
        layerId = ~name,  
        group = "Theme Landmarks"
      )
    }
    
    # ALL Parking Zones layer
    if (!is.null(zones_display) && nrow(zones_display) > 0) {
      
      zones_with_ids <- zones_display %>% filter(has_zone == TRUE)
      if (nrow(zones_with_ids) > 0) {
        zones_with_ids <- create_zone_popups_with_coords(
          zones_with_ids,
          sign_plates
        )
        map <- addAwesomeMarkers(
          map,
          data = zones_with_ids,
          icon = awesomeIcons(
            icon = "car",
            library = "fa",
            markerColor = "red",
            iconColor  = "white"
          ),
          label = ~paste0("Zone ", zone_id),
          popup = ~popup_html,
          group = "All Parking Zones",
          clusterOptions = markerClusterOptions()
        )
      }
      
      unmatched <- zones_display %>% filter(has_zone == FALSE)
      if (nrow(unmatched) > 0) {
        unmatched <- create_unmatched_popups(unmatched)
        map <- addAwesomeMarkers(
          map,
          data = unmatched,
          icon = awesomeIcons(
            icon = "car",
            library = "fa",
            markerColor = "orange",
            iconColor = "white"
          ),
          label = ~"No Zone ID",
          popup = ~popup_html,
          group = "All Parking Zones",
          clusterOptions = markerClusterOptions()
        )
      }
    }
    
    # control with new PT layer
    map <- addLayersControl(
      map,
      overlayGroups = c(
        "Boundary",
        "Theme Landmarks",
        "Selected Landmarks",
        "All Parking Zones",
        "Near by Parking",
        "Buffer",
        "Pedestrian Heatmap",
        "Nearby Public Transport"
      ),
      options = layersControlOptions(collapsed = FALSE)
    )
    
    map <- hideGroup(map, "Selected Landmarks")
    map <- hideGroup(map, "Near by Parking")
    map <- hideGroup(map, "Buffer")
    
    map
  })
  
  # ---- UPDATE MAP whenever theme, landmark, or radius changes
  observeEvent(
    list(input$theme_filter, input$lm_name, input$radius_m),
    {
      # theme base layer update:
      map <- leafletProxy("map_parking")
      
      # 1. refresh Theme Landmarks layer to match current theme
      tlm <- theme_landmarks_sf()
      map <- clearGroup(map, "Theme Landmarks")
      if (nrow(tlm) > 0) {
        map <- addCircleMarkers(
          map,
          data = tlm,
          radius = 4,
          stroke = TRUE,
          weight = 1,
          color = "#003366",
          fillColor = "#0000AA",
          fillOpacity = 0.6,
          label = ~name,
          popup = ~paste0("<b>", name, "</b><br>Theme: ", theme),
          layerId = ~name, 
          group = "Theme Landmarks"
        )
      }
      
      # 2. clear dynamic groups for a fresh redraw
      map <- clearGroup(map, "Selected Landmarks")
      map <- clearGroup(map, "Near by Parking")
      map <- clearGroup(map, "Buffer")
      map <- clearGroup(map, "Nearby Public Transport")
      
      sel_lm <- selected_landmarks_sf()
      if (nrow(sel_lm) > 0) {
        # we have explicit landmark selection -> draw buffer and zones
        
        # buffer
        buf <- tryCatch({
          combined_buffer()
        }, error = function(e) { NULL })
        
        if (!is.null(buf) && nrow(buf) > 0) {
          map <- addPolygons(
            map,
            data = buf,
            fill = TRUE,
            fillOpacity = 0.08,
            color = "#1f78b4",
            weight = 2,
            group = "Buffer"
          )
          
          # zoom to buffer bounds
          bounds <- sf::st_bbox(buf)
          map <- fitBounds(
            map,
            bounds[["xmin"]], bounds[["ymin"]],
            bounds[["xmax"]], bounds[["ymax"]],
            options = list(padding = c(50,50))
          )
        }
        
        # selected landmarks bigger + highlight
        sel_lm$popup_sel <- paste0(
          "<b>", sel_lm$name, "</b><br>Theme: ", sel_lm$theme
        )
        map <- addCircleMarkers(
          map,
          data = sel_lm,
          radius = 8,
          stroke = TRUE,
          weight = 2,
          fillOpacity = 0.9,
          fillColor = "#e31a1c",
          color = "#fff",
          label = ~name,
          popup = ~popup_sel,
          group = "Selected Landmarks"
        )
        
        # zones near buffer
        zones_sf <- zones_in_radius()
        if (!is.null(zones_sf) && nrow(zones_sf) > 0) {
          zones_with_ids <- zones_sf
          unmatched_segs <- zones_sf
          if ("has_zone" %in% names(zones_sf)) {
            zones_with_ids <- zones_sf %>% filter(has_zone == TRUE)
            unmatched_segs <- zones_sf %>% filter(has_zone == FALSE)
          }
          
          if (nrow(zones_with_ids) > 0) {
            zones_with_ids <- create_zone_popups_with_coords(
              zones_with_ids, sign_plates
            )
            map <- addAwesomeMarkers(
              map,
              data = zones_with_ids,
              icon = awesomeIcons(
                icon = "car",
                library = "fa",
                markerColor = "red",
                iconColor  = "white"
              ),
              label = ~paste0("Zone ", zone_id),
              popup = ~popup_html,
              group = "Near by Parking",
              clusterOptions = markerClusterOptions()
            )
          }
          
          if (nrow(unmatched_segs) > 0) {
            unmatched_segs <- create_unmatched_popups(unmatched_segs)
            map <- addAwesomeMarkers(
              map,
              data = unmatched_segs,
              icon = awesomeIcons(
                icon = "car",
                library = "fa",
                markerColor = "orange",
                iconColor = "white"
              ),
              label = ~"No Zone ID",
              popup = ~popup_html,
              group = "Near by Parking",
              clusterOptions = markerClusterOptions()
            )
          }
        }
        
        # ---- NEW: Nearby Public Transport (same radius_m) ----
        if (exists("stops_m") && exists("stations_m")) {
          radius_pt <- input$radius_m
          sel_m <- tryCatch(st_transform(sel_lm, 3857), error=function(e) NULL)
          
          if (!is.null(sel_m)) {
            # find nearby stops
            stops_near <- if (exists("stops") && nrow(stops) > 0) {
              idx <- as.numeric(st_distance(stops_m, sel_m)) <= radius_pt
              stops[idx, , drop = FALSE]
            } else stops[0,]
            
            # find nearby stations
            stations_near <- if (exists("stations") && nrow(stations) > 0) {
              idx2 <- as.numeric(st_distance(stations_m, sel_m)) <= radius_pt
              stations[idx2, , drop = FALSE]
            } else stations[0,]
            
            # add tram/bus/train stops
            if (nrow(stops_near) > 0) {
              # Assign clearer colors per mode
              stops_near <- stops_near %>%
                mutate(
                  iconColorFinal = "white",
                  markerColorFinal = case_when(
                    grepl("BUS", mode, ignore.case = TRUE)   ~ "darkgreen",   # 🟢 bus stops
                    grepl("TRAM", mode, ignore.case = TRUE)  ~ "lightgreen",  # 💚 tram stops
                    grepl("TRAIN", mode, ignore.case = TRUE) ~ "blue",        # 🔵 train stops
                    TRUE ~ "gray"
                  )
                )
              
              map <- addAwesomeMarkers(
                map,
                data = stops_near,
                lng = ~X, lat = ~Y,
                icon = ~awesomeIcons(
                  library = "fa",
                  markerColor = markerColorFinal,
                  icon = ~icon,
                  iconColor = iconColorFinal
                ),
                label = ~name,
                popup = ~paste0("<b>", name, "</b><br>Mode: ", mode),
                group = "Nearby Public Transport",
                clusterOptions = markerClusterOptions()
              )
            }
            
            
            # add stations (train entries data)
            if (nrow(stations_near) > 0) {
              map <- addAwesomeMarkers(
                map,
                data = stations_near,
                lng = ~Stop_long, lat = ~Stop_lat,
                icon = awesomeIcons(
                  library = "fa",
                  markerColor = "blue",
                  icon = "subway",
                  iconColor = "white"
                ),
                label = ~station,
                popup = ~paste0(
                  "<b>", station, "</b>",
                  if ("annual" %in% names(stations_near))
                    paste0("<br>Annual Entries: ", annual)
                  else ""
                ),
                group = "Nearby Public Transport",
                clusterOptions = markerClusterOptions()
              )
            }
          }
        }
        # ------------------------------------------------------
        
        # show layers related to selection
        map <- showGroup(map, "Selected Landmarks")
        map <- showGroup(map, "Near by Parking")
        if (!is.null(buf) && nrow(buf) > 0) {
          map <- showGroup(map, "Buffer")
        }
        map <- showGroup(map, "Nearby Public Transport")
        
        # We keep 'Theme Landmarks' visible for context.
        # We hide the global heatmap/parking only if you wish.
        map <- hideGroup(map, "All Parking Zones")
        map <- hideGroup(map, "Pedestrian Heatmap")
        session$sendCustomMessage(type = "uncheckLayers", message = list())
        
      } else {
        # no specific landmark chosen -> no radius, no Near by Parking
        # show everything again
        map <- hideGroup(map, "Selected Landmarks")
        map <- hideGroup(map, "Near by Parking")
        map <- hideGroup(map, "Buffer")
        map <- hideGroup(map, "Nearby Public Transport")
        map <- showGroup(map, "All Parking Zones")
        map <- showGroup(map, "Pedestrian Heatmap")
        session$sendCustomMessage(type = "recheckLayers", message = list())
        
        # also try to zoom to theme set so UX feels responsive
        tlm2 <- theme_landmarks_sf()
        if (nrow(tlm2) > 0) {
          bb <- sf::st_bbox(tlm2)
          map <- fitBounds(
            map,
            bb[["xmin"]], bb[["ymin"]],
            bb[["xmax"]], bb[["ymax"]],
            options = list(padding = c(50,50))
          )
        }
      }
    },
    ignoreInit = FALSE
  )
  
  # ---- info summary under map
  output$info_summary <- renderUI({
    # If user hasn't selected lm_name, show helpful text for theme browsing
    if (length(input$lm_name) == 0) {
      tlm <- theme_landmarks_sf()
      count_lm <- nrow(tlm)
      return(tags$div(
        class = "alert alert-info",
        role  = "alert",
        tags$strong("Browsing theme: "),
        sprintf(
          "%s — showing %d landmark%s on the map. Select one (or more) to see parking radius.",
          ifelse(input$theme_filter == "All", "All Themes", input$theme_filter),
          count_lm,
          ifelse(count_lm == 1, "", "s")
        )
      ))
    }
    
    # else, user has chosen at least 1 landmark
    zones_sf <- zones_in_radius()
    if (nrow(zones_sf) == 0) {
      return(tags$div(
        class = "alert alert-warning",
        role  = "alert",
        tags$strong("No parking zones found "),
        sprintf(
          "within %d meters of selected landmark(s). Try increasing the radius.",
          input$radius_m
        )
      ))
    }
    
    total_zones <- nrow(zones_sf)
    tags$div(
      class = "alert alert-success",
      role  = "alert",
      tags$strong("Search Results: "),
      sprintf(
        "Found %d parking zone%s within %d meters of %d selected landmark%s.",
        total_zones,
        ifelse(total_zones == 1, "", "s"),
        input$radius_m,
        length(input$lm_name),
        ifelse(length(input$lm_name) == 1, "", "s")
      )
    )
  })
  
  # ---- filtered_landmarks_pop for plots
  filtered_landmarks_pop <- reactive({
    if (input$theme_filter == "All") {
      landmark_popularity
    } else {
      landmark_popularity %>%
        filter(Theme == input$theme_filter)
    }
  })
  
  # ---- Ranking View plot
  output$popularity_plot <- renderPlotly({
    df <- filtered_landmarks_pop() %>%
      arrange(desc(nearest_count)) %>%
      head(15)
    
    df$Name <- factor(
      df$Name,
      levels = df$Name[order(df$nearest_count, decreasing = TRUE)]
    )
    df$Name <- forcats::fct_rev(df$Name)
    
    p <- ggplot(df, aes(x = Name, y = nearest_count, fill = Theme)) +
      geom_col() +
      coord_flip() +
      labs(
        x = "Landmark",
        y = "Avg Pedestrian Count (nearest sensor)",
        title = "Top 15 Landmarks by Nearby Pedestrian Volume"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # ---- Trend View plot
  output$trend_plot <- renderPlotly({
    selected_sensors <- filtered_landmarks_pop()$nearest_sensor
    
    trend_theme <- ped_raw %>%
      filter(Sensor_Name %in% selected_sensors) %>%
      mutate(Date = as.Date(Sensing_Date)) %>%
      group_by(Date) %>%
      summarise(
        Total_Pedestrians = sum(Total_of_Directions, na.rm = TRUE),
        .groups = "drop_last"
      ) %>%
      arrange(Date)
    
    plot_ly(
      data = trend_theme,
      x    = ~Date,
      y    = ~Total_Pedestrians,
      type = 'scatter',
      mode = 'lines+markers',
      line   = list(color = "#007BFF", width = 2),
      marker = list(size = 4, color = "#004080")
    ) %>%
      layout(
        title = paste0(
          "Pedestrian Flow Over Time — ",
          ifelse(input$theme_filter == "All",
                 "All Landmarks",
                 input$theme_filter)
        ),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Total Pedestrians"),
        hovermode = "x unified"
      )
  })
  # ---- NEW FEATURE: Click landmark dot to select it ----
  observeEvent(input$map_parking_marker_click, {
    click <- input$map_parking_marker_click
    req(click$id)
    
    # Identify if click corresponds to a landmark
    lm_sf <- theme_landmarks_sf()
    if (nrow(lm_sf) == 0) return(NULL)
    
    # Find landmark by name or ID (depending on how your layer is drawn)
    clicked_name <- click$id %||% click$label %||% click$popup
    match_idx <- which(
      tolower(lm_sf$name) == tolower(clicked_name)
    )
    
    if (length(match_idx) == 0) {
      # fallback: nearest point if click didn't match exact text
      coords <- c(click$lng, click$lat)
      distances <- sf::st_distance(lm_sf, sf::st_sfc(sf::st_point(coords), crs = 4326))
      match_idx <- which.min(distances)
    }
    
    if (length(match_idx) > 0) {
      clicked_lm <- lm_sf[match_idx, ]
      
      # --- update left panel selection ---
      updateSelectizeInput(
        session,
        "lm_name",
        selected = clicked_lm$name
      )
      
      # --- update theme (if not matching current) ---
      if (!is.null(clicked_lm$theme) &&
          clicked_lm$theme != input$theme_filter &&
          clicked_lm$theme %in% unique(landmark_popularity$Theme)) {
        updateSelectInput(session, "theme_filter", selected = clicked_lm$theme)
      }
      
      # --- draw buffer and show parking + public transport ---
      map <- leafletProxy("map_parking")
      
      # Clear old layers
      map <- clearGroup(map, "Selected Landmarks")
      map <- clearGroup(map, "Near by Parking")
      map <- clearGroup(map, "Buffer")
      map <- clearGroup(map, "Nearby Public Transport")
      
      # Selected landmark
      map <- addCircleMarkers(
        map,
        data = clicked_lm,
        radius = 8,
        stroke = TRUE,
        weight = 2,
        fillOpacity = 0.9,
        fillColor = "#e31a1c",
        color = "#fff",
        label = ~name,
        popup = ~paste0("<b>", name, "</b><br>Theme: ", theme),
        group = "Selected Landmarks"
      )
      
      # Buffer
      buf <- tryCatch({
        sel_m <- st_transform(clicked_lm, 7899)
        st_transform(st_buffer(sel_m, dist = input$radius_m), 4326)
      }, error = function(e) NULL)
      
      if (!is.null(buf) && nrow(buf) > 0) {
        map <- addPolygons(
          map,
          data = buf,
          fill = TRUE,
          fillOpacity = 0.08,
          color = "#1f78b4",
          weight = 2,
          group = "Buffer"
        )
      }
      
      # Nearby parking zones
      if (!is.null(zones_display) && nrow(zones_display) > 0 && !is.null(buf)) {
        zones_near <- zones_display[sf::st_intersects(zones_display, buf, sparse = FALSE), ]
        if (nrow(zones_near) > 0) {
          zones_near <- create_zone_popups_with_coords(zones_near, sign_plates)
          map <- addAwesomeMarkers(
            map,
            data = zones_near,
            icon = awesomeIcons(
              icon = "car",
              library = "fa",
              markerColor = "red",
              iconColor  = "white"
            ),
            label = ~paste0("Zone ", zone_id),
            popup = ~popup_html,
            group = "Near by Parking",
            clusterOptions = markerClusterOptions()
          )
        }
      }
      
      # Nearby public transport (same radius)
      if (exists("stops_m") && exists("stations_m") && nrow(stops_m) > 0) {
        sel_m <- st_transform(clicked_lm, 3857)
        radius_pt <- input$radius_m
        stops_near <- stops[as.numeric(st_distance(stops_m, sel_m)) <= radius_pt, ]
        stations_near <- stations[as.numeric(st_distance(stations_m, sel_m)) <= radius_pt, ]
        
        if (nrow(stops_near) > 0) {
          stops_near <- stops_near %>%
            mutate(
              iconColorFinal = "white",
              markerColorFinal = case_when(
                grepl("BUS", mode, ignore.case = TRUE) ~ "darkgreen",
                grepl("TRAM", mode, ignore.case = TRUE) ~ "lightgreen",
                grepl("TRAIN", mode, ignore.case = TRUE) ~ "blue",
                TRUE ~ "gray"
              )
            )
          map <- addAwesomeMarkers(
            map,
            data = stops_near,
            lng = ~X, lat = ~Y,
            icon = ~awesomeIcons(
              library = "fa",
              markerColor = markerColorFinal,
              icon = ~icon,
              iconColor = iconColorFinal
            ),
            label = ~name,
            popup = ~paste0("<b>", name, "</b><br>Mode: ", mode),
            group = "Nearby Public Transport",
            clusterOptions = markerClusterOptions()
          )
        }
        
        if (nrow(stations_near) > 0) {
          map <- addAwesomeMarkers(
            map,
            data = stations_near,
            lng = ~Stop_long, lat = ~Stop_lat,
            icon = awesomeIcons(
              library = "fa",
              markerColor = "blue",
              icon = "subway",
              iconColor = "white"
            ),
            label = ~station,
            popup = ~paste0("<b>", station, "</b><br>Annual Entries: ", annual),
            group = "Nearby Public Transport",
            clusterOptions = markerClusterOptions()
          )
        }
      }
      
      # Finally, zoom to buffer bounds
      if (!is.null(buf) && nrow(buf) > 0) {
        bb <- st_bbox(buf)
        map <- fitBounds(map, bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
      }
    }
  })
  # ------------------------------------------------------------
  # ---- Melbourne Crime Server ----
  # ------------------------------------------------------------
  
  # Overview
  output$total_offences <- renderValueBox({
    total <- sum(crime_data$table01$`Offence Count`, na.rm = TRUE)
    valueBox(format(total, big.mark = ","), "Total Offences",
             icon = icon("exclamation-triangle"), color = "red")
  })
  
  output$crime_rate <- renderValueBox({
    rate <- mean(crime_data$table01$`Rate per 100,000 population`, na.rm = TRUE)
    valueBox(round(rate, 1), "Crime Rate per 100,000",
             icon = icon("chart-line"), color = "orange")
  })
  
  output$police_region <- renderValueBox({
    region <- unique(crime_data$table01$`Police Region`)[1]
    valueBox(region, "Police Region",
             icon = icon("shield"), color = "blue")
  })
  
  output$overview_summary <- renderPrint({
    summary(crime_data$table01)
  })
  
  # Offence Categories
  output$category_plot <- renderPlotly({
    category_data <- crime_data$table02 %>%
      group_by(!!sym(input$category_level)) %>%
      summarise(Total = sum(`Offence Count`, na.rm = TRUE)) %>%
      arrange(desc(Total)) %>%
      rename(Category = !!sym(input$category_level))
    plot_ly(category_data,
            x = ~reorder(Category, Total),
            y = ~Total, type = 'bar',
            marker = list(color = 'steelblue')) %>%
      layout(title = paste("Offences by", input$category_level),
             xaxis = list(title = input$category_level, tickangle = -45),
             yaxis = list(title = "Number of Offences"))
  })
  
  output$top_categories <- renderPlotly({
    category_data <- crime_data$table02 %>%
      group_by(!!sym(input$category_level)) %>%
      summarise(Total = sum(`Offence Count`, na.rm = TRUE)) %>%
      arrange(desc(Total)) %>% head(10) %>%
      rename(Category = !!sym(input$category_level))
    plot_ly(category_data, labels = ~Category, values = ~Total, type = 'pie',
            textposition = 'inside', textinfo = 'label+percent') %>%
      layout(title = "Top 10 Categories")
  })
  
  output$category_rate <- renderPlotly({
    rate_data <- crime_data$table02 %>%
      group_by(!!sym(input$category_level)) %>%
      summarise(AvgRate = mean(`LGA Rate per 100,000 population`, na.rm = TRUE)) %>%
      arrange(desc(AvgRate)) %>% head(10) %>%
      rename(Category = !!sym(input$category_level))
    plot_ly(rate_data,
            x = ~AvgRate, y = ~reorder(Category, AvgRate),
            type = 'bar', orientation = 'h',
            marker = list(color = 'coral')) %>%
      layout(title = "Crime Rate by Category (Top 10)",
             xaxis = list(title = "Rate per 100,000"), yaxis = list(title = ""))
  })
  
  # Suburbs Analysis
  output$suburb_plot <- renderPlotly({
    suburb_data <- crime_data$table03
    if (input$suburb_category != "All") {
      suburb_data <- suburb_data %>% filter(`Offence Division` == input$suburb_category)
    }
    suburb_summary <- suburb_data %>%
      group_by(`Suburb/Town Name`) %>%
      summarise(Total = sum(`Offence Count`, na.rm = TRUE)) %>%
      arrange(desc(Total))
    plot_ly(suburb_summary,
            x = ~reorder(`Suburb/Town Name`, Total),
            y = ~Total, type = 'bar', marker = list(color = 'darkgreen')) %>%
      layout(title = "Offences by Suburb",
             xaxis = list(title = "Suburb", tickangle = -45),
             yaxis = list(title = "Number of Offences"))
  })
  
  output$top_suburbs <- renderPlotly({
    suburb_data <- crime_data$table03 %>%
      group_by(`Suburb/Town Name`) %>%
      summarise(Total = sum(`Offence Count`, na.rm = TRUE)) %>%
      arrange(desc(Total)) %>% head(10)
    plot_ly(suburb_data,
            y = ~reorder(`Suburb/Town Name`, Total),
            x = ~Total, type = 'bar', orientation = 'h',
            marker = list(color = 'forestgreen')) %>%
      layout(title = "Top 10 Suburbs")
  })
  
  output$postcode_plot <- renderPlotly({
    postcode_data <- crime_data$table03 %>%
      group_by(Postcode) %>%
      summarise(Total = sum(`Offence Count`, na.rm = TRUE)) %>%
      arrange(desc(Total))
    plot_ly(postcode_data,
            x = ~as.factor(Postcode), y = ~Total,
            type = 'bar', marker = list(color = 'teal')) %>%
      layout(title = "Offences by Postcode")
  })
  
  # Location Types
  output$location_plot <- renderPlotly({
    location_data <- crime_data$table04 %>%
      group_by(!!sym(input$location_level)) %>%
      summarise(Total = sum(`Offence Count`, na.rm = TRUE)) %>%
      rename(Location = !!sym(input$location_level))
    plot_ly(location_data, x = ~reorder(Location, Total), y = ~Total,
            type = 'bar', marker = list(color = 'purple')) %>%
      layout(title = paste("Offences by", input$location_level))
  })
  
  output$location_sunburst <- renderPlotly({
    level1 <- crime_data$table04 %>%
      group_by(`Location Division`) %>%
      summarise(Total = sum(`Offence Count`, na.rm = TRUE), .groups = 'drop') %>%
      mutate(labels = `Location Division`, parents = "")
    level2 <- crime_data$table04 %>%
      group_by(`Location Division`, `Location Subdivision`) %>%
      summarise(Total = sum(`Offence Count`, na.rm = TRUE), .groups = 'drop') %>%
      mutate(labels = `Location Subdivision`, parents = `Location Division`)
    level3 <- crime_data$table04 %>%
      group_by(`Location Subdivision`, `Location Group`) %>%
      summarise(Total = sum(`Offence Count`, na.rm = TRUE), .groups = 'drop') %>%
      mutate(labels = `Location Group`, parents = `Location Subdivision`)
    sunburst_data <- bind_rows(level1, level2, level3) %>% select(labels, parents, Total)
    plot_ly(sunburst_data, labels = ~labels, parents = ~parents, values = ~Total,
            type = 'sunburst', branchvalues = 'total')
  })
  
  # Investigation
  output$investigation_pie <- renderPlotly({
    investigation_data <- crime_data$table05 %>%
      group_by(`Investigation Status`) %>%
      summarise(Total = sum(`Offence Count`, na.rm = TRUE))
    plot_ly(investigation_data,
            labels = ~`Investigation Status`, values = ~Total,
            type = 'pie', textposition = 'inside', textinfo = 'label+percent')
  })
  
  output$investigation_bar <- renderPlotly({
    investigation_data <- crime_data$table05 %>%
      group_by(`Investigation Status`) %>%
      summarise(Total = sum(`Offence Count`, na.rm = TRUE))
    plot_ly(investigation_data,
            x = ~`Investigation Status`, y = ~Total,
            type = 'bar', marker = list(color = 'indianred'))
  })
  
  output$investigation_table <- renderDT({
    investigation_data <- crime_data$table05 %>%
      group_by(`Investigation Status`) %>%
      summarise(`Total Offences` = sum(`Offence Count`, na.rm = TRUE),
                `Percentage` = round(sum(`Offence Count`, na.rm = TRUE) /
                                       sum(crime_data$table05$`Offence Count`, na.rm = TRUE) * 100, 2))
    datatable(investigation_data, options = list(pageLength = 10))
  })
  
  # Drug Offences
  output$drug_plot <- renderPlotly({
    drug_data <- crime_data$table06 %>%
      group_by(!!sym(input$drug_filter)) %>%
      summarise(Total = sum(`Offence Count`, na.rm = TRUE)) %>%
      rename(DrugCategory = !!sym(input$drug_filter))
    plot_ly(drug_data,
            x = ~reorder(DrugCategory, Total), y = ~Total,
            type = 'bar', marker = list(color = 'darkred'))
  })
  
  output$drug_type_pie <- renderPlotly({
    drug_data <- crime_data$table06 %>%
      group_by(`CSA Drug Type`) %>%
      summarise(Total = sum(`Offence Count`, na.rm = TRUE))
    plot_ly(drug_data, labels = ~`CSA Drug Type`, values = ~Total, type = 'pie')
  })
  
  output$drug_category_bar <- renderPlotly({
    drug_data <- crime_data$table06 %>%
      group_by(`Offence Subdivision`) %>%
      summarise(Total = sum(`Offence Count`, na.rm = TRUE))
    plot_ly(drug_data, y = ~reorder(`Offence Subdivision`, Total),
            x = ~Total, type = 'bar', orientation = 'h')
  })
  
  output$data_table <- renderDT({
    selected_table <- crime_data[[input$table_select]]
    datatable(selected_table, options = list(pageLength = 25, scrollX = TRUE))
  })
  
  # --- Melbourne Crime Navigation Logic ---
  observeEvent(input$nav_overview, {
    output$crime_main_panel <- renderUI({
      tagList(
        h3("Overview"),
        br(),
        fluidRow(
          valueBoxOutput("total_offences", width = 4),
          valueBoxOutput("crime_rate", width = 4),
          valueBoxOutput("police_region", width = 4)
        ),
        br(),
        verbatimTextOutput("overview_summary")
      )
    })
  })
  
  observeEvent(input$nav_categories, {
    output$crime_main_panel <- renderUI({
      tagList(
        h3("Offence Categories"),
        selectInput("category_level", "Category Level:",
                    choices = c("Offence Division", "Offence Subdivision", "Offence Subgroup"),
                    selected = "Offence Division"),
        plotlyOutput("category_plot", height = 500),
        fluidRow(
          column(6, plotlyOutput("top_categories", height = 400)),
          column(6, plotlyOutput("category_rate", height = 400))
        )
      )
    })
  })
  
  observeEvent(input$nav_suburbs, {
    output$crime_main_panel <- renderUI({
      tagList(
        h3("Suburbs Analysis"),
        selectInput("suburb_category", "Offence Category:",
                    choices = c("All", unique(crime_data$table03$`Offence Division`)),
                    selected = "All"),
        plotlyOutput("suburb_plot", height = 500),
        fluidRow(
          column(6, plotlyOutput("top_suburbs", height = 400)),
          column(6, plotlyOutput("postcode_plot", height = 400))
        )
      )
    })
  })
  
  observeEvent(input$nav_locations, {
    output$crime_main_panel <- renderUI({
      tagList(
        h3("Location Types"),
        selectInput("location_level", "Location Level:",
                    choices = c("Location Division", "Location Subdivision", "Location Group"),
                    selected = "Location Division"),
        plotlyOutput("location_plot", height = 500),
        plotlyOutput("location_sunburst", height = 600)
      )
    })
  })
  
  observeEvent(input$nav_investigation, {
    output$crime_main_panel <- renderUI({
      tagList(
        h3("Investigation Status"),
        fluidRow(
          column(6, plotlyOutput("investigation_pie", height = 400)),
          column(6, plotlyOutput("investigation_bar", height = 400))
        ),
        DTOutput("investigation_table")
      )
    })
  })
  
  observeEvent(input$nav_drugs, {
    output$crime_main_panel <- renderUI({
      tagList(
        h3("Drug Offences"),
        selectInput("drug_filter", "Drug Filter By:",
                    choices = c("Offence Subdivision", "Offence Group", "CSA Drug Type"),
                    selected = "CSA Drug Type"),
        plotlyOutput("drug_plot", height = 500),
        fluidRow(
          column(6, plotlyOutput("drug_type_pie", height = 400)),
          column(6, plotlyOutput("drug_category_bar", height = 400))
        )
      )
    })
  })
  
  observeEvent(input$nav_tables, {
    output$crime_main_panel <- renderUI({
      tagList(
        h3("Data Tables"),
        selectInput("table_select", "Choose Table:",
                    choices = c("Table 01: Overview" = "table01",
                                "Table 02: Offence Categories" = "table02",
                                "Table 03: Suburbs" = "table03",
                                "Table 04: Location Types" = "table04",
                                "Table 05: Investigation" = "table05",
                                "Table 06: Drug Offences" = "table06"),
                    selected = "table01"),
        DTOutput("data_table")
      )
    })
  })
  
  # --- Default display (Overview when first opened) ---
  output$crime_main_panel <- renderUI({
    tagList(
      h3("Overview"),
      br(),
      fluidRow(
        column(4, valueBoxOutput("total_offences")),
        column(4, valueBoxOutput("crime_rate")),
        column(4, valueBoxOutput("police_region"))
      ),
      br(),
      verbatimTextOutput("overview_summary")
    )
  })
  
}


# -------------------------
# 18) RUN APP
# -------------------------
shinyApp(ui, server)
