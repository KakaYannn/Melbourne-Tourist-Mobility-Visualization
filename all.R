library(shiny)
library(sf)
library(dplyr)
library(stringr)
library(leaflet)
library(ggplot2)
library(ggiraph)

library(readxl)
library(readr)
library(leaflet.extras)
library(shinythemes)
library(janitor)
library(tidyr)
library(geosphere)
library(plotly)
library(forcats)
library(lubridate)

library(bslib)
library(readr)
library(scales)
library(DT)
library(shinydashboard)



# --- Map Preprocess ---

# read data
boundary <- st_read('./municipal-boundary.shp')
pois <- st_read('./landmarks-and-places-of-interest-including-schools-theatres-health-services-spor.geojson')

# add coord column
pois <- cbind(pois, st_coordinates(pois))

# rename columns
colnames(pois) <- c('type', 'subtype', 'name', 'lon', 'lat', 'geometry')

# filter poi types
pois <- pois[!pois$type %in% c('Education Centre', 'Residential Accommodation', 'Transport', 'Vacant Land'), ]
pois[pois$type == 'Mixed Use', c('type', 'subtype')] <- 'Retail'

# add unique id column
pois$id <- paste0('poi', seq_len(nrow(pois)))

# add colours and icons for pois
poiscolour <- setNames(c('lightblue', 'purple', 'darkpurple', 'black', 'black', 
                         'purple', 'beige', 'purple', 'red', 'pink', 
                         'cadetblue', 'lightred', 'black', 'pink', 'darkred', 
                         'lightgreen', 'lightred', 'darkgreen', 'red', 'lightblue', 
                         'cadetblue', 'lightgreen', 'darkblue', 'red', 'darkgreen', 
                         'pink', 'red', 'beige', 'darkred', 'black', 
                         'purple', 'beige'), 
                       c('Aquarium', 'Art Gallery/Museum', 'Casino', 'Cemetery', 'Church', 
                         'Cinema', 'Department Store', 'Film & RV Studio', 'Fire Station', 'Function/Conference/Exhibition Centre', 
                         'Government Building', 'Gymnasium/Health Club', 'Hostel', 'Indoor Recreation Facility', 'Industrial (Manufacturing)', 
                         'Informal Outdoor Facility (Park/Garden/Reserve)', 'Library', 'Major Sports & Recreation Facility', 'Medical Services', 'Observation Tower/Wheel', 
                         'Office', 'Outdoor Recreation Facility (Zoo, Golf Course)', 'Police Station', 'Private Hospital', 'Private Sports Club/Facility', 
                         'Public Buildings', 'Public Hospital', 'Retail', 'Store Yard', 'Synagogue', 
                         'Theatre Live', 'Visitor Centre'))
poisicon <- setNames(c('anchor', 'film', 'gift', 'crosshairs', 'user', 
                       'film', 'shopping-cart', 'film', 'fire', 'flag', 
                       'building', 'bolt', 'bed', 'gamepad', 'industry', 
                       'leaf', 'book', 'futbol-o', 'hospital-o', 'eye', 
                       'building', 'paw', 'car', 'hospital-o', 'futbol-o', 
                       'building', 'hospital-o', 'shopping-cart', 'truck', 'user', 
                       'film', 'info-circle'), 
                     c('Aquarium', 'Art Gallery/Museum', 'Casino', 'Cemetery', 'Church', 
                       'Cinema', 'Department Store', 'Film & RV Studio', 'Fire Station', 'Function/Conference/Exhibition Centre', 
                       'Government Building', 'Gymnasium/Health Club', 'Hostel', 'Indoor Recreation Facility', 'Industrial (Manufacturing)', 
                       'Informal Outdoor Facility (Park/Garden/Reserve)', 'Library', 'Major Sports & Recreation Facility', 'Medical Services', 'Observation Tower/Wheel', 
                       'Office', 'Outdoor Recreation Facility (Zoo, Golf Course)', 'Police Station', 'Private Hospital', 'Private Sports Club/Facility', 
                       'Public Buildings', 'Public Hospital', 'Retail', 'Store Yard', 'Synagogue', 
                       'Theatre Live', 'Visitor Centre'))
pois$colour <- unname(poiscolour[pois$subtype])
pois$icon <- unname(poisicon[pois$subtype])

# copy dataframes in epsg 3857 for distance measurements
pois2 <- st_transform(pois, 3857)



# --- PT Preprocess ---

# read data
stops <- st_read('./public_transport_stops.geojson')
stations <- read.csv('./annual_metropolitan_train_station_entries_fy_2024_2025.csv')
lines <- st_read('./public_transport_lines.geojson')

# add coord columns for stops
stops <- cbind(stops, st_coordinates(stops))

# add geometry column for stations
stations <- st_as_sf(stations, coords = c('Stop_long', 'Stop_lat'), crs = 4326, remove = FALSE)

# rename columns
colnames(stops) <- c('id', 'name', 'mode', 'lon', 'lat', 'geometry')
colnames(stations) <- c('year', 'id', 'station', 'lat', 'lon', 
                        'annual', 'weekday', 'normalweekday', 'schholweekday', 'saturday', 'sunday', 
                        'early', 'ampeak', 'interpeak', 'pmpeak', 'late', 'geometry')
colnames(lines) <- c('id', 'destination', 'shortname', 'longname', 'mode', 'geometry')

# filter modes
stops <- stops[stops$mode %in% c('METRO TRAM', 'METRO BUS'), ]
lines <- lines[lines$mode %in% c('METRO TRAIN', 'METRO TRAM', 'METRO BUS'), ]

# filter to within bounds
stops <- st_filter(stops, boundary, .predicate = st_within)
stations <- st_filter(stations, boundary, .predicate = st_within)
lines <- st_filter(lines, boundary, .predicate = st_intersects)

# filter redundant lines
flem <- lines[lines$destination == 'Flemington Racecourse', ]
lines <- lines %>%
  mutate(
    firstword = sub("^(\\S+).*", "\\1", longname),
    cityloop = paste0(firstword, " via City Loop"),
    type = case_when(
      destination == cityloop ~ 'cityloop',
      str_detect(longname, fixed(destination)) ~ 'contained', 
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(type)) %>%
  group_by(shortname, type) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(-firstword, -cityloop, -type)
lines <- lines[lines$shortname != 'Replacement Bus', ]
lines <- rbind(lines, flem)

# add unique id columns
stops$id <- paste0('stop', seq_len(nrow(stops)))
stations$id <- paste0('station', seq_len(nrow(stations)))
lines$id<- paste0('line', seq_len(nrow(lines)))

# add colours and icons for transport modes
mapcolour <- setNames(c('blue', 'green', 'orange'), c('METRO TRAIN', 'METRO TRAM', 'METRO BUS'))
mapicon <- setNames(c('subway', 'train', 'bus'), c('METRO TRAIN', 'METRO TRAM', 'METRO BUS'))
stops$colour <- unname(mapcolour[stops$mode])
stops$icon <- unname(mapicon[stops$mode])
stations$colour <- rep('blue', nrow(stations))
stations$icon <- rep('subway', nrow(stations))
lines$colour <- unname(mapcolour[lines$mode])
linecolour <- c('Sunbury' = '#ffbe00', 'Craigieburn' = '#ffbe00', 'Upfield' = '#ffbe00', 
                'Mernda' = '#be1014', 'Hurstbridge' = '#be1014', 
                'Lilydale' = '#152c6b', 'Belgrave' = '#152c6b', 'Alamein' = '#152c6b', 'Glen Waverley' = '#152c6b', 
                'Pakenham' = '#279fd5', 'Cranbourne' = '#279fd5', 
                'Frankston' = '#028430', 'Werribee' = '#028430', 'Williamstown' = '#028430', 
                'Sandringham' = '#f178af', 'Flemington Racecourse' = '#95979a')
lines$colour[lines$mode == 'METRO TRAIN'] <- linecolour[lines$shortname[lines$mode == 'METRO TRAIN']]

# copy dataframes in epsg 3857 for distance measurements
stops2 <- st_transform(stops, 3857)
stations2 <- st_transform(stations, 3857)
lines2 <- st_transform(lines, 3857)



# --- Crime Preprocess ---
# Load and preprocess data
load_data <- function() {
  file_path <- "Data_Tables_LGA_Recorded_Offences_Year_Ending_June_2025.xlsx"
  
  # Table 01: Overall offence counts by LGA
  table01 <- read_excel(file_path, sheet = "Table 01") %>%
    filter(`Local Government Area` == "Melbourne")
  
  # Table 02: Offences by category (Division, Subdivision, Subgroup)
  table02 <- read_excel(file_path, sheet = "Table 02") %>%
    filter(`Local Government Area` == "Melbourne")
  
  # Table 03: Offences by suburb/postcode
  table03 <- read_excel(file_path, sheet = "Table 03") %>%
    filter(`Local Government Area` == "Melbourne")
  
  # Table 04: Offences by location type
  table04 <- read_excel(file_path, sheet = "Table 04") %>%
    filter(`Local Government Area` == "Melbourne")
  
  # Table 05: Investigation status
  table05 <- read_excel(file_path, sheet = "Table 05") %>%
    filter(`Local Government Area` == "Melbourne")
  
  # Table 06: Drug offences
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

# Load data
crime_data <- load_data()



# --- Pedestrian Preprocess ---

# 1) File paths
ped_counts_path <- "./pedestrian-counting-system-monthly-counts-per-hour.xlsx"
sensor_csv_local <- "./pedestrian-counting-system-sensor-locations.csv"

# 3) Load & summarise pedestrian counts
ped_raw <- read_excel(ped_counts_path)
ped_by_sensor <- ped_raw %>%
  rename(
    Sensor_Name = Sensor_Name,
    Total = Total_of_Directions
  ) %>%
  group_by(Sensor_Name) %>%
  summarise(Avg_Count = mean(Total, na.rm = TRUE), .groups = "drop")

# 4) Load sensor locations (semicolon-separated)
sensor_locations <- read_delim(sensor_csv_local, delim = ";", show_col_types = FALSE)
sensors_geo <- sensor_locations %>%
  clean_names() %>%
  rename(
    Location_ID = location_id,
    Sensor_Name = sensor_name,
    Latitude = latitude,
    Longitude = longitude
  ) %>%
  select(Location_ID, Sensor_Name, Latitude, Longitude) %>%
  distinct()

# 5) Join pedestrian data with coordinates
ped_geo <- ped_by_sensor %>%
  left_join(sensors_geo, by = "Sensor_Name") %>%
  filter(!is.na(Latitude), !is.na(Longitude))

# 6) Compute Landmark Popularity (nearest sensor)
landmark_popularity <- pois %>%
  rowwise() %>%
  mutate(
    nearest_sensor = {
      distances <- distHaversine(
        cbind(ped_geo$Longitude, ped_geo$Latitude),
        c(lon, lat)
      )
      idx <- which.min(distances)
      ped_geo$Sensor_Name[idx]
    },
    nearest_count = {
      distances <- distHaversine(
        cbind(ped_geo$Longitude, ped_geo$Latitude),
        c(lon, lat)
      )
      idx <- which.min(distances)
      ped_geo$Avg_Count[idx]
    }
  ) %>%
  ungroup() %>%
  distinct(name, .keep_all = TRUE) %>%
  arrange(desc(nearest_count))



# --- Parking Preprocess ---

# safe readers
read_sf_cached <- function(url, local_path = NULL) {
  tryCatch({
    if (!is.null(local_path) && file.exists(local_path)) {
      return(sf::st_read(local_path, quiet = TRUE))
    }
    sf::st_read(url, quiet = TRUE)
  }, error = function(e) {
    message("SF read failed: ", conditionMessage(e))
    # return empty sf on failure to keep the app alive
    return(sf::st_sf())
  })
}

norm_col <- function(df, candidates, new) {
  hit <- intersect(names(df), candidates)
  if (length(hit) >= 1) {
    df %>% dplyr::rename(!!new := dplyr::all_of(hit[1]))
  } else df
}

# data endpoints
URL_BAYS     <- "https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/on-street-parking-bays/exports/geojson?limit=-1"
URL_ZONES_TO_SEGMENTS <- "https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/parking-zones-linked-to-street-segments/exports/csv?limit=-1"
URL_SIGN_PLATES <- "https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/sign-plates-located-in-each-parking-zone/exports/csv?limit=-1"

# preload layers
bays <- read_sf_cached(URL_BAYS)
if (nrow(bays) > 0) {
  bays <- bays |> sf::st_make_valid() |> sf::st_transform(4326)
}

bays  <- if (nrow(bays) > 0)  norm_col(bays,  c("bay_id","bayid","bayidentifier","id","BAY_ID","kerbsideid"), "bay_id") else bays

# Convert bay_id and roadsegmentid to character for consistent joining
if (nrow(bays) > 0 && "bay_id" %in% names(bays)) {
  bays <- bays |> dplyr::mutate(bay_id = as.character(bay_id))
}

if (nrow(bays) > 0 && "roadsegmentid" %in% names(bays)) {
  bays <- bays |> dplyr::mutate(roadsegmentid = as.character(roadsegmentid))
}

# load zone-based clustering datasets
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

# Normalize column names for zones and sign plates
if (nrow(zones_to_segments) > 0) {
  zones_to_segments <- zones_to_segments |>
    norm_col(c("segment_id", "segmentid", "roadsegmentid", "road_segment_id"), "segment_id") |>
    norm_col(c("parkingzone", "parking_zone", "zone_id", "zoneid"), "zone_id") |>
    dplyr::mutate(
      segment_id = as.character(segment_id),
      zone_id = as.character(zone_id)
    )
  cat("Loaded", nrow(zones_to_segments), "zone-to-segment mappings\n")
}

if (nrow(sign_plates) > 0) {
  sign_plates <- sign_plates |>
    norm_col(c("parkingzone", "parking_zone", "zone_id", "zoneid"), "zone_id") |>
    dplyr::mutate(zone_id = as.character(zone_id))
  # Don't rename restriction_display - keep original column names
  cat("Loaded", nrow(sign_plates), "sign plate records\n")
}

# PHASE 2: AGGREGATE BAYS TO SEGMENTS
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

# PHASE 3: MAP SEGMENTS TO ZONES
cat("PHASE 3: Mapping segments to parking zones...\n")
zone_locations <- NULL
if (!is.null(segment_locations) && nrow(segment_locations) > 0 && nrow(zones_to_segments) > 0) {
  # Join segments to zones
  segment_zone_map <- sf::st_drop_geometry(segment_locations) |>
    dplyr::select(segment_id, num_bays) |>
    dplyr::left_join(zones_to_segments, by = "segment_id", relationship = "many-to-many") |>
    dplyr::filter(!is.na(zone_id))
  
  cat("Matched", nrow(segment_zone_map), "segment-zone pairs\n")
  
  # Re-attach geometry and aggregate by zone
  zone_locations <- segment_zone_map |>
    dplyr::left_join(
      segment_locations |> dplyr::select(segment_id, segment_geometry),
      by = "segment_id"
    ) |>
    sf::st_as_sf(sf_column_name = "segment_geometry") |>
    dplyr::group_by(zone_id) |>
    dplyr::summarise(
      segment_count = dplyr::n(),
      zone_geometry = sf::st_centroid(sf::st_union(segment_geometry)),
      .groups = "drop"
    ) |>
    sf::st_as_sf(sf_column_name = "zone_geometry")
  
  cat("Created", nrow(zone_locations), "zone locations\n")
}

# PHASE 3.5: HANDLE UNMATCHED SEGMENTS (segments with no zone)
cat("PHASE 3.5: Processing unmatched segments (no zone data)...\n")
unmatched_segments <- NULL
if (!is.null(segment_locations) && nrow(segment_locations) > 0) {
  # Find segments that don't have zone mappings
  matched_segment_ids <- character(0)
  if (!is.null(zone_locations) && nrow(zone_locations) > 0 && nrow(zones_to_segments) > 0) {
    matched_segment_ids <- unique(zones_to_segments$segment_id)
  }
  
  # Filter for unmatched segments
  unmatched_segments <- segment_locations |>
    dplyr::filter(!segment_id %in% matched_segment_ids) |>
    dplyr::mutate(
      zone_id = paste0("NO_ZONE_", segment_id),  # Create pseudo zone ID
      restriction_days = "N/A",
      time_restrictions_start = "N/A",
      time_restrictions_finish = "N/A",
      restriction_display = "Zone ID not found. This area may not require a parking ticket. Please check the nearby sign plate for specific restrictions."
    ) |>
    dplyr::rename(zone_geometry = segment_geometry) |>
    sf::st_as_sf(sf_column_name = "zone_geometry")
  
  cat("Found", nrow(unmatched_segments), "unmatched segments (no zone data)\n")
}

# PHASE 4: PREPARE SIGN PLATES DATA FOR TABLE DISPLAY
cat("PHASE 4: Preparing restriction data for table display...\n")

# Function to create HTML table popup for a zone
create_zone_popup_table <- function(zone_id_val, sign_data, lat = NULL, lon = NULL) {
  # Filter sign plates for this zone
  zone_signs <- sign_data |> dplyr::filter(zone_id == zone_id_val)
  
  if (nrow(zone_signs) == 0) {
    return(paste0("<b>Zone ID:</b> ", zone_id_val, "<br><i>No restriction data available</i>"))
  }
  
  # Sort by weekday/weekend, then by start time
  zone_signs <- zone_signs |>
    dplyr::mutate(
      # Create sort key: 0 for weekdays, 1 for weekends
      day_order = ifelse(
        grepl("Mon|Tue|Wed|Thu|Fri|M-F|Mon-Fri", restriction_days, ignore.case = TRUE),
        0,  # Weekdays first
        1   # Weekends second
      ),
      # Convert time to sortable format (handle both HH:MM:SS and HH:MM)
      time_sort = as.character(time_restrictions_start)
    ) |>
    dplyr::arrange(day_order, time_sort) |>
    dplyr::select(-day_order, -time_sort)
  
  # Build HTML table
  html <- paste0("<b>Zone ID:</b> ", zone_id_val, "<br>")
  if (!is.null(lat) && !is.null(lon)) {
    html <- paste0(html, "<b>Location:</b> ", lat, ", ", lon, "<br>")
  }
  html <- paste0(html, "<b>Restrictions:</b><br>")
  html <- paste0(html, "<table style='border-collapse: collapse; width: 100%; font-size: 11px;'>")
  html <- paste0(html, "<tr style='background-color: #f0f0f0; font-weight: bold;'>",
                 "<th style='border: 1px solid #ddd; padding: 4px;'>Days</th>",
                 "<th style='border: 1px solid #ddd; padding: 4px;'>Start</th>",
                 "<th style='border: 1px solid #ddd; padding: 4px;'>Finish</th>",
                 "<th style='border: 1px solid #ddd; padding: 4px;'>Type</th>",
                 "</tr>")
  
  for (i in 1:nrow(zone_signs)) {
    row <- zone_signs[i, ]
    html <- paste0(html, "<tr>",
                   "<td style='border: 1px solid #ddd; padding: 4px;'>", row$restriction_days, "</td>",
                   "<td style='border: 1px solid #ddd; padding: 4px;'>", row$time_restrictions_start, "</td>",
                   "<td style='border: 1px solid #ddd; padding: 4px;'>", row$time_restrictions_finish, "</td>",
                   "<td style='border: 1px solid #ddd; padding: 4px;'>", row$restriction_display, "</td>",
                   "</tr>")
  }
  
  html <- paste0(html, "</table>")
  return(html)
}

# Helper function to create popups with coordinates for zones
create_zone_popups_with_coords <- function(zones_data, sign_data) {
  popup_list <- vector("character", nrow(zones_data))
  for (i in 1:nrow(zones_data)) {
    coords <- tryCatch({
      sf::st_coordinates(sf::st_geometry(zones_data[i, ]))
    }, error = function(e) { NULL })
    
    if (!is.null(coords) && nrow(coords) > 0) {
      lat <- round(coords[1, "Y"], 6)
      lon <- round(coords[1, "X"], 6)
      popup_list[i] <- create_zone_popup_table(zones_data$zone_id[i], sign_data, lat, lon)
    } else {
      popup_list[i] <- create_zone_popup_table(zones_data$zone_id[i], sign_data, NULL, NULL)
    }
  }
  zones_data$popup_html <- popup_list
  return(zones_data)
}

# Helper function to create popups for unmatched segments with coordinates
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

zones_display <- NULL
if (!is.null(zone_locations) && nrow(zone_locations) > 0) {
  # Just add has_zone marker, keep geometry
  zones_display <- zone_locations |>
    dplyr::mutate(has_zone = TRUE)
  
  cat("Prepared", nrow(zones_display), "zones for display with table popups\n")
}

# PHASE 4.5: COMBINE ZONES AND UNMATCHED SEGMENTS
cat("PHASE 4.5: Combining zones with unmatched segments...\n")
if (!is.null(unmatched_segments) && nrow(unmatched_segments) > 0) {
  # Add has_zone marker to unmatched segments
  unmatched_segments <- unmatched_segments |>
    dplyr::mutate(has_zone = FALSE)
  
  # Combine zones and unmatched segments
  if (!is.null(zones_display) && nrow(zones_display) > 0) {
    # Ensure both have same columns
    zones_display <- dplyr::bind_rows(zones_display, unmatched_segments)
    cat("Combined:", nrow(zones_display), "total markers (zones + unmatched segments)\n")
  } else {
    # If no zones_display, just use unmatched segments
    zones_display <- unmatched_segments
    cat("No zones found, using", nrow(zones_display), "unmatched segments only\n")
  }
}



# --- UI ---

ui <- navbarPage(
  id = 'mypage', 
  title = '',
  
  
  
  # --- PT Tab ---
  tabPanel(
    title = 'Public Transport', 
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = 'type', 
          label = 'POI Type', 
          choices = c('All', unique(pois$subtype)), 
          selected = 'All'
        ), 
        sliderInput(
          inputId = 'radius', 
          label = 'Search Radius (Metres): ', 
          min = 0, 
          max = 2000, 
          value = 500, 
          step = 50, 
          ticks = FALSE
        ), 
        width = 3, 
        style = 'max-width: 400px'
      ), 
      mainPanel(
        fluidRow(
          column(8, leafletOutput('mappt', width = 800, height = 800)), 
          column(4, uiOutput('patronage'))
        )
      )
    )
  ), 
  
  
  
  # --- Pedestrian Tab ---
  tabPanel(
    title = 'Pedestrian Counts',
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = 'type', 
          label = 'POI Type', 
          choices = c('All', unique(pois$subtype)), 
          selected = 'All'
        ), 
        hr(),
        h5("View Mode:"),
        tabsetPanel(id = "view_mode",
                    tabPanel("Heatmap View", value = "heatmap"),
                    tabPanel("Ranking View", value = "ranking"),
                    tabPanel("Trend View", value = "trend")),   # ✅ New tab
        width = 3, 
        style = 'max-width: 400px'
      ),
      mainPanel(
        conditionalPanel("input.view_mode == 'heatmap'",
                         leafletOutput("heatmap", width = 800, height = 800)
        ),
        conditionalPanel("input.view_mode == 'ranking'",
                         plotlyOutput("popularity_plot", width = 1400, height = 800)
        ),
        conditionalPanel("input.view_mode == 'trend'",
                         plotlyOutput("trend_plot", width = 1400, height = 800)  # ✅ Time trend
        )
      )
    )
  ), 
  
  
  
  # --- Parking Tab ---
  tabPanel(
    title = 'Parking Locations',
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = 'type_parking',
          label = 'POI Type',
          choices = c('All', unique(pois$subtype)),
          selected = 'All'
        ),
        h4("Select Landmarks for Parking Search"),
        helpText("Choose one or more landmarks to find nearby parking"),
        selectizeInput(
          "lm_name_parking",
          "Landmark(s)",
          choices = NULL,
          options = list(
            placeholder = "Type to search...",
            plugins = list('remove_button')
          ),
          multiple = TRUE
        ),
        sliderInput("radius_m_parking", "Search radius (meters)",
                    min = 100, max = 1000, value = 300, step = 50,
                    ticks = TRUE, sep = ""),
        hr(),
        tags$div(
          tags$strong("Parking Abbreviations Guide", style = "color: #2c3e50;"),
          tags$div(
            style = "margin-top: 10px; font-size: 12px; line-height: 1.6;",
            tags$table(
              style = "width: 100%; border-collapse: collapse;",
              tags$tbody(
                tags$tr(
                  tags$td(tags$strong("FP1P"), style = "padding: 4px 8px; color: #27ae60;"),
                  tags$td("Free Parking 1 hour", style = "padding: 4px 8px;")
                ),
                tags$tr(
                  tags$td(tags$strong("MP2P"), style = "padding: 4px 8px; color: #e74c3c;"),
                  tags$td("Meter Parking 2 hours", style = "padding: 4px 8px;")
                ),
                tags$tr(
                  tags$td(tags$strong("LZ30"), style = "padding: 4px 8px; color: #f39c12;"),
                  tags$td("Loading Zone 30 min", style = "padding: 4px 8px;")
                ),
                tags$tr(
                  tags$td(tags$strong("QP"), style = "padding: 4px 8px; color: #3498db;"),
                  tags$td("Quarter Parking (15 min)", style = "padding: 4px 8px;")
                ),
                tags$tr(
                  tags$td(tags$strong("DP"), style = "padding: 4px 8px; color: #9b59b6;"),
                  tags$td("Disabled Parking", style = "padding: 4px 8px;")
                ),
                tags$tr(
                  tags$td(tags$strong("HP"), style = "padding: 4px 8px; color: #16a085;"),
                  tags$td("Hospital/Health Permit", style = "padding: 4px 8px;")
                ),
                tags$tr(
                  tags$td(tags$strong("PP"), style = "padding: 4px 8px; color: #34495e;"),
                  tags$td("Permit Parking", style = "padding: 4px 8px;")
                ),
                tags$tr(
                  tags$td(tags$strong("SP"), style = "padding: 4px 8px; color: #7f8c8d;"),
                  tags$td("Special/Short-term Parking", style = "padding: 4px 8px;")
                )
              )
            )
          )
        ),
        width = 3,
        style = 'max-width: 400px'
      ),
      mainPanel(
        leafletOutput("map_parking", width = 800, height = 800),
        br(),
        uiOutput("info_summary_parking"),
        # JavaScript handlers for layer control checkbox manipulation
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
                    if (labelText === 'All Landmarks' || labelText === 'All Parking Zones') {
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
                    if (labelText === 'All Landmarks' || labelText === 'All Parking Zones') {
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
  
  # --- Crime Tab ---
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



# --- Server ---
server <- function(input, output, session) {
  
  

# --- PT Visualisations ---
  
  # station select
  selectedstation <- reactiveVal(NULL)
  
  # create map
  output$mappt <- renderLeaflet({
    
    # poi subset
    if (input$type == 'All') {
      currpois <- pois
    } else {
      currpois <- pois[pois$subtype == input$type, ]
    }
    
    leaflet() %>%
      
      # base map
      addProviderTiles(providers$CartoDB) %>%
      
      # boundary
      addPolygons(data = boundary, 
                  color = 'black', 
                  weight = 3, 
                  fill = FALSE) %>%
      
      # pois
      addAwesomeMarkers(., data = currpois, lng = ~lon, lat = ~lat, 
                        icon = ~awesomeIcons(library = 'fa', 
                                             markerColor = ~colour, 
                                             icon = ~icon, 
                                             iconColor = '#ffffff'), 
                        label = ~name, 
                        layerId = ~id, 
                        group = 'pois')
  })
  
  # on click
  observeEvent(input$mappt_marker_click, {
    
    # get click
    click <- input$mappt_marker_click
    if (is.null(click$id)) {
      return()
    }
    id <- click$id
    
    # poi click
    if (startsWith(id, 'poi')) {
      currpoi <- pois2[pois$id == id, ]
      selectedstation(NULL)
      
      # find stops and stations closest to clicked poi
      radius <- input$radius
      stopsnear <- stops[as.numeric(st_distance(stops2, currpoi)) <= radius, ]
      stationsnear <- stations[as.numeric(st_distance(stations2, currpoi)) <= radius, ]
      
      # update map
      leafletProxy('mappt') %>%
        clearGroup('stopsnear') %>%
        clearGroup('stationsnear') %>%
        clearGroup('linesnear') %>%
        
        # add stops
        {if (nrow(stopsnear) > 0) addAwesomeMarkers(., data = stopsnear, lng = ~lon, lat = ~lat, 
                                                    icon = ~awesomeIcons(library = 'fa', 
                                                                         markerColor = ~colour, 
                                                                         icon = ~icon, 
                                                                         iconColor = '#ffffff'), 
                                                    label = ~name, 
                                                    layerId = ~id, 
                                                    group = 'stopsnear')
          else .} %>%
        
        # add stations
        {if (nrow(stationsnear) > 0) addAwesomeMarkers(., data = stationsnear, lng = ~lon, lat = ~lat, 
                                                       icon = ~awesomeIcons(library = 'fa', 
                                                                            markerColor = ~colour, 
                                                                            icon = ~icon, 
                                                                            iconColor = '#ffffff'), 
                                                       label = ~station, 
                                                       layerId = ~id, 
                                                       group = 'stationsnear')
          else .}
    }
    
    # stop click
    if (startsWith(id, 'stop')) {
      currstop <- stops2[stops$id == id, ]
      selectedstation(NULL)
      
      # find lines associated with stop
      linesnear <- lines[as.numeric(st_distance(lines2, currstop)) <= 30, ]
      linesnear <- linesnear[linesnear$mode == currstop$mode, ]
      
      # update map
      leafletProxy('mappt') %>%
        clearGroup('linesnear') %>%
        
        # add lines
        {if (nrow(linesnear) > 0) addPolylines(., data = linesnear, 
                                               color = ~colour, 
                                               fill = FALSE, 
                                               label = ~shortname, 
                                               layerId = ~id, 
                                               group = 'linesnear')
          else .}
    }
    
    # station click
    if (startsWith(id, 'station')) {
      currstation <- stations2[stations2$id == id, ]
      selectedstation(id)
      
      # find lines associated with station
      linesnear <- lines[as.numeric(st_distance(lines2, currstation)) <= 500, ]
      linesnear <- linesnear[linesnear$mode == 'METRO TRAIN', ]
      
      # update map
      leafletProxy('mappt') %>%
        clearGroup('linesnear') %>%
        
        # add lines
        {if (nrow(linesnear) > 0) addPolylines(., data = linesnear, 
                                               color = ~colour, 
                                               fill = FALSE, 
                                               label = ~destination, 
                                               layerId = ~id, 
                                               group = 'linesnear')
          else .}
    }
    
    # station patronage charts
    output$patronage <- renderUI({
      stationid <- selectedstation()
      currstation <- stations2[stations2$id == id, ]
      
      if (is.null(stationid)) {
        return(NULL)
      }
      
      # panel
      tagList(
        h3(paste0(currstation$station, ' Station Patronage')), 
        h6(''), 
        h4(paste0('Annual: ', format(currstation$annual, big.mark = ','))), 
        h6(''), 
        girafeOutput('typeplot', height = 350), 
        h6(''), 
        girafeOutput('timeplot', height = 350)
      )
    })
    
    # type of day patronage
    output$typeplot <- renderGirafe({
      stationid <- selectedstation()
      currstation <- stations2[stations2$id == id, ]
      
      # create dataframe
      df <- data.frame(
        type = c('Weekday', 'Normal Weekday', 'Sch/Hol Weekday', 'Saturday', 'Sunday'), 
        value = c(currstation$weekday, 
                  currstation$normalweekday, 
                  currstation$schholweekday, 
                  currstation$saturday, 
                  currstation$sunday)
      )
      df$type <- factor(df$type, levels = c('Weekday', 'Normal Weekday', 'Sch/Hol Weekday', 'Saturday', 'Sunday'))
      
      # create plot
      p <- ggplot(df) + 
        aes(x = type, y = value, tooltip = value) + 
        geom_bar_interactive(stat = 'identity', fill = '#0071cd') + 
        labs(title = 'Average Patronage per Day Type', x = NULL, y = 'Average Patronage') + 
        theme_minimal()
      
      girafe(ggobj = p)
    })
    
    # time of day patronage
    output$timeplot <- renderGirafe({
      stationid <- selectedstation()
      currstation <- stations2[stations2$id == id, ]
      
      # create dataframe
      df <- data.frame(
        time = c('Before 7am', '7am - 9:30am', '9:30am - 3pm', '3pm - 7pm', 'After 7pm'), 
        value = c(currstation$early, 
                  currstation$ampeak, 
                  currstation$interpeak, 
                  currstation$pmpeak, 
                  currstation$late)
      )
      df$time <- factor(df$time, levels = c('Before 7am', '7am - 9:30am', '9:30am - 3pm', '3pm - 7pm', 'After 7pm'))
      
      # create plot
      p <- ggplot(df) + 
        aes(x = time, y = value, tooltip = value) + 
        geom_bar_interactive(stat = 'identity', fill = '#0071cd') + 
        labs(title = 'Average Patronage per Time Period on Weekdays', x = NULL, y = 'Average Patronage') + 
        theme_minimal()
      
      girafe(ggobj = p)
    })
  })
    
    
# --- Crime Visualisations ---    
    
    
    
# --- Pedestrian Visualisations ---
  
  # Heatmap View
  output$heatmap <- renderLeaflet({
    
    # poi subset
    if (input$type == 'All') {
      filtered_landmarks <- landmark_popularity
    } else {
      filtered_landmarks <- landmark_popularity[landmark_popularity$subtype == input$type, ]
    }
    
    leaflet() %>%
      
      # base map
      addProviderTiles(providers$CartoDB) %>%
      
      # boundary
      addPolygons(data = boundary, 
                  color = 'black', 
                  weight = 3, 
                  fill = FALSE) %>%
      
      # pois
      addAwesomeMarkers(
        ., data = filtered_landmarks, lng = ~lon, lat = ~lat,
        icon = ~awesomeIcons(
          library = 'fa',
          markerColor = ~colour,
          icon = ~icon,
          iconColor = '#ffffff'
        ),
        label = lapply(paste0(
          "<b>Landmark:</b> ", ifelse(!is.null(filtered_landmarks$name), filtered_landmarks$name, filtered_landmarks$subtype), "<br/>",
          "<b>Average Pedestrian Count:</b> ", format(round(filtered_landmarks$nearest_count), big.mark = ",")
        ), htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-size" = "12px", "font-family" = "Arial"),
          direction = "auto"
        ),
        layerId = ~id,
        group = 'pois'
      ) %>%
      
      # heatmap
      addHeatmap(
        data = ped_geo,
        lng = ~Longitude, lat = ~Latitude,
        intensity = ~Avg_Count,
        blur = 25, max = 0.9, radius = 15,
        gradient = c("navy", "cyan", "yellow", "orange", "red"),
        group = "Pedestrian Heatmap"
      )
  })
  
  # Ranking View
  output$popularity_plot <- renderPlotly({
    
    # poi subset
    if (input$type == 'All') {
      filtered_landmarks <- landmark_popularity
    } else {
      filtered_landmarks <- landmark_popularity[landmark_popularity$subtype == input$type, ]
    }
    
    df <- filtered_landmarks %>%
      arrange(desc(nearest_count)) %>%
      head(15)
    
    df$name <- factor(df$name, levels = df$name[order(df$nearest_count, decreasing = TRUE)])
    df$name <- forcats::fct_rev(df$name)
    
    colourmap <- df %>% 
      dplyr::distinct(subtype, colour)
    colourvals <- setNames(colourmap$colour, colourmap$subtype)
    
    p <- ggplot(df, aes(x = name, y = nearest_count, fill = subtype)) +
      geom_col() +
      coord_flip() +
      labs(x = "Point of Interest", y = "Avg Pedestrian Count (nearest sensor)",
           title = "Top 15 POIs by Nearby Pedestrian Volume") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom") + 
      scale_fill_manual(values = colourvals, name = 'Type')
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # Trend View (linked to theme filter)
  output$trend_plot <- renderPlotly({
    
    # poi subset
    if (input$type == 'All') {
      filtered_landmarks <- landmark_popularity
    } else {
      filtered_landmarks <- landmark_popularity[landmark_popularity$subtype == input$type, ]
    }
    
    selected_sensors <- filtered_landmarks$nearest_sensor
    
    trend_theme <- ped_raw %>%
      filter(Sensor_Name %in% selected_sensors) %>%
      mutate(Date = as.Date(Sensing_Date)) %>%
      group_by(Date) %>%
      summarise(Total_Pedestrians = sum(Total_of_Directions, na.rm = TRUE)) %>%
      arrange(Date)
    
    plot_ly(
      data = trend_theme,
      x = ~Date,
      y = ~Total_Pedestrians,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = "#007BFF", width = 2),
      marker = list(size = 4, color = "#004080")
    ) %>%
      layout(
        title = paste0("Pedestrian Flow Over Time — ",
                       ifelse(input$type == "All", "All POIs", input$type)),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Total Pedestrians"),
        hovermode = "x unified"
      )
  })
    
    
  
  # --- Parking Visualisations ---

  # Filtered POIs by type
  filtered_pois_parking <- reactive({
    if (input$type_parking == 'All') {
      return(pois)
    } else {
      return(pois[pois$subtype == input$type_parking, ])
    }
  })

  # Update landmark choices when POI type changes
  observe({
    filtered_pois <- filtered_pois_parking()
    updateSelectizeInput(
      session,
      "lm_name_parking",
      choices = if (nrow(filtered_pois) > 0) sort(unique(filtered_pois$name)) else NULL,
      selected = character(0)
    )
  })

  # Selected landmarks (for parking search)
  selected_landmarks_parking <- reactive({
    lm <- filtered_pois_parking()
    if (nrow(lm) == 0) return(lm)

    if (length(input$lm_name_parking) > 0) {
      lm <- lm |> dplyr::filter(name %in% input$lm_name_parking)
    }
    lm
  })

  # Combined buffer around all selected landmarks
  combined_buffer_parking <- reactive({
    sel_lm <- selected_landmarks_parking()
    req(nrow(sel_lm) > 0)

    # use EPSG:7899 (GDA2020 / MGA zone 55) for accurate meter-based buffering in Melbourne
    # Falls back to 3857 if transformation fails
    lm_proj <- tryCatch({
      sf::st_transform(sel_lm, 7899)
    }, error = function(e) {
      message("EPSG:7899 not available, using EPSG:3857")
      sf::st_transform(sel_lm, 3857)
    })

    # Create buffer around each landmark
    buf <- sf::st_buffer(lm_proj, dist = input$radius_m_parking)

    # Union all buffers into one
    if (nrow(buf) > 1) {
      buf <- sf::st_union(buf) |> sf::st_sf()
    }

    sf::st_transform(buf, 4326)
  })

  # Filter zones by landmark buffer
  zones_in_radius_parking <- reactive({
    req(nrow(selected_landmarks_parking()) > 0)
    buf <- combined_buffer_parking()
    req(nrow(buf) > 0)

    # Spatial filter: zones that intersect with buffer
    if (!is.null(zones_display) && nrow(zones_display) > 0) {
      intersects <- sf::st_intersects(zones_display, buf, sparse = FALSE)
      zones_filtered <- zones_display[as.vector(intersects), ]
      return(zones_filtered)
    } else {
      # Return empty data frame with expected structure
      return(data.frame(zone_id = character(0), sign_text = character(0)))
    }
  })

  # Initial map with all landmarks shown
  output$map_parking <- renderLeaflet({
    map <- leaflet(options = leafletOptions(minZoom = 11)) |>
      addProviderTiles(providers$CartoDB.Positron)

    if (nrow(boundary) > 0) {
      bb <- sf::st_bbox(boundary)
      map <- fitBounds(map,
                       lng1 = as.numeric(bb["xmin"]),
                       lat1 = as.numeric(bb["ymin"]),
                       lng2 = as.numeric(bb["xmax"]),
                       lat2 = as.numeric(bb["ymax"]))
      map <- addPolygons(map, data = boundary, weight = 2, color = "#222",
                        fill = FALSE, group = "Boundary")
    } else {
      map <- setView(map, lng = 144.9631, lat = -37.8136, zoom = 12)
    }

    # Show filtered landmarks by default (small dark blue markers)
    filtered_pois <- filtered_pois_parking()
    if (nrow(filtered_pois) > 0) {
      map <- addCircleMarkers(
        map,
        data = filtered_pois,
        radius = 3,
        stroke = FALSE,
        fillOpacity = 0.4,
        fillColor = "#00008B",
        label = ~name,
        layerId = ~name,
        group = "All Landmarks"
      )
    }

    # Show ALL parking zones and unmatched segments by default
    if (!is.null(zones_display) && nrow(zones_display) > 0) {
      # Add zones with zone IDs (red markers with clustering)
      zones_with_ids <- zones_display |> dplyr::filter(has_zone == TRUE)
      if (nrow(zones_with_ids) > 0) {
        # Create table popup for each zone using sign_plates data
        zones_with_ids <- create_zone_popups_with_coords(zones_with_ids, sign_plates)

        map <- addAwesomeMarkers(
          map,
          data = zones_with_ids,
          icon = awesomeIcons(
            icon = "car",
            library = "fa",
            markerColor = "red",
            iconColor = "white"
          ),
          label = ~paste0("Zone ", zone_id),
          popup = ~popup_html,
          group = "All Parking Zones",
          clusterOptions = markerClusterOptions(
            showCoverageOnHover = FALSE,
            spiderfyOnMaxZoom = TRUE,
            removeOutsideVisibleBounds = TRUE
          )
        )
      }

      # Add unmatched segments (orange markers)
      unmatched <- zones_display |> dplyr::filter(has_zone == FALSE)
      if (nrow(unmatched) > 0) {
        # Create popup for unmatched segments
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
          clusterOptions = markerClusterOptions(
            showCoverageOnHover = FALSE,
            spiderfyOnMaxZoom = TRUE,
            removeOutsideVisibleBounds = TRUE
          )
        )
      }
    }

    # Add layer control
    map <- addLayersControl(
      map,
      overlayGroups = c("Boundary", "All Landmarks", "All Parking Zones", "Buffer"),
      options = layersControlOptions(collapsed = FALSE)
    )

    # Initially hide filtered groups
    map <- hideGroup(map, "Filtered Landmarks")
    map <- hideGroup(map, "Filtered Zones")

    map
  })

  # Update landmarks on map when POI type changes
  observeEvent(input$type_parking, {
    filtered_pois <- filtered_pois_parking()

    map <- leafletProxy("map_parking")
    map <- clearGroup(map, "All Landmarks")

    if (nrow(filtered_pois) > 0) {
      map <- addCircleMarkers(
        map,
        data = filtered_pois,
        radius = 3,
        stroke = FALSE,
        fillOpacity = 0.4,
        fillColor = "#00008B",
        label = ~name,
        layerId = ~name,
        group = "All Landmarks"
      )
    }
  })

  # Handle landmark marker clicks
  observeEvent(input$map_parking_marker_click, {
    click <- input$map_parking_marker_click
    if (is.null(click$id)) return()

    # Check if clicked marker is a landmark (not a parking zone)
    # Landmarks use their name as layerId, parking zones use "Zone XXX"
    if (!startsWith(click$id, "Zone ") && click$id != "No Zone ID") {
      # Update the landmark selection dropdown
      updateSelectizeInput(
        session,
        "lm_name_parking",
        selected = click$id
      )
    }
  })

  # Update map layers when landmarks are selected
  observeEvent(c(input$lm_name_parking, input$radius_m_parking), {
    req(nrow(pois) > 0)
    req(!is.null(zones_display))

    map <- leafletProxy("map_parking")
    map <- clearGroup(map, "Buffer")
    map <- clearGroup(map, "Filtered Landmarks")
    map <- clearGroup(map, "Filtered Zones")

    # Check if user has filter active
    has_filter <- length(input$lm_name_parking) > 0

    if (has_filter) {
      # User applied filter
      sel_lm <- selected_landmarks_parking()
      buf <- combined_buffer_parking()
      zones_sf <- zones_in_radius_parking()

      # Hide "All" groups and uncheck their checkboxes
      map <- hideGroup(map, "All Landmarks")
      map <- hideGroup(map, "All Parking Zones")

      # Send JavaScript command to uncheck the checkboxes
      session$sendCustomMessage(type = "uncheckLayers", message = list())

      # Add buffer zones
      map <- addPolygons(
        map,
        data = buf,
        fill = TRUE,
        fillOpacity = 0.08,
        color = "#1f78b4",
        weight = 2,
        group = "Buffer"
      )

      # Automatically pan and zoom to the selected landmarks
      if (nrow(sel_lm) > 0) {
        bounds <- sf::st_bbox(buf)
        map <- fitBounds(map, bounds[["xmin"]], bounds[["ymin"]], bounds[["xmax"]], bounds[["ymax"]],
                        options = list(padding = c(50, 50)))
      }

      # Add filtered landmarks (larger red markers)
      sel_lm_data <- sel_lm
      sel_lm_data$popup_text <- paste0("<b>", sel_lm_data$name, "</b><br>Category: ", sel_lm_data$subtype)

      map <- addCircleMarkers(
        map,
        data = sel_lm_data,
        radius = 8,
        stroke = TRUE,
        weight = 2,
        fillOpacity = 0.9,
        fillColor = "#e31a1c",
        color = "#fff",
        label = ~name,
        popup = ~popup_text,
        group = "Filtered Landmarks"
      )

      # Show filtered landmarks
      map <- showGroup(map, "Filtered Landmarks")

      # Add filtered parking zones
      if (nrow(zones_sf) > 0) {
        # Split into zones with IDs and unmatched segments
        zones_with_ids <- zones_sf
        unmatched_segs <- zones_sf
        if ("has_zone" %in% names(zones_sf)) {
          zones_with_ids <- zones_sf |> dplyr::filter(has_zone == TRUE)
          unmatched_segs <- zones_sf |> dplyr::filter(has_zone == FALSE)
        }

        # Add zones with zone IDs (red markers)
        if (nrow(zones_with_ids) > 0) {
          zones_with_ids <- create_zone_popups_with_coords(zones_with_ids, sign_plates)

          map <- addAwesomeMarkers(
            map,
            data = zones_with_ids,
            icon = awesomeIcons(
              icon = "car",
              library = "fa",
              markerColor = "red",
              iconColor = "white"
            ),
            label = ~paste0("Zone ", zone_id),
            popup = ~popup_html,
            group = "Filtered Zones",
            clusterOptions = markerClusterOptions(
              showCoverageOnHover = FALSE,
              spiderfyOnMaxZoom = TRUE,
              removeOutsideVisibleBounds = TRUE
            )
          )
        }

        # Add unmatched segments
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
            group = "Filtered Zones",
            clusterOptions = markerClusterOptions(
              showCoverageOnHover = FALSE,
              spiderfyOnMaxZoom = TRUE,
              removeOutsideVisibleBounds = TRUE
            )
          )
        }

        # Show filtered zones
        map <- showGroup(map, "Filtered Zones")
      }

    } else {
      # No filter applied - show "All" groups, hide "Filtered" groups
      map <- hideGroup(map, "Filtered Landmarks")
      map <- hideGroup(map, "Filtered Zones")
      map <- showGroup(map, "All Landmarks")
      map <- showGroup(map, "All Parking Zones")

      # Send JavaScript command to re-check the checkboxes
      session$sendCustomMessage(type = "recheckLayers", message = list())
    }
  })

  # Summary info box
  output$info_summary_parking <- renderUI({
    if (length(input$lm_name_parking) == 0) {
      return(tags$div(
        class = "alert alert-secondary",
        role = "alert",
        tags$strong("Welcome! "),
        sprintf(
          "Select one or more landmarks from the sidebar to find nearby parking zones. %d landmarks shown on map.",
          nrow(pois)
        )
      ))
    }

    zones_sf <- zones_in_radius_parking()
    if (nrow(zones_sf) == 0) {
      return(tags$div(
        class = "alert alert-warning",
        role = "alert",
        tags$strong("No parking zones found "),
        sprintf("within %d meters of selected landmark(s). Try increasing the search radius or selecting landmarks in central Melbourne.", input$radius_m_parking)
      ))
    }

    total_zones <- nrow(zones_sf)

    # Format landmark names for display
    landmark_names <- input$lm_name_parking
    if (length(landmark_names) <= 3) {
      # Show all names if 3 or fewer
      landmark_text <- paste(landmark_names, collapse = ", ")
    } else {
      # Show first 3 and add "etc."
      landmark_text <- paste(c(landmark_names[1:3], "etc."), collapse = ", ")
    }

    tags$div(
      class = "alert alert-success",
      role = "alert",
      tags$strong("Search Results: "),
      sprintf(
        "Found %d parking zone%s within %d meters of %s.",
        total_zones,
        ifelse(total_zones == 1, "", "s"),
        input$radius_m_parking,
        landmark_text
      )
    )
  })
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

shinyApp(ui, server)
