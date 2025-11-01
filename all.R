library(shiny)
library(sf)
library(dplyr)
library(stringr)
library(leaflet)
library(ggplot2)
library(ggiraph)

library(htmltools)

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
pois$colour <- unname(poiscolour[pois$subtype])

# copy dataframe in epsg 3857 for distance measurements
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

# Load suburb boundaries for crime visualization
# Use OpenStreetMap data via osmdata package for accurate boundaries
load_suburb_boundaries <- function() {
  # Option 1: Try OpenStreetMap via osmdata package (most accurate, free, widely available)
  if (requireNamespace("osmdata", quietly = TRUE)) {
    tryCatch({
      library(osmdata, quietly = TRUE)
      cat('[PREPROCESS] Fetching suburb boundaries from OpenStreetMap (via osmdata)...\n')
      
      # Get Melbourne bounding box (larger area to capture all suburbs)
      bbox_melb <- getbb("Melbourne, Australia", featuretype = "city")
      if (is.null(bbox_melb) || nrow(bbox_melb) == 0) {
        # Fallback bbox if geocoding fails
        bbox_melb <- matrix(c(144.93, -37.84, 144.98, -37.79), nrow = 2, 
                            dimnames = list(c("x", "y"), c("min", "max")))
      }
      
      # Query all suburbs in Melbourne area - need ADMINISTRATIVE boundaries, not just place tags
      cat('[PREPROCESS] Querying OpenStreetMap for suburb administrative boundaries...\n')
      
      # Try query for administrative boundaries (admin_level 10 = suburb/neighborhood level)
      q <- opq(bbox = bbox_melb) %>%
        add_osm_feature(key = "admin_level", value = "10") %>%
        add_osm_feature(key = "boundary", value = "administrative")
      
      osm_data <- osmdata_sf(q)
      
      suburbs_all <- NULL
      
      # Extract polygons (prefer multipolygons, fall back to polygons)
      # These are actual boundary polygons, not point centroids
      if (!is.null(osm_data$osm_multipolygons) && nrow(osm_data$osm_multipolygons) > 0) {
        suburbs_all <- osm_data$osm_multipolygons %>%
          filter(!is.na(name)) %>%
          select(name) %>%
          rename(suburb_name = name) %>%
          st_transform(4326)
        cat('[PREPROCESS]   Found', nrow(suburbs_all), 'multipolygon boundaries\n')
      } 
      
      # If no admin boundaries, try place=suburb but filter for polygons only
      if (is.null(suburbs_all) || nrow(suburbs_all) == 0) {
        cat('[PREPROCESS]   Admin boundaries not found, trying place=suburb with polygon filter...\n')
        q2 <- opq(bbox = bbox_melb) %>%
          add_osm_feature(key = "place", value = "suburb")
        
        osm_data2 <- osmdata_sf(q2)
        
        if (!is.null(osm_data2$osm_multipolygons) && nrow(osm_data2$osm_multipolygons) > 0) {
          suburbs_all <- osm_data2$osm_multipolygons %>%
            filter(!is.na(name)) %>%
            select(name) %>%
            rename(suburb_name = name) %>%
            st_transform(4326)
          cat('[PREPROCESS]   Found', nrow(suburbs_all), 'multipolygon suburbs\n')
        } else if (!is.null(osm_data2$osm_polygons) && nrow(osm_data2$osm_polygons) > 0) {
          suburbs_all <- osm_data2$osm_polygons %>%
            filter(!is.na(name)) %>%
            select(name) %>%
            rename(suburb_name = name) %>%
            st_transform(4326)
          cat('[PREPROCESS]   Found', nrow(suburbs_all), 'polygon suburbs\n')
        }
      }
      
      if (!is.null(suburbs_all) && nrow(suburbs_all) > 0) {
        # Verify these are actually polygons, not points
        geom_types <- st_geometry_type(suburbs_all$geometry)
        polygon_count <- sum(geom_types %in% c("POLYGON", "MULTIPOLYGON"))
        cat('[PREPROCESS] ✓ Loaded', nrow(suburbs_all), 'features from OpenStreetMap\n')
        cat('[PREPROCESS]   Polygon/Multipolygon features:', polygon_count, '\n')
        cat('[PREPROCESS]   Found suburbs:', paste(head(suburbs_all$suburb_name, 5), collapse=", "), 
            if(nrow(suburbs_all) > 5) "...", '\n')
        
        # Filter to only polygon geometries (exclude any points/lines that got through)
        if (polygon_count > 0) {
          suburbs_all <- suburbs_all[geom_types %in% c("POLYGON", "MULTIPOLYGON"), ]
          return(suburbs_all)
        }
      }
    }, error = function(e) {
      cat('[PREPROCESS] ✗ OpenStreetMap query failed:', conditionMessage(e), '\n')
      cat('[PREPROCESS]   Will try alternative sources...\n')
    })
  }
  
  # Option 2: Try direct download from data.gov.au (Victorian government official data)
  tryCatch({
    cat('[PREPROCESS] Attempting to load suburb boundaries from data.gov.au API...\n')
    url_vic_suburbs <- "https://data.gov.au/geoserver/vic-suburb-locality-boundaries/wfs?request=GetFeature&typeName=vic-suburb-locality-boundaries:ckan_vic_suburb_locality_boundaries_psma_additional&outputFormat=application%2Fjson&srsName=EPSG:4326"
    suburbs_all <- st_read(url_vic_suburbs, quiet = TRUE)
    
    if (nrow(suburbs_all) > 0) {
      # Normalize column names
      if ("vic_loca_2" %in% names(suburbs_all)) {
        suburbs_all <- suburbs_all %>%
          rename(suburb_name = vic_loca_2) %>%
          select(suburb_name, geometry)
      } else if ("vic_locality_name" %in% names(suburbs_all)) {
        suburbs_all <- suburbs_all %>%
          rename(suburb_name = vic_locality_name) %>%
          select(suburb_name, geometry)
      } else if ("name" %in% names(suburbs_all)) {
        suburbs_all <- suburbs_all %>%
          rename(suburb_name = name) %>%
          select(suburb_name, geometry)
      }
      suburbs_all <- suburbs_all %>% st_transform(4326)
      cat('[PREPROCESS] ✓ Loaded', nrow(suburbs_all), 'suburb boundaries from data.gov.au API\n')
      return(suburbs_all)
    }
  }, error = function(e) {
    cat('[PREPROCESS] ✗ data.gov.au API failed:', conditionMessage(e), '\n')
  })
  
  # Option 3: Use local geojson file (fallback only if OpenStreetMap and API both failed)
  # Note: This file may have outdated boundaries - OpenStreetMap is preferred
  if (file.exists('vic_suburbs.geojson')) {
    cat('[PREPROCESS] WARNING: Using local vic_suburbs.geojson file (may have outdated boundaries)\n')
    cat('[PREPROCESS]          Consider deleting this file to force OpenStreetMap usage\n')
    suburbs_all <- st_read('vic_suburbs.geojson', quiet = TRUE) %>%
      st_transform(4326)
    cat('[PREPROCESS] ✓ Loaded suburb boundaries from local file\n')
    return(suburbs_all)
  }
  
  stop("No suburb boundary data source available.\n",
       "Please install osmdata package for accurate boundaries:\n",
       "  install.packages('osmdata')\n",
       "Or download boundaries from:\n",
       "  https://discover.data.vic.gov.au/dataset/vic-suburb-locality-boundaries")
}

# Load suburb boundaries
suburbs_all <- load_suburb_boundaries()

# Filter to Melbourne LGA suburbs (ALL suburbs from crime data)
# Normalize suburb names for matching - be flexible with name variations
melbourne_suburb_names <- c('MELBOURNE', 'SOUTHBANK', 'DOCKLANDS', 'CARLTON',
                           'CARLTON NORTH', 'NORTH MELBOURNE', 'WEST MELBOURNE',
                           'EAST MELBOURNE', 'PARKVILLE', 'KENSINGTON',
                           'PORT MELBOURNE', 'SOUTH WHARF', 'SOUTH YARRA',
                           'FLEMINGTON')

# Create flexible matching function that handles name variations
match_suburb_name <- function(name, targets) {
  name_upper <- toupper(name)
  # Direct match
  if (name_upper %in% targets) return(TRUE)
  # Partial match (handles "Melbourne", "Melbourne CBD", etc.)
  for (target in targets) {
    if (grepl(paste0("^", target, "$|^", target, " | ", target, "$| ", target, " "), name_upper, ignore.case = TRUE)) {
      return(TRUE)
    }
  }
  # Handle special cases
  if (grepl("^MELBOURNE", name_upper) && "MELBOURNE" %in% targets) return(TRUE)
  if (grepl("SOUTHBANK", name_upper) && "SOUTHBANK" %in% targets) return(TRUE)
  return(FALSE)
}

# Try to match suburbs - handle different column names with flexible matching
if ("suburb_name" %in% names(suburbs_all)) {
  name_col <- "suburb_name"
} else if ("vic_loca_2" %in% names(suburbs_all)) {
  name_col <- "vic_loca_2"
} else if ("name" %in% names(suburbs_all)) {
  name_col <- "name"
} else {
  # Last resort: use first column
  name_col <- names(suburbs_all)[1]
}

# Apply flexible matching
melbourne_suburbs_raw <- suburbs_all %>%
  mutate(
    name_upper = toupper(.data[[name_col]]),
    matches = sapply(.data[[name_col]], function(x) match_suburb_name(x, melbourne_suburb_names))
  ) %>%
  filter(matches) %>%
  rename(suburb_name = .data[[name_col]]) %>%
  select(suburb_name, geometry) %>%
  # Normalize suburb names to match crime data
  mutate(suburb_name = case_when(
    grepl("^MELBOURNE", toupper(suburb_name)) ~ "Melbourne",
    toupper(suburb_name) == "SOUTHBANK" ~ "Southbank",
    toupper(suburb_name) == "DOCKLANDS" ~ "Docklands",
    toupper(suburb_name) == "CARLTON" ~ "Carlton",
    toupper(suburb_name) == "CARLTON NORTH" | grepl("CARLTON.*NORTH", toupper(suburb_name)) ~ "Carlton North",
    toupper(suburb_name) == "NORTH MELBOURNE" | grepl("NORTH.*MELBOURNE", toupper(suburb_name)) ~ "North Melbourne",
    toupper(suburb_name) == "WEST MELBOURNE" | grepl("WEST.*MELBOURNE", toupper(suburb_name)) ~ "West Melbourne",
    toupper(suburb_name) == "EAST MELBOURNE" | grepl("EAST.*MELBOURNE", toupper(suburb_name)) ~ "East Melbourne",
    toupper(suburb_name) == "PARKVILLE" ~ "Parkville",
    toupper(suburb_name) == "KENSINGTON" ~ "Kensington",
    toupper(suburb_name) == "PORT MELBOURNE" | grepl("PORT.*MELBOURNE", toupper(suburb_name)) ~ "Port Melbourne",
    toupper(suburb_name) == "SOUTH WHARF" | grepl("SOUTH.*WHARF", toupper(suburb_name)) ~ "South Wharf",
    toupper(suburb_name) == "SOUTH YARRA" | grepl("SOUTH.*YARRA", toupper(suburb_name)) ~ "South Yarra",
    toupper(suburb_name) == "FLEMINGTON" ~ "Flemington",
    TRUE ~ suburb_name
  ))

cat('[PREPROCESS] Matched', nrow(melbourne_suburbs_raw), 'suburb boundaries:', paste(unique(melbourne_suburbs_raw$suburb_name), collapse=", "), '\n')

# Ensure both are in the same CRS
cat('[PREPROCESS] Checking CRS compatibility...\n')
cat('[PREPROCESS] Boundary CRS:', st_crs(boundary)$input, '\n')
cat('[PREPROCESS] Suburbs CRS:', st_crs(melbourne_suburbs_raw)$input, '\n')

# Transform both to a common CRS (WGS84/4326) for reliable intersection
boundary_for_clip <- st_transform(boundary, 4326)
melbourne_suburbs_raw <- melbourne_suburbs_raw %>%
  st_transform(4326) %>%
  mutate(geometry = suppressWarnings(st_make_valid(geometry))) %>%
  filter(!st_is_empty(geometry))

cat('[PREPROCESS] Both transformed to WGS84 (EPSG:4326)\n')

# Clip suburbs to city boundary with improved gap handling
cat('[PREPROCESS] Starting suburb clipping with improved gap handling...\n')

cat('[PREPROCESS] Processing', nrow(melbourne_suburbs_raw), 'suburbs individually to avoid intersection artifacts...\n')

# Clip each suburb individually to avoid intersection artifacts that cause misassignment
melbourne_suburbs_list <- list()
success_count <- 0

for (i in seq_len(nrow(melbourne_suburbs_raw))) {
  suburb <- melbourne_suburbs_raw[i, ]
  tryCatch({
    # First check if suburb intersects with boundary at all
    if (!st_intersects(suburb$geometry, boundary_for_clip, sparse = FALSE)[1, 1]) {
      cat('[PREPROCESS]   ⚠ Skipped', suburb$suburb_name, '- does not intersect boundary\n')
      next
    }
    
    # Verify suburb is actually a polygon before clipping
    suburb_geom_type <- as.character(st_geometry_type(suburb$geometry[[1]]))
    if (!suburb_geom_type %in% c("POLYGON", "MULTIPOLYGON")) {
      cat('[PREPROCESS]   ✗ Skipped', suburb$suburb_name, '- not a polygon (type:', suburb_geom_type, ')\n')
      next
    }
    
    # Clip this specific suburb to boundary using geometry-level intersection
    suburb_geom <- st_geometry(suburb) %>% st_make_valid()
    boundary_geom <- st_geometry(boundary_for_clip) %>% st_make_valid()
    
    # Perform intersection at geometry level for better control
    clipped_geom <- suppressWarnings(
      st_intersection(suburb_geom, boundary_geom)
    ) %>% st_make_valid()
    
    # Check if we got a valid geometry result
    if (st_is_empty(clipped_geom)) {
      cat('[PREPROCESS]   ⚠ Skipped', suburb$suburb_name, '- intersection is empty\n')
      next
    }
    
    # Check the actual geometry type using st_geometry_type
    geom_type_str <- as.character(st_geometry_type(clipped_geom))
    
    # Check if it's a polygon-type geometry
    if (geom_type_str %in% c("POLYGON", "MULTIPOLYGON")) {
      # Create sf object with clipped geometry
      clipped <- suburb %>%
        st_set_geometry(clipped_geom) %>%
        mutate(geometry = suppressWarnings(st_make_valid(geometry))) %>%
        filter(!st_is_empty(geometry))
      
      if (nrow(clipped) > 0 && !st_is_empty(clipped$geometry[[1]])) {
        melbourne_suburbs_list[[success_count + 1]] <- clipped
        success_count <- success_count + 1
        cat('[PREPROCESS]   ✓ Clipped:', suburb$suburb_name, '\n')
      } else {
        cat('[PREPROCESS]   ⚠ Skipped', suburb$suburb_name, '- became empty after validation\n')
      }
    } else {
      # Intersection returned non-polygon - suburb might be completely within or on edge
      if (st_within(suburb$geometry, boundary_for_clip, sparse = FALSE)[1, 1]) {
        # Suburb is completely within boundary, use it as-is
        melbourne_suburbs_list[[success_count + 1]] <- suburb %>%
          mutate(geometry = suppressWarnings(st_make_valid(geometry))) %>%
          filter(!st_is_empty(geometry))
        success_count <- success_count + 1
        cat('[PREPROCESS]   ✓ Using full suburb (within boundary):', suburb$suburb_name, '\n')
      } else {
        cat('[PREPROCESS]   ✗ Skipped', suburb$suburb_name, '- intersection returned:', geom_type_str, '(expected polygon)\n')
      }
    }
  }, error = function(e) {
    cat('[PREPROCESS]   ✗ Error clipping', suburb$suburb_name, ':', conditionMessage(e), '\n')
  })
}

cat('[PREPROCESS] Successfully clipped', success_count, 'out of', nrow(melbourne_suburbs_raw), 'suburbs\n')

# Combine all successfully clipped suburbs
if (length(melbourne_suburbs_list) > 0) {
  cat('[PREPROCESS] Combining and cleaning suburb geometries...\n')
  melbourne_suburbs <- bind_rows(melbourne_suburbs_list) %>%
    select(suburb_name, geometry) %>%
    st_as_sf() %>%
    st_make_valid()
  
  # Group by suburb name and union any fragmented pieces
  melbourne_suburbs <- melbourne_suburbs %>%
    group_by(suburb_name) %>%
    summarise(geometry = st_union(geometry), .groups = 'drop') %>%
    st_make_valid()
  
  cat('[PREPROCESS] Resolving overlaps between suburbs to prevent misassignment...\n')
  cat('[PREPROCESS] Priority: Specific suburbs keep their boundaries; Melbourne gets remaining areas\n')
  
  # First, detect if there are actually any overlaps
  cat('[PREPROCESS] Detecting overlaps between suburbs...\n')
  n_suburbs <- nrow(melbourne_suburbs)
  overlap_detected <- FALSE
  overlap_details <- list()
  
  if (n_suburbs > 1) {
    for (i in seq_len(n_suburbs - 1)) {
      for (j in (i + 1):n_suburbs) {
        suburb1 <- melbourne_suburbs[i, ]
        suburb2 <- melbourne_suburbs[j, ]
        
        # Check if they overlap (not just touch)
        intersects <- tryCatch({
          result <- st_intersects(suburb1$geometry, suburb2$geometry, sparse = FALSE)[1, 1]
          ifelse(is.na(result), FALSE, result)
        }, error = function(e) FALSE)
        
        if (intersects) {
          # Calculate overlap area
          overlap_geom <- tryCatch({
            suppressWarnings(st_intersection(suburb1$geometry, suburb2$geometry))
          }, error = function(e) NULL)
          
          if (!is.null(overlap_geom)) {
            # Check if empty with proper NA handling
            is_empty <- tryCatch({
              result <- st_is_empty(overlap_geom)
              if (length(result) == 0) {
                TRUE
              } else if (is.na(result)) {
                TRUE
              } else {
                as.logical(result)
              }
            }, error = function(e) TRUE)
            
            if (length(is_empty) > 0 && !is_empty) {
              overlap_area <- tryCatch({
                as.numeric(st_area(overlap_geom))
              }, error = function(e) 0)
              
              if (!is.na(overlap_area) && overlap_area > 0.000001) {  # Very small threshold to ignore tiny overlaps
                overlap_detected <- TRUE
                overlap_info <- paste0(suburb1$suburb_name, ' <-> ', suburb2$suburb_name, 
                                      ' (area: ', format(overlap_area, scientific = FALSE), ')')
                overlap_details[[length(overlap_details) + 1]] <- overlap_info
                cat('[PREPROCESS]   ⚠ OVERLAP DETECTED:', overlap_info, '\n')
              }
            }
          }
        }
      }
    }
    
    if (!overlap_detected) {
      cat('[PREPROCESS]   ✓ No overlaps detected between suburbs\n')
      cat('[PREPROCESS]   Note: If you see misclassification, it may be due to data accuracy, not overlaps\n')
    } else {
      cat('[PREPROCESS]   Found', length(overlap_details), 'overlapping pairs - will resolve with priority logic\n')
    }
  }
  
  # Priority-based overlap resolution:
  # 1. Specific suburbs (e.g., South Yarra, Carlton) keep their full boundaries
  # 2. "Melbourne" (central/general area) only gets areas NOT claimed by other suburbs
  # This prevents areas near specific suburbs from being incorrectly classified as "Melbourne"
  
  if (n_suburbs > 1) {
    # Identify "Melbourne" as the central area that should yield to others
    melbourne_central <- melbourne_suburbs %>%
      filter(toupper(suburb_name) == "MELBOURNE")
    
    # All other suburbs (specific suburbs) - these keep their boundaries
    other_suburbs_list <- melbourne_suburbs %>%
      filter(toupper(suburb_name) != "MELBOURNE")
    
    corrected_suburbs <- list()
    
    # Step 1: Keep all specific suburbs as-is (they have priority)
    if (nrow(other_suburbs_list) > 0) {
      cat('[PREPROCESS]   Keeping', nrow(other_suburbs_list), 'specific suburbs with full boundaries...\n')
      for (i in seq_len(nrow(other_suburbs_list))) {
        suburb <- other_suburbs_list[i, ]
        # Check for overlaps between specific suburbs only
        other_specific <- other_suburbs_list[-i, ]
        if (nrow(other_specific) > 0) {
          other_union <- suppressWarnings(
            st_union(other_specific$geometry) %>% st_make_valid()
          )
          # Remove overlaps only with other specific suburbs
          corrected_geom <- suppressWarnings(
            st_difference(suburb$geometry, other_union)
          ) %>% st_make_valid()
          
          if (!st_is_empty(corrected_geom) && 
              inherits(corrected_geom[[1]], c("POLYGON", "MULTIPOLYGON"))) {
            suburb$geometry <- corrected_geom
            corrected_suburbs[[length(corrected_suburbs) + 1]] <- suburb
            cat('[PREPROCESS]   ✓', suburb$suburb_name, '- kept (priority suburb)\n')
          } else {
            corrected_suburbs[[length(corrected_suburbs) + 1]] <- suburb
            cat('[PREPROCESS]   ✓', suburb$suburb_name, '- kept (no overlaps with other specific suburbs)\n')
          }
        } else {
          corrected_suburbs[[length(corrected_suburbs) + 1]] <- suburb
          cat('[PREPROCESS]   ✓', suburb$suburb_name, '- kept\n')
        }
      }
    }
    
    # Step 2: Process "Melbourne" last - remove all areas claimed by specific suburbs
    if (nrow(melbourne_central) > 0) {
      melbourne_sub <- melbourne_central[1, ]
      # Calculate original area for comparison
      melbourne_original_area <- tryCatch({
        as.numeric(st_area(melbourne_sub$geometry))
      }, error = function(e) 0)
      
      # Union all specific suburbs that have been processed
      if (length(corrected_suburbs) > 0) {
        specific_union <- bind_rows(corrected_suburbs) %>%
          st_as_sf() %>%
          st_union() %>%
          st_make_valid()
        
        # Check if Melbourne intersects with specific suburbs
        melbourne_intersects_specific <- tryCatch({
          st_intersects(melbourne_sub$geometry, specific_union, sparse = FALSE)[1, 1]
        }, error = function(e) FALSE)
        
        if (melbourne_intersects_specific) {
          # Calculate overlap area
          overlap_geom <- tryCatch({
            suppressWarnings(st_intersection(melbourne_sub$geometry, specific_union))
          }, error = function(e) NULL)
          
          overlap_area <- 0
          if (!is.null(overlap_geom)) {
            is_empty <- tryCatch({
              result <- st_is_empty(overlap_geom)
              if (length(result) == 0) {
                TRUE
              } else if (is.na(result)) {
                TRUE
              } else {
                as.logical(result)
              }
            }, error = function(e) TRUE)
            
            if (length(is_empty) > 0 && !is_empty) {
              overlap_area <- tryCatch({
                result <- as.numeric(st_area(overlap_geom))
                ifelse(is.na(result), 0, result)
              }, error = function(e) 0)
            }
          }
          
          cat('[PREPROCESS]   Melbourne overlaps with specific suburbs - overlap area:', 
              format(overlap_area, scientific = FALSE), '\n')
          cat('[PREPROCESS]   Melbourne original area:', format(melbourne_original_area, scientific = FALSE), '\n')
        } else {
          cat('[PREPROCESS]   Melbourne does not overlap with specific suburbs\n')
        }
        
        # Melbourne only gets areas NOT claimed by specific suburbs
        melbourne_corrected_geom <- suppressWarnings(
          st_difference(melbourne_sub$geometry, specific_union)
        ) %>% st_make_valid()
        
        # Calculate new area
        melbourne_new_area <- tryCatch({
          as.numeric(st_area(melbourne_corrected_geom))
        }, error = function(e) 0)
        
        if (!st_is_empty(melbourne_corrected_geom) && 
            inherits(melbourne_corrected_geom[[1]], c("POLYGON", "MULTIPOLYGON"))) {
          area_reduction <- melbourne_original_area - melbourne_new_area
          cat('[PREPROCESS]   Melbourne new area:', format(melbourne_new_area, scientific = FALSE), '\n')
          if (area_reduction > 0.000001) {
            cat('[PREPROCESS]   Area reduced by:', format(area_reduction, scientific = FALSE), 
                '(', round(100 * area_reduction / melbourne_original_area, 2), '%)\n')
          } else {
            cat('[PREPROCESS]   No significant area change (Melbourne did not overlap significantly)\n')
          }
          melbourne_sub$geometry <- melbourne_corrected_geom
          corrected_suburbs[[length(corrected_suburbs) + 1]] <- melbourne_sub
          cat('[PREPROCESS]   ✓ Melbourne - assigned only non-overlapping areas (yielded to specific suburbs)\n')
        } else {
          cat('[PREPROCESS]   ⚠ Warning: Melbourne has no unique areas after yielding to other suburbs\n')
          corrected_suburbs[[length(corrected_suburbs) + 1]] <- melbourne_sub
        }
      } else {
        # No other suburbs, keep Melbourne as-is
        corrected_suburbs[[length(corrected_suburbs) + 1]] <- melbourne_sub
        cat('[PREPROCESS]   ✓ Melbourne - kept (no other suburbs)\n')
      }
    }
    
    if (length(corrected_suburbs) > 0) {
      melbourne_suburbs <- bind_rows(corrected_suburbs) %>%
        st_as_sf() %>%
        st_make_valid() %>%
        filter(!st_is_empty(geometry)) %>%
        distinct(suburb_name, .keep_all = TRUE)
      
      cat('[PREPROCESS] ✓ Overlap resolution complete:', nrow(melbourne_suburbs), 'suburbs remain\n')
    }
  } else {
    melbourne_suburbs <- melbourne_suburbs %>%
      distinct(suburb_name, .keep_all = TRUE)
  }
  
  cat('[PREPROCESS] ✓ Final result:', nrow(melbourne_suburbs), 'unique suburbs (no overlaps, gaps preserved)\n')
} else {
  cat('[PREPROCESS] WARNING: Individual clipping failed for all suburbs\n')
  cat('[PREPROCESS] Attempting fallback: using suburbs as-is if within boundary...\n')
  
  # Fallback: use suburbs that are within boundary, clip those that overlap
  melbourne_suburbs_list_fallback <- list()
  
  for (i in seq_len(nrow(melbourne_suburbs_raw))) {
    suburb <- melbourne_suburbs_raw[i, ]
    
    # Check if suburb is within boundary
    is_within <- tryCatch({
      st_within(suburb$geometry, boundary_for_clip, sparse = FALSE)[1, 1]
    }, error = function(e) FALSE)
    
    if (is_within) {
      # Use suburb as-is
      melbourne_suburbs_list_fallback[[length(melbourne_suburbs_list_fallback) + 1]] <- suburb
      cat('[PREPROCESS]   ✓ Using', suburb$suburb_name, '(within boundary)\n')
    } else {
      # Try to clip using st_crop or st_intersection with different approach
      tryCatch({
        # Use st_filter with st_within predicate
        clipped <- st_filter(suburb, boundary_for_clip, .predicate = st_within)
        if (nrow(clipped) > 0 && !st_is_empty(clipped$geometry[[1]])) {
          melbourne_suburbs_list_fallback[[length(melbourne_suburbs_list_fallback) + 1]] <- clipped
          cat('[PREPROCESS]   ✓ Filtered', suburb$suburb_name, '\n')
        }
      }, error = function(e) {
        cat('[PREPROCESS]   ✗ Could not process', suburb$suburb_name, '\n')
      })
    }
  }
  
  if (length(melbourne_suburbs_list_fallback) > 0) {
    melbourne_suburbs <- bind_rows(melbourne_suburbs_list_fallback) %>%
      select(suburb_name, geometry) %>%
      st_as_sf() %>%
      st_make_valid() %>%
      group_by(suburb_name) %>%
      summarise(geometry = st_union(geometry), .groups = 'drop') %>%
      st_make_valid() %>%
      filter(!st_is_empty(geometry)) %>%
      distinct(suburb_name, .keep_all = TRUE)
    
    cat('[PREPROCESS] ✓ Fallback result:', nrow(melbourne_suburbs), 'suburbs\n')
    
    # Apply overlap resolution to fallback results too (with priority logic)
    cat('[PREPROCESS] Applying overlap resolution to fallback results...\n')
    cat('[PREPROCESS] Priority: Specific suburbs keep boundaries; Melbourne yields\n')
    n_suburbs <- nrow(melbourne_suburbs)
    if (n_suburbs > 1) {
      # Same priority logic: specific suburbs first, Melbourne last
      melbourne_central <- melbourne_suburbs %>%
        filter(toupper(suburb_name) == "MELBOURNE")
      other_suburbs_list <- melbourne_suburbs %>%
        filter(toupper(suburb_name) != "MELBOURNE")
      
      corrected_suburbs <- list()
      
      # Process specific suburbs first (they have priority)
      if (nrow(other_suburbs_list) > 0) {
        for (i in seq_len(nrow(other_suburbs_list))) {
          suburb <- other_suburbs_list[i, ]
          other_specific <- other_suburbs_list[-i, ]
          if (nrow(other_specific) > 0) {
            other_union <- suppressWarnings(
              st_union(other_specific$geometry) %>% st_make_valid()
            )
            corrected_geom <- suppressWarnings(
              st_difference(suburb$geometry, other_union)
            ) %>% st_make_valid()
            if (!st_is_empty(corrected_geom) && 
                inherits(corrected_geom[[1]], c("POLYGON", "MULTIPOLYGON"))) {
              suburb$geometry <- corrected_geom
            }
          }
          corrected_suburbs[[length(corrected_suburbs) + 1]] <- suburb
        }
      }
      
      # Process Melbourne last - remove areas claimed by specific suburbs
      if (nrow(melbourne_central) > 0) {
        melbourne_sub <- melbourne_central[1, ]
        if (length(corrected_suburbs) > 0) {
          specific_union <- bind_rows(corrected_suburbs) %>%
            st_as_sf() %>%
            st_union() %>%
            st_make_valid()
          melbourne_corrected_geom <- suppressWarnings(
            st_difference(melbourne_sub$geometry, specific_union)
          ) %>% st_make_valid()
          if (!st_is_empty(melbourne_corrected_geom) && 
              inherits(melbourne_corrected_geom[[1]], c("POLYGON", "MULTIPOLYGON"))) {
            melbourne_sub$geometry <- melbourne_corrected_geom
          }
        }
        corrected_suburbs[[length(corrected_suburbs) + 1]] <- melbourne_sub
      }
      
      if (length(corrected_suburbs) > 0) {
        melbourne_suburbs <- bind_rows(corrected_suburbs) %>%
          st_as_sf() %>%
          st_make_valid() %>%
          filter(!st_is_empty(geometry)) %>%
          distinct(suburb_name, .keep_all = TRUE)
      }
    }
    cat('[PREPROCESS] ✓ Overlap resolution applied to fallback (priority-based)\n')
  } else {
    # Last resort: use raw suburbs without clipping
    cat('[PREPROCESS] ERROR: All methods failed, using unclipped suburbs (may extend beyond boundary)\n')
    melbourne_suburbs <- melbourne_suburbs_raw %>%
      distinct(suburb_name, .keep_all = TRUE)
  }
}

cat('[PREPROCESS] ============================================\n')
cat('[PREPROCESS] SUBURB PREPROCESSING COMPLETE\n')
cat('[PREPROCESS] Total suburbs for mapping:', nrow(melbourne_suburbs), '\n')
cat('[PREPROCESS] Suburbs:', paste(melbourne_suburbs$suburb_name, collapse=", "), '\n')
cat('[PREPROCESS] Overlap resolution:', if(nrow(melbourne_suburbs) > 1) 'APPLIED' else 'N/A', '\n')
cat('[PREPROCESS] ============================================\n')



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
          column(4, uiOutput('infopt'))
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
                h4("Map Filters"),
                selectInput("crime_category_filter", "Crime Category:",
                           choices = c("All Offences" = "all",
                                      "Crimes Against Person" = "person",
                                      "Property & Deception" = "property",
                                      "Drug Offences" = "drug",
                                      "Public Order" = "public"),
                           selected = "all"),
                hr(),
                h4("Landmark Filters"),
                selectInput(
                  inputId = 'type_crime',
                  label = 'POI Type',
                  choices = c('All', unique(pois$subtype)),
                  selected = 'All'
                ),
                selectizeInput(
                  "lm_name_crime",
                  "Landmark(s)",
                  choices = NULL,
                  options = list(
                    placeholder = "Type to search...",
                    plugins = list('remove_button')
                  ),
                  multiple = TRUE
                )
              ),
          # Hidden links for tooltip redirects (not visible to users)
          tags$div(
            style = "display: none;",
            actionLink("nav_categories", ""),
            actionLink("nav_suburbs", ""),
            actionLink("nav_tables", ""),
            actionLink("nav_locations", "")
          )
        ),
        column(
          width = 9,
          tabsetPanel(
            id = "crime_tabs",
            type = "tabs",
            selected = "crime_map_tab",
            tabPanel(
              "Overview",
              value = "overview_tab",
              br(),
              fluidRow(
                column(4, uiOutput("total_offences")),
                column(4, uiOutput("crime_rate")),
                column(4, uiOutput("police_region"))
              ),
              br(),
              h4("Location Types Breakdown (All Suburbs - breakdown by suburb not available)"),
              plotlyOutput("location_sunburst", height = 600)
            ),
            tabPanel(
              "Crime Map",
              value = "crime_map_tab",
              h3("Interactive Visualization"),
              p("Click on suburbs to see detailed crime statistics. Use the 'Explore More' buttons in tooltips to navigate to other views. Use the filters on the left to customize the view."),
              leafletOutput("crime_map", width = "100%", height = 600)
            ),
            tabPanel(
              "Investigation Status",
              value = "investigation_tab",
              br(),
              fluidRow(
                column(6, plotlyOutput("investigation_pie", height = 400)),
                column(6, plotlyOutput("investigation_bar", height = 400))
              ),
              DTOutput("investigation_table")
            ),
            tabPanel(
              "Drug Offences",
              value = "drugs_tab",
              br(),
              selectInput("drug_filter", "Drug Filter By:",
                          choices = c("Offence Subdivision", "Offence Group", "CSA Drug Type"),
                          selected = "CSA Drug Type"),
              plotlyOutput("drug_plot", height = 500)
            )
          ),
          # Hidden panels for tooltip navigation (not visible as tabs)
          tags$div(
            id = "hidden_categories_panel",
            style = "display: none;",
            uiOutput("categories_panel")
          ),
          tags$div(
            id = "hidden_tables_panel",
            style = "display: none;",
            uiOutput("tables_panel")
          )
        )
      )
    )
  )
)



# --- Server ---
server <- function(input, output, session) {
  
  # Reactive value to store selected suburb from map tooltip
  selected_suburb_filter <- reactiveVal(NULL)
  
  # Reactive value to track current view for clear filter functionality
  current_view <- reactiveVal("crime_map")
  
  
  
# --- PT Visualisations ---
  
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
      addCircleMarkers(., data = currpois, lng = ~lon, lat = ~lat, 
                       radius = 7, 
                       stroke = FALSE, 
                       fill = TRUE, 
                       fillColor = ~colour, 
                       fillOpacity = 1, 
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
    else if (startsWith(id, 'stop')) {
      currstop <- stops2[stops$id == id, ]
      
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
      
      # stop info box
      output$infopt <- renderUI({
        
        # panel
        tagList(
          h3(currstop$name), 
          h6(''), 
          h4('Routes:'), 
          tags$ul(lapply(unique(linesnear$shortname), tags$li))
        )
      })
    }
    
    # station click
    else if (startsWith(id, 'station')) {
      currstation <- stations2[stations2$id == id, ]
      
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
    
      # station info box
      output$infopt <- renderUI({
        
        # panel
        tagList(
          h3(paste0(currstation$station, ' Station')), 
          h6(''), 
          h4('Lines:'), 
          tags$ul(lapply(unique(linesnear$shortname), tags$li)),
          h6(''), 
          h4(paste0('Annual Patronage: ', format(currstation$annual, big.mark = ','))), 
          h6(''), 
          girafeOutput('typeplot', height = 300), 
          h6(''), 
          girafeOutput('timeplot', height = 300)
        )
      })
      
      # type of day patronage
      output$typeplot <- renderGirafe({
        
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
    }
  })
    
    
    
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
      addCircleMarkers(
        ., data = filtered_landmarks, lng = ~lon, lat = ~lat,
        radius = 7, 
        stroke = FALSE, 
        fill = TRUE, 
        fillColor = ~colour, 
        fillOpacity = 1, 
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
        radius = 7,
        stroke = FALSE,
        fillOpacity = 1,
        fillColor = ~colour,
        label = ~name,
        layerId = ~id,
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
  
  
  
  # --- Crime Visualisations ---

  # Crime Map Active State
  output$crime_map_active <- reactive({ TRUE })
  outputOptions(output, "crime_map_active", suspendWhenHidden = FALSE)

  # Crime Map Data (reactive) - suburb-based crime data with geometries
  crime_suburb_data <- reactive({
    # Apply category filter - default to "all" if NULL or not set
    category_filter <- if (is.null(input$crime_category_filter)) "all" else input$crime_category_filter

    # Filter raw data by category if a specific category is selected
    if (category_filter != "all") {
      filtered_data <- crime_data$table03 %>%
        filter(
          case_when(
            category_filter == "person" ~ grepl("person", `Offence Division`, ignore.case = TRUE),
            category_filter == "property" ~ grepl("property|deception", `Offence Division`, ignore.case = TRUE),
            category_filter == "drug" ~ grepl("drug", `Offence Division`, ignore.case = TRUE),
            category_filter == "public" ~ grepl("public order|security|justice", `Offence Division`, ignore.case = TRUE),
            TRUE ~ TRUE
          )
        )
    } else {
      filtered_data <- crime_data$table03
    }

    # Get crime data by suburb - compute totals and category breakdowns
    crime_summary <- filtered_data %>%
      group_by(`Suburb/Town Name`) %>%
      summarise(
        Display_Count = sum(`Offence Count`, na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Also compute all category breakdowns for tooltip info (if needed in future)
    all_categories <- crime_data$table03 %>%
      group_by(`Suburb/Town Name`) %>%
      summarise(
        Total_Offences = sum(`Offence Count`, na.rm = TRUE),
        Person_Crimes = sum(`Offence Count`[grepl("person", `Offence Division`, ignore.case = TRUE)], na.rm = TRUE),
        Property_Crimes = sum(`Offence Count`[grepl("property|deception", `Offence Division`, ignore.case = TRUE)], na.rm = TRUE),
        Drug_Crimes = sum(`Offence Count`[grepl("drug", `Offence Division`, ignore.case = TRUE)], na.rm = TRUE),
        Public_Order = sum(`Offence Count`[grepl("public order|security|justice", `Offence Division`, ignore.case = TRUE)], na.rm = TRUE),
        .groups = 'drop'
      )
    
    # Merge Display_Count with all categories
    crime_summary <- crime_summary %>%
      left_join(all_categories, by = "Suburb/Town Name") %>%
      mutate(
        Total_Offences = ifelse(is.na(Total_Offences), 0, Total_Offences),
        Person_Crimes = ifelse(is.na(Person_Crimes), 0, Person_Crimes),
        Property_Crimes = ifelse(is.na(Property_Crimes), 0, Property_Crimes),
        Drug_Crimes = ifelse(is.na(Drug_Crimes), 0, Drug_Crimes),
        Public_Order = ifelse(is.na(Public_Order), 0, Public_Order)
      )
    
    # Filter out suburbs with 0 offences for the selected category
    crime_summary <- crime_summary %>%
      filter(Display_Count > 0)

    # Join with suburb geometries
    # Convert suburb names to uppercase to match the geometry data
    crime_with_geo <- melbourne_suburbs %>%
      mutate(suburb_match = toupper(`suburb_name`)) %>%
      left_join(
        crime_summary %>% mutate(suburb_match = toupper(`Suburb/Town Name`)),
        by = "suburb_match"
      ) %>%
      filter(!is.na(Display_Count)) %>%  # Only keep suburbs with crime data for selected category
      select(suburb_name, `Suburb/Town Name`, Total_Offences, Person_Crimes,
             Property_Crimes, Drug_Crimes, Public_Order, Display_Count, geometry)

    crime_with_geo
  })

  # Crime Map Output - Base map (renders once)
  output$crime_map <- renderLeaflet({
    # Get initial crime data with default filters
    isolate({
      # Get crime data by suburb with category breakdown
      crime_summary <- crime_data$table03 %>%
        group_by(`Suburb/Town Name`) %>%
        summarise(
          Total_Offences = sum(`Offence Count`, na.rm = TRUE),
          Person_Crimes = sum(`Offence Count`[grepl("person", `Offence Division`, ignore.case = TRUE)], na.rm = TRUE),
          Property_Crimes = sum(`Offence Count`[grepl("property|deception", `Offence Division`, ignore.case = TRUE)], na.rm = TRUE),
          Drug_Crimes = sum(`Offence Count`[grepl("drug", `Offence Division`, ignore.case = TRUE)], na.rm = TRUE),
          Public_Order = sum(`Offence Count`[grepl("public order|security|justice", `Offence Division`, ignore.case = TRUE)], na.rm = TRUE),
          .groups = 'drop'
        )
      
      # Default to "all" category
      category_filter <- "all"
      crime_summary <- crime_summary %>%
        mutate(Display_Count = Total_Offences)
      
      # Filter out suburbs with 0 offences
      crime_summary <- crime_summary %>%
        filter(Display_Count > 0)
      
      # Join with suburb geometries
      crime_suburbs <- melbourne_suburbs %>%
        mutate(suburb_match = toupper(`suburb_name`)) %>%
        left_join(
          crime_summary %>% mutate(suburb_match = toupper(`Suburb/Town Name`)),
          by = "suburb_match"
        ) %>%
        filter(!is.na(Total_Offences)) %>%
        select(suburb_name, `Suburb/Town Name`, Total_Offences, Person_Crimes,
               Property_Crimes, Drug_Crimes, Public_Order, Display_Count, geometry)
    })
    
    # Create color palette
    pal <- NULL
    if (nrow(crime_suburbs) > 0) {
      pal <- colorNumeric(
        palette = c("#FEF0D9", "#FDCC8A", "#FC8D59", "#E34A33", "#B30000"),
        domain = crime_suburbs$Display_Count
      )
    }
    
    # Create a base map centered on Melbourne (same as other tabs)
    map <- leaflet() %>%
      addProviderTiles(providers$CartoDB) %>%
      setView(lng = 144.9631, lat = -37.8136, zoom = 13) %>%
      # Add boundary (same as other tabs)
      addPolygons(data = boundary,
                 color = 'black',
                 weight = 3,
                 fill = FALSE)
    
    # Add landmarks by default (on separate "All Landmarks" layer)
    if (nrow(pois) > 0) {
      map <- map %>%
        addCircleMarkers(
          data = pois,
          lng = ~lon, lat = ~lat,
          radius = 7,
          stroke = FALSE,
          fill = TRUE,
          fillColor = ~colour,
          fillOpacity = 1,
          label = ~name,
          layerId = ~id,
          group = 'All Landmarks'
        )
    }
    
    # Add crime suburb polygons by default
    if (nrow(crime_suburbs) > 0 && !is.null(pal)) {
      map <- map %>%
        addPolygons(
          data = crime_suburbs,
          fillColor = ~pal(Display_Count),
          fillOpacity = 0.6,
          stroke = TRUE,
          weight = 0.8,
          color = "#666666",
          opacity = 0.7,
          dashArray = "5, 5",
          highlight = highlightOptions(
            weight = 1.5,
            color = "#333",
            fillOpacity = 0.8,
            bringToFront = TRUE,
            dashArray = "5, 5"
          ),
          popup = lapply(seq_len(nrow(crime_suburbs)), function(i) {
            suburb <- crime_suburbs[i, ]
            suburb_name_js <- gsub("'", "\\'", suburb$suburb_name)
            suburb_name_js <- gsub('"', '\\"', suburb_name_js)
            HTML(paste0(
              "<div style='font-family: Arial;'>",
              "<h4 style='margin: 0 0 10px 0;'>", htmlEscape(suburb$suburb_name), "</h4>",
              "<p><strong>Current Filter:</strong> ", format(suburb$Display_Count, big.mark = ","), " offences</p>",
              "<hr>",
              "<div style='margin-top: 10px;'>",
              "<a href='#' onclick=\"Shiny.setInputValue('map_suburb_filter', '", suburb_name_js, "', {priority: 'event'}); setTimeout(function() { $('#nav_categories').click(); }, 50); return false;\" style='background: #007bff; color: white; padding: 5px 10px; margin: 2px; border-radius: 3px; text-decoration: none; display: inline-block;'>Categories details</a> ",
              "<a href='#' onclick=\"Shiny.setInputValue('map_suburb_filter', '", suburb_name_js, "', {priority: 'event'}); setTimeout(function() { $('#nav_tables').click(); }, 50); return false;\" style='background: #6c757d; color: white; padding: 5px 10px; margin: 2px; border-radius: 3px; text-decoration: none; display: inline-block;'>Data details</a>",
              "</div>",
              "</div>"
            ))
          }),
          popupOptions = popupOptions(
            maxWidth = 300,
            style = list("font-size" = "12px")
          ),
          group = 'Crime Suburbs'
        ) %>%
        addLegend(
          "bottomleft",
          pal = pal,
          values = crime_suburbs$Display_Count,
          title = "All Offences<br/>Offence Count",
          opacity = 0.7,
          layerId = "crime_legend"
        )
    }
    
    # Add layer control
    map <- map %>%
      addLayersControl(
        overlayGroups = c("Crime Suburbs", "All Landmarks"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    map
  })

  # Initialize map when switching to Crime tab
  observeEvent(input$mypage, {
    if (!is.null(input$mypage) && input$mypage == "Melbourne Crime") {
      # Use isolate to prevent reactive dependencies
      isolate({
        crime_suburbs <- crime_suburb_data()

        if (nrow(crime_suburbs) > 0) {
          category_name <- if (!is.null(input$crime_category_filter)) {
            switch(input$crime_category_filter,
                   "all" = "All Offences",
                   "person" = "Crimes Against Person",
                   "property" = "Property & Deception",
                   "drug" = "Drug Offences",
                   "public" = "Public Order",
                   "All Offences")
          } else {
            "All Offences"
          }
          updateCrimeMap(crime_suburbs, category_name)
        }
      })
    }
  }, ignoreInit = FALSE)

  # Define the update function
  updateCrimeMap <- function(crime_suburbs, category_name) {
    # Create color palette
    if (nrow(crime_suburbs) > 0) {
      pal <- colorNumeric(
        palette = c("#FEF0D9", "#FDCC8A", "#FC8D59", "#E34A33", "#B30000"),
        domain = crime_suburbs$Display_Count
      )
    }

    # Get filtered POIs based on current input
    if (is.null(input$type_crime) || input$type_crime == 'All') {
      filtered_pois_to_add <- pois
    } else {
      filtered_pois_to_add <- pois[pois$subtype == input$type_crime, ]
    }
    
    # Update map layers
    leafletProxy("crime_map") %>%
      clearGroup("Crime Heatmap") %>%
      clearGroup("Crime Suburbs") %>%
      clearGroup("All Landmarks") %>%
      # Add filtered POI landmarks
      {
        if (nrow(filtered_pois_to_add) > 0) {
          addCircleMarkers(
            .,
            data = filtered_pois_to_add,
            lng = ~lon, lat = ~lat,
            radius = 7,
            stroke = FALSE,
            fill = TRUE,
            fillColor = ~colour,
            fillOpacity = 1,
            label = ~name,
            layerId = ~id,
            group = 'All Landmarks'
          )
        } else {
          .
        }
      } %>%
      {
        # Add suburb polygons
        if (nrow(crime_suburbs) > 0) {
          addPolygons(
            .,
            data = crime_suburbs,
            fillColor = ~pal(Display_Count),
            fillOpacity = 0.6,
            stroke = TRUE,
            weight = 0.8,
            color = "#666666",
            opacity = 0.7,
            dashArray = "5, 5",
            highlight = highlightOptions(
              weight = 1.5,
              color = "#333",
              fillOpacity = 0.8,
              bringToFront = TRUE,
              dashArray = "5, 5"
            ),
            popup = lapply(seq_len(nrow(crime_suburbs)), function(i) {
              suburb <- crime_suburbs[i, ]
              suburb_name_js <- gsub("'", "\\'", suburb$suburb_name)
              suburb_name_js <- gsub('"', '\\"', suburb_name_js)
              HTML(paste0(
                "<div style='font-family: Arial;'>",
                "<h4 style='margin: 0 0 10px 0;'>", htmlEscape(suburb$suburb_name), "</h4>",
                "<p><strong>Showing:</strong> ", htmlEscape(category_name), "</p>",
                "<p><strong>Current Filter:</strong> ", format(suburb$Display_Count, big.mark = ","), " offences</p>",
                "<hr>",
                "<div style='margin-top: 10px;'>",
                "<a href='#' onclick=\"Shiny.setInputValue('map_suburb_filter', '", suburb_name_js, "', {priority: 'event'}); setTimeout(function() { $('#nav_categories').click(); }, 50); return false;\" style='background: #007bff; color: white; padding: 5px 10px; margin: 2px; border-radius: 3px; text-decoration: none; display: inline-block;'>Categories details</a> ",
                "<a href='#' onclick=\"Shiny.setInputValue('map_suburb_filter', '", suburb_name_js, "', {priority: 'event'}); setTimeout(function() { $('#nav_tables').click(); }, 50); return false;\" style='background: #6c757d; color: white; padding: 5px 10px; margin: 2px; border-radius: 3px; text-decoration: none; display: inline-block;'>Data details</a>",
                "</div>",
                "</div>"
              ))
            }),
            popupOptions = popupOptions(
              maxWidth = 300,
              style = list("font-size" = "12px")
            ),
            group = 'Crime Suburbs'
          ) %>%
          addLegend(
            "bottomleft",
            pal = pal,
            values = crime_suburbs$Display_Count,
            title = paste(category_name, "<br/>Offence Count"),
            opacity = 0.7,
            layerId = "crime_legend"
          )
        } else {
          .
        }
      }
    }

  # --- Crime Tab Landmark Filtering Logic ---
  
  # Filtered POIs by type for crime tab
  filtered_pois_crime <- reactive({
    if (is.null(input$type_crime) || input$type_crime == 'All') {
      return(pois)
    } else {
      return(pois[pois$subtype == input$type_crime, ])
    }
  })

  # Update landmark choices when POI type changes
  observe({
    filtered_pois <- filtered_pois_crime()
    updateSelectizeInput(
      session,
      "lm_name_crime",
      choices = if (nrow(filtered_pois) > 0) sort(unique(filtered_pois$name)) else NULL,
      selected = character(0)
    )
  })
  
  # Update landmarks on map when POI type changes
  observeEvent(input$type_crime, {
    filtered_pois <- filtered_pois_crime()
    
    map <- leafletProxy("crime_map")
    map <- clearGroup(map, "All Landmarks")
    
    if (nrow(filtered_pois) > 0) {
      map <- addCircleMarkers(
        map,
        data = filtered_pois,
        lng = ~lon, lat = ~lat,
        radius = 7,
        stroke = FALSE,
        fill = TRUE,
        fillColor = ~colour,
        fillOpacity = 1,
        label = ~name,
        layerId = ~id,
        group = "All Landmarks"
      )
    }
  })

  # Update map when filters change
  observeEvent(input$crime_category_filter, {
    # Only update if we're on the Crime tab
    if (!is.null(input$mypage) && input$mypage == "Melbourne Crime") {
      crime_suburbs <- crime_suburb_data()

      # Only update if we have data
      if (nrow(crime_suburbs) > 0) {
        # Get category name for display
        category_name <- if (!is.null(input$crime_category_filter)) {
          switch(input$crime_category_filter,
                 "all" = "All Offences",
                 "person" = "Crimes Against Person",
                 "property" = "Property & Deception",
                 "drug" = "Drug Offences",
                 "public" = "Public Order",
                 "All Offences")
        } else {
          "All Offences"
        }

        # Call the update function
        updateCrimeMap(crime_suburbs, category_name)
      }
    }
  }, ignoreNULL = FALSE)

  # Crime Map Summary Table
  output$crime_map_table <- renderDT({
    # Get suburb-level summary from the crime data
    crime_suburb_summary <- crime_data$table03 %>%
      group_by(`Suburb/Town Name`) %>%
      summarise(
        Total_Offences = sum(`Offence Count`, na.rm = TRUE),
        Person_Crimes = sum(`Offence Count`[grepl("person", `Offence Division`, ignore.case = TRUE)], na.rm = TRUE),
        Property_Crimes = sum(`Offence Count`[grepl("property|deception", `Offence Division`, ignore.case = TRUE)], na.rm = TRUE),
        Drug_Crimes = sum(`Offence Count`[grepl("drug", `Offence Division`, ignore.case = TRUE)], na.rm = TRUE),
        Public_Order = sum(`Offence Count`[grepl("public order|justice", `Offence Division`, ignore.case = TRUE)], na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(desc(Total_Offences))

    # No minimum filter - show all suburbs

    datatable(
      crime_suburb_summary,
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE,
      colnames = c("Suburb", "Total Offences", "Person", "Property", "Drug", "Public Order")
    ) %>%
      formatStyle(
        'Total_Offences',
        background = styleColorBar(crime_suburb_summary$Total_Offences, '#FFA07A'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })

  # Overview - Custom info boxes (replacing valueBox for navbarPage compatibility)
  output$total_offences <- renderUI({
    total <- sum(crime_data$table01$`Offence Count`, na.rm = TRUE)
    tags$div(
      class = "well text-center",
      style = "background-color: #f8d7da; border-color: #f5c6cb; padding: 20px;",
      tags$h3(format(total, big.mark = ","), style = "margin: 0; color: #721c24;"),
      tags$p("Total Offences", style = "margin: 5px 0 0 0; color: #721c24;")
    )
  })

  output$crime_rate <- renderUI({
    rate <- mean(crime_data$table01$`Rate per 100,000 population`, na.rm = TRUE)
    tags$div(
      class = "well text-center",
      style = "background-color: #fff3cd; border-color: #ffeaa7; padding: 20px;",
      tags$h3(round(rate, 1), style = "margin: 0; color: #856404;"),
      tags$p("Crime Rate per 100,000", style = "margin: 5px 0 0 0; color: #856404;")
    )
  })
  
  output$police_region <- renderUI({
    region <- unique(crime_data$table01$`Police Region`)[1]
    if (is.na(region) || length(region) == 0) {
      region <- "N/A"
    }
    tags$div(
      class = "well text-center",
      style = "background-color: #d1ecf1; border-color: #bee5eb; padding: 20px;",
      tags$h3(region, style = "margin: 0; color: #0c5460;"),
      tags$p("Police Region", style = "margin: 5px 0 0 0; color: #0c5460;")
    )
  })
  
  output$overview_summary <- renderPrint({
    summary(crime_data$table01)
  })

  
  # Offence Categories
  # Note: Using table03 instead of table02 because table02 doesn't have suburb information
  # table03 has both suburb and category breakdown, allowing suburb filtering
  output$category_plot <- renderPlotly({
    category_data <- crime_data$table03
    suburb_filter <- selected_suburb_filter()
    # Fallback to input if reactive value not set yet
    if (is.null(suburb_filter) || suburb_filter == "") {
      if (!is.null(input$map_suburb_filter) && input$map_suburb_filter != "") {
        suburb_filter <- input$map_suburb_filter
      }
    }
    if (!is.null(suburb_filter) && suburb_filter != "") {
      category_data <- category_data %>% filter(`Suburb/Town Name` == suburb_filter)
    }
    category_data <- category_data %>%
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
    category_data <- crime_data$table03
    suburb_filter <- selected_suburb_filter()
    # Fallback to input if reactive value not set yet
    if (is.null(suburb_filter) || suburb_filter == "") {
      if (!is.null(input$map_suburb_filter) && input$map_suburb_filter != "") {
        suburb_filter <- input$map_suburb_filter
      }
    }
    if (!is.null(suburb_filter) && suburb_filter != "") {
      category_data <- category_data %>% filter(`Suburb/Town Name` == suburb_filter)
    }
    category_data <- category_data %>%
      group_by(!!sym(input$category_level)) %>%
      summarise(Total = sum(`Offence Count`, na.rm = TRUE)) %>%
      arrange(desc(Total)) %>% head(10) %>%
      rename(Category = !!sym(input$category_level))
    plot_ly(category_data, labels = ~Category, values = ~Total, type = 'pie',
            textposition = 'inside', textinfo = 'label+percent') %>%
      layout(
        title = "Top 10 Categories",
        width = 600,
        height = 600,
        margin = list(l = 50, r = 50, t = 50, b = 50)
      )
  })
  
  output$category_rate <- renderPlotly({
    # For rate calculation, we can't use table03 directly as it doesn't have rate columns
    # But we can calculate rate if suburb is filtered, or show aggregated data
    category_data <- crime_data$table03
    suburb_filter <- selected_suburb_filter()
    # Fallback to input if reactive value not set yet
    if (is.null(suburb_filter) || suburb_filter == "") {
      if (!is.null(input$map_suburb_filter) && input$map_suburb_filter != "") {
        suburb_filter <- input$map_suburb_filter
      }
    }
    if (!is.null(suburb_filter) && suburb_filter != "") {
      category_data <- category_data %>% filter(`Suburb/Town Name` == suburb_filter)
      # When filtered by suburb, show count instead of rate (since table03 doesn't have rate data)
      rate_data <- category_data %>%
        group_by(!!sym(input$category_level)) %>%
        summarise(Total = sum(`Offence Count`, na.rm = TRUE)) %>%
        arrange(desc(Total)) %>% head(10) %>%
        rename(Category = !!sym(input$category_level))
      plot_ly(rate_data,
              x = ~Total, y = ~reorder(Category, Total),
              type = 'bar', orientation = 'h',
              marker = list(color = 'coral')) %>%
        layout(title = "Offence Count by Category (Top 10) - Filtered Suburb",
               xaxis = list(title = "Number of Offences"), yaxis = list(title = ""))
    } else {
      # When not filtered, use table02 which has rate information
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
    }
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
    tryCatch({
      # table04 has location data but no suburb info
      # table03 has suburb info but no location data
      # So we always use table04 for location plots, regardless of suburb filter
      location_data <- crime_data$table04
      suburb_filter <- selected_suburb_filter()
      
      if (nrow(location_data) == 0 || !input$location_level %in% names(location_data)) {
        return(plot_ly() %>% 
               layout(title = paste("No data available for", input$location_level),
                      xaxis = list(title = ""), yaxis = list(title = "")))
      }
      
      location_summary <- location_data %>%
        group_by(!!sym(input$location_level)) %>%
        summarise(Total = sum(`Offence Count`, na.rm = TRUE), .groups = 'drop') %>%
        rename(Location = !!sym(input$location_level)) %>%
        filter(Total > 0)
      
      if (nrow(location_summary) == 0) {
        return(plot_ly() %>% 
               layout(title = paste("No offences found for", input$location_level),
                      xaxis = list(title = ""), yaxis = list(title = "")))
      }
      
      # Note: Location data is shown for all suburbs since table04 doesn't have suburb breakdown
      title_text <- paste("Offences by", input$location_level)
      if (!is.null(suburb_filter) && suburb_filter != "") {
        title_text <- paste0(title_text, " (All Suburbs - location breakdown by suburb not available)")
      }
      
      plot_ly(location_summary, x = ~reorder(Location, Total), y = ~Total,
              type = 'bar', marker = list(color = 'purple')) %>%
        layout(title = title_text)
    }, error = function(e) {
      plot_ly() %>% 
        layout(title = "Error loading location data",
               annotations = list(text = paste("Error:", conditionMessage(e)),
                                x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                                showarrow = FALSE),
               xaxis = list(title = ""), yaxis = list(title = ""))
    })
  })
  
  output$location_sunburst <- renderPlotly({
    tryCatch({
      # table04 has location data but no suburb info
      # table03 has suburb info but no location data
      # So we always use table04 for location hierarchy, regardless of suburb filter
      location_data <- crime_data$table04
      suburb_filter <- selected_suburb_filter()
      required_cols <- c("Location Division", "Location Subdivision", "Location Group")
      
      if (nrow(location_data) == 0 || !all(required_cols %in% names(location_data))) {
        return(plot_ly() %>% 
               layout(title = "No location data available",
                      xaxis = list(title = ""), yaxis = list(title = "")))
      }
      
      level1 <- location_data %>%
        group_by(`Location Division`) %>%
        summarise(Total = sum(`Offence Count`, na.rm = TRUE), .groups = 'drop') %>%
        mutate(labels = `Location Division`, parents = "") %>%
        filter(Total > 0)
      
      level2 <- location_data %>%
        group_by(`Location Division`, `Location Subdivision`) %>%
        summarise(Total = sum(`Offence Count`, na.rm = TRUE), .groups = 'drop') %>%
        mutate(labels = `Location Subdivision`, parents = `Location Division`) %>%
        filter(Total > 0)
      
      level3 <- location_data %>%
        group_by(`Location Subdivision`, `Location Group`) %>%
        summarise(Total = sum(`Offence Count`, na.rm = TRUE), .groups = 'drop') %>%
        mutate(labels = `Location Group`, parents = `Location Subdivision`) %>%
        filter(Total > 0)
      
      sunburst_data <- bind_rows(level1, level2, level3) %>% 
        select(labels, parents, Total) %>%
        filter(Total > 0)
      
      if (nrow(sunburst_data) == 0) {
        return(plot_ly() %>% 
               layout(title = "No location data to display",
                      xaxis = list(title = ""), yaxis = list(title = "")))
      }
      
      # Note: Location data is shown for all suburbs since table04 doesn't have suburb breakdown
      title_text <- "Location Hierarchy Breakdown"
      if (!is.null(suburb_filter) && suburb_filter != "") {
        title_text <- "Location Hierarchy (All Suburbs - breakdown by suburb not available)"
      }
      
      plot_ly(sunburst_data, labels = ~labels, parents = ~parents, values = ~Total,
              type = 'sunburst', branchvalues = 'total') %>%
               layout(title = title_text)
    }, error = function(e) {
      plot_ly() %>% 
        layout(title = "Error loading location hierarchy",
               annotations = list(text = paste("Error:", conditionMessage(e)),
                               x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                               showarrow = FALSE),
               xaxis = list(title = ""), yaxis = list(title = ""))
    })
  })
  
  # Investigation Status
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
            type = 'bar', marker = list(color = 'indianred')) %>%
      layout(xaxis = list(title = "Investigation Status"))
  })
  
  output$investigation_table <- renderDT({
    investigation_data <- crime_data$table05 %>%
      group_by(`Investigation Status`) %>%
      summarise(`Total Offences` = sum(`Offence Count`, na.rm = TRUE),
                `Percentage` = round(sum(`Offence Count`, na.rm = TRUE) /
                                       sum(crime_data$table05$`Offence Count`, na.rm = TRUE) * 100, 2))
    datatable(investigation_data, 
              options = list(pageLength = 10, scrollX = TRUE, 
                            lengthMenu = FALSE,
                            dom = 'tip'),  # 't' = table, 'i' = info, 'p' = pagination (no 'l' for length, no 'f' for search)
              rownames = FALSE)
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
  
  
  output$data_table <- renderDT({
    # Always use table03 (suburbs table)
    selected_table <- crime_data$table03
    
    # Apply category filter if one is selected
    category_filter <- if (is.null(input$crime_category_filter)) "all" else input$crime_category_filter
    if (category_filter != "all") {
      selected_table <- selected_table %>%
        filter(
          case_when(
            category_filter == "person" ~ grepl("person", `Offence Division`, ignore.case = TRUE),
            category_filter == "property" ~ grepl("property|deception", `Offence Division`, ignore.case = TRUE),
            category_filter == "drug" ~ grepl("drug", `Offence Division`, ignore.case = TRUE),
            category_filter == "public" ~ grepl("public order|security|justice", `Offence Division`, ignore.case = TRUE),
            TRUE ~ TRUE
          )
        )
    }
    
    # Apply suburb filter
    suburb_filter <- selected_suburb_filter()
    # Fallback to input if reactive value not set yet
    if (is.null(suburb_filter) || suburb_filter == "") {
      if (!is.null(input$map_suburb_filter) && input$map_suburb_filter != "") {
        suburb_filter <- input$map_suburb_filter
      }
    }
    if (!is.null(suburb_filter) && suburb_filter != "") {
      selected_table <- selected_table %>% filter(`Suburb/Town Name` == suburb_filter)
    }
    datatable(selected_table, 
              options = list(pageLength = 10, scrollX = TRUE, 
                            lengthMenu = FALSE,
                            dom = 'tip'),  # 't' = table, 'i' = info, 'p' = pagination (no 'l' for length, no 'f' for search)
              rownames = FALSE)
  })
  
  # --- Melbourne Crime Navigation Logic ---
  # Handle suburb filter from map
  observeEvent(input$map_suburb_filter, {
    selected_suburb_filter(input$map_suburb_filter)
  })
  
  observeEvent(input$nav_categories, {
    current_view("categories")
    # Ensure filter is set from the input (if it was just set via JavaScript)
    # Read directly from input first, then update reactive value if needed
    if (!is.null(input$map_suburb_filter) && input$map_suburb_filter != "") {
      selected_suburb_filter(input$map_suburb_filter)
    }
    # Get suburb name for title
    suburb_filter <- selected_suburb_filter()
    if (is.null(suburb_filter) || suburb_filter == "") {
      if (!is.null(input$map_suburb_filter) && input$map_suburb_filter != "") {
        suburb_filter <- input$map_suburb_filter
      }
    }
    modal_title <- if (!is.null(suburb_filter) && suburb_filter != "") {
      paste0("Offence Categories (", suburb_filter, ")")
    } else {
      "Offence Categories"
    }
    # Show categories content in a modal
    showModal(modalDialog(
      title = modal_title,
      uiOutput("categories_modal_content"),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Render categories panel content for modal
  # This should be reactive to selected_suburb_filter changes
  output$categories_modal_content <- renderUI({
    # Force reactivity by accessing the reactive value
    # Also check input$map_suburb_filter as a fallback
    suburb_filter <- selected_suburb_filter()
    if (is.null(suburb_filter) || suburb_filter == "") {
      # Fallback to reading directly from input if reactive value hasn't updated yet
      if (!is.null(input$map_suburb_filter) && input$map_suburb_filter != "") {
        suburb_filter <- input$map_suburb_filter
        selected_suburb_filter(suburb_filter)  # Update reactive value
      }
    }
    
    tagList(
      selectInput("category_level", "Category Level:",
                  choices = c("Offence Division", "Offence Subdivision", "Offence Subgroup"),
                  selected = "Offence Division"),
      plotlyOutput("category_plot", height = 500)
    )
  })
  
  # Render categories panel content (kept for potential future use)
  output$categories_panel <- renderUI({
    suburb_filter <- selected_suburb_filter()
    filter_text <- if (!is.null(suburb_filter) && suburb_filter != "") {
      paste0(" (Filtered by: ", suburb_filter, ")")
    } else {
      " (Filtered by: All)"
    }
    
    tagList(
      h3(paste0("Offence Categories", filter_text)),
      if (!is.null(suburb_filter) && suburb_filter != "") {
        actionButton("clear_suburb_filter", "Clear Filter", style = "margin-bottom: 10px;")
      },
      selectInput("category_level", "Category Level:",
                  choices = c("Offence Division", "Offence Subdivision", "Offence Subgroup"),
                  selected = "Offence Division"),
      plotlyOutput("category_plot", height = 500),
      fluidRow(
        column(8, plotlyOutput("top_categories", height = 600)),
        column(4, plotlyOutput("category_rate", height = 400))
      )
    )
  })
  
  observeEvent(input$clear_suburb_filter, {
    selected_suburb_filter(NULL)
    # Re-render the current view without the filter
    view <- current_view()
    
    if (view == "categories") {
      # Trigger re-render of categories modal content
      output$categories_modal_content <- renderUI({
        tagList(
          selectInput("category_level", "Category Level:",
                      choices = c("Offence Division", "Offence Subdivision", "Offence Subgroup"),
                      selected = "Offence Division"),
          plotlyOutput("category_plot", height = 500)
        )
      })
    } else if (view == "tables") {
      # Trigger re-render of tables modal content
      output$tables_modal_content <- renderUI({
        tagList(
          DTOutput("data_table")
        )
      })
    }
  })
  
  # Handle clear filter from modal
  observeEvent(input$clear_suburb_filter_modal, {
    selected_suburb_filter(NULL)
    view <- current_view()
    
    if (view == "categories") {
      output$categories_modal_content <- renderUI({
        tagList(
          selectInput("category_level", "Category Level:",
                      choices = c("Offence Division", "Offence Subdivision", "Offence Subgroup"),
                      selected = "Offence Division"),
          plotlyOutput("category_plot", height = 500)
        )
      })
    } else if (view == "tables") {
      output$tables_modal_content <- renderUI({
        tagList(
          DTOutput("data_table")
        )
      })
    }
  })
  
  observeEvent(input$nav_suburbs, {
    # Suburbs Analysis can be accessed via tooltip if needed
    # For now, this handler can be removed or redirected to Overview
    updateTabsetPanel(session, "crime_tabs", selected = "overview_tab")
  })
  
  observeEvent(input$nav_locations, {
    # Location types are shown in Overview tab
    updateTabsetPanel(session, "crime_tabs", selected = "overview_tab")
  })
  
  observeEvent(input$nav_tables, {
    current_view("tables")
    # Ensure filter is set from the input (if it was just set via JavaScript)
    if (!is.null(input$map_suburb_filter) && input$map_suburb_filter != "") {
      selected_suburb_filter(input$map_suburb_filter)
    }
    # Get suburb name for title
    suburb_filter <- selected_suburb_filter()
    if (is.null(suburb_filter) || suburb_filter == "") {
      if (!is.null(input$map_suburb_filter) && input$map_suburb_filter != "") {
        suburb_filter <- input$map_suburb_filter
      }
    }
    modal_title <- if (!is.null(suburb_filter) && suburb_filter != "") {
      paste0("Data Tables (", suburb_filter, ")")
    } else {
      "Data Tables"
    }
    # Show data tables content in a modal
    showModal(modalDialog(
      title = modal_title,
      uiOutput("tables_modal_content"),
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Render tables panel content for modal
  output$tables_modal_content <- renderUI({
    # Force reactivity by accessing the reactive value
    # Also check input$map_suburb_filter as a fallback
    suburb_filter <- selected_suburb_filter()
    if (is.null(suburb_filter) || suburb_filter == "") {
      # Fallback to reading directly from input if reactive value hasn't updated yet
      if (!is.null(input$map_suburb_filter) && input$map_suburb_filter != "") {
        suburb_filter <- input$map_suburb_filter
        selected_suburb_filter(suburb_filter)  # Update reactive value
      }
    }
    
    tagList(
      DTOutput("data_table")
    )
  })
  
  # Render tables panel content (kept for potential future use)
  output$tables_panel <- renderUI({
    suburb_filter <- selected_suburb_filter()
    filter_text <- if (!is.null(suburb_filter) && suburb_filter != "") {
      paste0(" (Filtered by: ", suburb_filter, ")")
    } else {
      " (Filtered by: All)"
    }
    
    tagList(
      h3(paste0("Data Tables", filter_text)),
      if (!is.null(suburb_filter) && suburb_filter != "") {
        actionButton("clear_suburb_filter", "Clear Filter", style = "margin-bottom: 10px;")
      },
      DTOutput("data_table")
    )
  })
  
  
}

shinyApp(ui, server)
