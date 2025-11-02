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
cran <- lines[match(TRUE, (lines$destination == 'Flinders Street via City Loop') & (lines$shortname == 'Cranbourne')), ]
cran$destination <- 'Cranbourne via City Loop'
pak <- lines[match(TRUE, (lines$destination == 'Flinders Street via City Loop') & (lines$shortname == 'Pakenham')), ]
pak$destination <- 'Pakenham via City Loop'
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
lines <- rbind(lines, flem, cran, pak)

# Systematic deduplication of stops: first by name similarity, then by location proximity
# This handles cases like "Melbourne Central Station/Elizabeth St #5" appearing multiple times

# Helper function to normalize names for comparison
normalize_name_for_grouping <- function(name) {
  # Remove stop numbers like #5, #D1, #D2, etc. at the end
  name <- gsub("\\s*#[A-Z0-9]+\\s*$", "", name, perl = TRUE)
  # Remove any trailing whitespace or punctuation
  name <- trimws(name)
  # Convert to uppercase for comparison
  name <- toupper(name)
  return(name)
}

# Step 1: Group stops by normalized name and mode (handles cases where same name appears multiple times)
# If stops have the same normalized name and mode, they are duplicates regardless of location
stops$normalized_name <- normalize_name_for_grouping(stops$name)
stops$group_id <- NA_integer_

# Convert to metric CRS for accurate distance calculations (will be used later)
stops_proj <- st_transform(stops, 3857)

# Group all stops with same normalized name AND mode together
# This handles cases like "Melbourne Central Station/Elizabeth St #5" appearing multiple times
stops_grouped <- stops %>%
  group_by(normalized_name, mode) %>%
  mutate(
    group_id = cur_group_id()  # Assign same group_id to all stops with same normalized name and mode
  ) %>%
  ungroup()

stops$group_id <- stops_grouped$group_id
current_group <- max(stops$group_id, na.rm = TRUE) + 1L

# Step 2: For stops not yet grouped, group by location and name similarity (catch nearby stops with similar names)
for (i in seq_len(nrow(stops_proj))) {
  if (!is.na(stops$group_id[i])) next  # Already assigned to a group
  
  # Find all stops within 20 meters (more generous threshold)
  distances <- as.numeric(st_distance(stops_proj[i, ], stops_proj))
  within_20m <- distances <= 20  # Within 20m (including itself)
  candidates <- which(within_20m & is.na(stops$group_id))
  
  if (length(candidates) > 0) {
    # Normalize the reference name
    name_base_i <- stops$normalized_name[i]
    
    # Find candidates with similar names
    similar_names <- sapply(candidates, function(j) {
      name_j_base <- stops$normalized_name[j]
      
      # Strategy 1: Exact match after normalization (handles #5, #D1 variations)
      if (name_base_i == name_j_base) return(TRUE)
      
      # Strategy 2: If names contain "/", compare first part
      if (grepl("/", name_base_i, fixed = TRUE) && grepl("/", name_j_base, fixed = TRUE)) {
        parts_i <- strsplit(name_base_i, "/", fixed = TRUE)[[1]]
        parts_j <- strsplit(name_j_base, "/", fixed = TRUE)[[1]]
        # Match if first part is identical
        if (length(parts_i) > 0 && length(parts_j) > 0) {
          if (trimws(parts_i[1]) == trimws(parts_j[1])) return(TRUE)
        }
      }
      
      # Strategy 3: Original names are identical
      if (stops$name[i] == stops$name[j]) return(TRUE)
      
      # Strategy 4: If normalized names share a significant prefix (first 20 chars for better matching)
      if (nchar(name_base_i) >= 20 && nchar(name_j_base) >= 20) {
        if (substr(name_base_i, 1, 20) == substr(name_j_base, 1, 20)) return(TRUE)
      }
      
      return(FALSE)
    })
    
    # Group by mode as well - same physical location can have tram AND bus stops
    same_mode <- stops$mode[candidates] == stops$mode[i]
    
    # Assign group to stops with similar names and same mode
    group_members <- candidates[similar_names & same_mode]
    if (length(group_members) > 0) {
      stops$group_id[group_members] <- current_group
      current_group <- current_group + 1L
    }
  }
}

# For each group, keep only one representative stop (prefer name without # markers, then shorter)
stops_dedup <- stops %>%
  filter(!is.na(group_id)) %>%
  mutate(
    has_marker = grepl("\\s*#[A-Z0-9]+\\s*$", name),  # Prefer names without stop markers
    name_length = nchar(name)
  ) %>%
  group_by(group_id, mode) %>%
  arrange(has_marker, name_length) %>%  # Prefer names without # markers, then shorter names
  slice_head(n = 1) %>%  # Take first (best representative)
  ungroup() %>%
  select(-group_id, -has_marker, -name_length, -normalized_name)

# Handle any stops that weren't grouped (standalone stops)
stops_ungrouped <- stops[is.na(stops$group_id), ]
if (nrow(stops_ungrouped) > 0) {
  stops_ungrouped <- stops_ungrouped %>% select(-group_id, -normalized_name)
  stops <- rbind(stops_dedup, stops_ungrouped)
} else {
  stops <- stops_dedup
}

# Final safety deduplication: remove any remaining duplicates by location, mode, and normalized name
# Use aggressive rounding to catch any remaining duplicates
stops <- stops %>%
  mutate(
    lon_round = round(lon, 3),  # ~100m precision (more aggressive)
    lat_round = round(lat, 3),
    name_normalized = normalize_name_for_grouping(name)
  ) %>%
  distinct(lon_round, lat_round, mode, name_normalized, .keep_all = TRUE) %>%
  select(-lon_round, -lat_round, -name_normalized)

# Additional deduplication: if stops have same name, mode, AND routes, keep only one (regardless of distance)
# First, we need to get lines data for route checking (lines is loaded earlier)
# Convert stops to projected CRS for distance calculations
stops_proj_for_routes <- st_transform(stops, 3857)
lines_proj_for_routes <- st_transform(lines, 3857)

# For each stop, find routes within 30m and create a routes signature
cat("Finding routes for each stop to check for duplicates...\n")
routes_signatures <- sapply(seq_len(nrow(stops_proj_for_routes)), function(i) {
  stop_proj <- stops_proj_for_routes[i, ]
  # Find lines within 30m with same mode
  lines_near <- lines_proj_for_routes[
    as.numeric(st_distance(lines_proj_for_routes, stop_proj)) <= 30 & 
      lines_proj_for_routes$mode == stop_proj$mode, 
  ]
  
  # Create signature: sorted unique route shortnames
  if (nrow(lines_near) > 0) {
    routes <- sort(unique(lines_near$shortname))
    return(paste(routes, collapse = "|"))
  } else {
    return("")  # No routes found
  }
})

# Add routes signature to stops
stops$routes_signature <- routes_signatures

# Group by normalized name + mode + routes signature, keep only one per group
stops <- stops %>%
  mutate(name_normalized = normalize_name_for_grouping(name)) %>%
  group_by(name_normalized, mode, routes_signature) %>%
  arrange(name) %>%  # Prefer first alphabetically
  slice_head(n = 1) %>%  # Keep first (preferred name)
  ungroup() %>%
  select(-routes_signature, -name_normalized)

cat("Routes-based deduplication complete.\n")

# Deduplicate stations based on coordinates and station name
stations <- stations %>%
  mutate(
    lon_round = round(lon, 6),
    lat_round = round(lat, 6)
  ) %>%
  distinct(lon_round, lat_round, station, .keep_all = TRUE) %>%
  select(-lon_round, -lat_round)

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
zones_to_segments <- tryCatch({
  readr::read_delim(URL_ZONES_TO_SEGMENTS, delim = ";", show_col_types = FALSE)
}, error = function(e) {
  message("Zones-to-segments CSV read failed: ", conditionMessage(e))
  data.frame()
})

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
}

if (nrow(sign_plates) > 0) {
  sign_plates <- sign_plates |>
    norm_col(c("parkingzone", "parking_zone", "zone_id", "zoneid"), "zone_id") |>
    dplyr::mutate(zone_id = as.character(zone_id))
  # Don't rename restriction_display - keep original column names
}

# PHASE 2: AGGREGATE BAYS TO SEGMENTS
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
}

# PHASE 3: MAP SEGMENTS TO ZONES
zone_locations <- NULL
if (!is.null(segment_locations) && nrow(segment_locations) > 0 && nrow(zones_to_segments) > 0) {
  # Join segments to zones
  segment_zone_map <- sf::st_drop_geometry(segment_locations) |>
    dplyr::select(segment_id, num_bays) |>
    dplyr::left_join(zones_to_segments, by = "segment_id", relationship = "many-to-many") |>
    dplyr::filter(!is.na(zone_id))
  
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
}

# PHASE 3.5: HANDLE UNMATCHED SEGMENTS (segments with no zone)
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
}

# PHASE 4: PREPARE SIGN PLATES DATA FOR TABLE DISPLAY

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
}

# PHASE 4.5: COMBINE ZONES AND UNMATCHED SEGMENTS
if (!is.null(unmatched_segments) && nrow(unmatched_segments) > 0) {
  # Add has_zone marker to unmatched segments
  unmatched_segments <- unmatched_segments |>
    dplyr::mutate(has_zone = FALSE)
  
  # Combine zones and unmatched segments
  if (!is.null(zones_display) && nrow(zones_display) > 0) {
    # Ensure both have same columns
    zones_display <- dplyr::bind_rows(zones_display, unmatched_segments)
  } else {
    # If no zones_display, just use unmatched segments
    zones_display <- unmatched_segments
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
      
      # Get Melbourne bounding box (larger area to capture all suburbs)
      bbox_melb <- getbb("Melbourne, Australia", featuretype = "city")
      if (is.null(bbox_melb) || nrow(bbox_melb) == 0) {
        # Fallback bbox if geocoding fails
        bbox_melb <- matrix(c(144.93, -37.84, 144.98, -37.79), nrow = 2, 
                            dimnames = list(c("x", "y"), c("min", "max")))
      }
      
      # Query all suburbs in Melbourne area - need ADMINISTRATIVE boundaries, not just place tags
      
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
      } 
      
      # If no admin boundaries, try place=suburb but filter for polygons only
      if (is.null(suburbs_all) || nrow(suburbs_all) == 0) {
        q2 <- opq(bbox = bbox_melb) %>%
          add_osm_feature(key = "place", value = "suburb")
        
        osm_data2 <- osmdata_sf(q2)
        
        if (!is.null(osm_data2$osm_multipolygons) && nrow(osm_data2$osm_multipolygons) > 0) {
          suburbs_all <- osm_data2$osm_multipolygons %>%
            filter(!is.na(name)) %>%
            select(name) %>%
            rename(suburb_name = name) %>%
            st_transform(4326)
        } else if (!is.null(osm_data2$osm_polygons) && nrow(osm_data2$osm_polygons) > 0) {
          suburbs_all <- osm_data2$osm_polygons %>%
            filter(!is.na(name)) %>%
            select(name) %>%
            rename(suburb_name = name) %>%
            st_transform(4326)
        }
      }
      
      if (!is.null(suburbs_all) && nrow(suburbs_all) > 0) {
        # Verify these are actually polygons, not points
        geom_types <- st_geometry_type(suburbs_all$geometry)
        polygon_count <- sum(geom_types %in% c("POLYGON", "MULTIPOLYGON"))
        
        # Filter to only polygon geometries (exclude any points/lines that got through)
        if (polygon_count > 0) {
          suburbs_all <- suburbs_all[geom_types %in% c("POLYGON", "MULTIPOLYGON"), ]
          return(suburbs_all)
        }
      }
    }, error = function(e) {
      # OpenStreetMap query failed, will try alternative sources
    })
  }
  
  # Option 2: Try direct download from data.gov.au (Victorian government official data)
  tryCatch({
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
      return(suburbs_all)
    }
  }, error = function(e) {
    # data.gov.au API failed
  })
  
  # Option 3: Use local geojson file (fallback only if OpenStreetMap and API both failed)
  # Note: This file may have outdated boundaries - OpenStreetMap is preferred
  if (file.exists('vic_suburbs.geojson')) {
    suburbs_all <- st_read('vic_suburbs.geojson', quiet = TRUE) %>%
      st_transform(4326)
    return(suburbs_all)
  }
  
  # Option 4: Try backup file if main file doesn't exist
  if (file.exists('vic_suburbs.geojson.backup')) {
    suburbs_all <- st_read('vic_suburbs.geojson.backup', quiet = TRUE) %>%
      st_transform(4326)
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
    name_upper = toupper(!!sym(name_col)),
    matches = sapply(!!sym(name_col), function(x) match_suburb_name(x, melbourne_suburb_names))
  ) %>%
  filter(matches) %>%
  rename(suburb_name = !!sym(name_col)) %>%
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

# Ensure both are in the same CRS

# Transform both to a common CRS (WGS84/4326) for reliable intersection
boundary_for_clip <- st_transform(boundary, 4326)
melbourne_suburbs_raw <- melbourne_suburbs_raw %>%
  st_transform(4326) %>%
  mutate(geometry = suppressWarnings(st_make_valid(geometry))) %>%
  filter(!st_is_empty(geometry))

# Clip suburbs to city boundary with improved gap handling

# Clip each suburb individually to avoid intersection artifacts that cause misassignment
melbourne_suburbs_list <- list()
success_count <- 0

for (i in seq_len(nrow(melbourne_suburbs_raw))) {
  suburb <- melbourne_suburbs_raw[i, ]
  tryCatch({
    # First check if suburb intersects with boundary at all
    if (!st_intersects(suburb$geometry, boundary_for_clip, sparse = FALSE)[1, 1]) {
      next
    }
    
    # Verify suburb is actually a polygon before clipping
    suburb_geom_type <- as.character(st_geometry_type(suburb$geometry[[1]]))
    if (!suburb_geom_type %in% c("POLYGON", "MULTIPOLYGON")) {
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
      }
    } else {
      # Intersection returned non-polygon - suburb might be completely within or on edge
      if (st_within(suburb$geometry, boundary_for_clip, sparse = FALSE)[1, 1]) {
        # Suburb is completely within boundary, use it as-is
        melbourne_suburbs_list[[success_count + 1]] <- suburb %>%
          mutate(geometry = suppressWarnings(st_make_valid(geometry))) %>%
          filter(!st_is_empty(geometry))
        success_count <- success_count + 1
      }
    }
  }, error = function(e) {
    # Error clipping suburb
  })
}

# Combine all successfully clipped suburbs
if (length(melbourne_suburbs_list) > 0) {
  melbourne_suburbs <- bind_rows(melbourne_suburbs_list) %>%
    select(suburb_name, geometry) %>%
    st_as_sf() %>%
    st_make_valid()
  
  # Group by suburb name and union any fragmented pieces
  melbourne_suburbs <- melbourne_suburbs %>%
    group_by(suburb_name) %>%
    summarise(geometry = st_union(geometry), .groups = 'drop') %>%
    st_make_valid()
  
  # First, detect if there are actually any overlaps
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
              }
            }
          }
        }
      }
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
          } else {
            corrected_suburbs[[length(corrected_suburbs) + 1]] <- suburb
          }
        } else {
          corrected_suburbs[[length(corrected_suburbs) + 1]] <- suburb
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
          melbourne_sub$geometry <- melbourne_corrected_geom
          corrected_suburbs[[length(corrected_suburbs) + 1]] <- melbourne_sub
        } else {
          corrected_suburbs[[length(corrected_suburbs) + 1]] <- melbourne_sub
        }
      } else {
        # No other suburbs, keep Melbourne as-is
        corrected_suburbs[[length(corrected_suburbs) + 1]] <- melbourne_sub
      }
    }
    
    if (length(corrected_suburbs) > 0) {
      melbourne_suburbs <- bind_rows(corrected_suburbs) %>%
        st_as_sf() %>%
        st_make_valid() %>%
        filter(!st_is_empty(geometry)) %>%
        distinct(suburb_name, .keep_all = TRUE)
    }
  } else {
    melbourne_suburbs <- melbourne_suburbs %>%
      distinct(suburb_name, .keep_all = TRUE)
  }
} else {
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
    } else{
      # Try to clip using st_crop or st_intersection with different approach
      tryCatch({
        # Use st_filter with st_within predicate
        clipped <- st_filter(suburb, boundary_for_clip, .predicate = st_within)
        if (nrow(clipped) > 0 && !st_is_empty(clipped$geometry[[1]])) {
          melbourne_suburbs_list_fallback[[length(melbourne_suburbs_list_fallback) + 1]] <- clipped
        }
      }, error = function(e) {
        # Could not process suburb
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
    
    # Apply overlap resolution to fallback results too (with priority logic)
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
  } else {
    # Last resort: use raw suburbs without clipping
    melbourne_suburbs <- melbourne_suburbs_raw %>%
      distinct(suburb_name, .keep_all = TRUE)
  }
}

# --- UI ---
ui <- navbarPage(
  id = 'mypage', 
  title = '',
  theme = bslib::bs_theme(
    version = 4,
    bootswatch = "flatly",
    base_font = bslib::font_google("Inter", local = FALSE),
    heading_font = bslib::font_google("Inter", local = FALSE),
    primary = "#2c3e50",
    secondary = "#95a5a6",
    success = "#27ae60",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c"
  ),
  header = tags$head(
    tags$style(HTML("
      * {
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
      }
      /* Preserve Font Awesome icon fonts */
      .fa, .fas, .far, .fal, .fab, .fa-solid, .fa-regular, .fa-light, .fa-brands,
      [class*='fa-'], [class^='fa-'] {
        font-family: 'Font Awesome 5 Free', 'Font Awesome 5 Pro', 'Font Awesome 5 Brands', 'FontAwesome' !important;
      }
      body {
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
        font-size: 14px;
        line-height: 1.6;
      }
      h1, h2, h3, h4, h5, h6 {
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
        font-weight: 600;
      }
      .well {
        border-radius: 6px;
        border: 1px solid #dee2e6;
      }
      .form-group {
        margin-bottom: 15px;
      }
      .nav-tabs {
        border-bottom: 2px solid #dee2e6;
      }
      .nav-tabs > li > a {
        border-radius: 4px 4px 0 0;
        margin-right: 2px;
      }
      .leaflet-container {
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
      }
      /* Preserve Font Awesome in Leaflet markers */
      .leaflet-container .fa, .leaflet-container .fas, .leaflet-container .far, 
      .leaflet-container .fal, .leaflet-container .fab,
      .leaflet-container [class*='fa-'], .leaflet-container [class^='fa-'] {
        font-family: 'Font Awesome 5 Free', 'Font Awesome 5 Pro', 'Font Awesome 5 Brands', 'FontAwesome' !important;
      }
      .irs-grid-text, .leaflet-control {
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif !important;
      }
      .sidebar-panel {
        padding: 15px;
      }
      .main-panel {
        padding: 15px;
      }
      .well {
        padding: 15px;
        background-color: #ffffff;
      }
      selectInput, selectizeInput {
        width: 100%;
      }
      .navbar {
        min-height: 50px;
        border-bottom: 2px solid #dee2e6;
      }
      .tab-content {
        padding: 15px 0;
      }
      #crime_tabs .nav-tabs > li > a {
        color: #000000;
      }
      #crime_tabs .nav-tabs > li.active > a {
        color: #28a745;
      }
      #crime_tabs .nav-tabs > li.active > a:hover {
        color: #28a745;
      }
      #crime_tabs .nav-tabs > li > a:hover {
        color: #000000;
      }
      /* Hide layer control for parking map only */
      #map_parking .leaflet-control-layers {
        display: none !important;
      }
      /* Hide layer control for crime map only */
      #crime_map .leaflet-control-layers {
        display: none !important;
      }
    "))
  ),
  
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
        h4("Select Landmarks to Find Nearby Transport"),
        helpText("Choose one or more landmarks to find nearby PT stops and stations"),
        selectizeInput(
          "lm_name_pt",
          "Landmark(s)",
          choices = NULL,
          options = list(
            placeholder = "Type to search...",
            plugins = list('remove_button')
          ),
          multiple = TRUE
        ),
        sliderInput(
          inputId = 'radius',
          label = 'Search Radius (Metres): ',
          min = 100,
          max = 1000,
          value = 300,
          step = 50,
          ticks = TRUE
        ),
        hr(),
        tags$div(
          tags$strong("Transport Mode Legend", style = "color: #2c3e50;"),
          tags$div(
            style = "margin-top: 10px; font-size: 12px;",
            tags$style(HTML("
              .legend-marker-pt {
                display: inline-block;
                position: relative;
                width: 25px;
                height: 35px;
                margin-right: 4px;
                vertical-align: middle;
                border-radius: 50% 50% 50% 0;
                transform: rotate(-45deg);
                box-shadow: 0 2px 4px rgba(0,0,0,0.3);
              }
              .legend-marker-pt.blue {
                background: #38AADD;
              }
              .legend-marker-pt.green {
                background: #72B026;
              }
              .legend-marker-pt.orange {
                background: #F3952F;
              }
              .legend-marker-pt i {
                position: absolute;
                top: 50%;
                left: 50%;
                transform: translate(-50%, -50%) rotate(45deg);
                color: white;
                font-size: 11px;
                font-weight: 900;
              }
            ")),
            tags$table(
              style = "width: 100%; border-collapse: collapse;",
              tags$tbody(
                tags$tr(
                  tags$td(
                    HTML('<div class="legend-marker-pt blue"><i class="fa fa-subway"></i></div>'),
                    style = "padding: 2px 4px;"
                  ),
                  tags$td(tags$strong("Train"), style = "padding: 2px 4px;")
                ),
                tags$tr(
                  tags$td(
                    HTML('<div class="legend-marker-pt green"><i class="fa fa-train"></i></div>'),
                    style = "padding: 2px 4px;"
                  ),
                  tags$td(tags$strong("Tram"), style = "padding: 2px 4px;")
                ),
                tags$tr(
                  tags$td(
                    HTML('<div class="legend-marker-pt orange"><i class="fa fa-bus"></i></div>'),
                    style = "padding: 2px 4px;"
                  ),
                  tags$td(tags$strong("Bus"), style = "padding: 2px 4px;")
                )
              )
            )
          )
        ),
        width = 3,
        style = 'max-width: 400px'
      ), 
      mainPanel(
        leafletOutput('mappt', width = "100%", height = 600),
        br(),
        uiOutput('infopt')
      )
    )
  ), 
  
  
  # --- Parking Tab ---
  tabPanel(
    title = 'On-street Parking',
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
        sliderInput("radius_m_parking", "Search Radius (Metres):",
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
        leafletOutput("map_parking", width = "100%", height = 600),
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

          // Hide the entire layer control panel on parking map
          function hideParkingMapLayerControl() {
            // Search all layer controls and find the one with parking map labels
            var allControls = document.querySelectorAll('.leaflet-control-layers');
            for (var i = 0; i < allControls.length; i++) {
              var labels = allControls[i].querySelectorAll('label');
              var foundParkingLayer = false;
              for (var j = 0; j < labels.length; j++) {
                var labelText = labels[j].textContent.trim();
                // Check if this is the parking map layer control
                if (labelText === 'Boundary' || labelText === 'All Landmarks' || 
                    labelText === 'All Parking Zones' || labelText === 'Buffer') {
                  foundParkingLayer = true;
                  break;
                }
              }
              // If found, hide the entire layer control panel
              if (foundParkingLayer) {
                allControls[i].style.display = 'none';
              }
            }
          }

          // Run on map initialization
          $(document).on('shiny:value', function(event) {
            if (event.target.id === 'map_parking') {
              hideParkingMapLayerControl();
            }
          });

          // Run periodically to catch layer control when it appears
          var checkInterval = setInterval(function() {
            hideParkingMapLayerControl();
            // Stop checking after 5 seconds
          }, 500);
          setTimeout(function() { clearInterval(checkInterval); }, 5000);
        "))
      )
    )
  ), 
  
  
  # --- Pedestrian Tab ---
  tabPanel(
    title = 'Pedestrian Counts',
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = 'type_pedestrian',
          label = 'POI Type',
          choices = c('All', unique(pois$subtype)),
          selected = 'All'
        ),
        h4("Select Landmarks to Find Nearby Sensors"),
        helpText("Choose one or more landmarks to find nearby pedestrian sensors. The circles you see represent pedestrian counting sensors - larger and darker circles indicate higher pedestrian traffic. Each sensor shows the average daily pedestrian count at that location."),
        selectizeInput(
          "lm_name_pedestrian",
          "Landmark(s)",
          choices = NULL,
          options = list(
            placeholder = "Type to search...",
            plugins = list('remove_button')
          ),
          multiple = TRUE
        ),
        sliderInput(
          inputId = 'radius_pedestrian',
          label = 'Search Radius (Metres): ',
          min = 100,
          max = 1000,
          value = 300,
          step = 50,
          ticks = TRUE
        ),
        width = 3,
        style = 'max-width: 400px'
      ),
      mainPanel(
        tabsetPanel(
          id = "view_mode",
          type = "tabs",
          selected = "heatmap",
          tabPanel(
            "Heatmap View",
            value = "heatmap",
            leafletOutput("heatmap", width = "100%", height = 600)
          ),
          tabPanel(
            "Ranking View",
            value = "ranking",
            plotlyOutput("popularity_plot", width = "100%", height = 600)
          ),
          tabPanel(
            "Trend View",
            value = "trend",
            plotlyOutput("trend_plot", width = "100%", height = 600)
          )
        )
      )
    )
  ), 
  
  
  
  # --- Crime Tab ---
  tabPanel(
    "Security View",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput(
          inputId = 'type_crime',
          label = 'POI Type',
          choices = c('All', unique(pois$subtype)),
          selected = 'All'
        ),
        h4("Select Landmarks to Filter Crime Map"),
        helpText("Choose one or more landmarks to filter and explore crime data around specific locations"),
        selectizeInput(
          "lm_name_crime",
          "Landmark(s)",
          choices = NULL,
          options = list(
            placeholder = "Type to search...",
            plugins = list('remove_button')
          ),
          multiple = TRUE
        ),
        hr(),
        selectInput("crime_category_filter", "Crime Category:",
                    choices = c("All Offences" = "all",
                                "Crimes Against Person" = "person",
                                "Property & Deception" = "property",
                                "Drug Offences" = "drug",
                                "Public Order" = "public"),
                    selected = "all"),
        # Hidden links for tooltip redirects (not visible to users)
        tags$div(
          style = "display: none;",
          actionLink("nav_categories", ""),
          actionLink("nav_suburbs", ""),
          actionLink("nav_tables", ""),
          actionLink("nav_locations", "")
        ),
        style = 'max-width: 400px'
      ),
      mainPanel(
        tabsetPanel(
          id = "crime_tabs",
          type = "tabs",
          selected = "crime_map_tab",
          tabPanel(
            "Overview",
            value = "overview_tab",
            h5("Melbourne Crime Statistics - Year Ending June 2025"),
            br(),
            fluidRow(
              column(4, uiOutput("total_offences")),
              column(4, uiOutput("crime_rate")),
              column(4, uiOutput("police_region"))
            ),
            br(),
            h6("Location Types Breakdown (All Suburbs - breakdown by suburb not available)"),
            plotlyOutput("location_sunburst", width = "100%", height = 600)
          ),
          tabPanel(
            "Interactive Map",
            value = "crime_map_tab",
            leafletOutput("crime_map", width = "100%", height = 600)
          ),
          tabPanel(
            "Investigation Status",
            value = "investigation_tab",
            br(),
            fluidRow(
              column(6, plotlyOutput("investigation_pie", width = "100%", height = 400)),
              column(6, plotlyOutput("investigation_bar", width = "100%", height = 400))
            ),
            br(),
            DTOutput("investigation_table", width = "100%")
          ),
          tabPanel(
            "Drug Offences",
            value = "drugs_tab",
            br(),
            selectInput("drug_filter", "Drug Filter By:",
                        choices = c("Offence Subdivision", "Offence Group", "CSA Drug Type"),
                        selected = "CSA Drug Type"),
            br(),
            plotlyOutput("drug_plot", width = "100%", height = 600)
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
        ),
        # JavaScript to hide layer control for crime map
        tags$script(HTML("
            // Hide the entire layer control panel on crime map
            function hideCrimeMapLayerControl() {
              // Search all layer controls and find the one with crime map labels
              var allControls = document.querySelectorAll('.leaflet-control-layers');
              for (var i = 0; i < allControls.length; i++) {
                var labels = allControls[i].querySelectorAll('label');
                var foundCrimeLayer = false;
                for (var j = 0; j < labels.length; j++) {
                  var labelText = labels[j].textContent.trim();
                  // Check if this is the crime map layer control
                  if (labelText === 'Crime Suburbs' || labelText === 'All Landmarks' || 
                      labelText === 'Filtered Landmarks' || labelText === 'Crime Heatmap') {
                    foundCrimeLayer = true;
                    break;
                  }
                }
                // If found, hide the entire layer control panel
                if (foundCrimeLayer) {
                  allControls[i].style.display = 'none';
                }
              }
            }

            // Run on map initialization
            $(document).on('shiny:value', function(event) {
              if (event.target.id === 'crime_map') {
                hideCrimeMapLayerControl();
              }
            });

            // Run periodically to catch layer control when it appears
            var checkIntervalCrime = setInterval(function() {
              hideCrimeMapLayerControl();
              // Stop checking after 5 seconds
            }, 500);
            setTimeout(function() { clearInterval(checkIntervalCrime); }, 5000);
          "))
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
  
  # Store current station for chart rendering
  selected_station_data <- reactiveVal(NULL)
  
  
  
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
                       fillOpacity = 1.0, 
                       label = ~name, 
                       layerId = ~id, 
                       group = 'pois')
  })
  
  # on click
  observeEvent(input$mappt_marker_click, {
    # Only process if we're on the Public Transport tab
    if (is.null(input$mypage) || input$mypage != "Public Transport") return()
    
    # get click
    click <- input$mappt_marker_click
    if (is.null(click$id)) {
      return()
    }
    id <- click$id
    
    # poi click
    if (startsWith(id, 'poi')) {
      currpoi <- pois2[pois$id == id, ]
      
      # Get landmark name from clicked POI
      landmark_name <- pois[pois$id == id, ]$name
      
      # Get current selections and add clicked landmark if not already selected
      current_selection <- input$lm_name_pt
      if (is.null(current_selection)) {
        current_selection <- character(0)
      }
      
      # If clicked landmark is not already in selection, add it; otherwise, remove it (toggle)
      if (landmark_name %in% current_selection) {
        # Remove from selection if already selected (toggle behavior)
        new_selection <- current_selection[current_selection != landmark_name]
      } else {
        # Add to selection
        new_selection <- c(current_selection, landmark_name)
      }
      
      # Update the landmark selection dropdown
      # This will automatically trigger the observeEvent for filtering
      updateSelectizeInput(
        session,
        "lm_name_pt",
        selected = new_selection
      )
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
      
      # Clear info box - information is now in tooltip
      output$infopt <- renderUI({
        NULL
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
      
      # Clear info box - information is now in tooltip
      output$infopt <- renderUI({
        NULL
      })
      
      # Store current station for chart rendering
      selected_station_data(currstation)
    }
  })
  
  # Handle chart button clicks from station tooltips
  observeEvent(input$show_station_chart, {
    if (is.null(input$show_station_chart) || input$show_station_chart == "") return()
    
    chart_info <- strsplit(input$show_station_chart, "_")[[1]]
    if (length(chart_info) < 2) return()
    
    station_id <- paste(chart_info[-length(chart_info)], collapse = "_")
    chart_type <- chart_info[length(chart_info)]
    
    # Find station by ID
    currstation <- stations2[stations2$id == station_id, ]
    if (nrow(currstation) == 0) return()
    
    selected_station_data(currstation)
    
    if (chart_type == "daytype") {
      # Show day type chart modal
      output$modal_day_type_plot <- renderGirafe({
        df <- data.frame(
          type = c('Weekday', 'Normal Weekday', 'Sch/Hol Weekday', 'Saturday', 'Sunday'), 
          value = c(currstation$weekday, 
                    currstation$normalweekday, 
                    currstation$schholweekday, 
                    currstation$saturday, 
                    currstation$sunday)
        )
        df$type <- factor(df$type, levels = c('Weekday', 'Normal Weekday', 'Sch/Hol Weekday', 'Saturday', 'Sunday'))
        
        p <- ggplot(df) + 
          aes(x = type, y = value, tooltip = value) + 
          geom_bar_interactive(stat = 'identity', fill = '#0071cd') + 
          labs(title = 'Average Patronage per Day Type', x = NULL, y = 'Average Patronage') + 
          theme_minimal()
        
        girafe(ggobj = p)
      })
      
      showModal(modalDialog(
        title = paste0(currstation$station, " Station - Average Patronage per Day Type"),
        girafeOutput("modal_day_type_plot", height = 400),
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "l"
      ))
    } else if (chart_type == "timeperiod") {
      # Show time period chart modal
      output$modal_time_period_plot <- renderGirafe({
        df <- data.frame(
          time = c('Before 7am', '7am - 9:30am', '9:30am - 3pm', '3pm - 7pm', 'After 7pm'), 
          value = c(currstation$early, 
                    currstation$ampeak, 
                    currstation$interpeak, 
                    currstation$pmpeak, 
                    currstation$late)
        )
        df$time <- factor(df$time, levels = c('Before 7am', '7am - 9:30am', '9:30am - 3pm', '3pm - 7pm', 'After 7pm'))
        
        p <- ggplot(df) + 
          aes(x = time, y = value, tooltip = value) + 
          geom_bar_interactive(stat = 'identity', fill = '#0071cd') + 
          labs(title = 'Average Patronage per Time Period on Weekdays', x = NULL, y = 'Average Patronage') + 
          theme_minimal()
        
        girafe(ggobj = p)
      })
      
      showModal(modalDialog(
        title = paste0(currstation$station, " Station - Average Patronage per Time Period"),
        girafeOutput("modal_time_period_plot", height = 400),
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "l"
      ))
    }
  })
  
  # --- PT Landmark Filter Logic ---
  
  # Filtered POIs by type for PT tab
  filtered_pois_pt <- reactive({
    if (input$type == 'All') {
      return(pois)
    } else {
      return(pois[pois$subtype == input$type, ])
    }
  })
  
  # Update PT landmark choices when POI type changes
  observe({
    # Only update if we're on the Public Transport tab
    if (is.null(input$mypage) || input$mypage != "Public Transport") return()
    
    filtered_pois <- filtered_pois_pt()
    updateSelectizeInput(
      session,
      "lm_name_pt",
      choices = if (nrow(filtered_pois) > 0) sort(unique(filtered_pois$name)) else NULL,
      server = TRUE
    )
  })
  
  # Selected landmarks for PT tab
  selected_landmarks_pt <- reactive({
    lm <- filtered_pois_pt()
    if (nrow(lm) == 0) return(lm)
    
    if (length(input$lm_name_pt) > 0) {
      lm <- lm |> dplyr::filter(name %in% input$lm_name_pt)
    }
    lm
  })
  
  # Combined buffer around selected PT landmarks
  combined_buffer_pt <- reactive({
    sel_lm <- selected_landmarks_pt()
    req(nrow(sel_lm) > 0)
    
    # use EPSG:7899 (GDA2020 / MGA zone 55) for accurate meter-based buffering in Melbourne
    lm_proj <- tryCatch({
      sf::st_transform(sel_lm, 7899)
    }, error = function(e) {
      message("EPSG:7899 not available, using EPSG:3857")
      sf::st_transform(sel_lm, 3857)
    })
    
    # Create buffer around each landmark
    buf <- sf::st_buffer(lm_proj, dist = input$radius)
    
    # Union all buffers into one
    if (nrow(buf) > 1) {
      buf <- sf::st_union(buf) |> sf::st_sf()
    }
    
    sf::st_transform(buf, 4326)
  })
  
  # Filter PT stops within radius
  stops_in_radius_pt <- reactive({
    req(nrow(selected_landmarks_pt()) > 0)
    buf <- combined_buffer_pt()
    req(nrow(buf) > 0)
    
    # Spatial filter: stops that intersect with buffer
    if (nrow(stops) > 0) {
      intersects <- sf::st_intersects(stops, buf, sparse = FALSE)
      stops_filtered <- stops[as.vector(intersects), ]
      return(stops_filtered)
    } else {
      return(stops[0, ])
    }
  })
  
  # Filter PT stations within radius
  stations_in_radius_pt <- reactive({
    req(nrow(selected_landmarks_pt()) > 0)
    buf <- combined_buffer_pt()
    req(nrow(buf) > 0)
    
    # Spatial filter: stations that intersect with buffer
    if (nrow(stations) > 0) {
      intersects <- sf::st_intersects(stations, buf, sparse = FALSE)
      stations_filtered <- stations[as.vector(intersects), ]
      return(stations_filtered)
    } else {
      return(stations[0, ])
    }
  })
  
  # Update map when PT landmarks are selected
  observeEvent(c(input$lm_name_pt, input$radius), {
    # Only update if we're on the Public Transport tab
    if (is.null(input$mypage) || input$mypage != "Public Transport") return()
    
    req(nrow(pois) > 0)
    
    map <- leafletProxy("mappt")
    map <- clearGroup(map, "Buffer")
    map <- clearGroup(map, "Filtered Landmarks")
    map <- clearGroup(map, "Filtered Stops")
    map <- clearGroup(map, "Filtered Stations")
    map <- clearGroup(map, "linesnear")  # Clear route lines when landmarks change
    
    # Check if user has filter active
    has_filter <- length(input$lm_name_pt) > 0
    
    if (has_filter) {
      # User applied filter
      sel_lm <- selected_landmarks_pt()
      buf <- combined_buffer_pt()
      stops_sf <- stops_in_radius_pt()
      stations_sf <- stations_in_radius_pt()
      
      # Hide "pois" group
      map <- hideGroup(map, "pois")
      
      # Add buffer zone
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
        layerId = ~id,
        group = "Filtered Landmarks"
      )
      
      # Show filtered landmarks
      map <- showGroup(map, "Filtered Landmarks")
      
      # Add filtered PT stops
      if (nrow(stops_sf) > 0) {
        # Filter stops to only include those with routes and create popups
        stops_with_routes_list <- lapply(seq_len(nrow(stops_sf)), function(i) {
          stop_row <- stops_sf[i, ]
          stop_proj <- st_transform(stop_row, 3857)
          lines_near <- lines[as.numeric(st_distance(lines2, stop_proj)) <= 30, ]
          lines_near <- lines_near[lines_near$mode == stop_row$mode, ]
          
          # Only return stop if it has routes
          if (nrow(lines_near) > 0) {
            routes_list <- paste(unique(lines_near$shortname), collapse = ", ")
            popup_html <- paste0(
              "<div style='font-family: Inter, sans-serif;'>",
              "<h4 style='margin: 0 0 10px 0; font-weight: 600;'>", htmlEscape(stop_row$name), "</h4>",
              "<p style='margin: 5px 0;'><strong>Mode:</strong> ", htmlEscape(stop_row$mode), "</p>",
              "<p style='margin: 5px 0 0 0;'><strong>Routes:</strong> ", routes_list, "</p>",
              "</div>"
            )
            return(list(stop = stop_row, popup = popup_html))
          } else {
            return(NULL)
          }
        })
        
        # Filter out NULL entries (stops with no routes)
        stops_with_routes_list <- stops_with_routes_list[!sapply(stops_with_routes_list, is.null)]
        
        # Only add stops that have routes
        if (length(stops_with_routes_list) > 0) {
          # Reconstruct sf object with only stops that have routes
          stops_with_routes <- do.call(rbind, lapply(stops_with_routes_list, function(x) x$stop))
          stops_with_routes$popup_html <- sapply(stops_with_routes_list, function(x) x$popup)
          
          # Final deduplication before adding to map: remove stops at same location
          stops_with_routes <- stops_with_routes %>%
            mutate(
              lon_round = round(lon, 5),
              lat_round = round(lat, 5)
            ) %>%
            distinct(lon_round, lat_round, mode, .keep_all = TRUE) %>%
            select(-lon_round, -lat_round)
          
          map <- addAwesomeMarkers(
            map,
            data = stops_with_routes,
            lng = ~lon, lat = ~lat,
            icon = ~awesomeIcons(library = 'fa',
                                 markerColor = ~colour,
                                 icon = ~icon,
                                 iconColor = '#ffffff'),
            label = ~name,
            popup = ~popup_html,
            popupOptions = popupOptions(
              maxWidth = 300,
              style = list("font-size" = "12px")
            ),
            layerId = ~id,
            group = 'Filtered Stops'
          )
          map <- showGroup(map, "Filtered Stops")
        }
      }
      
      # Add filtered PT stations
      if (nrow(stations_sf) > 0) {
        # Create popups for stations with chart buttons
        stations_sf$popup_html <- sapply(seq_len(nrow(stations_sf)), function(i) {
          station_row <- stations_sf[i, ]
          station_proj <- st_transform(station_row, 3857)
          lines_near <- lines[as.numeric(st_distance(lines2, station_proj)) <= 500, ]
          lines_near <- lines_near[lines_near$mode == 'METRO TRAIN', ]
          
          station_lines <- if (nrow(lines_near) > 0) {
            paste(unique(lines_near$shortname), collapse = ", ")
          } else {
            "No lines found"
          }
          
          station_id_js <- gsub("'", "\\'", station_row$id)
          station_id_js <- gsub('"', '\\"', station_id_js)
          
          paste0(
            "<div style='font-family: Inter, sans-serif;'>",
            "<h4 style='margin: 0 0 10px 0; font-weight: 600;'>", htmlEscape(station_row$station), "</h4>",
            "<p style='margin: 5px 0 10px 0;'><strong>Lines:</strong> ", station_lines, "</p>",
            "<p style='margin: 10px 0;'><strong>Annual Patronage:</strong> ", format(station_row$annual, big.mark = ","), "</p>",
            "<div style='margin-top: 10px;'>",
            "<a href='#' onclick=\"Shiny.setInputValue('show_station_chart', '", station_id_js, "_daytype', {priority: 'event'}); return false;\" style='background: #007bff; color: white; padding: 5px 10px; margin: 2px; border-radius: 3px; text-decoration: none; display: inline-block; font-size: 11px;'>Average Patronage per Day Type</a> ",
            "<a href='#' onclick=\"Shiny.setInputValue('show_station_chart', '", station_id_js, "_timeperiod', {priority: 'event'}); return false;\" style='background: #6c757d; color: white; padding: 5px 10px; margin: 2px; border-radius: 3px; text-decoration: none; display: inline-block; font-size: 11px;'>Average Patronage per Time Period</a>",
            "</div>",
            "</div>"
          )
        })
        
        map <- addAwesomeMarkers(
          map,
          data = stations_sf,
          lng = ~lon, lat = ~lat,
          icon = ~awesomeIcons(library = 'fa',
                               markerColor = ~colour,
                               icon = ~icon,
                               iconColor = '#ffffff'),
          label = ~station,
          popup = ~popup_html,
          popupOptions = popupOptions(
            maxWidth = 300,
            style = list("font-size" = "12px")
          ),
          layerId = ~id,
          group = 'Filtered Stations'
        )
        map <- showGroup(map, "Filtered Stations")
      }
      
    } else {
      # No filter applied - show "pois" group, hide filtered groups
      map <- hideGroup(map, "Filtered Landmarks")
      map <- hideGroup(map, "Filtered Stops")
      map <- hideGroup(map, "Filtered Stations")
      map <- showGroup(map, "pois")
      
      # Zoom out to show boundary
      if (nrow(boundary) > 0) {
        bounds <- sf::st_bbox(boundary)
        map <- fitBounds(map, bounds[["xmin"]], bounds[["ymin"]], bounds[["xmax"]], bounds[["ymax"]],
                         options = list(padding = c(50, 50)))
      }
    }
  })
  
  
  
  # --- Pedestrian Visualisations ---
  
  # Heatmap View
  output$heatmap <- renderLeaflet({
    
    # poi subset
    if (input$type_pedestrian == 'All') {
      filtered_landmarks <- landmark_popularity
    } else {
      filtered_landmarks <- landmark_popularity[landmark_popularity$subtype == input$type_pedestrian, ]
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
        fillOpacity = 1.0, 
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
    if (input$type_pedestrian == 'All') {
      filtered_landmarks <- landmark_popularity
    } else {
      filtered_landmarks <- landmark_popularity[landmark_popularity$subtype == input$type_pedestrian, ]
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
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold", family = "Inter"),
        text = element_text(family = "Inter", size = 12),
        axis.text = element_text(family = "Inter", size = 12),
        axis.title = element_text(family = "Inter", size = 12),
        legend.text = element_text(family = "Inter", size = 12),
        legend.title = element_text(family = "Inter", size = 12)
      ) + 
      scale_fill_manual(values = colourvals, name = 'Type')
    
    ggplotly(p, tooltip = c("x", "y", "fill")) %>%
      layout(
        font = list(family = "Inter", size = 12),
        title = list(
          text = "Top 15 POIs by Nearby Pedestrian Volume",
          font = list(family = "Inter", size = 14)
        ),
        xaxis = list(
          title = list(text = "Avg Pedestrian Count (nearest sensor)", font = list(family = "Inter", size = 12)),
          tickfont = list(family = "Inter", size = 12)
        ),
        yaxis = list(
          title = list(text = "Point of Interest", font = list(family = "Inter", size = 12)),
          tickfont = list(family = "Inter", size = 12)
        ),
        legend = list(
          orientation = "h",
          y = -0.3,       # move legend below chart
          x = 0.5,
          xanchor = "center"
        ),
        margin = list(b = 120)   # add bottom space for legend
      )
  })
  
  
  # Trend View (linked to theme filter)
  output$trend_plot <- renderPlotly({
    
    # poi subset
    if (input$type_pedestrian == 'All') {
      filtered_landmarks <- landmark_popularity
    } else {
      filtered_landmarks <- landmark_popularity[landmark_popularity$subtype == input$type_pedestrian, ]
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
        title = list(
          text = paste0("Pedestrian Flow Over Time — ",
                        ifelse(input$type_pedestrian == "All", "All POIs", input$type_pedestrian)),
          font = list(family = "Inter", size = 14)
        ),
        xaxis = list(
          title = list(text = "Date", font = list(family = "Inter", size = 12)),
          tickfont = list(family = "Inter", size = 12)
        ),
        yaxis = list(
          title = list(text = "Total Pedestrians", font = list(family = "Inter", size = 12)),
          tickfont = list(family = "Inter", size = 12)
        ),
        hovermode = "x unified",
        font = list(family = "Inter", size = 12)
      )
  })
  
  # --- Pedestrian Landmark Filter Logic ---
  
  # Filtered POIs by type for Pedestrian tab
  filtered_pois_pedestrian <- reactive({
    if (input$type_pedestrian == 'All') {
      return(pois)
    } else {
      return(pois[pois$subtype == input$type_pedestrian, ])
    }
  })
  
  # Update Pedestrian landmark choices when POI type changes
  observe({
    # Only update if we're on the Pedestrian Counts tab
    if (is.null(input$mypage) || input$mypage != "Pedestrian Counts") return()
    
    filtered_pois <- filtered_pois_pedestrian()
    updateSelectizeInput(
      session,
      "lm_name_pedestrian",
      choices = if (nrow(filtered_pois) > 0) sort(unique(filtered_pois$name)) else NULL,
      server = TRUE
    )
  })
  
  # Selected landmarks for Pedestrian tab
  selected_landmarks_pedestrian <- reactive({
    lm <- filtered_pois_pedestrian()
    if (nrow(lm) == 0) return(lm)
    
    if (length(input$lm_name_pedestrian) > 0) {
      lm <- lm |> dplyr::filter(name %in% input$lm_name_pedestrian)
    }
    lm
  })
  
  # Combined buffer around selected Pedestrian landmarks
  combined_buffer_pedestrian <- reactive({
    sel_lm <- selected_landmarks_pedestrian()
    req(nrow(sel_lm) > 0)
    
    # use EPSG:7899 (GDA2020 / MGA zone 55) for accurate meter-based buffering in Melbourne
    lm_proj <- tryCatch({
      sf::st_transform(sel_lm, 7899)
    }, error = function(e) {
      message("EPSG:7899 not available, using EPSG:3857")
      sf::st_transform(sel_lm, 3857)
    })
    
    # Create buffer around each landmark
    buf <- sf::st_buffer(lm_proj, dist = input$radius_pedestrian)
    
    # Union all buffers into one
    if (nrow(buf) > 1) {
      buf <- sf::st_union(buf) |> sf::st_sf()
    } else {
      # Ensure single buffer is also an sf object
      if (!inherits(buf, "sf")) {
        buf <- sf::st_sf(geometry = buf)
      }
    }
    
    sf::st_transform(buf, 4326)
  })
  
  # Filter pedestrian sensors within radius
  sensors_in_radius_pedestrian <- reactive({
    req(nrow(selected_landmarks_pedestrian()) > 0)
    buf <- combined_buffer_pedestrian()
    req(nrow(buf) > 0)
    
    # Spatial filter: sensors that intersect with buffer
    if (nrow(ped_geo) > 0) {
      # Convert ped_geo to sf object if it's not already
      if (!inherits(ped_geo, "sf")) {
        ped_geo_sf <- st_as_sf(ped_geo, coords = c("Longitude", "Latitude"), crs = 4326)
      } else {
        ped_geo_sf <- ped_geo
      }
      
      # Ensure buffer is an sf object
      if (!inherits(buf, "sf")) {
        buf <- st_as_sf(buf)
      }
      
      intersects <- sf::st_intersects(ped_geo_sf, buf, sparse = FALSE)
      sensors_filtered <- ped_geo[as.vector(intersects), ]
      return(sensors_filtered)
    } else {
      return(ped_geo[0, ])
    }
  })
  
  # Update heatmap when Pedestrian landmarks are selected
  observeEvent(c(input$lm_name_pedestrian, input$radius_pedestrian), {
    # Only update if we're on the Pedestrian Counts tab
    if (is.null(input$mypage) || input$mypage != "Pedestrian Counts") return()
    
    req(nrow(pois) > 0)
    
    map <- leafletProxy("heatmap")
    map <- clearGroup(map, "Buffer")
    map <- clearGroup(map, "Filtered Landmarks")
    map <- clearGroup(map, "Filtered Sensors")
    
    # Check if user has filter active
    has_filter <- length(input$lm_name_pedestrian) > 0
    
    if (has_filter) {
      # User applied filter
      sel_lm <- selected_landmarks_pedestrian()
      buf <- combined_buffer_pedestrian()
      sensors_sf <- sensors_in_radius_pedestrian()
      
      # Hide "pois" group (all landmarks) when filter is active
      map <- hideGroup(map, "pois")
      
      # Add buffer zone
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
        layerId = ~id,
        group = "Filtered Landmarks"
      )
      
      # Show filtered landmarks
      map <- showGroup(map, "Filtered Landmarks")
      
      # Add filtered pedestrian sensors
      if (nrow(sensors_sf) > 0) {
        # Create formatted labels with HTML
        sensors_sf$label_html <- lapply(seq_len(nrow(sensors_sf)), function(i) {
          htmltools::HTML(paste0(
            "<b>Pedestrian Sensor:</b> ", htmltools::htmlEscape(sensors_sf$Sensor_Name[i]), "<br/>",
            "<b>Average Daily Count:</b> ", format(round(sensors_sf$Avg_Count[i]), big.mark = ","), " pedestrians"
          ))
        })
        
        map <- addCircleMarkers(
          map,
          data = sensors_sf,
          lng = ~Longitude, lat = ~Latitude,
          radius = ~scales::rescale(Avg_Count, to = c(3, 15)),
          stroke = TRUE,
          weight = 1,
          color = "#333333",
          fillOpacity = 0.6,
          fillColor = ~colorNumeric(palette = "YlOrRd", domain = ped_geo$Avg_Count)(Avg_Count),
          label = ~label_html,
          labelOptions = labelOptions(
            style = list("font-size" = "12px", "font-family" = "Inter, sans-serif"),
            direction = "auto"
          ),
          group = "Filtered Sensors"
        )
        map <- showGroup(map, "Filtered Sensors")
      }
      
    } else {
      # No filter applied - show default groups, hide filtered groups
      map <- hideGroup(map, "Filtered Landmarks")
      map <- hideGroup(map, "Filtered Sensors")
      map <- showGroup(map, "pois")  # Show all landmarks when no filter
      
      # Zoom out to show boundary
      if (nrow(boundary) > 0) {
        bounds <- sf::st_bbox(boundary)
        map <- fitBounds(map, bounds[["xmin"]], bounds[["ymin"]], bounds[["xmax"]], bounds[["ymax"]],
                         options = list(padding = c(50, 50)))
      }
    }
  })
  
  # Handle landmark marker clicks on Pedestrian heatmap
  observeEvent(input$heatmap_marker_click, {
    # Only process if we're on the Pedestrian Counts tab
    if (is.null(input$mypage) || input$mypage != "Pedestrian Counts") return()
    
    click <- input$heatmap_marker_click
    if (is.null(click$id)) return()
    
    # Check if clicked marker is a landmark (POIs start with 'poi')
    if (startsWith(click$id, 'poi')) {
      # Get landmark name from clicked POI
      landmark_name <- pois[pois$id == click$id, ]$name
      
      # Get current selections and add clicked landmark if not already selected
      current_selection <- input$lm_name_pedestrian
      if (is.null(current_selection)) {
        current_selection <- character(0)
      }
      
      # If clicked landmark is not already in selection, add it; otherwise, remove it (toggle)
      if (landmark_name %in% current_selection) {
        # Remove from selection if already selected (toggle behavior)
        new_selection <- current_selection[current_selection != landmark_name]
      } else {
        # Add to selection
        new_selection <- c(current_selection, landmark_name)
      }
      
      # Update the landmark selection dropdown
      # This will automatically trigger the observeEvent for filtering
      updateSelectizeInput(
        session,
        "lm_name_pedestrian",
        selected = new_selection
      )
    }
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
    # Only update if we're on the On-street Parking tab
    if (is.null(input$mypage) || input$mypage != "On-street Parking") return()
    
    filtered_pois <- filtered_pois_parking()
    updateSelectizeInput(
      session,
      "lm_name_parking",
      choices = if (nrow(filtered_pois) > 0) sort(unique(filtered_pois$name)) else NULL,
      server = TRUE
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
        fillOpacity = 1.0,
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
    # Only update if we're on the Parking tab
    if (is.null(input$mypage) || input$mypage != "On-street Parking") return()
    
    filtered_pois <- filtered_pois_parking()
    
    map <- leafletProxy("map_parking")
    map <- clearGroup(map, "All Landmarks")
    
    if (nrow(filtered_pois) > 0) {
      map <- addCircleMarkers(
        map,
        data = filtered_pois,
        radius = 7,
        stroke = FALSE,
        fillOpacity = 1.0,
        fillColor = ~colour,
        label = ~name,
        layerId = ~id,
        group = "All Landmarks"
      )
    }
  })
  
  # Handle landmark marker clicks
  observeEvent(input$map_parking_marker_click, {
    # Only process if we're on the Parking tab
    if (is.null(input$mypage) || input$mypage != "On-street Parking") return()
    
    click <- input$map_parking_marker_click
    if (is.null(click$id)) return()
    
    # Check if clicked marker is a landmark (not a parking zone)
    # Landmarks use their ID (poi1, poi2, etc.) as layerId, parking zones use "Zone XXX"
    if (!startsWith(click$id, "Zone ") && click$id != "No Zone ID") {
      # Get landmark name from clicked POI ID
      landmark_name <- pois[pois$id == click$id, ]$name
      
      # Get current selections and add clicked landmark if not already selected
      current_selection <- input$lm_name_parking
      if (is.null(current_selection)) {
        current_selection <- character(0)
      }
      
      # If clicked landmark is not already in selection, add it; otherwise, remove it (toggle)
      if (landmark_name %in% current_selection) {
        # Remove from selection if already selected (toggle behavior)
        new_selection <- current_selection[current_selection != landmark_name]
      } else {
        # Add to selection
        new_selection <- c(current_selection, landmark_name)
      }
      
      # Update the landmark selection dropdown
      # This will automatically trigger the observeEvent for filtering
      updateSelectizeInput(
        session,
        "lm_name_parking",
        selected = new_selection
      )
    }
  })
  
  # Update map layers when landmarks are selected
  observeEvent(c(input$lm_name_parking, input$radius_m_parking), {
    # Only update if we're on the Parking tab
    if (is.null(input$mypage) || input$mypage != "On-street Parking") return()
    
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
      
      # Add filtered landmarks (larger markers with their original colors)
      sel_lm_data <- sel_lm
      sel_lm_data$popup_text <- paste0("<b>", sel_lm_data$name, "</b><br>Category: ", sel_lm_data$subtype)
      
      map <- addCircleMarkers(
        map,
        data = sel_lm_data,
        radius = 8,
        stroke = TRUE,
        weight = 2,
        fillOpacity = 1.0,
        fillColor = ~colour,
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
      
      # Zoom out to show boundary (same as initial map)
      if (nrow(boundary) > 0) {
        bb <- sf::st_bbox(boundary)
        map <- fitBounds(map,
                         lng1 = as.numeric(bb["xmin"]),
                         lat1 = as.numeric(bb["ymin"]),
                         lng2 = as.numeric(bb["xmax"]),
                         lat2 = as.numeric(bb["ymax"]))
      } else {
        map <- setView(map, lng = 144.9631, lat = -37.8136, zoom = 12)
      }
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
    
    # Add crime suburb polygons FIRST (so landmarks can be on top)
    # Add landmarks by default (on separate "All Landmarks" layer)
    # Note: Landmarks are added AFTER polygons so they appear on top
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
          label = ~suburb_name,  # Show suburb name on hover
          labelOptions = labelOptions(
            textsize = "12px",
            direction = "auto",
            style = list("font-weight" = "bold")
          ),
          options = pathOptions(zIndex = 100),  # Lower z-index so landmarks can be clicked
          highlight = highlightOptions(
            weight = 1.5,
            color = "#333",
            fillOpacity = 0.8,
            bringToFront = FALSE,  # Don't bring polygons to front so landmarks stay clickable
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
    
    # Add landmarks AFTER polygons so they're on top and clickable
    if (nrow(pois) > 0) {
      pois_popup <- pois
      pois_popup$popup_text <- paste0("<b>", pois_popup$name, "</b><br>Category: ", pois_popup$subtype)
      map <- map %>%
        addCircleMarkers(
          data = pois_popup,
          lng = ~lon, lat = ~lat,
          radius = 7,
          stroke = FALSE,
          fill = TRUE,
          fillColor = ~colour,
          fillOpacity = 1.0,
          label = ~name,
          popup = ~popup_text,
          layerId = ~name,
          group = 'All Landmarks',
          options = markerOptions(zIndexOffset = 1000)  # Put markers on top so they're clickable
        )
    }
    
    # Add layer control
    map <- map %>%
      addLayersControl(
        overlayGroups = c("Crime Suburbs", "All Landmarks"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      hideGroup("Filtered Landmarks")  # Initially hide filtered landmarks group
    
    map
  })
  
  # Initialize map when switching to Crime tab
  observeEvent(input$mypage, {
    if (!is.null(input$mypage) && input$mypage == "Security View") {
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
      filtered_pois_by_type <- pois
    } else {
      filtered_pois_by_type <- pois[pois$subtype == input$type_crime, ]
    }
    
    # Check if landmarks are selected - if so, show only selected ones
    has_landmark_filter <- length(input$lm_name_crime) > 0
    
    if (has_landmark_filter) {
      # Filter by selected landmark names
      filtered_pois_to_add <- filtered_pois_by_type[filtered_pois_by_type$name %in% input$lm_name_crime, ]
      landmark_group <- "Filtered Landmarks"
      landmark_radius <- 8
      landmark_stroke <- TRUE
      landmark_weight <- 2
      landmark_fill_opacity <- 0.9
      landmark_fill_color <- "#e31a1c"
      landmark_color <- "#fff"
    } else {
      # Show all landmarks of the selected type
      filtered_pois_to_add <- filtered_pois_by_type
      landmark_group <- "All Landmarks"
      landmark_radius <- 7
      landmark_stroke <- FALSE
      landmark_weight <- NULL
      landmark_fill_opacity <- 1.0  # Match other tabs - fully opaque
      landmark_color <- NULL
    }
    
    # Update map layers
    # IMPORTANT: Add polygons FIRST, then landmarks LAST so landmarks are clickable
    leafletProxy("crime_map") %>%
      clearGroup("Crime Heatmap") %>%
      clearGroup("Crime Suburbs") %>%
      clearGroup("All Landmarks") %>%
      clearGroup("Filtered Landmarks") %>%
      {
        # Add suburb polygons FIRST
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
            label = ~suburb_name,  # Show suburb name on hover
            labelOptions = labelOptions(
              textsize = "12px",
              direction = "auto",
              style = list("font-weight" = "bold")
            ),
            options = pathOptions(zIndex = 100),  # Lower z-index so landmarks can be clicked
            highlight = highlightOptions(
              weight = 1.5,
              color = "#333",
              fillOpacity = 0.8,
              bringToFront = FALSE,  # Don't bring polygons to front so landmarks stay clickable
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
      } %>%
      {
        # Add POI landmarks LAST so they're on top and clickable
        if (nrow(filtered_pois_to_add) > 0) {
          filtered_pois_popup <- filtered_pois_to_add
          filtered_pois_popup$popup_text <- paste0("<b>", filtered_pois_popup$name, "</b><br>Category: ", filtered_pois_popup$subtype)
          
          if (has_landmark_filter) {
            addCircleMarkers(
              .,
              data = filtered_pois_popup,
              lng = ~lon, lat = ~lat,
              radius = landmark_radius,
              stroke = landmark_stroke,
              weight = landmark_weight,
              fillOpacity = landmark_fill_opacity,
              fillColor = landmark_fill_color,
              color = landmark_color,
              label = ~name,
              popup = ~popup_text,
              layerId = ~name,
              group = landmark_group,
              options = markerOptions(zIndexOffset = 1000)  # Put markers on top
            ) %>%
              hideGroup("All Landmarks") %>%
              showGroup("Filtered Landmarks")
          } else {
            addCircleMarkers(
              .,
              data = filtered_pois_popup,
              lng = ~lon, lat = ~lat,
              radius = landmark_radius,
              stroke = landmark_stroke,
              fill = TRUE,
              fillColor = ~colour,
              fillOpacity = landmark_fill_opacity,
              label = ~name,
              popup = ~popup_text,
              layerId = ~name,
              group = landmark_group,
              options = markerOptions(zIndexOffset = 1000)  # Put markers on top
            ) %>%
              showGroup("All Landmarks") %>%
              hideGroup("Filtered Landmarks")
          }
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
    # Only update if we're on the Security View tab
    if (is.null(input$mypage) || input$mypage != "Security View") return()
    
    filtered_pois <- filtered_pois_crime()
    updateSelectizeInput(
      session,
      "lm_name_crime",
      choices = if (nrow(filtered_pois) > 0) sort(unique(filtered_pois$name)) else NULL,
      server = TRUE
    )
  })
  
  # Selected landmarks (for crime tab filtering)
  selected_landmarks_crime <- reactive({
    lm <- filtered_pois_crime()
    if (nrow(lm) == 0) return(lm)
    
    if (!is.null(input$lm_name_crime) && length(input$lm_name_crime) > 0) {
      lm <- lm |> dplyr::filter(name %in% input$lm_name_crime)
    }
    lm
  })
  
  # Update landmarks on map when POI type changes
  observeEvent(input$type_crime, {
    # Only update if we're on the Security View tab
    if (is.null(input$mypage) || input$mypage != "Security View") return()
    
    filtered_pois <- filtered_pois_crime()
    
    map <- leafletProxy("crime_map")
    map <- clearGroup(map, "All Landmarks")
    map <- clearGroup(map, "Filtered Landmarks")
    
    # If landmarks are selected, show filtered ones, otherwise show all
    # Handle NULL and empty character vectors
    has_filter_in_type <- !is.null(input$lm_name_crime) && length(input$lm_name_crime) > 0
    if (has_filter_in_type) {
      sel_lm <- selected_landmarks_crime()
      if (nrow(sel_lm) > 0) {
        sel_lm_popup <- sel_lm
        sel_lm_popup$popup_text <- paste0("<b>", sel_lm_popup$name, "</b><br>Category: ", sel_lm_popup$subtype)
        map <- addCircleMarkers(
          map,
          data = sel_lm_popup,
          lng = ~lon, lat = ~lat,
          radius = 8,
          stroke = TRUE,
          weight = 2,
          fillOpacity = 0.9,
          fillColor = "#e31a1c",
          color = "#fff",
          label = ~name,
          popup = ~popup_text,
          layerId = ~name,
          group = "Filtered Landmarks",
          options = markerOptions(zIndexOffset = 1000)  # Put markers on top
        )
        map <- hideGroup(map, "All Landmarks")
        map <- showGroup(map, "Filtered Landmarks")
        
        # Automatically pan and zoom to the selected landmarks
        bounds <- sf::st_bbox(sel_lm)
        map <- fitBounds(map, bounds[["xmin"]], bounds[["ymin"]], bounds[["xmax"]], bounds[["ymax"]],
                         options = list(padding = c(50, 50)))
      }
    } else {
      # No filter - show all landmarks of the selected type
      if (nrow(filtered_pois) > 0) {
        filtered_pois_popup <- filtered_pois
        filtered_pois_popup$popup_text <- paste0("<b>", filtered_pois_popup$name, "</b><br>Category: ", filtered_pois_popup$subtype)
        map <- addCircleMarkers(
          map,
          data = filtered_pois_popup,
          lng = ~lon, lat = ~lat,
          radius = 7,
          stroke = FALSE,
          fill = TRUE,
          fillColor = ~colour,
          fillOpacity = 1.0,
          label = ~name,
          popup = ~popup_text,
          layerId = ~name,
          group = "All Landmarks",
          options = markerOptions(zIndexOffset = 1000)  # Put markers on top
        )
        map <- showGroup(map, "All Landmarks")
        map <- hideGroup(map, "Filtered Landmarks")
      }
      
      # Zoom out to default view (same as initial map)
      map <- setView(map, lng = 144.9631, lat = -37.8136, zoom = 13)
    }
  })
  
  # Handle landmark marker clicks
  observeEvent(input$crime_map_marker_click, {
    # Only process if we're on the Security View tab
    if (is.null(input$mypage) || input$mypage != "Security View") return()
    
    click <- input$crime_map_marker_click
    if (is.null(click$id)) return()
    
    # Check if clicked marker is a landmark (not a suburb polygon)
    # Suburbs use their suburb_name as id, landmarks use their name
    # We can identify landmarks by checking if they exist in pois
    if (!is.null(click$id) && click$id %in% pois$name) {
      # Update the landmark selection dropdown
      updateSelectizeInput(
        session,
        "lm_name_crime",
        selected = click$id
      )
    }
  })
  
  # Handle polygon clicks - check if there's a landmark nearby
  observeEvent(input$crime_map_shape_click, {
    click <- input$crime_map_shape_click
    if (is.null(click) || is.null(click$lat) || is.null(click$lng)) return()
    
    # Get currently visible landmarks
    current_landmarks <- if (!is.null(input$lm_name_crime) && length(input$lm_name_crime) > 0) {
      selected_landmarks_crime()
    } else {
      filtered_pois_crime()
    }
    
    if (nrow(current_landmarks) > 0) {
      # Check if click is near any landmark (within 50 meters)
      click_point <- st_sfc(st_point(c(click$lng, click$lat)), crs = 4326)
      landmarks_sf <- st_as_sf(current_landmarks, coords = c("lon", "lat"), crs = 4326)
      
      # Transform to a metric CRS for accurate distance calculation
      tryCatch({
        click_proj <- st_transform(click_point, 3857)
        landmarks_proj <- st_transform(landmarks_sf, 3857)
        distances <- st_distance(click_proj, landmarks_proj)
        min_dist <- min(distances)
        min_idx <- which.min(distances)
        
        # If click is within 50 meters of a landmark, treat it as landmark click
        if (min_dist < 50) {
          clicked_landmark <- current_landmarks[min_idx, ]
          if (!is.null(clicked_landmark$name)) {
            # Update the landmark selection dropdown
            updateSelectizeInput(
              session,
              "lm_name_crime",
              selected = clicked_landmark$name
            )
          }
        }
      }, error = function(e) {
        # If transformation fails, skip the check
      })
    }
  })
  
  # Update map layers when landmarks are selected
  observeEvent(input$lm_name_crime, {
    # Only update if we're on the Security View tab
    if (is.null(input$mypage) || input$mypage != "Security View") return()
    
    req(!is.null(pois))
    # Force reactivity - trigger even when selection is cleared
    
    map <- leafletProxy("crime_map")
    map <- clearGroup(map, "Filtered Landmarks")
    map <- clearGroup(map, "All Landmarks")
    
    # Check if user has filter active - handle NULL and empty character vectors
    has_filter <- !is.null(input$lm_name_crime) && length(input$lm_name_crime) > 0
    
    if (has_filter) {
      # User applied filter - show selected landmarks
      sel_lm <- selected_landmarks_crime()
      
      if (nrow(sel_lm) > 0) {
        # Hide "All Landmarks" and show filtered ones
        map <- hideGroup(map, "All Landmarks")
        
        sel_lm_popup <- sel_lm
        sel_lm_popup$popup_text <- paste0("<b>", sel_lm_popup$name, "</b><br>Category: ", sel_lm_popup$subtype)
        
        map <- addCircleMarkers(
          map,
          data = sel_lm_popup,
          lng = ~lon, lat = ~lat,
          radius = 8,
          stroke = TRUE,
          weight = 2,
          fillOpacity = 0.9,
          fillColor = "#e31a1c",
          color = "#fff",
          label = ~name,
          popup = ~popup_text,
          layerId = ~name,
          group = "Filtered Landmarks",
          options = markerOptions(zIndexOffset = 1000)  # Put markers on top
        )
        
        # Show filtered landmarks
        map <- showGroup(map, "Filtered Landmarks")
        
        # Automatically pan and zoom to the selected landmarks
        bounds <- sf::st_bbox(sel_lm)
        map <- fitBounds(map, bounds[["xmin"]], bounds[["ymin"]], bounds[["xmax"]], bounds[["ymax"]],
                         options = list(padding = c(50, 50)))
      }
    } else {
      # No filter - show all landmarks based on POI type
      filtered_pois <- filtered_pois_crime()
      if (nrow(filtered_pois) > 0) {
        filtered_pois_popup <- filtered_pois
        filtered_pois_popup$popup_text <- paste0("<b>", filtered_pois_popup$name, "</b><br>Category: ", filtered_pois_popup$subtype)
        map <- addCircleMarkers(
          map,
          data = filtered_pois_popup,
          lng = ~lon, lat = ~lat,
          radius = 7,
          stroke = FALSE,
          fill = TRUE,
          fillColor = ~colour,
          fillOpacity = 1.0,
          label = ~name,
          popup = ~popup_text,
          layerId = ~name,
          group = "All Landmarks",
          options = markerOptions(zIndexOffset = 1000)  # Put markers on top
        )
        map <- showGroup(map, "All Landmarks")
        map <- hideGroup(map, "Filtered Landmarks")
      }
      
      # Zoom out to default view (same as initial map)
      map <- setView(map, lng = 144.9631, lat = -37.8136, zoom = 13)
    }
  }, ignoreNULL = FALSE)  # Trigger even when input becomes NULL/empty
  
  # Update map when filters change
  observeEvent(input$crime_category_filter, {
    # Only update if we're on the Crime tab
    if (!is.null(input$mypage) && input$mypage == "Security View") {
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
      style = "background-color: #f8d7da; border-color: #f5c6cb; padding: 10px;",
      tags$h4(format(total, big.mark = ","), style = "margin: 0; color: #721c24; font-size: 20px;"),
      tags$p("Total Offences", style = "margin: 3px 0 0 0; color: #721c24; font-size: 12px;")
    )
  })
  
  output$crime_rate <- renderUI({
    rate <- mean(crime_data$table01$`Rate per 100,000 population`, na.rm = TRUE)
    tags$div(
      class = "well text-center",
      style = "background-color: #fff3cd; border-color: #ffeaa7; padding: 10px;",
      tags$h4(round(rate, 1), style = "margin: 0; color: #856404; font-size: 20px;"),
      tags$p("Crime Rate per 100,000", style = "margin: 3px 0 0 0; color: #856404; font-size: 12px;")
    )
  })
  
  output$police_region <- renderUI({
    region <- unique(crime_data$table01$`Police Region`)[1]
    if (is.na(region) || length(region) == 0) {
      region <- "N/A"
    }
    tags$div(
      class = "well text-center",
      style = "background-color: #d1ecf1; border-color: #bee5eb; padding: 10px;",
      tags$h4(region, style = "margin: 0; color: #0c5460; font-size: 20px;"),
      tags$p("Police Region", style = "margin: 3px 0 0 0; color: #0c5460; font-size: 12px;")
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
        layout(
          title = list(
            text = title_text,
            font = list(family = "Inter", size = 12)
          ),
          font = list(family = "Inter", size = 12)
        )
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
              options = list(pageLength = 100, scrollX = TRUE, 
                             lengthMenu = FALSE,
                             paging = FALSE,
                             info = FALSE,
                             dom = 't'),  # 't' = table only (no info, no pagination)
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
              options = list(
                pageLength = 15, 
                scrollX = TRUE, 
                scrollY = "500px",  # Fixed height for better control
                lengthMenu = FALSE,
                autoWidth = TRUE,  # Auto-size columns based on content
                columnDefs = list(
                  list(width = 'auto', targets = '_all')  # All columns auto-width
                ),
                dom = 'tip'  # 't' = table, 'i' = info, 'p' = pagination
              ), 
              rownames = FALSE,
              width = "100%"
    )
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
    # Show data tables content in a modal with custom size for better table display
    showModal(modalDialog(
      title = modal_title,
      uiOutput("tables_modal_content"),
      size = "xl",  # Use extra-large size for better width
      easyClose = TRUE,
      footer = modalButton("Close"),
      style = "width: 95%; max-width: 1400px;"  # Custom width to fit table columns
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
      tags$div(
        style = "width: 100%; overflow-x: auto;",
        DTOutput("data_table", width = "100%")
      )
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

shinyApp(ui, server, options=list(port=6245))
