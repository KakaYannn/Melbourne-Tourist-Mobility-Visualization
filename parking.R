# app.R — Tourist: Parking near Landmarks
# Force app to open in external browser (RStudio Viewer has issues with leaflet)
options(shiny.launch.browser = TRUE)

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

# ---------- safe readers ----------
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

# ---------- data endpoints ----------
URL_BOUNDARY <- "https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/municipal-boundary/exports/geojson?limit=-1"
URL_LANDMARK <- "https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/landmarks-and-places-of-interest-including-schools-theatres-health-services-spor/exports/geojson?limit=-1"
URL_BAYS     <- "https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/on-street-parking-bays/exports/geojson?limit=-1"
# Zone-based clustering datasets
URL_ZONES_TO_SEGMENTS <- "https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/parking-zones-linked-to-street-segments/exports/csv?limit=-1"
URL_SIGN_PLATES <- "https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/sign-plates-located-in-each-parking-zone/exports/csv?limit=-1"

# ---------- preload layers ----------
mel_boundary <- read_sf_cached(URL_BOUNDARY)
if (nrow(mel_boundary) > 0) {
  mel_boundary <- mel_boundary |> sf::st_make_valid() |> sf::st_transform(4326)
}

landmarks <- read_sf_cached(URL_LANDMARK)
if (nrow(landmarks) > 0) {
  landmarks <- landmarks |>
    dplyr::select(
      name  = dplyr::matches("name", ignore.case = TRUE),
      theme = dplyr::matches("theme|category|type", ignore.case = TRUE),
      dplyr::everything()
    ) |>
    dplyr::mutate(name = ifelse(is.na(name) | nchar(trimws(name)) == 0, "Unknown", as.character(name))) |>
    sf::st_make_valid() |>
    sf::st_transform(4326)
}

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

# ---------- Load zone-based clustering datasets ----------
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

# === PHASE 2: AGGREGATE BAYS TO SEGMENTS ===
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

# === PHASE 3: MAP SEGMENTS TO ZONES ===
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

# === PHASE 3.5: HANDLE UNMATCHED SEGMENTS (segments with no zone) ===
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

# === PHASE 4: PREPARE SIGN PLATES DATA FOR TABLE DISPLAY ===
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

# === PHASE 4.5: COMBINE ZONES AND UNMATCHED SEGMENTS ===
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

# ---------- UI ----------
ui <- navbarPage(
  title = "Tourist Explorer",
  theme = bslib::bs_theme(bootswatch = "flatly"),
  header = tags$head(
    tags$style(HTML("
      /* Fix slider label spacing to prevent overlap */
      .irs-grid-text {
        font-size: 10px;
      }
      .irs-grid-text:last-child {
        margin-left: -10px;
      }
    "))
  ),
  tabPanel(
    "Parking near Landmarks",
    sidebarLayout(
      sidebarPanel(
        h4("Select Landmarks for Parking Search"),
        helpText("Choose one or more landmarks to find nearby parking"),
        selectizeInput(
          "lm_name",
          "Landmark(s)",
          choices = if (nrow(landmarks) > 0) sort(unique(landmarks$name)) else NULL,
          options = list(
            placeholder = "Type to search...",
            plugins = list('remove_button')
          ),
          multiple = TRUE
        ),
        sliderInput("radius_m", "Search radius (meters)",
                    min = 100, max = 1000, value = 300, step = 50,
                    ticks = TRUE, sep = ""),
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
        width = 3
      ),
      mainPanel(
        leafletOutput("map_parking", height = 560),
        br(),
        uiOutput("info_summary"),
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
  )
)

# ---------- SERVER ----------
server <- function(input, output, session) {
  
  # notify if critical data are missing
  observe({
    if (nrow(landmarks) == 0 || nrow(bays) == 0) {
      showNotification("Data failed to load. Please check network or replace URLs with local files.", type = "error", duration = NULL)
    }
  })
  
  # Selected landmarks (for parking search)
  selected_landmarks <- reactive({
    lm <- landmarks
    if (nrow(lm) == 0) return(lm)

    if (length(input$lm_name) > 0) {
      lm <- lm |> dplyr::filter(name %in% input$lm_name)
    }
    lm
  })

  # Combined buffer around all selected landmarks
  combined_buffer <- reactive({
    sel_lm <- selected_landmarks()
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
    buf <- sf::st_buffer(lm_proj, dist = input$radius_m)

    # Union all buffers into one
    if (nrow(buf) > 1) {
      buf <- sf::st_union(buf) |> sf::st_sf()
    }

    sf::st_transform(buf, 4326)
  })
  # === PHASE 5: FILTER ZONES BY LANDMARK BUFFER ===
  zones_in_radius <- reactive({
    req(nrow(selected_landmarks()) > 0)
    buf <- combined_buffer()
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
  
  # initial map with all landmarks shown
  output$map_parking <- renderLeaflet({
    map <- leaflet(options = leafletOptions(minZoom = 11)) |>
      addProviderTiles(providers$CartoDB.Positron)

    if (nrow(mel_boundary) > 0) {
      bb <- sf::st_bbox(mel_boundary)
      # fitBounds needs: lng1, lat1, lng2, lat2 (SW corner, NE corner)
      map <- fitBounds(map,
                       lng1 = as.numeric(bb["xmin"]),
                       lat1 = as.numeric(bb["ymin"]),
                       lng2 = as.numeric(bb["xmax"]),
                       lat2 = as.numeric(bb["ymax"]))
      map <- addPolygons(map, data = mel_boundary, weight = 2, color = "#222",
                        fill = FALSE, group = "Boundary")
    } else {
      map <- setView(map, lng = 144.9631, lat = -37.8136, zoom = 12)
    }

    # Show ALL landmarks by default (small dark blue markers)
    if (nrow(landmarks) > 0) {
      map <- addCircleMarkers(
        map,
        data = landmarks,
        radius = 3,
        stroke = FALSE,
        fillOpacity = 0.4,
        fillColor = "#00008B",
        label = ~name,
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
          clusterOptions = markerClusterOptions()  # ADD CLUSTERING
        )
      }

      # Add unmatched segments (orange markers with clustering)
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
          clusterOptions = markerClusterOptions()  # ADD CLUSTERING
        )
      }
    }

    # Add legend for parking zones (will update when zones are displayed)
    # Add layer control (only show user-facing groups, hide internal filtered groups)
    map <- addLayersControl(
      map,
      overlayGroups = c("Boundary", "All Landmarks", "All Parking Zones", "Buffer"),
      options = layersControlOptions(collapsed = FALSE)
    )

    # Initially hide filtered groups (internal groups, not shown in layer control)
    map <- hideGroup(map, "Filtered Landmarks")
    map <- hideGroup(map, "Filtered Zones")

    map
  })
  
  # Update map layers when landmarks are selected (ignore layer control checkbox changes to prevent loops)
  observeEvent(c(input$lm_name, input$radius_m), {
    req(nrow(landmarks) > 0)
    req(!is.null(zones_display))

    map <- leafletProxy("map_parking")
    map <- clearGroup(map, "Buffer")
    map <- clearGroup(map, "Filtered Landmarks")
    map <- clearGroup(map, "Filtered Zones")

    # Check if user has filter active
    has_filter <- length(input$lm_name) > 0

    if (has_filter) {
      # User applied filter
      sel_lm <- selected_landmarks()
      buf <- combined_buffer()
      zones_sf <- zones_in_radius()

      # Hide "All" groups and uncheck their checkboxes using JavaScript
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

      # Add filtered landmarks (larger red markers) to "Filtered Landmarks" group
      sel_lm_data <- sel_lm
      sel_lm_data$popup_text <- paste0("<b>", sel_lm_data$name, "</b><br>Category: ", sel_lm_data$theme)

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

      # Show filtered landmarks (this checks the checkbox)
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

        # Add zones with zone IDs (red with clustering)
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
            clusterOptions = markerClusterOptions()
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
            clusterOptions = markerClusterOptions()
          )
        }

        # Show filtered zones (this checks the checkbox)
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

  # Scale landmark markers based on zoom level
  observe({
    zoom <- input$map_parking_zoom
    if (!is.null(zoom) && nrow(landmarks) > 0) {
      # Calculate radius based on zoom: larger radius at higher zoom levels
      # Zoom 10-12: radius 3-4
      # Zoom 13-15: radius 5-8
      # Zoom 16+: radius 10+
      radius <- max(3, min(15, (zoom - 9) * 1.5))

      # Only update if "All Landmarks" group is visible
      if (!is.null(input$map_parking_groups) && "All Landmarks" %in% input$map_parking_groups) {
        leafletProxy("map_parking") |>
          clearGroup("All Landmarks") |>
          addCircleMarkers(
            data = landmarks,
            radius = radius,
            stroke = FALSE,
            fillOpacity = 0.4,
            fillColor = "#00008B",
            label = ~name,
            group = "All Landmarks"
          )
      }
    }
  })

  # summary info box
  output$info_summary <- renderUI({
    if (length(input$lm_name) == 0) {
      return(tags$div(
        class = "alert alert-secondary",
        role = "alert",
        tags$strong("Welcome! "),
        sprintf(
          "Select one or more landmarks from the sidebar to find nearby parking zones. %d landmarks shown on map.",
          nrow(landmarks)
        )
      ))
    }

    zones_sf <- zones_in_radius()
    if (nrow(zones_sf) == 0) {
      return(tags$div(
        class = "alert alert-warning",
        role = "alert",
        tags$strong("No parking zones found "),
        sprintf("within %d meters of selected landmark(s). Try increasing the search radius or selecting landmarks in central Melbourne.", input$radius_m)
      ))
    }

    total_zones <- nrow(zones_sf)

    tags$div(
      class = "alert alert-success",
      role = "alert",
      tags$strong("Search Results: "),
      sprintf(
        "Found %d parking zone%s within %d meters of %d landmark%s.",
        total_zones,
        ifelse(total_zones == 1, "", "s"),
        input$radius_m,
        length(input$lm_name),
        ifelse(length(input$lm_name) == 1, "", "s")
      )
    )
  })
}

# ---------- return shiny app ----------
shinyApp(ui, server)
