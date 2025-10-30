# ======================================================
# Melbourne Landmarks & Pedestrian Sensors — Interactive Dashboard (Trend Follows Theme)
# ======================================================

library(readxl)
library(shiny)
library(dplyr)
library(readr)
library(leaflet)
library(leaflet.extras)
library(shinythemes)
library(janitor)
library(tidyr)
library(geosphere)
library(plotly)
library(forcats)
library(lubridate)

# -------------------------
# 1) File paths
# -------------------------
landmarks_path  <- "./landmark.xlsx"
ped_counts_path <- "./pedestrian-counting-system-monthly-counts-per-hour.xlsx"
sensor_csv_local <- "./pedestrian-counting-system-sensor-locations.csv"

# -------------------------
# 2) Load & clean Landmarks
# -------------------------
landmarks_raw <- read_excel(landmarks_path)
landmarks <- landmarks_raw %>%
  rename(
    Theme = `Theme`,
    SubTheme = `Sub Theme`,
    Name = `Feature Name`,
    Coordinates = `Co-ordinates`
  ) %>%
  separate(Coordinates, into = c("Latitude", "Longitude"), sep = ",\\s*", remove = FALSE) %>%
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  ) %>%
  filter(!is.na(Latitude), !is.na(Longitude))

# -------------------------
# 3) Load & summarise pedestrian counts
# -------------------------
ped_raw <- read_excel(ped_counts_path)
ped_by_sensor <- ped_raw %>%
  rename(
    Sensor_Name = Sensor_Name,
    Total = Total_of_Directions
  ) %>%
  group_by(Sensor_Name) %>%
  summarise(Avg_Count = mean(Total, na.rm = TRUE), .groups = "drop")

# -------------------------
# 4) Load sensor locations (semicolon-separated)
# -------------------------
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

# -------------------------
# 5) Join pedestrian data with coordinates
# -------------------------
ped_geo <- ped_by_sensor %>%
  left_join(sensors_geo, by = "Sensor_Name") %>%
  filter(!is.na(Latitude), !is.na(Longitude))

# -------------------------
# 6) Compute Landmark Popularity (nearest sensor)
# -------------------------
landmark_popularity <- landmarks %>%
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
# 7) Shiny UI
# -------------------------
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Melbourne Landmarks & Pedestrian Sensors — Interactive Dashboard"),
  tags$p("Explore pedestrian activity patterns, popular landmarks, and trends across time."),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("theme_filter", "Filter by Landmark Theme:",
                  choices = c("All", sort(unique(landmarks$Theme))),
                  selected = "All"),
      hr(),
      h5("View Mode:"),
      tabsetPanel(id = "view_mode",
                  tabPanel("Heatmap View", value = "heatmap"),
                  tabPanel("Ranking View", value = "ranking"),
                  tabPanel("Trend View", value = "trend"))  # ✅ New tab
    ),
    
    mainPanel(
      conditionalPanel("input.view_mode == 'heatmap'",
                       leafletOutput("heatmap", height = "750px")
      ),
      conditionalPanel("input.view_mode == 'ranking'",
                       plotlyOutput("popularity_plot", height = "750px")
      ),
      conditionalPanel("input.view_mode == 'trend'",
                       plotlyOutput("trend_plot", height = "750px")  # ✅ Time trend
      )
    )
  )
)

# -------------------------
# 8) Shiny Server
# -------------------------
server <- function(input, output, session) {
  
  # Reactive: Filter landmarks by selected theme
  filtered_landmarks <- reactive({
    if (input$theme_filter == "All") {
      landmark_popularity
    } else {
      landmark_popularity %>% filter(Theme == input$theme_filter)
    }
  })
  
  # --- Heatmap View ---
  output$heatmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addHeatmap(
        data = ped_geo,
        lng = ~Longitude, lat = ~Latitude,
        intensity = ~Avg_Count,
        blur = 25, max = 0.9, radius = 15,
        gradient = c("navy", "cyan", "yellow", "orange", "red"),
        group = "Pedestrian Heatmap"
      ) %>%
      addCircleMarkers(
        data = filtered_landmarks(),
        lng = ~Longitude, lat = ~Latitude,
        fillColor = "#007BFF",
        color = "#004080",
        radius = 5,
        weight = 1,
        stroke = TRUE,
        fillOpacity = 1.0,
        popup = ~paste0(
          "<b>Landmark:</b> ", Name,
          "<br><b>Theme:</b> ", Theme,
          "<br><b>Nearby Avg Count:</b> ",
          format(round(nearest_count), big.mark = ",")
        ),
        group = "Landmarks"
      ) %>%
      addLayersControl(
        overlayGroups = c("Pedestrian Heatmap", "Landmarks"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # --- Ranking View ---
  output$popularity_plot <- renderPlotly({
    df <- filtered_landmarks() %>%
      arrange(desc(nearest_count)) %>%
      head(15)
    
    df$Name <- factor(df$Name, levels = df$Name[order(df$nearest_count, decreasing = TRUE)])
    df$Name <- forcats::fct_rev(df$Name)
    
    p <- ggplot(df, aes(x = Name, y = nearest_count, fill = Theme)) +
      geom_col() +
      coord_flip() +
      labs(x = "Landmark", y = "Avg Pedestrian Count (nearest sensor)",
           title = "Top 15 Landmarks by Nearby Pedestrian Volume") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # --- Trend View (linked to theme filter) ---
  output$trend_plot <- renderPlotly({
    selected_sensors <- filtered_landmarks()$nearest_sensor
    
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
                       ifelse(input$theme_filter == "All", "All Landmarks", input$theme_filter)),
        xaxis = list(title = "Date"),
        yaxis = list(title = "Total Pedestrians"),
        hovermode = "x unified"
      )
  })
}

# -------------------------
# 9) Run the App
# -------------------------
shinyApp(ui, server)
