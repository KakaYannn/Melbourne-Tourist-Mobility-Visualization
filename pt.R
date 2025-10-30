library(shiny)
library(sf)
library(dplyr)
library(stringr)
library(leaflet)
library(ggplot2)
library(ggiraph)

# read data
boundary <- st_read('./municipal-boundary.shp')
stops <- st_read('./public_transport_stops.geojson')
stations <- read.csv('./annual_metropolitan_train_station_entries_fy_2024_2025.csv')
lines <- st_read('./public_transport_lines.geojson')
pois <- st_read('./landmarks-and-places-of-interest-including-schools-theatres-health-services-spor.geojson')

# add coord columns for stops and pois
stops <- cbind(stops, st_coordinates(stops))
pois <- cbind(pois, st_coordinates(pois))

# add geometry column for stations
stations <- st_as_sf(stations, coords = c('Stop_long', 'Stop_lat'), crs = 4326, remove = FALSE)

# rename columns
colnames(stops) <- c('id', 'name', 'mode', 'lon', 'lat', 'geometry')
colnames(stations) <- c('year', 'id', 'station', 'lat', 'lon', 
                        'annual', 'weekday', 'normalweekday', 'schholweekday', 'saturday', 'sunday', 
                        'early', 'ampeak', 'interpeak', 'pmpeak', 'late', 'geometry')
colnames(lines) <- c('id', 'destination', 'shortname', 'longname', 'mode', 'geometry')
colnames(pois) <- c('type', 'subtype', 'name', 'lon', 'lat', 'geometry')

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

# filter poi types
pois <- pois[!pois$type %in% c('Education Centre', 'Residential Accommodation', 'Transport', 'Vacant Land'), ]
pois[pois$type == 'Mixed Use', c('type', 'subtype')] <- 'Retail'

# add unique id columns
stops$id <- paste0('stop', seq_len(nrow(stops)))
stations$id <- paste0('station', seq_len(nrow(stations)))
lines$id<- paste0('line', seq_len(nrow(lines)))
pois$id <- paste0('poi', seq_len(nrow(pois)))

# add colours and icons for transport modes
mapcolour <- setNames(c('blue', 'green', 'orange'), c('METRO TRAIN', 'METRO TRAM', 'METRO BUS'))
mapicon <- setNames(c('subway', 'train', 'bus'), c('METRO TRAIN', 'METRO TRAM', 'METRO BUS'))
stops$colour <- unname(mapcolour[stops$mode])
stops$icon <- unname(mapicon[stops$mode])
stations$colour <- rep('blue', nrow(stations))
stations$icon <- rep('subway', nrow(stations))
lines$colour <- unname(mapcolour[lines$mode])

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
stops2 <- st_transform(stops, 3857)
stations2 <- st_transform(stations, 3857)
lines2 <- st_transform(lines, 3857)
pois2 <- st_transform(pois, 3857)

# pt tab
pttab <- tabPanel(
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
      style = 'max-width: 280px'
    ), 
    mainPanel(
      fluidRow(
        column(8, leafletOutput('mappt', width = 800, height = 800)), 
        column(4, uiOutput('patronage'))
      )
    )
  )
)

# ui
ui <- navbarPage(
  id = 'mypage', 
  title = '',
  pttab
)

# server
server <- function(input, output, session) {
  
  # station select
  selectedstation <- reactiveVal(NULL)
  
  # create map
  output$mappt <- renderLeaflet({
    
    # filter type
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
      h4(paste0('Annual: ', currstation$annual)), 
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
      geom_bar_interactive(stat = 'identity', fill = 'blue') + 
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
      geom_bar_interactive(stat = 'identity', fill = 'blue') + 
      labs(title = 'Average Patronage per Time Period on Weekdays', x = NULL, y = 'Average Patronage') + 
      theme_minimal()
    
    girafe(ggobj = p)
  })
    
  })
}

shinyApp(ui, server)
