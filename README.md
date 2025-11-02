# Melbourne Urban Analytics Dashboard

## Project Overview

This project is an interactive Shiny dashboard that visualizes various urban analytics datasets for the City of Melbourne. The application integrates data on public transport, pedestrian activity, on-street parking, crime statistics, and points of interest to provide a comprehensive view of urban patterns and infrastructure.

The main application file is `all.R`, which consolidates all functionality into a single, integrated Shiny app with four main tabs: Public Transport, Pedestrian Counts, On-street Parking, and Security View (Crime).

## How to Run the Application

### Prerequisites

Ensure you have R installed with the following packages:

```r
# Core Shiny and UI
shiny

# Spatial data handling
sf
lwgeom

# Data manipulation and visualization
dplyr
tidyr
readxl
janitor
DT

# Mapping
leaflet
leaflet.extras

# Plotting
ggplot2
plotly
```

### Running the App

1. Open R or RStudio
2. Set your working directory to the project folder
3. Run one of the following commands:

```r
# Option 1: Source and run
source('all.R')

# Option 2: Run directly
shiny::runApp()
```

The application will open in your default web browser.

## Required Files

The following files are essential for `all.R` to function properly. Do not delete these files.

### Spatial Boundary Data

**municipal-boundary.shp** (10 KB)
- Shapefile defining the City of Melbourne boundary
- Used across all tabs to clip data and define the study area
- Requires companion files: `municipal-boundary.dbf`, `municipal-boundary.prj`, `municipal-boundary.shx`

**municipal-boundary.dbf** (2.3 KB)
- Database file containing attribute data for the shapefile

**municipal-boundary.prj** (143 B)
- Projection file defining the coordinate reference system

**municipal-boundary.shx** (108 B)
- Shape index file for efficient spatial queries

### Points of Interest

**landmarks-and-places-of-interest-including-schools-theatres-health-services-spor.geojson** (51 KB)
- Contains 242 landmark locations across Melbourne
- Used in all four tabs for spatial filtering and context
- Categories include schools, theatres, health services, sports facilities, etc.

### Public Transport Data

**public_transport_stops.geojson** (7.0 MB)
- Contains 29,202 public transport stops across Victoria
- Includes metro trains, trams, and buses
- Filtered to Melbourne area during runtime

**public_transport_lines.geojson** (362 MB)
- Contains 10,700 public transport route line segments
- Displays routes when users click on transport stops
- Large file but necessary for route visualization

**annual_metropolitan_train_station_entries_fy_2024_2025.csv** (21 KB)
- Annual patronage data for metropolitan train stations
- Used to size station markers based on usage

### Pedestrian Activity Data

**pedestrian-counting-system-monthly-counts-per-hour.xlsx** (57 MB)
- Historical pedestrian count data from sensors across Melbourne
- Contains hourly counts aggregated by month
- Large file due to temporal granularity

**pedestrian-counting-system-sensor-locations.csv** (19 KB)
- Geographic locations of pedestrian counting sensors
- Links sensor IDs to coordinates for mapping

### Crime Statistics

**Data_Tables_LGA_Recorded_Offences_Year_Ending_June_2025.xlsx** (18 MB)
- Crime statistics for Melbourne Local Government Area
- Contains 6 tables with different crime category breakdowns
- Source: Victoria Police crime data

**vic_suburbs.geojson.backup** (2.8 MB)
- Victorian suburb boundary polygons
- Used to visualize crime data by suburb
- Falls back to online source if this file is missing

### Application Code

**all.R** (165 KB)
- Main Shiny application file
- Integrates all data sources and provides interactive visualizations
- Self-contained application requiring only the data files listed above

## Files That Can Be Deleted

The following files are legacy code from earlier development stages. All functionality has been integrated into `all.R`, so these files are no longer necessary for the application to run:

### Legacy R Scripts

**crime.R** (18 KB)
- Original standalone crime visualization script
- All functionality now in `all.R`

**Integration.R** (64 KB)
- Earlier attempt at integrating different modules
- Superseded by `all.R`

**parking.R** (29 KB)
- Original parking data visualization script
- All functionality now in `all.R`

**pedestrian.R** (7.2 KB)
- Original pedestrian count visualization script
- All functionality now in `all.R`

**pt.R** (14 KB)
- Original public transport visualization script
- All functionality now in `all.R`

### Unused Data Files

**landmark.xlsx** (23 KB)
- Excel version of landmarks data
- Replaced by the GeoJSON file for better spatial handling

**people.csv** (76 MB)
- Appears to be raw pedestrian or demographic data
- Not referenced in `all.R`
- Can be safely deleted unless needed for external analysis

### Documentation Files

**FINAL_FIXES_SUMMARY.md** (5.6 KB)
- Development notes about bug fixes
- Useful for understanding development history but not required for running the app
- Can be archived or deleted

## Application Features

### Public Transport Tab

- Interactive map showing train stations, tram stops, and bus stops
- Color-coded by transport mode: blue (train), green (tram), orange (bus)
- Click stops to view routes passing through that location
- Filter by landmark proximity using adjustable buffer radius
- Station markers sized by annual patronage

### Pedestrian Counts Tab

- Heatmap visualization of pedestrian activity
- Filter by time of day, day of week, and month
- Integration with landmark locations
- Shows pedestrian sensor locations with count statistics

### On-street Parking Tab

- Parking restriction zones across Melbourne
- Shows parking signs and restriction types
- Buffer analysis around selected landmarks
- Parking duration and permit information

### Security View (Crime Tab)

- Crime statistics visualized by suburb
- Color-coded choropleth map showing crime intensity
- Filter by crime category: all offences, crimes against persons, property crimes, drug offences, public order offences
- Adjustable minimum offence threshold
- Interactive suburb polygons with detailed statistics
- Optional crime heatmap overlay
- Integrated landmark markers for spatial context

## Data Sources

All data is sourced from official government datasets:

- City of Melbourne Open Data Portal
- Public Transport Victoria
- Victoria Police Crime Statistics
- Geoscape Australia (suburb boundaries)

## Technical Notes

### Performance Considerations

The application loads several large files on startup. Initial load time may take 30-60 seconds depending on system performance. The largest files are:

1. `public_transport_lines.geojson` (362 MB)
2. `pedestrian-counting-system-monthly-counts-per-hour.xlsx` (57 MB)
3. `people.csv` (76 MB) - not used, can be deleted

### Tab Isolation

User interactions within one tab do not affect visualizations in other tabs. Each tab maintains independent filter states and map views to prevent cross-contamination of user selections.

### Spatial Processing

The application performs several spatial operations on startup:

- Clipping data to Melbourne city boundary
- Geocoding landmarks and infrastructure
- Computing spatial relationships for filtering
- Deduplicating overlapping transport stops

These operations happen once during initialization and are cached for performance.

## File Size Summary

Total required data: approximately 470 MB
Total deletable files: approximately 95 MB

After cleanup, the project will contain only the essential files needed to run the application.

## Support and Troubleshooting

If the application fails to load:

1. Verify all required data files are present in the project directory
2. Check that file paths are correct (all files should be in the same directory as `all.R`)
3. Ensure all required R packages are installed
4. Check the R console for error messages indicating missing files or packages

Common issues:

- **Missing vic_suburbs file**: The app will attempt to download suburb boundaries from data.gov.au if the local file is missing
- **Font Awesome icons not displaying**: Ensure internet connection is available for CDN resources
- **Slow initial load**: Large geospatial files take time to read; this is normal

## Version Information

- R version recommended: 4.0 or higher
- Shiny version: 1.7 or higher
- sf package version: 1.0 or higher

This application was developed as part of GEOM90007 coursework.
