# Melbourne Urban Analytics Dashboard

## Project Overview

This project is a **Tableau dashboard with embedded Shiny elements** that visualizes urban analytics datasets for the City of Melbourne. The hybrid interface combines Tableau's data visualization capabilities with R Shiny's interactive web application framework to provide comprehensive analysis of public transport, pedestrian activity, on-street parking, crime statistics, and points of interest.

The project consists of:
- **Tableau Dashboard** (`IV project.twb`): Main visualization interface
- **R Shiny Application** (`all.R`): Interactive web components embedded within Tableau
- **Data Files**: Geospatial and statistical datasets from official Melbourne government sources

## How to Launch the Interface (For Markers)

### Method 1: Launch Tableau Dashboard (Recommended)

This is the primary interface for assessment.

1. **Open Tableau Desktop**
   - Ensure Tableau Desktop 2020.4 or later is installed

2. **Open the Project File**
   - Double-click `IV project.twb` or
   - Open Tableau Desktop and select File > Open > `IV project.twb`

3. **View the Dashboard**
   - The dashboard will load with all visualizations
   - Embedded Shiny components will be visible within the Tableau interface

### Method 2: Launch Shiny Application Standalone

For testing individual Shiny components outside of Tableau.

**Prerequisites:**

Ensure you have R (version 4.0 or higher) installed with the following packages:

```r
# Install required packages
install.packages(c("shiny", "sf", "lwgeom", "dplyr", "tidyr",
                   "readxl", "janitor", "DT", "leaflet",
                   "leaflet.extras", "ggplot2", "plotly"))
```

**Running the Shiny App:**

1. Open R or RStudio
2. Set your working directory to the project folder:
   ```r
   setwd("/path/to/GEOM90007-project")
   ```
3. Run the application:
   ```r
   # Option 1: Source and run
   source('all.R')

   # Option 2: Run directly
   shiny::runApp()
   ```

4. The application will open in your default web browser

**Note:** The standalone Shiny app provides the same functionality as the embedded components but in a separate interface with four tabs: Public Transport, Pedestrian Counts, On-street Parking, and Security View (Crime).

## System Requirements

### For Tableau Dashboard:
- Tableau Desktop 2020.4 or later
- Internet connection (for API-based boundary data)
- Minimum 8 GB RAM (16 GB recommended due to large geospatial datasets)

### For Shiny Application:
- R version 4.0 or higher
- RStudio (recommended but not required)
- Internet connection (for loading Melbourne municipal boundary from API)
- Minimum 8 GB RAM (due to 362 MB public transport lines dataset)

### Required R Packages:
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

## Project Structure

### Required Files

The following files must remain in the project directory:

#### Application Files

**IV project.twb** (15 KB)
- Main Tableau dashboard containing embedded Shiny visualizations
- Primary interface for project assessment

**all.R** (166 KB)
- R Shiny application file
- Can be run standalone or embedded in Tableau
- Provides interactive mapping and filtering functionality

#### Data Files

**Boundary Data:**
- Loaded via API: https://data.melbourne.vic.gov.au (City of Melbourne boundary)
- No local boundary files required (replaced with API call)

**landmarks-and-places-of-interest-including-schools-theatres-health-services-spor.geojson** (51 KB)
- 242 landmark locations across Melbourne
- Used across all visualization tabs

**Public Transport Data:**
- `public_transport_stops.geojson` (7.0 MB) - 29,202 PT stops
- `public_transport_lines.geojson` (362 MB) - 10,700 route segments
- `annual_metropolitan_train_station_entries_fy_2024_2025.csv` (21 KB) - Patronage data

**Pedestrian Data:**
- `pedestrian-counting-system-monthly-counts-per-hour.xlsx` (57 MB) - Historical counts
- `pedestrian-counting-system-sensor-locations.csv` (19 KB) - Sensor locations

**Crime Data:**
- `Data_Tables_LGA_Recorded_Offences_Year_Ending_June_2025.xlsx` (18 MB) - Crime statistics
- `vic_suburbs.geojson.backup` (2.8 MB) - Suburb boundaries (fallback)

### Optional/Deletable Files

These files are not required for the application to run:

- `people.csv` (76 MB) - Not referenced in code
- `landmark.xlsx` (23 KB) - Replaced by GeoJSON version
- `FINAL_FIXES_SUMMARY.md` - Development notes

**Total project size (required files only): ~451 MB**

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

## Data Sources

All data is sourced from official government datasets:

- **City of Melbourne Open Data Portal**: Boundary, landmarks, parking, pedestrian counts
- **Public Transport Victoria**: PT stops, routes, patronage
- **Victoria Police**: Crime statistics
- **Geoscape Australia**: Suburb boundaries

### API Integration

The project uses live API calls for:
- Municipal boundary data (loaded on startup from data.melbourne.vic.gov.au)
- Ensures always-current boundary definitions
- Requires internet connection during initialization

## Technical Notes

### Performance Considerations

Initial load time: 30-60 seconds depending on system performance

Large files that impact loading time:
1. `public_transport_lines.geojson` (362 MB)
2. `pedestrian-counting-system-monthly-counts-per-hour.xlsx` (57 MB)

### Tab Isolation

User interactions within one tab do not affect visualizations in other tabs. Each tab maintains independent filter states and map views to prevent cross-contamination.

### Spatial Processing

On startup, the application performs:
- API call to load Melbourne city boundary
- Spatial filtering of data to Melbourne area
- Geocoding of landmarks and infrastructure
- Deduplication of overlapping transport stops

These operations are cached after initial processing for improved performance.

### Boundary Data Source

The application loads the Melbourne municipal boundary via API instead of local files:
- Source: `https://data.melbourne.vic.gov.au/explore/dataset/municipal-boundary/`
- Format: GeoJSON
- Benefits: Always up-to-date, reduces project file count
- Trade-off: Requires internet connection on startup

## Troubleshooting

### Tableau Dashboard Issues

**Problem:** Dashboard does not load
- **Solution:** Ensure Tableau Desktop is installed and up to date
- Check that all data files are in the project directory

**Problem:** Shiny components not appearing
- **Solution:** Verify R and required packages are installed
- Check that `all.R` is in the same directory as the Tableau workbook

### Shiny Application Issues

**Problem:** App fails to start
- **Solution:** Check R console for missing package errors
- Install missing packages: `install.packages("package_name")`
- Verify all data files are present

**Problem:** Boundary map does not appear
- **Solution:** Check internet connection (boundary loads via API)
- If offline, the app will use fallback coordinates

**Problem:** Long startup time
- **Expected behavior:** Large datasets require 30-60 seconds to load
- Progress messages will appear in R console

## Version Information

- **R version**: 4.0 or higher recommended
- **Tableau Desktop**: 2020.4 or higher
- **Shiny package**: 1.7 or higher
- **sf package**: 1.0 or higher

## Academic Context

This application was developed as part of GEOM90007 (Geospatial Information Systems and Cartography) coursework. The hybrid Tableau-Shiny architecture demonstrates integration of business intelligence tools with custom geospatial analysis capabilities.

## Contact and Support

For technical issues or questions about the implementation, please refer to:
- Project repository documentation
- R package documentation: https://shiny.rstudio.com/
- Tableau documentation: https://help.tableau.com/
