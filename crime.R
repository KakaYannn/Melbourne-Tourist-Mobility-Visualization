# Melbourne Crime Data Visualization Shiny App
# Data: LGA Recorded Offences Year Ending June 2025

library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(DT)

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
data_list <- load_data()

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Melbourne Crime Statistics - Year Ending June 2025"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Offence Categories", tabName = "categories", icon = icon("list")),
      menuItem("Suburbs Analysis", tabName = "suburbs", icon = icon("map")),
      menuItem("Location Types", tabName = "locations", icon = icon("location-dot")),
      menuItem("Investigation Status", tabName = "investigation", icon = icon("search")),
      menuItem("Drug Offences", tabName = "drugs", icon = icon("pills")),
      menuItem("Data Tables", tabName = "tables", icon = icon("table"))
    )
  ),

  dashboardBody(
    tabItems(
      # Tab 1: Overview (Table 01)
      tabItem(tabName = "overview",
              fluidRow(
                box(width = 12, title = "Melbourne Crime Overview", solidHeader = TRUE, status = "primary",
                    valueBoxOutput("total_offences", width = 4),
                    valueBoxOutput("crime_rate", width = 4),
                    valueBoxOutput("police_region", width = 4)
                )
              ),
              fluidRow(
                box(width = 12, title = "Summary Statistics",
                    verbatimTextOutput("overview_summary")
                )
              )
      ),

      # Tab 2: Offence Categories (Table 02)
      tabItem(tabName = "categories",
              fluidRow(
                box(width = 12, title = "Select Category Level", status = "primary",
                    selectInput("category_level", "Category Level:",
                                choices = c("Offence Division", "Offence Subdivision", "Offence Subgroup"),
                                selected = "Offence Division")
                )
              ),
              fluidRow(
                box(width = 12, title = "Offences by Category",
                    plotlyOutput("category_plot", height = 500)
                )
              ),
              fluidRow(
                box(width = 6, title = "Top 10 Categories",
                    plotlyOutput("top_categories", height = 400)
                ),
                box(width = 6, title = "Crime Rate by Category",
                    plotlyOutput("category_rate", height = 400)
                )
              )
      ),

      # Tab 3: Suburbs Analysis (Table 03)
      tabItem(tabName = "suburbs",
              fluidRow(
                box(width = 12, title = "Filter Options", status = "primary",
                    selectInput("suburb_category", "Offence Category:",
                                choices = c("All", unique(data_list$table03$`Offence Division`)),
                                selected = "All")
                )
              ),
              fluidRow(
                box(width = 12, title = "Offences by Suburb",
                    plotlyOutput("suburb_plot", height = 500)
                )
              ),
              fluidRow(
                box(width = 6, title = "Top 10 Suburbs",
                    plotlyOutput("top_suburbs", height = 400)
                ),
                box(width = 6, title = "Offences by Postcode",
                    plotlyOutput("postcode_plot", height = 400)
                )
              )
      ),

      # Tab 4: Location Types (Table 04)
      tabItem(tabName = "locations",
              fluidRow(
                box(width = 12, title = "Select Location Level", status = "primary",
                    selectInput("location_level", "Location Level:",
                                choices = c("Location Division", "Location Subdivision", "Location Group"),
                                selected = "Location Division")
                )
              ),
              fluidRow(
                box(width = 12, title = "Offences by Location Type",
                    plotlyOutput("location_plot", height = 500)
                )
              ),
              fluidRow(
                box(width = 12, title = "Location Breakdown (Sunburst Chart)",
                    plotlyOutput("location_sunburst", height = 600)
                )
              )
      ),

      # Tab 5: Investigation Status (Table 05)
      tabItem(tabName = "investigation",
              fluidRow(
                box(width = 6, title = "Investigation Status Distribution",
                    plotlyOutput("investigation_pie", height = 400)
                ),
                box(width = 6, title = "Investigation Status Counts",
                    plotlyOutput("investigation_bar", height = 400)
                )
              ),
              fluidRow(
                box(width = 12, title = "Investigation Status Summary",
                    DTOutput("investigation_table")
                )
              )
      ),

      # Tab 6: Drug Offences (Table 06)
      tabItem(tabName = "drugs",
              fluidRow(
                box(width = 12, title = "Filter Options", status = "primary",
                    selectInput("drug_filter", "Filter by:",
                                choices = c("Offence Subdivision", "Offence Group", "CSA Drug Type"),
                                selected = "CSA Drug Type")
                )
              ),
              fluidRow(
                box(width = 12, title = "Drug Offences Analysis",
                    plotlyOutput("drug_plot", height = 500)
                )
              ),
              fluidRow(
                box(width = 6, title = "Drug Types Distribution",
                    plotlyOutput("drug_type_pie", height = 400)
                ),
                box(width = 6, title = "Drug Offence Categories",
                    plotlyOutput("drug_category_bar", height = 400)
                )
              )
      ),

      # Tab 7: Data Tables
      tabItem(tabName = "tables",
              fluidRow(
                box(width = 12, title = "Select Data Table", status = "primary",
                    selectInput("table_select", "Choose Table:",
                                choices = c("Table 01: Overview" = "table01",
                                            "Table 02: Offence Categories" = "table02",
                                            "Table 03: Suburbs" = "table03",
                                            "Table 04: Location Types" = "table04",
                                            "Table 05: Investigation Status" = "table05",
                                            "Table 06: Drug Offences" = "table06"),
                                selected = "table01")
                )
              ),
              fluidRow(
                box(width = 12, title = "Data Table View",
                    DTOutput("data_table")
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Overview Tab (Table 01)
  output$total_offences <- renderValueBox({
    total <- sum(data_list$table01$`Offence Count`, na.rm = TRUE)
    valueBox(
      format(total, big.mark = ","),
      "Total Offences",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })

  output$crime_rate <- renderValueBox({
    rate <- mean(data_list$table01$`Rate per 100,000 population`, na.rm = TRUE)
    valueBox(
      round(rate, 1),
      "Crime Rate per 100,000",
      icon = icon("chart-line"),
      color = "orange"
    )
  })

  output$police_region <- renderValueBox({
    region <- unique(data_list$table01$`Police Region`)[1]
    valueBox(
      region,
      "Police Region",
      icon = icon("shield"),
      color = "blue"
    )
  })

  output$overview_summary <- renderPrint({
    summary(data_list$table01)
  })

  # Categories Tab (Table 02)
  output$category_plot <- renderPlotly({
    category_data <- data_list$table02 %>%
      group_by(!!sym(input$category_level)) %>%
      summarise(Total = sum(`Offence Count`, na.rm = TRUE)) %>%
      arrange(desc(Total)) %>%
      rename(Category = !!sym(input$category_level))

    plot_ly(category_data,
            x = ~reorder(Category, Total),
            y = ~Total,
            type = 'bar',
            marker = list(color = 'steelblue')) %>%
      layout(title = paste("Offences by", input$category_level),
             xaxis = list(title = input$category_level, tickangle = -45),
             yaxis = list(title = "Number of Offences"),
             margin = list(b = 150))
  })

  output$top_categories <- renderPlotly({
    category_data <- data_list$table02 %>%
      group_by(!!sym(input$category_level)) %>%
      summarise(Total = sum(`Offence Count`, na.rm = TRUE)) %>%
      arrange(desc(Total)) %>%
      head(10) %>%
      rename(Category = !!sym(input$category_level))

    plot_ly(category_data,
            labels = ~Category,
            values = ~Total,
            type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent') %>%
      layout(title = "Top 10 Categories (Pie Chart)")
  })

  output$category_rate <- renderPlotly({
    rate_data <- data_list$table02 %>%
      group_by(!!sym(input$category_level)) %>%
      summarise(AvgRate = mean(`LGA Rate per 100,000 population`, na.rm = TRUE)) %>%
      arrange(desc(AvgRate)) %>%
      head(10) %>%
      rename(Category = !!sym(input$category_level))

    plot_ly(rate_data,
            x = ~AvgRate,
            y = ~reorder(Category, AvgRate),
            type = 'bar',
            orientation = 'h',
            marker = list(color = 'coral')) %>%
      layout(title = "Crime Rate by Category (Top 10)",
             xaxis = list(title = "Rate per 100,000 population"),
             yaxis = list(title = ""))
  })

  # Suburbs Tab (Table 03)
  output$suburb_plot <- renderPlotly({
    suburb_data <- data_list$table03

    if (input$suburb_category != "All") {
      suburb_data <- suburb_data %>%
        filter(`Offence Division` == input$suburb_category)
    }

    suburb_summary <- suburb_data %>%
      group_by(`Suburb/Town Name`) %>%
      summarise(Total = sum(`Offence Count`, na.rm = TRUE)) %>%
      arrange(desc(Total))

    plot_ly(suburb_summary,
            x = ~reorder(`Suburb/Town Name`, Total),
            y = ~Total,
            type = 'bar',
            marker = list(color = 'darkgreen')) %>%
      layout(title = "Offences by Suburb",
             xaxis = list(title = "Suburb", tickangle = -45),
             yaxis = list(title = "Number of Offences"),
             margin = list(b = 150))
  })

  output$top_suburbs <- renderPlotly({
    suburb_data <- data_list$table03 %>%
      group_by(`Suburb/Town Name`) %>%
      summarise(Total = sum(`Offence Count`, na.rm = TRUE)) %>%
      arrange(desc(Total)) %>%
      head(10)

    plot_ly(suburb_data,
            y = ~reorder(`Suburb/Town Name`, Total),
            x = ~Total,
            type = 'bar',
            orientation = 'h',
            marker = list(color = 'forestgreen')) %>%
      layout(title = "Top 10 Suburbs",
             xaxis = list(title = "Number of Offences"),
             yaxis = list(title = ""))
  })

  output$postcode_plot <- renderPlotly({
    postcode_data <- data_list$table03 %>%
      group_by(Postcode) %>%
      summarise(Total = sum(`Offence Count`, na.rm = TRUE)) %>%
      arrange(desc(Total))

    plot_ly(postcode_data,
            x = ~as.factor(Postcode),
            y = ~Total,
            type = 'bar',
            marker = list(color = 'teal')) %>%
      layout(title = "Offences by Postcode",
             xaxis = list(title = "Postcode", tickangle = -45),
             yaxis = list(title = "Number of Offences"),
             margin = list(b = 100))
  })

  # Location Types Tab (Table 04)
  output$location_plot <- renderPlotly({
    location_data <- data_list$table04 %>%
      group_by(!!sym(input$location_level)) %>%
      summarise(Total = sum(`Offence Count`, na.rm = TRUE)) %>%
      arrange(desc(Total)) %>%
      rename(Location = !!sym(input$location_level))

    plot_ly(location_data,
            x = ~reorder(Location, Total),
            y = ~Total,
            type = 'bar',
            marker = list(color = 'purple')) %>%
      layout(title = paste("Offences by", input$location_level),
             xaxis = list(title = input$location_level, tickangle = -45),
             yaxis = list(title = "Number of Offences"),
             margin = list(b = 150))
  })

  output$location_sunburst <- renderPlotly({
    # Create hierarchical data for sunburst
    # Level 1: Division
    level1 <- data_list$table04 %>%
      group_by(`Location Division`) %>%
      summarise(Total = sum(`Offence Count`, na.rm = TRUE), .groups = 'drop') %>%
      mutate(labels = `Location Division`,
             parents = "")

    # Level 2: Subdivision
    level2 <- data_list$table04 %>%
      group_by(`Location Division`, `Location Subdivision`) %>%
      summarise(Total = sum(`Offence Count`, na.rm = TRUE), .groups = 'drop') %>%
      mutate(labels = `Location Subdivision`,
             parents = `Location Division`)

    # Level 3: Group
    level3 <- data_list$table04 %>%
      group_by(`Location Subdivision`, `Location Group`) %>%
      summarise(Total = sum(`Offence Count`, na.rm = TRUE), .groups = 'drop') %>%
      mutate(labels = `Location Group`,
             parents = `Location Subdivision`)

    # Combine all levels
    sunburst_data <- bind_rows(level1, level2, level3) %>%
      select(labels, parents, Total)

    plot_ly(sunburst_data,
            labels = ~labels,
            parents = ~parents,
            values = ~Total,
            type = 'sunburst',
            branchvalues = 'total') %>%
      layout(title = "Location Hierarchy Breakdown")
  })

  # Investigation Status Tab (Table 05)
  output$investigation_pie <- renderPlotly({
    investigation_data <- data_list$table05 %>%
      group_by(`Investigation Status`) %>%
      summarise(Total = sum(`Offence Count`, na.rm = TRUE))

    plot_ly(investigation_data,
            labels = ~`Investigation Status`,
            values = ~Total,
            type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            marker = list(colors = c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd'))) %>%
      layout(title = "Investigation Status Distribution")
  })

  output$investigation_bar <- renderPlotly({
    investigation_data <- data_list$table05 %>%
      group_by(`Investigation Status`) %>%
      summarise(Total = sum(`Offence Count`, na.rm = TRUE)) %>%
      arrange(desc(Total))

    plot_ly(investigation_data,
            x = ~`Investigation Status`,
            y = ~Total,
            type = 'bar',
            marker = list(color = 'indianred')) %>%
      layout(title = "Investigation Status Counts",
             xaxis = list(title = "Status", tickangle = -45),
             yaxis = list(title = "Number of Offences"),
             margin = list(b = 100))
  })

  output$investigation_table <- renderDT({
    investigation_data <- data_list$table05 %>%
      group_by(`Investigation Status`) %>%
      summarise(
        `Total Offences` = sum(`Offence Count`, na.rm = TRUE),
        `Percentage` = round(sum(`Offence Count`, na.rm = TRUE) / sum(data_list$table05$`Offence Count`, na.rm = TRUE) * 100, 2)
      ) %>%
      arrange(desc(`Total Offences`))

    datatable(investigation_data, options = list(pageLength = 10))
  })

  # Drug Offences Tab (Table 06)
  output$drug_plot <- renderPlotly({
    drug_data <- data_list$table06 %>%
      group_by(!!sym(input$drug_filter)) %>%
      summarise(Total = sum(`Offence Count`, na.rm = TRUE)) %>%
      arrange(desc(Total)) %>%
      rename(DrugCategory = !!sym(input$drug_filter))

    plot_ly(drug_data,
            x = ~reorder(DrugCategory, Total),
            y = ~Total,
            type = 'bar',
            marker = list(color = 'darkred')) %>%
      layout(title = paste("Drug Offences by", input$drug_filter),
             xaxis = list(title = input$drug_filter, tickangle = -45),
             yaxis = list(title = "Number of Offences"),
             margin = list(b = 200))
  })

  output$drug_type_pie <- renderPlotly({
    drug_data <- data_list$table06 %>%
      group_by(`CSA Drug Type`) %>%
      summarise(Total = sum(`Offence Count`, na.rm = TRUE))

    plot_ly(drug_data,
            labels = ~`CSA Drug Type`,
            values = ~Total,
            type = 'pie',
            textposition = 'outside',
            textinfo = 'label+percent') %>%
      layout(title = "Drug Types Distribution")
  })

  output$drug_category_bar <- renderPlotly({
    drug_data <- data_list$table06 %>%
      group_by(`Offence Subdivision`) %>%
      summarise(Total = sum(`Offence Count`, na.rm = TRUE)) %>%
      arrange(desc(Total))

    plot_ly(drug_data,
            y = ~reorder(`Offence Subdivision`, Total),
            x = ~Total,
            type = 'bar',
            orientation = 'h',
            marker = list(color = 'maroon')) %>%
      layout(title = "Drug Offence Categories",
             xaxis = list(title = "Number of Offences"),
             yaxis = list(title = ""))
  })

  # Data Tables Tab
  output$data_table <- renderDT({
    selected_table <- data_list[[input$table_select]]
    datatable(selected_table,
              options = list(
                pageLength = 25,
                scrollX = TRUE,
                autoWidth = TRUE
              ),
              filter = 'top')
  })
}

# Run the application
shinyApp(ui = ui, server = server)
