#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(DBI)
library(odbc)

# # Database connection function
# get_detections_data <- function() {
#   # Database connection parameters
#   # Replace these with your actual SQL Server connection details
#   con <- DBI::dbConnect(
#     odbc::odbc(),
#     Driver = "ODBC Driver 17 for SQL Server",  # or "ODBC Driver 17 for SQL Server"
#     Server = "DNRCPWPF53ENDQ",  # e.g., "localhost" or "server.domain.com"
#     Database = "SCT",
#     trusted_connection = "yes",
#     # Port = 1433  # default SQL Server port
#   )
#   
#   # Execute query and fetch data
#   detections <- 
#     rbind(DBI::dbReadTable(con,Id("BLRA","v_Detections_NatDB")),
#           dbReadTable(con,Id("BLRA","V_Detections_OLD")))
#   
#   # Close connection
#   DBI::dbDisconnect(con)
#   
#   return(detections)
# }
# Create dummy data that mimics the structure of your original database data
set.seed(42)  # For reproducible dummy data

# Generate dummy detection data with ORIGINAL column names
get_detections_data <- function() {
  n_records <- 500
  
  properties <- c("Blackwater NWR", "Patuxent Research Refuge", "Prime Hook NWR", "Bombay Hook NWR")
  species <- c("BLRA", "KING", "VIRG", "SORA", "COYE", "REBL", "na", "uk", "CLRA")
  time_segments <- c("Evening", "Night", "Pre-dawn", "Dawn")
  moon_phases <- c("NEW", "Waxing crescent", "First quarter", "Waxing gibbous", 
                   "FULL", "Waning gibbous", "Last quarter", "Waning crescent")
  
  # Create site points based on properties
  site_points <- c()
  for(prop in properties) {
    site_points <- c(site_points, paste0(substr(prop, 1, 3), "_", sprintf("%02d", 1:sample(8:12, 1))))
  }
  
  # Generate random dates between 2018 and 2024
  start_date <- as.Date("2018-01-01")
  end_date <- as.Date("2024-12-31")
  
  # Create data frame with ORIGINAL column names (before renaming)
  data <- data.frame(
    Property.Identifier = sample(properties, n_records, replace = TRUE),
    SitePointID = sample(site_points, n_records, replace = TRUE),
    Species = sample(species, n_records, replace = TRUE, 
                     prob = c(0.25, 0.15, 0.15, 0.1, 0.08, 0.08, 0.05, 0.05, 0.04)),
    SurDate = sample(seq(start_date, end_date, by = "day"), n_records, replace = TRUE),
    TimeSegment = sample(time_segments, n_records, replace = TRUE),
    MoonPhase = sample(moon_phases, n_records, replace = TRUE),
    LunarAge = sample(0:29, n_records, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Add Year and Month columns
  data$Year <- as.numeric(format(data$SurDate, "%Y"))
  data$Month <- as.numeric(format(data$SurDate, "%m"))
  
  return(data)
}

# Load data at app startup
# You can also make this reactive if you want to refresh data periodically
Detections <- get_detections_data()

# Ensure date columns are properly formatted
Detections$SurDate <- as.Date(Detections$SurDate)
Detections<-Detections %>% 
  mutate(MoonPhase=factor(MoonPhase, 
                          levels=c("NEW","Waxing crescent",
                                   "First quarter","Waxing gibbous",
                                   "FULL","Waning gibbous",
                                   "Last quarter","Waning crescent"))) %>% 
  rename(
    "Time of Day"="TimeSegment",
    "Property"="Property.Identifier",
    "Site Point"="SitePointID",
    "Survey Date"="SurDate",
    "Moon Phase"="MoonPhase",
    "Lunar Age"="LunarAge")

ui <- page_sidebar(
  title = "Eastern Black Rail Survey Data Exploration",
  sidebar = sidebar(
    width = 300,
    h4("Plot Configuration"),
    
    # X-axis variable
    selectInput("x_var", "X-axis Variable:", 
                choices = c("Year", "Month", "Site Point","Property", "Moon Phase", "Time of Day"),
                selected = "Year"),
    
    # Color variable
    selectInput("color_var", "Color Variable:", 
                choices = c("Property", "Site Point", "Species", "Moon Phase", "Year","Month","Time of Day"),
                selected = "Property"),
    
    # Facet variable
    selectInput("facet_var", "Facet Variable:", 
                choices = c("None", "Species", "Property","Site Point", "Year","Month", "Moon Phase", "Time of Day"),
                selected = "Species"),
    
    hr(),
    
    h4("Filters"),
    
    # Property filter
    selectInput("property_filter", "Filter by Property:",
                choices = c("All", unique(Detections$Property)),
                selected = "All",
                multiple = TRUE),
    
    # Site Point filter - will be updated dynamically based on property selection
    selectInput("site_point_filter", "Filter by Site Point:",
                choices = c("All", unique(Detections$`Site Point`)),
                selected = "All",
                multiple = TRUE),
    
    # Species filter
    selectInput("species_filter", "Filter by Species:",
                choices = c("All", unique(Detections$Species)),
                selected = "All",
                multiple = TRUE),
    
    # Year filter
    sliderInput("year_range", "Year Range:",
                min = min(Detections$Year, na.rm = TRUE),
                max = max(Detections$Year, na.rm = TRUE),
                value = c(min(Detections$Year, na.rm = TRUE), 
                          max(Detections$Year, na.rm = TRUE)),
                step = 1,
                sep = ""),
    
    # Additional filters
    selectInput("time_segment_filter", "Filter by Time of Day:",
                choices = c("All", unique(Detections$`Time of Day`)),
                selected = "All",
                multiple = TRUE),
    
    # Data processing options
  ),
  
  navset_card_tab(
    nav_panel("Main Plot",
              card(
                card_header("Species Detection Analysis"),
                plotlyOutput("main_plot", height = "600px")
              )
    ),
    
    nav_panel("Summary Table",
              card(
                card_header("Detection Summary"),
                DT::dataTableOutput("summary_table")
              )
    ),
    
    nav_panel("Raw Data",
              card(
                card_header("Filtered Detection Data"),
                DT::dataTableOutput("raw_data_table")
              )
    ),
    
    nav_panel("Statistics",
              layout_column_wrap(
                width = 1/2,
                card(
                  card_header("Detection Counts by Variable"),
                  plotlyOutput("stats_plot1")
                ),
                card(
                  card_header("Species Diversity"),
                  plotlyOutput("stats_plot2")
                )
              ),
              card(
                card_header("Summary Statistics"),
                verbatimTextOutput("summary_stats")
              )
    )
  )
)


server <- function(input, output, session) {
  
  # Dynamic site point filter based on Property selection
  observe({
    if ("All" %in% input$property_filter || is.null(input$property_filter) ||
        length(input$property_filter) == 0) {
      # If "All" is selected or no property is selected, show all site points
      available_sites <- unique(Detections$`Site Point`)
    } else {
      # Filter site points based on selected properties
      filtered_data <- Detections %>% 
        filter(Property %in% input$property_filter)
      available_sites <- unique(filtered_data$`Site Point`)
    }
    
    # Get currently selected site points that are still available
    current_selection <- input$site_point_filter
    if (is.null(current_selection)) current_selection <- "All"
    
    # Keep only the selections that are still valid
    valid_selection <- intersect(current_selection, c("All", available_sites))
    if (length(valid_selection) == 0) valid_selection <- "All"
    
    # Update the Site Point filter choices
    updateSelectInput(
      session, 
      "site_point_filter",
      choices = c("All", sort(available_sites)),
      selected = valid_selection
    )
  })
  
  ## Add these observers to make sure All deselects if another option is chosen:
  
  # Property filter - auto-handle "All" selection
  observe({
    selected <- input$property_filter
    if (length(selected) > 1 && "All" %in% selected) {
      # If "All" and other options are selected, keep only the other options
      other_selections <- setdiff(selected, "All")
      updateSelectInput(session, "property_filter", selected = other_selections)
    }
  }) %>% 
    bindEvent(input$property_filter)
  
  # Site Point filter - auto-handle "All" selection  
  observe({
    selected <- input$site_point_filter
    if (length(selected) > 1 && "All" %in% selected) {
      # If "All" and other options are selected, keep only the other options
      other_selections <- setdiff(selected, "All")
      updateSelectInput(session, "site_point_filter", selected = other_selections)
    }
  }) %>% 
    bindEvent(input$site_point_filter)
  
  # Species filter - auto-handle "All" selection
  observe({
    selected <- input$species_filter
    if (length(selected) > 1 && "All" %in% selected) {
      # If "All" and other options are selected, keep only the other options
      other_selections <- setdiff(selected, "All")
      updateSelectInput(session, "species_filter", selected = other_selections)
    }
  }) %>% 
    bindEvent(input$species_filter)
  
  # Time of Day filter - auto-handle "All" selection
  observe({
    selected <- input$time_segment_filter
    if (length(selected) > 1 && "All" %in% selected) {
      # If "All" and other options are selected, keep only the other options
      other_selections <- setdiff(selected, "All")
      updateSelectInput(session, "time_segment_filter", selected = other_selections)
    }
  }) %>% 
    bindEvent(input$time_segment_filter)
  ## end of added observers
  
  # Reactive data filtering
  filtered_data <- reactive({
    data <- Detections
    
    # Handle unknown species - automatically group na and uk as Unknown
    data <- data %>%
      mutate(Species = ifelse(Species %in% c("na", "uk"), "Unknown", Species))
    
    # Apply filters
    if (!"All" %in% input$property_filter && !is.null(input$property_filter) && length(input$property_filter) > 0) {
      data <- data %>% filter(Property %in% input$property_filter)
    }
    
    if (!"All" %in% input$site_point_filter && !is.null(input$site_point_filter) && length(input$site_point_filter) > 0) {
      data <- data %>% filter(`Site Point` %in% input$site_point_filter)
    }
    
    if (!"All" %in% input$species_filter && !is.null(input$species_filter) && length(input$species_filter) > 0) {
      data <- data %>% filter(Species %in% input$species_filter)
    }
    
    # Apply year filter
    data <- data %>%
      filter(Year >= input$year_range[1] & Year <= input$year_range[2])
    
    # Apply time segment filter
    if (!"All" %in% input$time_segment_filter && !is.null(input$time_segment_filter) && length(input$time_segment_filter) > 0) {
      data <- data %>% filter(`Time of Day` %in% input$time_segment_filter)
    }
    
    return(data)
  })
  
  # Plot data reactive - simplified approach without mapping function
  plot_data <- reactive({
    data <- filtered_data()
    
    # Create grouping variables - use the input values directly as they match column names
    group_vars <- c(input$x_var, input$color_var)
    if (input$facet_var != "None") {
      group_vars <- c(group_vars, input$facet_var)
    }
    
    # Remove duplicates
    group_vars <- unique(group_vars)
    
    # For columns with spaces, we need to use sym() and !! to handle them properly
    group_syms <- syms(group_vars)
    
    # Group and summarize
    summary_data <- data %>%
      group_by(!!!group_syms) %>%
      summarise(Count = n(), .groups = 'drop')
    
    return(summary_data)
  })
  
  # Main plot output
  output$main_plot <- renderPlotly({
    req(plot_data())
    
    data <- plot_data()
    
    # Handle edge case where no data is available
    if (nrow(data) == 0) {
      return(plotly_empty())
    }
    
    # Create base plot using aes() with sym() for proper column name handling
    p <- ggplot(data, aes(x = !!sym(input$x_var), 
                          y = Count, 
                          color = !!sym(input$color_var)))
    
    # Add geom_line Plot
    p <- p + geom_line(size = 1, aes(group = !!sym(input$color_var))) + 
      geom_point(size = 2)
    
    # Add faceting if selected - handle column names with spaces
    if (input$facet_var != "None") {
      facet_formula <- as.formula(paste("~", "`", input$facet_var, "`", sep = ""))
      p <- p + facet_wrap(facet_formula, scales = "free")
    }
    
    # Force integer scales for Year and Month
    if (input$x_var %in% c("Year", "Month")) {
      p <- p +
        scale_x_continuous(breaks = function(x) seq(floor(min(x)), 
                                                    ceiling(max(x)), by = 1),
                           labels = function(x) as.integer(x))
    }
    
    
    # Apply theme and labels
    p <- p + 
      theme_void() +
      labs(
        title = paste("Species Detection Counts by", input$x_var),
        x = input$x_var,
        y = "Count",
        color = input$color_var
      ) +
      theme(
        # Axis lines - only x and y
        axis.line.x = element_line(color = "black", size = 0.5),
        axis.line.y = element_line(color = "black", size = 0.5),
        
        # Axis text - black color
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        
        # Axis titles - black color
        axis.title.x = element_text(color = "black", size = 12, margin = margin(t = 10)),
        axis.title.y = element_text(color = "black", size = 12, margin = margin(r = 10)),
        
        # Plot title - black color
        plot.title = element_text(color = "black", size = 14, hjust = 0.5, margin = margin(b = 20)),
        
        # Legend - black text
        legend.text = element_text(color = "black", size = 10),
        legend.title = element_text(color = "black", size = 11),
        legend.position = "bottom",
        
        # Facet labels - black text
        strip.text = element_text(color = "black", size = 11, margin = margin(b = 5)),
        strip.background = element_blank(),
        
        # Panel background
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        
        # Remove all gridlines (already done with theme_void, but explicit)
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        # Panel spacing for facets
        panel.spacing = unit(1, "lines")
      )
    
    # Convert to plotly
    ggplotly(p) %>%
      layout(
        hovermode = "closest",
        legend = list(orientation = "h", y = -0.2)
      )
  })
  
  # Summary table
  output$summary_table <- DT::renderDataTable({
    DT::datatable(
      plot_data(),
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        order = list(list(ncol(plot_data()) - 1, 'desc'))  # Order by Count descending
      ),
      filter = "top"
    )
  })
  
  # Raw data table
  output$raw_data_table <- DT::renderDataTable({
    DT::datatable(
      filtered_data(),
      options = list(
        pageLength = 15,
        scrollX = TRUE
      ),
      filter = "top"
    )
  })
  
  # Statistics plot 1
  output$stats_plot1 <- renderPlotly({
    req(filtered_data())
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(plotly_empty())
    }
    
    stats_data <- data %>%
      group_by(!!sym(input$color_var)) %>%
      summarise(Total_Detections = n(), .groups = 'drop')
    
    p <- ggplot(stats_data, aes(x = !!sym(input$color_var), 
                                y = Total_Detections, 
                                fill = !!sym(input$color_var))) +
      geom_col() +
      theme_minimal() +
      labs(
        title = paste("Total Detections by", input$color_var),
        x = input$color_var,
        y = "Total Detections"
      )
    # Add this after creating each plot p, before the final theme application:
    if (input$color_var %in% c("Year", "Month")) {
      p <- p + scale_x_continuous(breaks = function(x) 
        seq(floor(min(x)), ceiling(max(x)), by = 1),
        labels = function(x) as.integer(x))
    }
    
    p<-p+theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
    
    
    ggplotly(p)
  })
  
  # Statistics plot 2
  output$stats_plot2 <- renderPlotly({
    req(filtered_data())
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return(plotly_empty())
    }
    
    diversity_data <- data %>%
      group_by(!!sym(input$color_var)) %>%
      summarise(
        Species_Count = n_distinct(Species),
        .groups = 'drop'
      )
    
    p <- ggplot(diversity_data, aes(x = !!sym(input$color_var), 
                                    y = Species_Count, 
                                    fill = !!sym(input$color_var))) +
      geom_col() +
      theme_minimal() +
      labs(
        title = paste("Species Diversity by", input$color_var),
        x = input$color_var,
        y = "Number of Species"
      )
    # Add this after creating each plot p, before the final theme application:
    if (input$color_var %in% c("Year", "Month")) {
      p <- p +
        scale_x_continuous(breaks = function(x) seq(floor(min(x)),
                                                    ceiling(max(x)), by = 1),
                           labels = function(x) as.integer(x))
    }
    
    p<-p+theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
    
    ggplotly(p)
  })
  
  # Summary statistics
  output$summary_stats <- renderText({
    req(filtered_data())
    
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      return("No data available with current filters.")
    }
    
    total_detections <- nrow(data)
    unique_species <- n_distinct(data$Species)
    unique_properties <- n_distinct(data$Property)
    unique_points <- n_distinct(data$`Site Point`)
    date_range <- paste(min(data$`Survey Date`, na.rm = TRUE), "to", max(data$`Survey Date`, na.rm = TRUE))
    
    paste(
      "FILTERED DATA SUMMARY:",
      paste("Total Detections:", total_detections),
      paste("Unique Species:", unique_species),
      paste("Unique Properties:", unique_properties),
      paste("Unique Site Points:", unique_points),
      paste("Date Range:", date_range),
      "",
      "SPECIES BREAKDOWN:",
      paste(capture.output(data %>% 
                             count(Species, sort = TRUE) %>% 
                             head(10) %>% 
                             print()), collapse = "\n"),
      sep = "\n"
    )
  })
}

shinyApp(ui = ui, server = server)