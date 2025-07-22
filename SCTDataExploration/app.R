# Load necessary libraries
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(DBI)
library(odbc)
library(leaflet)
library(geojsonio)

# --- Database Connection and Data Loading ---

# Database connection function
get_detections_data <- function() {
  # This is a placeholder for your actual database connection.
  # When running locally, this will fail unless you have the specified DSN.
  # For demonstration, we will use a dummy data function if the connection fails.
  tryCatch({
    con <- DBI::dbConnect(
      odbc::odbc(),
      Driver = "ODBC Driver 17 for SQL Server",
      Server = "DNRCPWPF53ENDQ",
      Database = "SCT",
      trusted_connection = "yes"
    )
    on.exit(DBI::dbDisconnect(con))
    
    # Combine tables from the database
    rbind(
      DBI::dbReadTable(con, DBI::Id(schema = "BLRA", table = "v_Detections_NatDB")),
      DBI::dbReadTable(con, DBI::Id(schema = "BLRA", table = "V_Detections_OLD"))
    )
  }, error = function(e) {
    # If the database connection fails, create and use dummy data instead.
    # This ensures the app is always runnable for demonstration purposes.
    message("Database connection failed. Using dummy data. Error: ", e$message)
    
    return(data)
  })
}
# Load Colorado counties data
co_counties <- geojsonio::geojson_read("https://raw.githubusercontent.com/johan/world.geo.json/master/countries/USA/CO.geo.json", what = "sp")


theme_custom<-theme(     # Axis lines - only x and y
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
  panel.spacing = unit(1, "lines"))

# Load and preprocess data at app startup
Detections <- get_detections_data()

# Ensure date columns are properly formatted and rename columns
Detections$SurDate <- as.Date(Detections$SurDate)
Detections <- Detections %>%
  mutate(MoonPhase = factor(MoonPhase,
                            levels = c("NEW", "Waxing crescent", "First quarter", "Waxing gibbous",
                                       "FULL", "Waning gibbous", "Last quarter", "Waning crescent"))) %>%
  rename(
    "Time of Day" = "TimeSegment",
    "Property" = "Property.Identifier",
    "Site Point" = "SitePointID",
    "Survey Date" = "SurDate",
    "Moon Phase" = "MoonPhase",
    "Lunar Age" = "LunarAge"
  )

# Define the UI for the Eastern Black Rail section. This will be rendered dynamically.
eastern_black_rail_ui <- tagList(
  accordion(
    open = TRUE,
    accordion_panel(
      "Filters & Plot Configuration",
      icon = bsicons::bs_icon("filter-circle"),
      layout_column_wrap(
        width = 1/3,
        selectInput("x_var", "X-axis Variable:",
                    choices = c("Year", "Month", "Site Point", "Property", "Moon Phase", "Time of Day"),
                    selected = "Year"),
        selectInput("color_var", "Color Variable:",
                    choices = c("Property", "Site Point", "Species", "Moon Phase", "Year", "Month", "Time of Day"),
                    selected = "Property"),
        selectInput("facet_var", "Facet Variable:",
                    choices = c("None", "Species", "Property", "Site Point", "Year", "Month", "Moon Phase", "Time of Day"),
                    selected = "Species")
      ),
      hr(),
      layout_column_wrap(
        width = 1/3,
        selectInput("property_filter", "Filter by Property:",
                    choices = c("All", unique(Detections$Property)), selected = "All", multiple = TRUE),
        selectInput("site_point_filter", "Filter by Site Point:",
                    choices = c("All", unique(Detections$`Site Point`)), selected = "All", multiple = TRUE),
        selectInput("species_filter", "Filter by Species:",
                    choices = c("All", unique(Detections$Species)), selected = "All", multiple = TRUE)
      ),
      layout_column_wrap(
        width = 1/2,
        sliderInput("year_range", "Year Range:",
                    min = min(Detections$Year, na.rm = TRUE), max = max(Detections$Year, na.rm = TRUE),
                    value = c(min(Detections$Year, na.rm = TRUE), max(Detections$Year, na.rm = TRUE)),
                    step = 1, sep = ""),
        selectInput("time_segment_filter", "Filter by Time of Day:",
                    choices = c("All", unique(Detections$`Time of Day`)), selected = "All", multiple = TRUE)
      ),
      # Added checkbox for map point size
      checkboxInput("size_by_count", "Size map points by detection count", value = FALSE)
    )
  ),
  uiOutput("filter_summary_ui"),
  navset_card_tab(
    id = "main_tabs",
    nav_panel("Main Plot", card(card_header("Species Detection Analysis"), plotlyOutput("main_plot", height = "600px"))),
    nav_panel("Statistics",
              card(card_header("Detection Counts by Variable"), plotlyOutput("stats_plot1", height = "500px")),
              card(card_header("Species Diversity"), plotlyOutput("stats_plot2", height = "500px")),
              card(card_header("Summary Statistics"), verbatimTextOutput("summary_stats"))),
    nav_panel("Map", card(card_header("Site Point Map"), leafletOutput("site_map", height = "600px"))),
    nav_panel("Summary Table", card(card_header("Detection Summary"), DT::dataTableOutput("summary_table"))),
    nav_panel("Raw Data", card(card_header("Filtered Detection Data"), DT::dataTableOutput("raw_data_table")))
    
  )
)

# Main UI with a left-hand sidebar for navigation
ui <- page_fluid(
  title = "Wildlife Survey Data Explorer",
  theme = bslib::bs_theme(bootswatch = "darkly"), #sandstone is another nice option
  layout_sidebar(
    sidebar = sidebar(
      title = "Species Modules",
      # This creates the clickable left navigation
      navset_pill_list(
        id = "species_nav", # An ID to track the selected tab
        nav_panel(title = "Home", value = "home"),
        nav_panel(title = "Eastern Black Rail", value = "eabr"),
        nav_panel(title = "White Tailed Ptarmigan", value = "wtp")
      )
    ),
    # The main panel's content is now dynamic based on the sidebar selection
    uiOutput("main_content_ui")
  )
)


# --- Server Logic ---

server <- function(input, output, session) {
  # Dynamically render the main content UI based on the sidebar navigation
  output$main_content_ui <- renderUI({
    req(input$species_nav)
    switch(input$species_nav,
           "home" = card(h4("Welcome!"), p("Please select a species module from the sidebar to begin analysis.")),
           "eabr" = eastern_black_rail_ui,
           "wtp" = card(h4("White Tailed Ptarmigan"), p("Analysis module for White Tailed Ptarmigan is under construction."))
    )
  })
  
  # Render the filter summary UI element
  output$filter_summary_ui <- renderUI({
    req(input$species_nav == "eabr")
    
    # Helper function to format filter text
    format_filter <- function(label, values) {
      if (!is.null(values) && !("All" %in% values) && length(values) > 0) {
        paste0("<b>", label, ":</b> ", paste(values, collapse = ", "))
      } else {
        NULL
      }
    }
    
    # Build the summary text parts
    summary_parts <- c(
      "<u>Plot Config</u>",
      paste0("<b>X-axis:</b> ", input$x_var),
      paste0("<b>Color:</b> ", input$color_var),
      if (input$facet_var != "None") paste0("<b>Facet:</b> ", input$facet_var) else NULL,
      "<hr style='margin: 5px 0;'>", # Separator
      "<u>Filters</u>",
      format_filter("Property", input$property_filter),
      format_filter("Site Point", input$site_point_filter),
      format_filter("Species", input$species_filter),
      format_filter("Time of Day", input$time_segment_filter),
      paste0("<b>Year Range:</b> ", input$year_range[1], " - ", input$year_range[2])
    )
    
    # Remove NULLs and combine
    summary_text <- paste(na.omit(summary_parts), collapse = "<br>")
    
    # Create the tooltip UI
    div(
      style = "text-align: left; padding: 5px; margin-bottom: 10px;",
      tooltip(
        span(bsicons::bs_icon("info-circle-fill"), " Current Settings"),
        HTML(summary_text),
        placement = "left"
      )
    )
  })
  
  # Dynamic site point filter based on Property selection
  observe({
    req(input$species_nav == "eabr", input$property_filter) # Only run for the EABR module
    if ("All" %in% input$property_filter || is.null(input$property_filter) || length(input$property_filter) == 0) {
      available_sites <- unique(Detections$`Site Point`)
    } else {
      filtered_data <- Detections %>% filter(Property %in% input$property_filter)
      available_sites <- unique(filtered_data$`Site Point`)
    }
    current_selection <- input$site_point_filter
    if (is.null(current_selection)) current_selection <- "All"
    valid_selection <- intersect(current_selection, c("All", available_sites))
    if (length(valid_selection) == 0) valid_selection <- "All"
    updateSelectInput(session, "site_point_filter", choices = c("All", sort(available_sites)), selected = valid_selection)
  })
  
  
  # Dynamic site point filter based on Property selection
  observe({
    req(input$property_filter) # Ensure this input exists before proceeding
    if ("All" %in% input$property_filter || is.null(input$property_filter) || length(input$property_filter) == 0) {
      available_sites <- unique(Detections$`Site Point`)
    } else {
      filtered_data <- Detections %>% filter(Property %in% input$property_filter)
      available_sites <- unique(filtered_data$`Site Point`)
    }
    current_selection <- input$site_point_filter
    if (is.null(current_selection)) current_selection <- "All"
    valid_selection <- intersect(current_selection, c("All", available_sites))
    if (length(valid_selection) == 0) valid_selection <- "All"
    updateSelectInput(session, "site_point_filter", choices = c("All", sort(available_sites)), selected = valid_selection)
  })
  
  # Observers to handle "All" selection logic for multi-select inputs
  observe_all_selection <- function(input_id) {
    observe({
      req(input[[input_id]])
      if (length(input[[input_id]]) > 1 && "All" %in% input[[input_id]]) {
        other_selections <- setdiff(input[[input_id]], "All")
        updateSelectInput(session, input_id, selected = other_selections)
      }
    }) %>% bindEvent(input[[input_id]])
  }
  observe_all_selection("property_filter")
  observe_all_selection("site_point_filter")
  observe_all_selection("species_filter")
  observe_all_selection("time_segment_filter")
  
  # Reactive data filtering
  filtered_data <- reactive({
    req(input$year_range) # Require filters to be present
    data <- Detections %>%
      mutate(Species = ifelse(Species %in% c("na", "uk"), "Unknown", Species))
    
    if (!"All" %in% input$property_filter && !is.null(input$property_filter) && length(input$property_filter) > 0) {
      data <- data %>% filter(Property %in% input$property_filter)
    }
    if (!"All" %in% input$site_point_filter && !is.null(input$site_point_filter) && length(input$site_point_filter) > 0) {
      data <- data %>% filter(`Site Point` %in% input$site_point_filter)
    }
    if (!"All" %in% input$species_filter && !is.null(input$species_filter) && length(input$species_filter) > 0) {
      data <- data %>% filter(Species %in% input$species_filter)
    }
    data <- data %>% filter(Year >= input$year_range[1] & Year <= input$year_range[2])
    if (!"All" %in% input$time_segment_filter && !is.null(input$time_segment_filter) && length(input$time_segment_filter) > 0) {
      data <- data %>% filter(`Time of Day` %in% input$time_segment_filter)
    }
    return(data)
  })
  
  # Reactive data specifically for the map
  map_data <- reactive({
    req(input$species_nav == "eabr")
    data <- filtered_data() # Use the fully filtered data
    
    if (nrow(data) == 0) {
      return(data.frame()) # Return empty frame if no data
    }
    
    # Group by site point and get the most common value for the color variable
    data %>%
      group_by(Property, `Site Point`, Lat, Long) %>%
      summarise(
        Count = n(),
        # Get the most frequent value for the chosen color variable at that site
        ColorValue = names(which.max(table(.data[[input$color_var]]))),
        .groups = 'drop'
      )
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
      theme_custom
    
    # Convert to plotly
    ggplotly(p) %>%
      layout(
        hovermode = "closest",
        legend = list(orientation = "h", y = -0.2)
      )
  })
  
  # Map output
  output$site_map <- renderLeaflet({
    req(input$species_nav == "eabr")
    data <- map_data()
    
    if (nrow(data) == 0) {
      return(
        leaflet() %>%
          addProviderTiles(providers$CartoDB.Positron) %>%
          addPolygons(data = co_counties, weight = 1, color = "#FFFFFF", fillOpacity = 0.1, label = ~name)
      )
    }
    
    # Create a color palette based on the 'ColorValue' column
    pal <- colorFactor(palette = "viridis", domain = data$ColorValue)
    
    # Conditionally set radius based on checkbox
    radius <- if (input$size_by_count) {
      sqrt(data$Count) * 3 
    } else {
      5
    }
    
    leaflet(data) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = co_counties, weight = 1, color = "#FFFFFF", fillOpacity = 0.1, label = ~name) %>%
      addCircleMarkers(
        lng = ~Long,
        lat = ~Lat,
        radius = radius,
        stroke = FALSE,
        fillOpacity = 0.7,
        fillColor = ~pal(ColorValue), # Use the palette to color points
        popup = ~paste(
          "<b>Property:</b>", Property, "<br>", 
          "<b>Site Point:</b>", `Site Point`, "<br>",
          "<b>Detections:</b>", Count
        )
      ) %>%
      # Add a legend for the colors
      addLegend("bottomright", pal = pal, values = ~ColorValue, title = input$color_var, opacity = 1)
  })
  
  
  # Summary table
  output$summary_table <- DT::renderDataTable({
    DT::datatable(plot_data(), filter = "top", options = list(pageLength = 15, scrollX = TRUE, order = list(list(ncol(plot_data()) - 1, 'desc'))))
  })
  
  # Raw data table
  output$raw_data_table <- DT::renderDataTable({
    DT::datatable(filtered_data(), filter = "top", options = list(pageLength = 15, scrollX = TRUE))
  })
  
  # Generic function for statistics plots
  render_stats_plot <- function(y_var, y_lab) {
    renderPlotly({
      req(nrow(filtered_data()) > 0)
      data <- filtered_data()
      stats_data <- data %>%
        group_by(.data[[input$color_var]]) %>% # Use .data pronoun
        summarise(Value = {{y_var}}, .groups = 'drop')
      
      p <- ggplot(stats_data, aes(x = .data[[input$color_var]], y = Value, fill = .data[[input$color_var]])) +
        geom_col() +
        theme_minimal() +
        labs(title = paste(y_lab, "by", input$color_var), x = input$color_var, y = y_lab) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")+
        theme_custom
      
      if (input$color_var %in% c("Year", "Month")) {
        p <- p + scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1))
      }
      ggplotly(p)
    })
  }
  
  output$stats_plot1 <- render_stats_plot(n(), "Total Detections")
  output$stats_plot2 <- render_stats_plot(n_distinct(Species), "Number of Species")
  
  # Summary statistics
  output$summary_stats <- renderText({
    req(nrow(filtered_data()) > 0)
    data <- filtered_data()
    paste(
      "FILTERED DATA SUMMARY:",
      paste("Total Detections:", nrow(data)),
      paste("Unique Species:", n_distinct(data$Species)),
      paste("Unique Properties:", n_distinct(data$Property)),
      paste("Unique Site Points:", n_distinct(data$`Site Point`)),
      paste("Date Range:", min(data$`Survey Date`, na.rm = TRUE), "to", max(data$`Survey Date`, na.rm = TRUE)),
      "",
      "TOP 10 SPECIES:",
      paste(capture.output(data %>% count(Species, sort = TRUE) %>% head(10) %>% print()), collapse = "\n"),
      sep = "\n"
    )
  })
}

shinyApp(ui = ui, server = server)
