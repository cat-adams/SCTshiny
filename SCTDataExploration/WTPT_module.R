# --- WTPT Data Loading Function ---
# This function is now removed from the module and lives in the main app.R file.


# --- White Tailed Ptarmigan Module UI ---

wtpt_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    h4("White Tailed Ptarmigan Analysis"),
    p("This module explores WTPT capture, telemetry, and encounter data."),
    
    # Accordion for filters
    accordion(
      open = TRUE,
      accordion_panel(
        "Filters",
        icon = bsicons::bs_icon("filter-circle"),
        layout_column_wrap(
          width = 1/3,
          # UI placeholders for filters - to be rendered on the server
          uiOutput(ns("plot_filter_ui")),
          uiOutput(ns("bird_id_filter_ui")),
          uiOutput(ns("year_filter_ui"))
        )
      )
    ),
    
    # Tabs for displaying data and plots
    navset_card_tab(
      id = ns("wtpt_tabs"),
      nav_panel("Map", card(card_header("Telemetry and Encounter Map"), leafletOutput(ns("wtpt_map"), height = "600px"))),
      nav_panel("Bird Summaries", card(card_header("Summary by Bird"), p("Bird summary table will be here."))),
      nav_panel("Raw Data", card(card_header("Combined Raw Data"), DT::dataTableOutput(ns("raw_data_table"))))
    )
  )
}

# --- White Tailed Ptarmigan Module Server ---

# The server function now accepts the WTPT data and co_counties as arguments
wtpt_server <- function(id, wtpt_data, co_counties) {
  moduleServer(id, function(input, output, session) {
    
    # --- Load and Process Data ---
    
    # The raw data is now passed in directly
    wtpt_raw_data <- reactive({
      wtpt_data
    })
    
    # Create a reactive expression to combine the tables into a single spatial dataframe
    combined_data <- reactive({
      req(wtpt_raw_data())
      
      # Extract tables
      birds <- wtpt_raw_data()$birds
      banding <- wtpt_raw_data()$banding
      encounters <- wtpt_raw_data()$encounters
      resights <- wtpt_raw_data()$resights
      telemetry <- wtpt_raw_data()$telemetry
      plots <- wtpt_raw_data()$plots
      
      # Prepare each location-based table
      banding_locs <- banding %>% select(BirdsID, Lat, Long) %>% mutate(Type = "Capture")
      encounters_locs <- encounters %>% select(BirdsID, Lat, Long) %>% mutate(Type = "Encounter")
      resights_locs <- resights %>% select(BirdsID, Lat, Long) %>% mutate(Type = "Resight")
      telemetry_locs <- telemetry %>% select(BirdsID, Lat, Long) %>% mutate(Type = "Telemetry")
      
      # Combine all location data
      all_locations <- bind_rows(banding_locs, encounters_locs, resights_locs, telemetry_locs) %>%
        filter(!is.na(Lat) & !is.na(Long)) # Remove records without coordinates
      
      # Join with bird and plot information
      all_locations %>%
        left_join(birds %>% select(BirdsID, StateBand, BirdName, PlotID), by = "BirdsID") %>%
        left_join(plots %>% select(PlotID, PlotName), by = "PlotID")
    })
    
    # --- Dynamic Filter UI ---
    
    # Render a filter for Plot Name
    output$plot_filter_ui <- renderUI({
      req(combined_data())
      choices <- c("All", unique(combined_data()$PlotName))
      selectInput(session$ns("plot_filter"), "Filter by Plot:", choices = choices, selected = "All", multiple = TRUE)
    })
    
    # --- Filtered Data for Map ---
    
    filtered_map_data <- reactive({
      req(combined_data(), input$plot_filter)
      
      data <- combined_data()
      
      # Apply plot filter
      if (!"All" %in% input$plot_filter) {
        data <- data %>% filter(PlotName %in% input$plot_filter)
      }
      
      return(data)
    })
    
    # --- Outputs ---
    
    # Render the interactive map
    output$wtpt_map <- renderLeaflet({
      map_data <- filtered_map_data()
      
      if (nrow(map_data) == 0) {
        return(
          leaflet() %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            addPolygons(data = co_counties, weight = 1, color = "#FFFFFF", fillOpacity = 0.1)
        )
      }
      
      # Create a color palette based on the type of location
      pal <- colorFactor(palette = c("red", "blue", "green", "orange"), domain = map_data$Type)
      
      leaflet(map_data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(data = co_counties, weight = 1, color = "#FFFFFF", fillOpacity = 0.1, label = ~name) %>%
        addCircleMarkers(
          lng = ~Long,
          lat = ~Lat,
          radius = 5,
          stroke = FALSE,
          fillOpacity = 0.7,
          fillColor = ~pal(Type),
          popup = ~paste(
            "<b>Bird ID:</b>", BirdsID, "<br>",
            "<b>Bird Name:</b>", BirdName, "<br>",
            "<b>State Band:</b>", StateBand, "<br>",
            "<b>Plot:</b>", PlotName, "<br>",
            "<b>Type:</b>", Type
          )
        ) %>%
        addLegend("bottomright", pal = pal, values = ~Type, title = "Location Type", opacity = 1)
    })
    
    # Display the combined data in a table
    output$raw_data_table <- DT::renderDataTable({
      req(combined_data())
      DT::datatable(combined_data(), filter = "top", options = list(pageLength = 15, scrollX = TRUE))
    })
    
  })
}
