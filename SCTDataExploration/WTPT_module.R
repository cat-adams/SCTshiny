# --- White Tailed Ptarmigan Module UI ---

wtpt_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    h4("White Tailed Ptarmigan"),
    p("This module explores WTPT capture, telemetry, and encounter data from 2007, 2012-2017"),
    
    # Accordion for filters
    accordion(
      open = FALSE,
      accordion_panel(
        "Filters & Map Configuration",
        icon = bsicons::bs_icon("filter-circle"),
        layout_column_wrap(
          width = 1/2,
          # UI placeholders for filters - to be rendered on the server
          uiOutput(ns("plot_filter_ui")),
          uiOutput(ns("bird_filter_ui")) 
        ),
        layout_column_wrap(
          width = 1/2,
          uiOutput(ns("year_filter_ui")),
          uiOutput(ns("sex_filter_ui")),
          uiOutput(ns("age_filter_ui")),
          uiOutput(ns("color_by_ui"))
        ),
        # This filter gets its own row
        uiOutput(ns("type_filter_ui"))
      )
    ),
    
    # Filter summary popover
    div(
      style = "margin-bottom: 20px; text-align: left;",
      bslib::popover(
        trigger = tagList(bsicons::bs_icon("info-circle-fill"), "Current Filters"),
        title = "Active Plot & Map Settings",
        verbatimTextOutput(ns("current_filters_summary"))
      )
    ),
    
    # Tabs for displaying data and plots
    navset_card_tab(
      id = ns("wtpt_tabs"),
      nav_panel("Map", card(card_header("Telemetry and Encounter Map"), leafletOutput(ns("wtpt_map"), height = "600px"))),
      # nav_panel("Bird Summaries", card(card_header("Summary by Bird"), DT::dataTableOutput(ns("bird_summary_table")))),
      nav_panel("Plot & Demographic Summary", 
                card(
                  card_header("Summary by Plot and Demographics"),
                  layout_sidebar(
                    sidebar = sidebar(
                      checkboxGroupInput(ns("demographic_group_by"), "Group By:", 
                                         choices = c("Plot" = "PlotName", "Year", "Sex", "Age", "Data Type" = "DataType"),
                                         selected = c("PlotName", "Year")),
                      radioButtons(ns("demographic_count_type"), "Count:",
                                   choices = c("All Observations" = "n", "Unique Birds" = "n_distinct"),
                                   selected = "n_distinct")
                    ),
                    DT::dataTableOutput(ns("demographic_summary_table"))
                  )
                )
      ),
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
    
    # Create a foundational reactive expression that prepares and joins all data
    prepared_data <- reactive({
      req(wtpt_raw_data())
      
      # Extract tables
      birds <- wtpt_raw_data()$birds
      banding <- wtpt_raw_data()$banding
      encounters <- wtpt_raw_data()$encounters
      resights <- wtpt_raw_data()$resights
      telemetry <- wtpt_raw_data()$telemetry
      plots <- wtpt_raw_data()$plots
      
      # Handle unidentified birds by replacing NA with a placeholder
      unidentified_placeholder <- 0
      
      # Prepare each location-based table, standardizing the date column
      banding_locs <- banding %>% 
        select(BirdsID, Lat, Long, ObservationDate = Combined_Date) %>% 
        mutate(DataType = "Capture")
      
      encounters_locs <- encounters %>% 
        select(BirdsID, Lat, Long, ObservationDate = Combined_Date) %>% 
        mutate(DataType = "Encounter")
      
      resights_locs <- resights %>% 
        mutate(
          BirdsID = tidyr::replace_na(BirdsID, unidentified_placeholder),
          # Construct date from Year, Month, Day columns
          ObservationDate = as.Date(paste(Year, Month, Day, sep="-"), format="%Y-%m-%d")
        ) %>% 
        select(BirdsID, Lat, Long, ObservationDate) %>% 
        mutate(DataType = "Resight")
      
      telemetry_locs <- telemetry %>% 
        select(BirdsID, Lat, Long, ObservationDate = combined_date) %>% 
        mutate(DataType = "Telemetry")
      
      # Combine all location data
      all_locations <- bind_rows(banding_locs, encounters_locs, resights_locs, telemetry_locs) %>%
        filter(!is.na(Lat) & !is.na(Long)) %>% # Remove records without coordinates
        filter(Lat >= 36.9 & Lat <= 41.1 & Long >= -109.1 & Long <= -102.0) %>%
        mutate(
          ObservationDate = as.Date(ObservationDate),
          Year = lubridate::year(ObservationDate)
        ) %>%
        filter(!is.na(Year)) # Ensure we only have rows with valid years
      
      # Get demographic info from banding table
      demographics <- banding %>%
        select(BirdsID, Sex, Age) %>%
        distinct(BirdsID, .keep_all = TRUE) # Get first record for each bird
      
      # Create a consistent bird label for filters and legends
      birds_labeled <- birds %>%
        mutate(
          StateBand = as.character(StateBand),
          BirdLabel = if_else(is.na(BirdName) | BirdName == "", StateBand, paste0(StateBand, " (", tools::toTitleCase(tolower(BirdName)), ")"))
        ) %>%
        left_join(demographics, by = "BirdsID") %>%
        mutate(
          Sex = as.character(Sex),
          Age = as.character(Age),
          # Standardize Sex column
          Sex = case_when(
            Sex %in% c("F", "Female", "FEMALE") ~ "Female",
            Sex %in% c("M", "Male", "MALE") ~ "Male",
            Sex %in% c("U", "UNK") ~ "Unk",
            TRUE ~ Sex
          )
        )
      
      # Add a row for unidentified birds to the main birds table for joining
      unidentified_bird_info <- tibble(BirdsID = unidentified_placeholder, StateBand = "N/A", BirdName = "Unidentified", PlotID = NA, BirdLabel = "Unidentified", Sex = "Unk", Age = "U")
      
      # Join with bird and plot information and select final columns to avoid blob issues
      all_locations %>%
        left_join(bind_rows(birds_labeled, unidentified_bird_info), by = "BirdsID") %>%
        left_join(plots %>% select(PlotID, PlotName), by = "PlotID") %>%
        select(BirdsID, BirdLabel, StateBand, BirdName, PlotName, DataType, ObservationDate, Year, Lat, Long, Sex, Age)
    })
    
    # --- Dynamic Filter UI ---
    
    output$plot_filter_ui <- renderUI({
      req(prepared_data())
      choices <- c("All", sort(unique(prepared_data()$PlotName)))
      selectInput(session$ns("plot_filter"), "Filter by Plot:", choices = choices, selected = "All", multiple = TRUE)
    })
    
    available_birds_reactive <- reactive({
      req(prepared_data(), input$plot_filter)
      
      if ("All" %in% input$plot_filter) {
        sort(unique(prepared_data()$BirdLabel))
      } else {
        prepared_data() %>%
          filter(PlotName %in% input$plot_filter) %>%
          pull(BirdLabel) %>%
          unique() %>%
          sort()
      }
    })
    
    output$bird_filter_ui <- renderUI({
      choices <- c("All", available_birds_reactive())
      selectInput(session$ns("bird_filter"), "Filter by Bird:", choices = choices, selected = "All", multiple = TRUE)
    })
    
    output$color_by_ui <- renderUI({
      selectInput(session$ns("color_by"), "Color Map Points By:", 
                  choices = c("Data Type" = "DataType", "Plot Name" = "PlotName", "Bird" = "BirdLabel", "Year" = "Year"))
    })
    
    output$type_filter_ui <- renderUI({
      req(prepared_data())
      choices <- unique(prepared_data()$DataType)
      checkboxGroupInput(session$ns("type_filter"), "Filter by Data Type:",
                         choices = choices, selected = choices, inline = TRUE)
    })
    
    output$year_filter_ui <- renderUI({
      req(prepared_data())
      choices <- sort(unique(prepared_data()$Year), decreasing = TRUE)
      selectInput(session$ns("year_filter"), "Filter by Year:", choices = choices, selected = choices, multiple = TRUE)
    })
    
    output$sex_filter_ui <- renderUI({
      req(prepared_data())
      choices <- c("All", sort(unique(prepared_data()$Sex)))
      selectInput(session$ns("sex_filter"), "Filter by Sex:", choices = choices, selected = "All", multiple = TRUE)
    })
    
    output$age_filter_ui <- renderUI({
      req(prepared_data())
      choices <- c("All", sort(unique(prepared_data()$Age)))
      selectInput(session$ns("age_filter"), "Filter by Age:", choices = choices, selected = "All", multiple = TRUE)
    })
    
    # This ensures the filters are rendered even if the accordion is closed
    outputOptions(output, "plot_filter_ui", suspendWhenHidden = FALSE)
    outputOptions(output, "bird_filter_ui", suspendWhenHidden = FALSE)
    outputOptions(output, "color_by_ui", suspendWhenHidden = FALSE)
    outputOptions(output, "type_filter_ui", suspendWhenHidden = FALSE)
    outputOptions(output, "year_filter_ui", suspendWhenHidden = FALSE)
    outputOptions(output, "sex_filter_ui", suspendWhenHidden = FALSE)
    outputOptions(output, "age_filter_ui", suspendWhenHidden = FALSE)
    
    # --- Filtered Data ---
    
    filtered_data <- reactive({
      req(prepared_data(), input$plot_filter, input$type_filter, input$year_filter, input$bird_filter, input$sex_filter, input$age_filter)
      
      data <- prepared_data()
      
      if (!"All" %in% input$plot_filter) {
        data <- data %>% filter(PlotName %in% input$plot_filter)
      }
      if (!"All" %in% input$bird_filter) {
        data <- data %>% filter(BirdLabel %in% input$bird_filter)
      }
      if (!"All" %in% input$sex_filter) {
        data <- data %>% filter(Sex %in% input$sex_filter)
      }
      if (!"All" %in% input$age_filter) {
        data <- data %>% filter(Age %in% input$age_filter)
      }
      data <- data %>% filter(DataType %in% input$type_filter)
      data <- data %>% filter(Year %in% input$year_filter)
      
      return(data)
    })
    
    # --- Outputs ---
    
    output$current_filters_summary <- renderPrint({
      req(input$plot_filter, input$bird_filter, input$year_filter, input$type_filter, input$sex_filter, input$age_filter)
      format_selection <- function(selection) if ("All" %in% selection) "All" else paste(selection, collapse = ", ")
      cat("Plot:", format_selection(input$plot_filter), "\n",
          "Bird:", format_selection(input$bird_filter), "\n",
          "Year:", format_selection(input$year_filter), "\n",
          "Data Type:", format_selection(input$type_filter), "\n",
          "Sex:", format_selection(input$sex_filter), "\n",
          "Age:", format_selection(input$age_filter))
    })
    
    output$wtpt_map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        fitBounds(lng1 = -109.0602, lat1 = 36.9924, lng2 = -102.0415, lat2 = 41.0034) %>%
        addPolygons(data = co_counties, weight = 1, color = "#FFFFFF", fillOpacity = 0.1)
    })
    
    observe({
      req(input$year_filter) # This prevents the map from drawing before the year filter is ready
      map_data <- filtered_data()
      color_variable <- input$color_by
      
      if(nrow(map_data) == 0) {
        leafletProxy("wtpt_map") %>% clearMarkers() %>% clearControls()
        return()
      }
      req(color_variable)
      
      pal_domain <- unique(map_data[[color_variable]])
      
      pal <- if (color_variable == "DataType") {
        colorFactor(palette = c("red", "blue", "green", "orange"), domain = pal_domain)
      } else if (color_variable == "Year") {
        colorFactor(palette = "viridis", domain = pal_domain) # Changed to colorFactor for discrete years
      } else {
        colorFactor(palette = "viridis", domain = pal_domain)
      }
      
      leafletProxy("wtpt_map", data = map_data) %>%
        clearMarkers() %>%
        clearControls() %>%
        addCircleMarkers(
          lng = ~Long, lat = ~Lat, radius = 5, stroke = FALSE, fillOpacity = 0.7,
          fillColor = ~pal(get(color_variable)),
          popup = ~paste(
            "<b>Bird:</b>", BirdLabel, "<br>",
            "<b>Plot:</b>", PlotName, "<br>", 
            "<b>Type:</b>", DataType, "<br>",
            "<b>Date:</b>", as.Date(ObservationDate)
          )
        ) %>%
        addLegend("bottomright", pal = pal, values = ~get(color_variable), 
                  title = gsub("_", " ", tools::toTitleCase(color_variable)), opacity = 1)
    })
    
    # output$bird_summary_table <- DT::renderDataTable({
    #   summary_data <- filtered_data() %>%
    #     group_by(StateBand, BirdName, PlotName, BirdLabel) %>%
    #     summarise(
    #       Encounters = sum(DataType == "Encounter"),
    #       Resights = sum(DataType == "Resight"),
    #       Telemetry = sum(DataType == "Telemetry"),
    #       Captures = sum(DataType == "Capture"),
    #       .groups = 'drop'
    #     ) %>%
    #     select(StateBand, BirdName, PlotName, Captures, Encounters, Resights, Telemetry)
    #   
    #   DT::datatable(summary_data, filter = "top", options = list(pageLength = 10, scrollX = TRUE))
    # })
    # 
    output$demographic_summary_table <- DT::renderDataTable({
      req(input$demographic_group_by)
      
      # Use a version of the data that ignores the individual bird filter
      summary_data <- prepared_data() %>%
        filter(
          ("All" %in% input$plot_filter | PlotName %in% input$plot_filter),
          Year %in% input$year_filter,
          DataType %in% input$type_filter,
          ("All" %in% input$sex_filter | Sex %in% input$sex_filter),
          ("All" %in% input$age_filter | Age %in% input$age_filter)
        ) 
      
      if (input$demographic_count_type == "n") {
        summary_data <- summary_data %>%
          group_by(!!!syms(input$demographic_group_by)) %>%
          summarise(Count = n(), .groups = 'drop')
      } else {
        summary_data <- summary_data %>%
          group_by(!!!syms(input$demographic_group_by)) %>%
          summarise(Count = n_distinct(BirdsID), .groups = 'drop')
      }
      
      DT::datatable(summary_data, filter = "top", options = list(pageLength = 10, scrollX = TRUE))
    })
    
    output$raw_data_table <- DT::renderDataTable({
      DT::datatable(filtered_data(), filter = "top", options = list(pageLength = 15, scrollX = TRUE))
    })
    
  })
}
