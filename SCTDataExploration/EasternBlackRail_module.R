# --- Eastern Black Rail Module UI ---

ebr_ui <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
      h4("Eastern Black Rail"),
      p("This module explores Eastern Black Rail (BLRA) broadcast survey data from 2018-current in Eastern Colorado"),
      
    accordion(
      open = FALSE,
      accordion_panel(
        "Filters & Plot Configuration",
        icon = bsicons::bs_icon("filter-circle"),
        layout_column_wrap(
          width = 1/3,
          selectInput(ns("x_var"), "X-axis Variable:",
                      choices = c("Year", "Month", "Site Point", "Property", "Moon Phase", "Time of Day"),
                      selected = "Year"),
          selectInput(ns("color_var"), "Color Variable:",
                      choices = c("Property", "Site Point", "Species", "Moon Phase", "Year", "Month", "Time of Day"),
                      selected = "Site Point"),
          selectInput(ns("facet_var"), "Facet Variable:",
                      choices = c("None", "Species", "Property", "Site Point", "Year", "Month", "Moon Phase", "Time of Day"),
                      selected = "Property")
        ),
        hr(),
        layout_column_wrap(
          width = 1/3,
          selectInput(ns("property_filter"), "Filter by Property:",
                      choices = c("All"), selected = "All", multiple = TRUE),
          selectInput(ns("site_point_filter"), "Filter by Site Point:",
                      choices = c("All"), selected = "All", multiple = TRUE),
          selectInput(ns("species_filter"), "Filter by Species:",
                      choices = c("All"), selected = "BLRA", multiple = TRUE)
        ),
        layout_column_wrap(
          width = 1/2,
          # The slider is now a dynamic UI element, rendered on the server
          uiOutput(ns("year_range_ui")),
          selectInput(ns("time_segment_filter"), "Filter by Time of Day:",
                      choices = c("All"), selected = "All", multiple = TRUE)
        ),
        checkboxInput(ns("size_by_count"), "Size map points by detection count", value = TRUE),
        checkboxInput(ns("toggle_legend"), "Show Plot Legends", value = FALSE)
      )
    ),
    div(
      style = "margin-bottom: 20px; text-align: left;",
      bslib::popover(
        trigger = tagList(bsicons::bs_icon("info-circle-fill"), "Current Filters"),
        title = "Active Plot & Map Settings",
        verbatimTextOutput(ns("current_filters_summary"))
      )
    ),
    navset_card_tab(
      id = ns("main_tabs"),
      nav_panel("Main Plot", 
                card(
                  card_header("Species Detection Analysis",
                              actionButton(ns("popout_main_plot"), "Enlarge", icon = icon("expand"), class = "btn-sm float-end")),
                  withSpinner(plotlyOutput(ns("main_plot"), height = "600px"))
                )),
      nav_panel("Statistics",
                layout_column_wrap(
                  width = 1/2,
                  card(card_header("Detection Counts by Variable",
                                   actionButton(ns("popout_stats1"), "Enlarge", icon = icon("expand"), class = "btn-sm float-end")), 
                       withSpinner(plotlyOutput(ns("stats_plot1"), height = "350px"))),
                  card(
                    card_header("Survey & Broadcast Counts",
                                actionButton(ns("popout_stats2"), "Enlarge", icon = icon("expand"), class = "btn-sm float-end")), 
                    radioButtons(ns("survey_broadcast_toggle"), "View:", 
                                 choices = c("Surveys", "Broadcasts"), 
                                 selected = "Surveys", inline = TRUE),
                    withSpinner(plotlyOutput(ns("stats_plot2"), height = "350px"))
                  )
                ),
                card(card_header("Summary Statistics"), verbatimTextOutput(ns("summary_stats")))),
      nav_panel("Map", card(card_header("Site Point Map"), withSpinner(leafletOutput(ns("site_map"), height = "600px")))),
      nav_panel("Summary Table", card(card_header("Detection Summary"), withSpinner(DT::dataTableOutput(ns("summary_table"))))),
      nav_panel("Raw Data", card(card_header("Filtered Detection Data"), withSpinner(DT::dataTableOutput(ns("raw_data_table")))))
    )
  )
}

# --- Eastern Black Rail Module Server ---

ebr_server <- function(id, Detections, co_counties, theme_custom) {
  moduleServer(id, function(input, output, session) {
    
    # --- Dynamic UI Rendering ---
    
    # This renders the year slider only after the correct range is known
    output$year_range_ui <- renderUI({
      req(Detections) # Ensure data is available
      
      min_year <- min(Detections$Year, na.rm = TRUE)
      max_year <- max(Detections$Year, na.rm = TRUE)
      
      sliderInput(session$ns("year_range"), "Year Range:",
                  min = min_year, 
                  max = max_year,
                  value = c(min_year, max_year),
                  step = 1, 
                  sep = "")
    })
    
    # This line prevents the dynamic UI from being suspended when hidden
    outputOptions(output, "year_range_ui", suspendWhenHidden = FALSE)
    
    # --- Dynamic Filter Updates ---
    
    # This observer populates the other filter choices ONCE, after the year slider is created.
    observeEvent(input$year_range, {
      req(Detections)
      property_choices <- c("All", unique(Detections$Property))
      site_point_choices <- c("All", unique(Detections$`Site Point`))
      species_choices <- c("All", unique(Detections$Species))
      time_segment_choices <- c("All", unique(Detections$`Time of Day`))
      
      updateSelectInput(session, "property_filter", choices = property_choices, selected = "All")
      updateSelectInput(session, "site_point_filter", choices = site_point_choices, selected = "All")
      updateSelectInput(session, "species_filter", choices = species_choices, selected = "BLRA")
      updateSelectInput(session, "time_segment_filter", choices = time_segment_choices, selected = "All")
    }, once = TRUE) # This is crucial: it ensures this only runs one time.
    
    observeEvent(input$property_filter, {
      req(input$year_range) 
      
      if (is.null(input$property_filter) || "All" %in% input$property_filter) {
        available_sites <- unique(Detections$`Site Point`)
      } else {
        available_sites <- Detections %>%
          filter(Property %in% input$property_filter) %>%
          pull(`Site Point`) %>%
          unique()
      }
      
      current_selection <- isolate(input$site_point_filter)
      valid_selection <- intersect(current_selection, available_sites)
      
      if (length(valid_selection) == 0) {
        valid_selection <- "All"
      }
      
      updateSelectInput(session, "site_point_filter",
                        choices = c("All", available_sites),
                        selected = valid_selection)
      
    }, ignoreNULL = FALSE, ignoreInit = TRUE)
    
    # --- Reactive Data Expressions ---
    
    # Reactive data filtering (for plots and tables)
    filtered_data <- reactive({
      req(input$year_range)
      
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
    
    # Reactive data that IGNORES the species filter 
    filtered_data_for_surveys <- reactive({
      req(input$year_range)
      data <- Detections
      
      if (!"All" %in% input$property_filter && !is.null(input$property_filter) && length(input$property_filter) > 0) {
        data <- data %>% filter(Property %in% input$property_filter)
      }
      if (!"All" %in% input$site_point_filter && !is.null(input$site_point_filter) && length(input$site_point_filter) > 0) {
        data <- data %>% filter(`Site Point` %in% input$site_point_filter)
      }
      data <- data %>% filter(Year >= input$year_range[1] & Year <= input$year_range[2])
      if (!"All" %in% input$time_segment_filter && !is.null(input$time_segment_filter) && length(input$time_segment_filter) > 0) {
        data <- data %>% filter(`Time of Day` %in% input$time_segment_filter)
      }
      return(data)
    })
    
    # Reactive data specifically for the map
    map_data <- reactive({
      data <- filtered_data()
      if (nrow(data) == 0) return(data.frame())
      
      data %>%
        group_by(Property, `Site Point`, Lat, Long) %>%
        summarise(
          Count = n(),
          ColorValue = names(which.max(table(.data[[input$color_var]]))),
          .groups = 'drop'
        )
    })
    
    # Reactive for plot data summary
    plot_data <- reactive({
      req(input$x_var, input$color_var, input$facet_var)
      data <- filtered_data()
      group_vars <- unique(c(input$x_var, input$color_var, if (input$facet_var != "None") input$facet_var))
      group_syms <- syms(group_vars)
      data %>%
        group_by(!!!group_syms) %>%
        summarise(Count = n(), .groups = 'drop')
    })
    
    # --- Reactive ggplot objects ---
    
    main_ggplot_obj <- reactive({
      req(nrow(plot_data()) > 0)
      data <- plot_data()
      
      p <- ggplot(data)
      
      if (input$x_var %in% c("Year", "Month")) {
        p <- p + geom_line(aes(x = !!sym(input$x_var), y = Count, color = !!sym(input$color_var), group = !!sym(input$color_var)), size = 1) +
          geom_point(aes(x = !!sym(input$x_var), y = Count, color = !!sym(input$color_var), group = !!sym(input$color_var)), size = 2)
      } else {
        p <- p + geom_col(aes(x = !!sym(input$x_var), y = Count, fill = !!sym(input$color_var)), position = "dodge")
      }
      
      if (input$facet_var != "None") {
        p <- p + facet_wrap(as.formula(paste("~", "`", input$facet_var, "`", sep = "")), scales = "free")
      }
      
      if (input$x_var %in% c("Year", "Month")) {
        p <- p + scale_x_continuous(
          breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1),
          labels = function(x) as.integer(x)
        )
      }
      
      p <- p + theme_void() +
        labs(title = paste("Species Detection Counts by", input$x_var), x = input$x_var, y = "Count", color = input$color_var, fill = input$color_var) +
        theme_custom + theme(axis.line = element_line()) +
        scale_y_continuous(breaks = function(x) unique(floor(pretty(x))), limits = c(0, max(data$Count)))
      
      return(p)
    })
    
    create_stats_ggplot <- function(y_var, y_lab) {
      reactive({
        req(nrow(filtered_data()) > 0)
        data <- filtered_data()
        stats_data <- data %>% group_by(.data[[input$color_var]]) %>% summarise(Value = {{y_var}}, .groups = 'drop')
        
        p <- ggplot(stats_data, aes(x = .data[[input$color_var]], y = Value, fill = .data[[input$color_var]])) +
          geom_col() + theme_minimal() +
          labs(title = paste(y_lab, "by", input$color_var), x = input$color_var, y = y_lab) +
          theme_custom + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
        
        if (input$color_var %in% c("Year", "Month")) {
          p <- p + scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1))
        }
        return(p)
      })
    }
    
    stats1_ggplot_obj <- create_stats_ggplot(n(), "Total Detections")
    
    # --- Reactive ggplot object for Survey/Broadcast plot ---
    survey_broadcast_plot_obj <- reactive({
      req(nrow(filtered_data_for_surveys()) > 0, input$survey_broadcast_toggle)
      
      data <- filtered_data_for_surveys()
      
      # Determine which column to count and set the label
      if (input$survey_broadcast_toggle == "Surveys") {
        y_var <- sym("SurveyID")
        y_lab <- "Number of Surveys"
      } else {
        y_var <- sym("SitePointBroadcastID")
        y_lab <- "Number of Broadcasts"
      }
      
      stats_data <- data %>%
        group_by(.data[[input$color_var]]) %>%
        summarise(Value = n_distinct(!!y_var), .groups = 'drop')
      
      p <- ggplot(stats_data, aes(x = .data[[input$color_var]], y = Value, fill = .data[[input$color_var]])) +
        geom_col() +
        theme_minimal() +
        labs(title = paste(y_lab, "by", input$color_var), x = input$color_var, y = y_lab) +
        theme_custom + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
      
      if (input$color_var %in% c("Year", "Month")) {
        p <- p + scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = 1))
      }
      return(p)
    })
    
    # --- Outputs ---
    
    output$current_filters_summary <- renderPrint({
      req(input$year_range)
      format_selection <- function(selection) if ("All" %in% selection || is.null(selection)) "All" else paste(selection, collapse = ", ")
      cat("Property:", format_selection(input$property_filter), "\n",
          "Site Point:", format_selection(input$site_point_filter), "\n",
          "Species:", format_selection(input$species_filter), "\n",
          "Time of Day:", format_selection(input$time_segment_filter), "\n",
          "Year Range:", paste(input$year_range, collapse = " - "))
    })
    
    output$main_plot <- renderPlotly({
      p <- main_ggplot_obj(); req(p)
      ggplotly(p) %>% layout(hovermode = "closest", legend = list(orientation = "h", y = -0.2), showlegend = isTRUE(input$toggle_legend))
    })
    
    output$stats_plot1 <- renderPlotly({
      p <- stats1_ggplot_obj(); req(p)
      ggplotly(p) %>% layout(showlegend = isTRUE(input$toggle_legend))
    })
    
    # Update stats_plot2 to use the new reactive plot object
    output$stats_plot2 <- renderPlotly({
      p <- survey_broadcast_plot_obj(); req(p)
      ggplotly(p) %>% layout(showlegend = FALSE) # Legends are redundant for this plot
    })
    
    output$site_map <- renderLeaflet({
      data <- map_data()
      if (nrow(data) == 0) {
        return(leaflet() %>% addProviderTiles(providers$CartoDB.Positron) %>% addPolygons(data = co_counties, weight = 1, color = "#FFFFFF", fillOpacity = 0.1, label = ~name))
      }
      pal <- colorFactor(palette = "viridis", domain = data$ColorValue)
      radius <- if (input$size_by_count) sqrt(data$Count) * 3 else 5
      leaflet(data) %>% addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(data = co_counties, weight = 1, color = "#FFFFFF", fillOpacity = 0.1, label = ~name) %>%
        addCircleMarkers(lng = ~Long, lat = ~Lat, radius = radius, stroke = FALSE, fillOpacity = 0.7, fillColor = ~pal(ColorValue),
                         popup = ~paste("<b>Property:</b>", Property, "<br>", "<b>Site Point:</b>", `Site Point`, "<br>", "<b>Detections:</b>", Count)) %>%
        addLegend("bottomright", pal = pal, values = ~ColorValue, title = input$color_var, opacity = 1)
    })
    
    output$summary_table <- DT::renderDataTable({
      DT::datatable(plot_data(), filter = "top", options = list(pageLength = 15, scrollX = TRUE, order = list(list(ncol(plot_data()) - 1, 'desc'))))
    })
    
    output$raw_data_table <- DT::renderDataTable({
      DT::datatable(filtered_data(), filter = "top", options = list(pageLength = 15, scrollX = TRUE))
    })
    
    output$summary_stats <- renderText({
      req(nrow(filtered_data()) > 0); data <- filtered_data()
      paste("FILTERED DATA SUMMARY:", paste("Total Detections:", nrow(data)), paste("Unique Species:", n_distinct(data$Species)),
            paste("Unique Properties:", n_distinct(data$Property)), paste("Unique Site Points:", n_distinct(data$`Site Point`)),
            paste("Date Range:", min(data$`Survey Date`, na.rm = TRUE), "to", max(data$`Survey Date`, na.rm = TRUE)), "",
            "TOP 10 SPECIES:", paste(capture.output(data %>% count(Species, sort = TRUE) %>% head(10) %>% print()), collapse = "\n"),
            sep = "\n")
    })
    
    # --- Modal Dialog Logic ---
    
    observeEvent(input$popout_main_plot, {
      showModal(modalDialog(title = "Enlarged: Species Detection Analysis", withSpinner(plotlyOutput(session$ns("modal_main_plot"), height = "80vh")), size = "xl", easyClose = TRUE, footer = modalButton("Close")))
    })
    output$modal_main_plot <- renderPlotly({
      p <- main_ggplot_obj(); req(p)
      ggplotly(p) %>% layout(hovermode = "closest", legend = list(orientation = "h", y = -0.2), showlegend = isTRUE(input$toggle_legend))
    })
    
    observeEvent(input$popout_stats1, {
      showModal(modalDialog(title = "Enlarged: Detection Counts by Variable", withSpinner(plotlyOutput(session$ns("modal_stats1_plot"), height = "80vh")), size = "xl", easyClose = TRUE, footer = modalButton("Close")))
    })
    output$modal_stats1_plot <- renderPlotly({
      p <- stats1_ggplot_obj(); req(p)
      ggplotly(p) %>% layout(showlegend = isTRUE(input$toggle_legend))
    })
    
    # Update the modal for stats_plot2 to use the new reactive plot object
    observeEvent(input$popout_stats2, {
      showModal(modalDialog(title = "Enlarged: Survey & Broadcast Counts", withSpinner(plotlyOutput(session$ns("modal_stats2_plot"), height = "80vh")), size = "xl", easyClose = TRUE, footer = modalButton("Close")))
    })
    output$modal_stats2_plot <- renderPlotly({
      p <- survey_broadcast_plot_obj(); req(p)
      ggplotly(p) %>% layout(showlegend = FALSE)
    })
  })
}
