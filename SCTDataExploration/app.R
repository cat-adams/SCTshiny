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
library(shinymanager) # For authentication
library(RSQLite)      # For the user database

# --- User Authentication and Database Setup ---

# Create local directory for user database if it doesn't exist
local_db_dir <- file.path(getwd(), "app_data")
if (!dir.exists(local_db_dir)) {
  dir.create(local_db_dir)
}
db_path <- file.path(local_db_dir, "user_database.db")

# Initialize database with user tables
init_database <- function() {
  con <- dbConnect(SQLite(), db_path)
  # User credentials table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS user_credentials (
      username TEXT PRIMARY KEY,
      password TEXT NOT NULL,
      is_admin INTEGER DEFAULT 0,
      last_updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP
    )
  ")
  # User permissions table
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS user_permissions (
      username TEXT NOT NULL,
      module_id TEXT NOT NULL, -- e.g., 'eabr', 'wtp'
      PRIMARY KEY (username, module_id)
    )
  ")
  dbDisconnect(con)
}

# Initialize credentials and permissions in database if none exist
init_credentials <- function() {
  con <- dbConnect(SQLite(), db_path)
  existing <- dbGetQuery(con, "SELECT username FROM user_credentials")
  if (nrow(existing) == 0) {
    # Insert default credentials
    dbExecute(con, "INSERT OR IGNORE INTO user_credentials (username, password, is_admin) VALUES ('admin', 'admin123', 1)")
    dbExecute(con, "INSERT OR IGNORE INTO user_credentials (username, password, is_admin) VALUES ('demo_bio', 'bio123', 0)")
    
    # Grant admin all permissions
    dbExecute(con, "INSERT OR IGNORE INTO user_permissions (username, module_id) VALUES ('admin', 'eabr')")
    dbExecute(con, "INSERT OR IGNORE INTO user_permissions (username, module_id) VALUES ('admin', 'wtp')")
  }
  dbDisconnect(con)
}

# Load credentials from the database for shinymanager
load_credentials <- function() {
  con <- dbConnect(SQLite(), db_path)
  creds <- dbGetQuery(con, "SELECT username as user, password, is_admin FROM user_credentials") %>%
    mutate(admin = as.logical(is_admin)) %>% # Convert 0/1 to FALSE/TRUE for shinymanager
    select(-is_admin) # Remove the old column
  dbDisconnect(con)
  return(creds)
}

# --- Database Helper Functions for User Management ---

update_password <- function(username, new_password) {
  con <- dbConnect(SQLite(), db_path)
  tryCatch({
    dbExecute(con, 
              "UPDATE user_credentials SET password = ?, last_updated = CURRENT_TIMESTAMP WHERE username = ?",
              params = list(new_password, username))
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  }, finally = {
    dbDisconnect(con)
  })
}

create_new_user <- function(username, password, is_admin) {
  con <- dbConnect(SQLite(), db_path)
  tryCatch({
    existing <- dbGetQuery(con, "SELECT username FROM user_credentials WHERE username = ?", params = list(username))
    if (nrow(existing) > 0) {
      return(list(success = FALSE, message = "Username already exists"))
    }
    dbExecute(con, "INSERT INTO user_credentials (username, password, is_admin) VALUES (?, ?, ?)",
              params = list(username, password, as.integer(is_admin)))
    return(list(success = TRUE, message = "User created successfully"))
  }, error = function(e) {
    return(list(success = FALSE, message = paste("Error:", e$message)))
  }, finally = {
    dbDisconnect(con)
  })
}

delete_user <- function(username) {
  con <- dbConnect(SQLite(), db_path)
  tryCatch({
    dbExecute(con, "DELETE FROM user_credentials WHERE username = ?", params = list(username))
    dbExecute(con, "DELETE FROM user_permissions WHERE username = ?", params = list(username))
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  }, finally = {
    dbDisconnect(con)
  })
}

update_permissions <- function(username, modules) {
  con <- dbConnect(SQLite(), db_path)
  tryCatch({
    # Clear existing permissions for the user
    dbExecute(con, "DELETE FROM user_permissions WHERE username = ?", params = list(username))
    # Add new permissions if any are selected
    if (length(modules) > 0) {
      for (mod in modules) {
        dbExecute(con, "INSERT INTO user_permissions (username, module_id) VALUES (?, ?)", params = list(username, mod))
      }
    }
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  }, finally = {
    dbDisconnect(con)
  })
}


# Initialize the database on startup
init_database()
init_credentials()


# --- Main Data Connection and Loading ---

# Database connection function for wildlife data
get_detections_data <- function() {
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
}

# Load Colorado counties data
co_counties <- geojsonio::geojson_read("https://raw.githubusercontent.com/johan/world.geo.json/master/countries/USA/CO.geo.json", what = "sp")

# Custom ggplot theme
theme_custom <- theme(
  axis.line.x = element_line(color = "black", size = 0.5),
  axis.line.y = element_line(color = "black", size = 0.5),
  axis.text.x = element_text(angle = 45, hjust = 1, color = "black", size = 10),
  axis.text.y = element_text(color = "black", size = 10),
  axis.title.x = element_text(color = "black", size = 12, margin = margin(t = 10)),
  axis.title.y = element_text(color = "black", size = 12, margin = margin(r = 10)),
  plot.title = element_text(color = "black", size = 14, hjust = 0.5, margin = margin(b = 20)),
  legend.text = element_text(color = "black", size = 10),
  legend.title = element_text(color = "black", size = 11),
  legend.position = "bottom",
  strip.text = element_text(color = "black", size = 11, margin = margin(b = 5)),
  strip.background = element_blank(),
  panel.background = element_rect(fill = "white", color = NA),
  plot.background = element_rect(fill = "white", color = NA),
  panel.grid = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.spacing = unit(1, "lines")
)

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

# --- UI Definition ---

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

# Define the UI for the new Home tab
home_ui <- tagList(
  layout_column_wrap(
    width = 1/2,
    card(
      card_header("My Profile"),
      uiOutput("user_profile_ui")
    ),
    card(
      card_header("My Permissions"),
      p("You have access to the following modules:"),
      uiOutput("user_permissions_ui")
    )
  ),
  # Admin panel - only shown to admin users
  conditionalPanel(
    condition = "output.is_admin == true",
    card(
      card_header(
        class = "bg-primary",
        "Admin Panel"
      ),
      navset_tab(
        nav_panel("User Management", uiOutput("admin_user_management_ui")),
        nav_panel("Permission Management", uiOutput("admin_permission_management_ui"))
      )
    )
  )
)

# Main UI definition
ui_main <- page_sidebar(
  title = "SCT Data Explorer",
  theme = bslib::bs_theme(bootswatch = "darkly"),
  sidebar = sidebar(
    title = "CPW SCT Projects",
    navset_pill_list(
      id = "species_nav",
      nav_panel(title = "Home", value = "home"),
      nav_panel(title = "Eastern Black Rail", value = "eabr"),
      nav_panel(title = "White Tailed Ptarmigan", value = "wtp")
    )
  ),
  uiOutput("main_content_ui")
)


# Wrap the main UI with the authentication layer
ui <- secure_app(ui_main, choose_language = FALSE)


# --- Server Logic ---

server <- function(input, output, session) {
  
  # Authentication server logic
  res_auth <- secure_server(
    check_credentials = check_credentials(load_credentials())
  )
  
  # Reactive values to store user-specific info
  user_info <- reactiveValues(
    permissions = character(0),
    is_admin = FALSE,
    all_users = data.frame(),
    loaded = FALSE # Flag to check if user info has been loaded
  )
  
  # This observer triggers once after login to populate user info
  observe({
    req(res_auth$user)
    user_info$is_admin <- res_auth$admin
    
    con <- dbConnect(SQLite(), db_path)
    perms <- dbGetQuery(con, "SELECT module_id FROM user_permissions WHERE username = ?", params = list(res_auth$user))
    all_users_data <- dbGetQuery(con, "SELECT username, is_admin FROM user_credentials ORDER BY username")
    dbDisconnect(con)
    
    user_info$permissions <- perms$module_id
    user_info$all_users <- all_users_data
    user_info$loaded <- TRUE # Set the flag to TRUE after loading
  })
  
  output$is_admin <- reactive({
    req(user_info$loaded) # Wait for user info to be loaded
    user_info$is_admin
  })
  outputOptions(output, "is_admin", suspendWhenHidden = FALSE)
  
  # Dynamically render the main content UI based on the sidebar navigation
  output$main_content_ui <- renderUI({
    req(input$species_nav, user_info$loaded) # Wait for nav and user info
    
    # Check permissions for non-admin users
    if (!isTRUE(user_info$is_admin) && input$species_nav != "home") {
      if (!(input$species_nav %in% user_info$permissions)) {
        return(card(h4("Access Denied"), p("You do not have permission to view this module.")))
      }
    }
    
    switch(input$species_nav,
           "home" = home_ui,
           "eabr" = eastern_black_rail_ui,
           "wtp" = card(h4("White Tailed Ptarmigan"), p("Analysis module for White Tailed Ptarmigan is under construction."))
    )
  })
  
  # --- Home Tab Server Logic ---
  
  output$user_profile_ui <- renderUI({
    req(res_auth$user, user_info$loaded) # Ensure user is logged in and info is loaded
    tagList(
      p(strong("Username:"), res_auth$user),
      p(strong("Role:"), if (isTRUE(user_info$is_admin)) "Admin" else "Standard User"),
      hr(),
      h5("Change Password"),
      passwordInput("new_password", "New Password"),
      passwordInput("confirm_password", "Confirm New Password"),
      actionButton("change_password_btn", "Update Password", icon = icon("key")),
      uiOutput("password_feedback")
    )
  })
  
  output$user_permissions_ui <- renderUI({
    req(res_auth$user, user_info$loaded) # Ensure user is logged in and info is loaded
    
    # Special message for admin
    if (isTRUE(user_info$is_admin)) {
      return(p(strong("All Modules (Admin)")))
    }
    
    if (length(user_info$permissions) > 0) {
      tags$ul(
        lapply(user_info$permissions, function(p) {
          tags$li(switch(p,
                         "eabr" = "Eastern Black Rail",
                         "wtp" = "White Tailed Ptarmigan",
                         p))
        })
      )
    } else {
      p("You currently have no permissions assigned.")
    }
  })
  
  # --- Admin Panel Server Logic ---
  
  # User Management UI
  output$admin_user_management_ui <- renderUI({
    tagList(
      fluidRow(
        column(6,
               h5("Add New User"),
               textInput("new_username", "Username"),
               passwordInput("new_user_password", "Temporary Password"),
               checkboxInput("new_user_is_admin", "Make Admin?", FALSE),
               actionButton("add_user_btn", "Add User", icon = icon("user-plus")),
               uiOutput("add_user_feedback")
        ),
        column(6,
               h5("Existing Users"),
               DT::dataTableOutput("users_table")
        )
      )
    )
  })
  
  # Permission Management UI
  output$admin_permission_management_ui <- renderUI({
    tagList(
      h5("Manage Module Permissions"),
      selectInput("permission_user", "Select User:", choices = user_info$all_users$username),
      checkboxInput("permission_is_admin", "Make Admin?", value = FALSE),
      hr(), # Optional separator
      checkboxGroupInput("permission_modules", "Assign Modules:",
                         choices = c("Eastern Black Rail" = "eabr", "White Tailed Ptarmigan" = "wtp")),
      actionButton("update_permissions_btn", "Update Permissions", icon = icon("save"))
    )
  })
  
  # --- Server-side logic for Home Tab buttons ---
  
  # Handle password change button click
  observeEvent(input$change_password_btn, {
    req(res_auth$user)
    
    if (input$new_password == "" || input$confirm_password == "") {
      output$password_feedback <- renderUI({ p("Both password fields must be filled out.", style = "color: red;") })
      return()
    }
    
    if (input$new_password != input$confirm_password) {
      output$password_feedback <- renderUI({ p("Passwords do not match.", style = "color: red;") })
      return()
    }
    
    success <- update_password(res_auth$user, input$new_password)
    
    if (success) {
      showNotification("Password successfully changed!", type = "message")
      updateTextInput(session, "new_password", value = "")
      updateTextInput(session, "confirm_password", value = "")
      output$password_feedback <- renderUI({ p("Password updated successfully.", style = "color: green;") })
      delay(3000, { output$password_feedback <- renderUI(NULL) })
    } else {
      output$password_feedback <- renderUI({ p("An error occurred. Password not updated.", style = "color: red;") })
    }
  })
  
  # Handle "Add User" button click
  observeEvent(input$add_user_btn, {
    req(input$new_username, input$new_user_password)
    
    result <- create_new_user(
      username = input$new_username,
      password = input$new_user_password,
      is_admin = input$new_user_is_admin
    )
    
    if (result$success) {
      showNotification("User created successfully!", type = "message")
      updateTextInput(session, "new_username", value = "")
      updateTextInput(session, "new_user_password", value = "")
      updateCheckboxInput(session, "new_user_is_admin", value = FALSE)
      
      # Refresh the user list
      user_info$all_users <- dbGetQuery(dbConnect(SQLite(), db_path), "SELECT username, is_admin FROM user_credentials ORDER BY username")
      output$add_user_feedback <- renderUI(NULL)
    } else {
      output$add_user_feedback <- renderUI({ p(result$message, style = "color: red;") })
    }
  })
  
  # Render the users table
  output$users_table <- DT::renderDataTable({
    req(user_info$loaded)
    
    users_with_actions <- user_info$all_users %>%
      mutate(
        Role = ifelse(is_admin == 1, "Admin", "User"),
        Actions = ifelse(
          username == res_auth$user,
          "", # No delete button for self
          sprintf('<button id="delete_%s" class="btn btn-danger btn-sm delete-btn" onclick="Shiny.setInputValue(\'delete_user_button\', \'%s\', {priority: \'event\'})">Delete</button>', username, username)
        )
      ) %>%
      select(Username = username, Role, Actions)
    
    DT::datatable(users_with_actions, escape = FALSE, selection = 'none', options = list(pageLength = 5))
  })
  
  # Handle delete user button clicks
  observeEvent(input$delete_user_button, {
    user_to_delete <- input$delete_user_button
    delete_user(user_to_delete)
    showNotification(paste("User", user_to_delete, "deleted."), type = "warning")
    # Refresh the user list
    user_info$all_users <- dbGetQuery(dbConnect(SQLite(), db_path), "SELECT username, is_admin FROM user_credentials ORDER BY username")
  })
  
  # Handle permission updates
  observeEvent(input$update_permissions_btn, {
    req(input$permission_user)
    
    # Update module permissions
    success_perms <- update_permissions(input$permission_user, input$permission_modules)
    
    # Update the user's admin status
    con <- dbConnect(SQLite(), db_path)
    dbExecute(con, "UPDATE user_credentials SET is_admin = ? WHERE username = ?", 
              params = list(as.integer(input$permission_is_admin), input$permission_user))
    dbDisconnect(con)
    
    showNotification(paste("Permissions and role updated for", input$permission_user), type = "message")
    
    # Refresh the user list to reflect the role change
    user_info$all_users <- dbGetQuery(dbConnect(SQLite(), db_path), "SELECT username, is_admin FROM user_credentials ORDER BY username")
  })
  
  # Update permission checkboxes AND admin status when a new user is selected
  observeEvent(input$permission_user, {
    req(input$permission_user)
    con <- dbConnect(SQLite(), db_path)
    
    # Get current module permissions
    current_perms <- dbGetQuery(con, "SELECT module_id FROM user_permissions WHERE username = ?", params = list(input$permission_user))
    
    # Get current admin status
    current_admin_status <- dbGetQuery(con, "SELECT is_admin FROM user_credentials WHERE username = ?", params = list(input$permission_user))
    
    dbDisconnect(con)
    
    # Update the UI elements
    updateCheckboxGroupInput(session, "permission_modules", selected = current_perms$module_id)
    updateCheckboxInput(session, "permission_is_admin", value = as.logical(current_admin_status$is_admin))
  })
  # ... (rest of server logic for plots, maps, tables, etc.)
  
  # Reactive data filtering (for plots and tables)
  filtered_data <- reactive({
    req(input$species_nav == "eabr", input$year_range) # Require EABR module and filters
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
  
  # Reactive for plot data summary
  plot_data <- reactive({
    req(input$species_nav == "eabr", input$x_var, input$color_var, input$facet_var)
    data <- filtered_data()
    group_vars <- unique(c(input$x_var, input$color_var, if (input$facet_var != "None") input$facet_var))
    group_syms <- syms(group_vars)
    data %>%
      group_by(!!!group_syms) %>%
      summarise(Count = n(), .groups = 'drop')
  })
  
  # Main plot output
  output$main_plot <- renderPlotly({
    req(input$species_nav == "eabr", nrow(plot_data()) > 0)
    data <- plot_data()
    
    # Create base plot using aes() with sym() for proper column name handling
    p <- ggplot(data)
    
    # Add geom_line or geom_col based on x-axis variable
    if (input$x_var %in% c("Year", "Month")) {
      p <- p + geom_line(aes(x = !!sym(input$x_var), y = Count, color = !!sym(input$color_var), group = !!sym(input$color_var)), size = 1) +
        geom_point(aes(x = !!sym(input$x_var), y = Count, color = !!sym(input$color_var), group = !!sym(input$color_var)), size = 2)
    } else {
      p <- p + geom_col(aes(x = !!sym(input$x_var), y = Count, fill = !!sym(input$color_var)), position = "dodge")
    }
    
    # Add faceting if selected
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
        color = input$color_var,
        fill = input$color_var
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
    
    pal <- colorFactor(palette = "viridis", domain = data$ColorValue)
    
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
        fillColor = ~pal(ColorValue),
        popup = ~paste(
          "<b>Property:</b>", Property, "<br>", 
          "<b>Site Point:</b>", `Site Point`, "<br>",
          "<b>Detections:</b>", Count
        )
      ) %>%
      addLegend("bottomright", pal = pal, values = ~ColorValue, title = input$color_var, opacity = 1)
  })
  
  # Summary table
  output$summary_table <- DT::renderDataTable({
    req(input$species_nav == "eabr")
    DT::datatable(plot_data(), filter = "top", options = list(pageLength = 15, scrollX = TRUE, order = list(list(ncol(plot_data()) - 1, 'desc'))))
  })
  
  # Raw data table
  output$raw_data_table <- DT::renderDataTable({
    req(input$species_nav == "eabr")
    DT::datatable(filtered_data(), filter = "top", options = list(pageLength = 15, scrollX = TRUE))
  })
  
  # Generic function for statistics plots
  render_stats_plot <- function(y_var, y_lab) {
    renderPlotly({
      req(input$species_nav == "eabr", nrow(filtered_data()) > 0)
      data <- filtered_data()
      stats_data <- data %>%
        group_by(.data[[input$color_var]]) %>%
        summarise(Value = {{y_var}}, .groups = 'drop')
      
      p <- ggplot(stats_data, aes(x = .data[[input$color_var]], y = Value, fill = .data[[input$color_var]])) +
        geom_col() +
        theme_minimal() +
        labs(title = paste(y_lab, "by", input$color_var), x = input$color_var, y = y_lab) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
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
    req(input$species_nav == "eabr", nrow(filtered_data()) > 0)
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
