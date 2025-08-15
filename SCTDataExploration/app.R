

# Load necessary libraries
library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(DBI)
library(odbc)
library(leaflet)         # for map
library(geojsonio)       # for downloading county lines
library(shinymanager)    # For authentication
library(RSQLite)         # For the user database
library(shinyjs)         # For dynamic UI features like disabling inputs
library(shinycssloaders) # For loading spinners
library(ggthemes)        # For plot themes

# --- Source the module files ---
source("EasternBlackRail_module.R")
source("WTPT_module.R")

# --- Define Themes ---
# Explicitly set the Bootstrap version to 5 for both themes to avoid conflicts.
light_theme <- bslib::bs_theme(version = 5, bootswatch = "sandstone")
dark_theme <- bslib::bs_theme(version = 5, bootswatch = "darkly")

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

# WTPT Data Loading Function
get_wtpt_data <- function() {
  con <- DBI::dbConnect(
    odbc::odbc(),
    Driver = "ODBC Driver 17 for SQL Server",
    Server = "DNRCPWPF53ENDQ",
    Database = "SCT",
    trusted_connection = "yes"
  )
  on.exit(DBI::dbDisconnect(con))
  
  # Read all the required tables from the WTPT schema
  birds_data <- DBI::dbReadTable(con, DBI::Id(schema = "WTPT", table = "Birds"))
  banding_data <- DBI::dbReadTable(con, DBI::Id(schema = "WTPT", table = "master_banding_raw"))
  encounters_data <- DBI::dbReadTable(con, DBI::Id(schema = "WTPT", table = "Encounters_Final_raw"))
  resights_data <- DBI::dbReadTable(con, DBI::Id(schema = "WTPT", table = "Resights_raw"))
  telemetry_data <- DBI::dbReadTable(con, DBI::Id(schema = "WTPT", table = "WTPT_Telemetry_raw"))
  plot_data <- DBI::dbReadTable(con, DBI::Id(schema = "WTPT", table = "Plot"))
  
  # Return a list of tables to be processed in the server
  list(
    birds = birds_data,
    banding = banding_data,
    encounters = encounters_data,
    resights = resights_data,
    telemetry = telemetry_data,
    plots = plot_data
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
  axis.title.y = element_text(color = "black", size = 12, margin = margin(r = 10), angle=90),
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
  panel.spacing = unit(1, "lines"),
  plot.margin = margin(t = 10, r = 10, b = 10, l = 20, unit = "pt")
)

# Load and preprocess data at app startup
Detections <- get_detections_data()
WTPT_data <- get_wtpt_data() # Load WTPT data once at startup

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

# Define the UI for the new Home tab
home_ui <- tagList(
  layout_column_wrap(
    width = 1/2,
    # Welcome and Permissions card is now on the left
    card(
      card_header("Welcome to the SCT Data Explorer App"),
      p("The SCT Data Explorer app allows you to explore data from the Species Conservation Team (SCT) database. The data in the app reflects the database, and as data is added the app will show the updates in real time."),
      p("It is required that you have a username and password. Use the My Profile section to manage your password and view your login information and Role. Your permissions to view projects are controlled by the admin. If you wish to change your permissions, please contact an app admin. Use the side bar to navigate between the different projects you have access to."),
      hr(),
      p("You have access to the following modules:"),
      uiOutput("user_permissions_ui")
    ),
    # My Profile card is now on the right
    card(
      card_header("My Profile"),
      uiOutput("user_profile_ui")
    )
  ),
  # Admin panel - now collapsible
  conditionalPanel(
    condition = "output.is_admin == true",
    accordion(
      open = FALSE, # Start closed
      accordion_panel(
        "Admin Panel",
        icon = bsicons::bs_icon("person-badge"),
        navset_tab(
          nav_panel("User Management", uiOutput("admin_user_management_ui")),
          nav_panel("Permission Management", uiOutput("admin_permission_management_ui"))
        )
      )
    )
  )
)

# Main UI definition
ui_main <- 
  page_sidebar(
    title = div(
      "SCT Data Explorer",
      class = "bg-blue p-4 text-white", # Add background color, padding, and text color
      style = "font-size: 1.5em; font-weight: bold;" # Increase font size
    ),
    # bslib::page_navbar(
    #   header = div( # Use header for full width placement
    #     "SCT Data Explorer",
    #     class = "bg-blue p-4 text-white", # Add background color, padding, and text color
    #     style = "font-size: 1.5em; font-weight: bold; width: 100%;" # Ensure full width
    #   ),
    theme = dark_theme, # Start with dark theme
    sidebar = sidebar(
      title = "CPW SCT Projects",
      navset_pill_list(
        id = "species_nav",
        nav_panel(title = "User Profile", value = "home"), # Renamed tab
        nav_panel(title = "Eastern Black Rail", value = "eabr"),
        nav_panel(title = "White Tailed Ptarmigan", value = "wtp")
      )
    ),
    uiOutput("main_content_ui")
  )


# Wrap the main UI with the authentication layer and add shinyjs
ui <- secure_app(
  ui = tagList(
    shinyjs::useShinyjs(),
    ui_main
  ),
  # This is the key fix: inject the BS5 theme dependencies into the login page header
  head_auth = tags$head(
    bslib::bs_theme_dependencies(dark_theme),
    tags$style(HTML("
        .modal-xl { max-width: 95%; }
      .modal-l { max-width: 80%; }
      .accordion-button {
        color: white;
      }
    "))
  ),
  choose_language = FALSE
)


# --- Server Logic ---

server <- function(input, output, session) {
  
  # # --- Theme Toggling ---
  # observeEvent(input$dark_mode, {
  #   # This ensures the theme is only set after a user is successfully logged in.
  #   req(res_auth$user)
  #   session$setCurrentTheme(
  #     if (isTRUE(input$dark_mode)) dark_theme else light_theme
  #   )
  # }, ignoreInit = TRUE) # Use ignoreInit to only trigger on user interaction
  
  # --- Theme Toggling ---
  observe({
    # This ensures the theme is only set after a user is successfully logged in.
    req(res_auth$user)
    session$setCurrentTheme(
      if (isTRUE(input$dark_mode)) dark_theme else light_theme
    )
  })
  
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
  
  # --- Call the Module Servers ---
  ebr_server("eabr_module", Detections, co_counties, theme_custom)
  wtpt_server("wtpt_module", WTPT_data, co_counties)
  
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
           "eabr" = ebr_ui("eabr_module"), 
           "wtp" = wtpt_ui("wtpt_module")
    )
  })
  
  # --- Home Tab Server Logic ---
  
  output$user_profile_ui <- renderUI({
    req(res_auth$user, user_info$loaded) # Ensure user is logged in and info is loaded
    tagList(
      p(strong("Username:"), res_auth$user),
      p(strong("Role:"), if (isTRUE(user_info$is_admin)) "Admin" else "Standard User"),
      hr(),
      # Add the theme toggle here
      checkboxInput("dark_mode", "Enable Dark Mode", value = TRUE), # Start with dark mode checked
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
  
  output$admin_permission_management_ui <- renderUI({
    req(user_info$all_users) # Ensure user list is loaded
    
    selected_user_is_admin <- if (!is.null(input$permission_user) && input$permission_user %in% user_info$all_users$username) {
      user_info$all_users %>%
        filter(username == input$permission_user) %>%
        pull(is_admin) %>%
        as.logical()
    } else {
      FALSE
    }
    
    tagList(
      h5("Manage User Role & Permissions"),
      fluidRow(
        column(6,
               selectInput("permission_user", "Select User:", choices = user_info$all_users$username, selected = input$permission_user)
        ),
        column(6,
               checkboxInput("permission_is_admin", "Make Admin?", value = selected_user_is_admin)
        )
      ),
      hr(),
      
      checkboxGroupInput("permission_modules", "Assign Modules (for Standard Users):",
                         choices = c("Eastern Black Rail" = "eabr", "White Tailed Ptarmigan" = "wtp")),
      conditionalPanel(
        condition = "input.permission_is_admin == true",
        tags$p(tags$i("Admins have access to all modules automatically."), style = "color: #6c757d;")
      ),
      br(),
      actionButton("update_permissions_btn", "Update User", icon = icon("save"))
    )
  })
  
  
  # --- Server-side logic for Home Tab buttons ---
  
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
      
      user_info$all_users <- dbGetQuery(dbConnect(SQLite(), db_path), "SELECT username, is_admin FROM user_credentials ORDER BY username")
      output$add_user_feedback <- renderUI(NULL)
    } else {
      output$add_user_feedback <- renderUI({ p(result$message, style = "color: red;") })
    }
  })
  
  output$users_table <- DT::renderDataTable({
    req(user_info$loaded)
    
    users_with_actions <- user_info$all_users %>%
      mutate(
        Role = ifelse(is_admin == 1, "Admin", "User"),
        Actions = ifelse(
          username == res_auth$user,
          "", 
          sprintf('<button id="delete_%s" class="btn btn-danger btn-sm delete-btn" onclick="Shiny.setInputValue(\'delete_user_button\', \'%s\', {priority: \'event\'})">Delete</button>', username, username)
        )
      ) %>%
      select(Username = username, Role, Actions)
    
    DT::datatable(users_with_actions, escape = FALSE, selection = 'none', options = list(pageLength = 5))
  })
  
  observeEvent(input$delete_user_button, {
    user_to_delete <- input$delete_user_button
    delete_user(user_to_delete)
    showNotification(paste("User", user_to_delete, "deleted."), type = "warning")
    user_info$all_users <- dbGetQuery(dbConnect(SQLite(), db_path), "SELECT username, is_admin FROM user_credentials ORDER BY username")
  })
  
  observeEvent(input$update_permissions_btn, {
    req(input$permission_user)
    
    is_admin_flag <- as.integer(input$permission_is_admin)
    con <- dbConnect(SQLite(), db_path)
    dbExecute(con, "UPDATE user_credentials SET is_admin = ? WHERE username = ?",
              params = list(is_admin_flag, input$permission_user))
    dbDisconnect(con)
    
    if (input$permission_is_admin) {
      update_permissions(input$permission_user, character(0)) 
    } else {
      update_permissions(input$permission_user, input$permission_modules)
    }
    
    showNotification(paste("Permissions and role updated for", input$permission_user), type = "message")
    
    user_info$all_users <- dbGetQuery(dbConnect(SQLite(), db_path), "SELECT username, is_admin FROM user_credentials ORDER BY username")
  })
  
  observeEvent(input$permission_user, {
    req(input$permission_user)
    con <- dbConnect(SQLite(), db_path)
    
    current_perms <- dbGetQuery(con, "SELECT module_id FROM user_permissions WHERE username = ?", params = list(input$permission_user))
    current_admin_status <- dbGetQuery(con, "SELECT is_admin FROM user_credentials WHERE username = ?", params = list(input$permission_user))
    
    dbDisconnect(con)
    
    updateCheckboxGroupInput(session, "permission_modules", selected = current_perms$module_id)
    updateCheckboxInput(session, "permission_is_admin", value = as.logical(current_admin_status$is_admin))
  })
  
  observeEvent(input$permission_is_admin, {
    shinyjs::toggleState(
      id = "permission_modules",
      condition = !isTRUE(input$permission_is_admin)
    )
  }, ignoreNULL = FALSE) 
  
}

shinyApp(ui = ui, server = server)
