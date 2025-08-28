# ui.R - Gaza Damage Assessment UI - Enhanced Professional Version
# ===================================================================

# Load required libraries for UI
library(shiny)
library(bslib)
library(leaflet)
library(gt)
library(gtExtras)

# helper function to create sidebar UI
create_sidebar_ui <- function() {
  div(
    class = "p-3 sidebar-panel",
    style = "height: 100vh; overflow: hidden;",
    # Enhanced title with subtle animation
    div(
      class = "title-section mb-4",
      h3("Gaza Damage Assessment", 
         class = "app-title text-center mb-1"),
      p(
        "Source of Data: ", 
        tags$a("https://unosat.org/", 
               href = "https://unosat.org/", 
               target = "_blank",
               style = "color: #3b82f6; text-decoration: underline;"),
        class = "subtitle text-center text-muted small"
      )
    ),
    # Enhanced governorate selector
    div(
      class = "input-group-modern mb-3",
      tags$label("Governorate", class = "modern-label"),
      selectInput(
        inputId = "governorate",
        label = NULL,
        choices = c("All", if(exists("long_data")) sort(unique(long_data$Governorate)) else c("Gaza", "North Gaza", "Middle Area", "Khan Younis", "Rafah")),
        selected = "Gaza",
        width = "100%"
      )
    ),
    
    # Enhanced date selector
    div(
      class = "input-group-modern mb-4",
      tags$label("Sensor Date", class = "modern-label"),
      selectInput(
        inputId = "date_filter",
        label = NULL,
        choices = if(exists("long_data")) sort(unique(as.Date(long_data$sensor_date))) else as.Date(c("2023-10-14", "2024-07-08")),
        selected = if(exists("long_data")) max(as.Date(long_data$sensor_date), na.rm = TRUE) else as.Date("2024-07-08"),
        width = "100%"
      )
    ),
    
    # Enhanced status indicator with pulse effect
    div(
      class = "status-card mb-4",
      div(
        class = "status-icon",
        icon("database", class = "fa-solid")
      ),
      div(
        class = "status-text",
        textOutput("status")
      )
    ),
    
    # Enhanced damage statistics with modern cards
    div(
      class = "stats-section",
      h5("Impact Overview", class = "stats-title mb-3"),
      
      # Destroyed buildings - enhanced card
      div(
        class = "stat-card destroyed-card mb-3",
        div(class = "stat-icon-wrapper",
            div(class = "stat-icon destroyed-icon",
                icon("building", class = "fa-solid"))),
        div(class = "stat-content",
            div(class = "stat-number",
                strong(textOutput("destroyed_count", inline = TRUE))),
            div(class = "stat-label", "Buildings Destroyed"))
      ),
      
      # Severely damaged - enhanced card
      div(
        class = "stat-card severely-card mb-3",
        div(class = "stat-icon-wrapper",
            div(class = "stat-icon severely-icon",
                icon("building", class = "fa-solid"))),
        div(class = "stat-content",
            div(class = "stat-number",
                strong(textOutput("severely_damaged_count", inline = TRUE))),
            div(class = "stat-label", "Severely Damaged"))
      ),
      
      # Moderately damaged - enhanced card
      div(
        class = "stat-card moderate-card mb-3",
        div(class = "stat-icon-wrapper",
            div(class = "stat-icon moderate-icon",
                icon("building", class = "fa-solid"))),
        div(class = "stat-content",
            div(class = "stat-number",
                strong(textOutput("moderately_damaged_count", inline = TRUE))),
            div(class = "stat-label", "Moderately Damaged"))
      ),
      
      # No damage - enhanced card
      div(
        class = "stat-card intact-card mb-3",
        div(class = "stat-icon-wrapper",
            div(class = "stat-icon intact-icon",
                icon("building", class = "fa-solid"))),
        div(class = "stat-content",
            div(class = "stat-number",
                strong(textOutput("no_dmg_cnt", inline = TRUE))),
            div(class = "stat-label", "No Damage"))
      )
    ),
    
    downloadButton(
      'btn_export_excel',
      'Export Tabular Data',
      icon = shiny::icon('file-excel'),
      class = "btn-export-full-width"
    ),
    
    # export button (for sample geoJson/map)
    downloadButton(
      'btn_export_sf',
      'Export Map Data',
      icon = shiny::icon('map'),
      class = "btn-export-full-width"
    )
  )
}

# helper function to create map UI
create_map_ui <- function() {
  div(
    class = "map-container",
    style = "height: 100vh; position: relative;",
    leafletOutput("map", height = "100%")
  )
}

# helper function to create chart UI
create_chart_ui <- function() {
  div(
    class = "analysis-panel",
    style = "height: 100vh; overflow-y: auto;",
    
    # Enhanced header section
    div(
      class = "analysis-header p-4 mb-0",
      h4("Damage Analysis", class = "analysis-title mb-2"),
      p("Statistical overview and trends", class = "analysis-subtitle text-muted small mb-0")
    ),
    
    # Chart section with modern styling
    div(
      class = "chart-section p-4",
      div(class = "chart-container",
          plotOutput("damage_chart", height = "320px"))
    ),
    
    # Table section with enhanced styling
    div(
      class = "table-section p-4",
      div(
        class = "table-header mb-3",
        h5("Destruction Trends", class = "table-title mb-1"),
        p("Smoothed Curves", class = "table-subtitle text-muted small")
      ),
      div(
        class = "table-container",
        style = "max-height: 420px; overflow-y: auto;",
        gt_output("data_table")
      )
    )
  )
}

# main UI with enhanced styling
ui <- page_fillable(
  
  # Enhanced theme
  theme = bs_theme(
    version = 5, 
    bootswatch = "flatly",
    base_font = font_google("Inter", wght = c(300, 400, 500, 600, 700)),
    heading_font = font_google("Inter", wght = c(500, 600, 700)),
    primary = "#2563eb",
    secondary = "#64748b",
    success = "#16a34a",
    warning = "#ea580c",
    danger = "#dc2626",
    info = "#0ea5e9"
  ),
  
  # Enhanced custom CSS with modern design
  tags$head(
    tags$style(HTML("
      /* Enhanced global styling */
      :root {
        --primary-gradient: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        --danger-gradient: linear-gradient(135deg, #ff6b6b 0%, #ee5a6f 100%);
        --warning-gradient: linear-gradient(135deg, #feca57 0%, #ff9ff3 100%);
        --success-gradient: linear-gradient(135deg, #48cae4 0%, #023e8a 100%);
        --sidebar-bg: linear-gradient(180deg, #f8fafc 0%, #f1f5f9 100%);
        --card-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06);
        --card-shadow-hover: 0 10px 15px -3px rgba(0, 0, 0, 0.1), 0 4px 6px -2px rgba(0, 0, 0, 0.05);
        --border-radius: 12px;
        --transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
      }
      
      /* Enhanced body and layout */
      body, html {
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif !important;
        background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
        font-weight: 400;
        line-height: 1.6;
      }
      
      /* Enhanced sidebar styling */
      .sidebar-panel {
        background: var(--sidebar-bg);
        border-right: 1px solid rgba(148, 163, 184, 0.2);
        backdrop-filter: blur(10px);
        position: relative;
      }
      
      .sidebar-panel::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 4px;
        background: #000000;
      }
      
      /* Enhanced title styling */
      .title-section {
        text-align: center;
        padding: 1rem 0;
        border-bottom: 1px solid rgba(148, 163, 184, 0.1);
        margin-bottom: 2rem !important;
      }
      
      .app-title {
        font-weight: 700 !important;
        color: #000000 !important;
        font-size: 1.5rem !important;
        margin-bottom: 0.5rem !important;
      }
      
      .subtitle {
        font-weight: 400 !important;
        color: #000000 !important;
        font-size: 0.875rem !important;
      }
      
      /* Enhanced input styling */
      .input-group-modern {
        position: relative;
      }
      
      .modern-label {
        font-weight: 600 !important;
        color: #374151 !important;
        font-size: 0.875rem !important;
        margin-bottom: 0.5rem !important;
        display: block !important;
      }
      
      .form-select, .selectize-input {
        border: 2px solid #e5e7eb !important;
        border-radius: var(--border-radius) !important;
        transition: var(--transition) !important;
        background: white !important;
        font-weight: 500 !important;
        padding: 0.75rem 1rem !important;
        box-shadow: 0 1px 2px 0 rgba(0, 0, 0, 0.05) !important;
      }
      
      .form-select:focus, .selectize-input.focus {
        border-color: #3b82f6 !important;
        box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1) !important;
        outline: none !important;
      }
      
      /* Enhanced status card */
      .status-card {
        background: linear-gradient(135deg, #1e293b 0%, #334155 100%);
        color: white;
        padding: 1rem;
        border-radius: var(--border-radius);
        box-shadow: var(--card-shadow);
        display: flex;
        align-items: center;
        gap: 0.75rem;
        position: relative;
        overflow: hidden;
      }
      
      .status-card::before {
        content: '';
        position: absolute;
        top: 0;
        left: -100%;
        width: 100%;
        height: 100%;
        background: linear-gradient(90deg, transparent, rgba(255,255,255,0.1), transparent);
        animation: shimmer 2s infinite;
      }
      
      @keyframes shimmer {
        0% { left: -100%; }
        100% { left: 100%; }
      }
      
      .status-icon {
        font-size: 1.25rem;
        opacity: 0.9;
      }
      
      .status-text {
        font-weight: 600;
        font-size: 0.875rem;
      }
      
      /* Enhanced statistics cards */
      .stats-title {
        font-weight: 700 !important;
        color: #1e293b !important;
        font-size: 1rem !important;
        text-align: center;
      }
      
      .stat-card {
        background: white;
        border-radius: var(--border-radius);
        padding: 1.25rem;
        box-shadow: var(--card-shadow);
        transition: var(--transition);
        border: 1px solid rgba(148, 163, 184, 0.1);
        display: flex;
        align-items: center;
        gap: 1rem;
        position: relative;
        overflow: hidden;
      }
      
      .stat-card:hover {
        transform: translateY(-2px);
        box-shadow: var(--card-shadow-hover);
      }
      
      .stat-card::before {
        content: '';
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        height: 3px;
        transition: var(--transition);
      }
      
      .destroyed-card::before { background: linear-gradient(135deg, #E57373 0%, #d32f2f 100%); }
      .severely-card::before { background: linear-gradient(135deg, #FFB74D 0%, #f57c00 100%); }
      .moderate-card::before { background: linear-gradient(135deg, #FFE451 0%, #f9a825 100%); }
      .intact-card::before { background: linear-gradient(135deg, #4CAF50 0%, #2e7d32 100%); }
      
      .stat-icon-wrapper {
        flex-shrink: 0;
      }
      
      .stat-icon {
        width: 48px;
        height: 48px;
        border-radius: 12px;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 1.25rem;
        color: white;
        font-weight: 600;
      }
      
      .destroyed-icon { background: linear-gradient(135deg, #E57373 0%, #d32f2f 100%); }
      .severely-icon { background: linear-gradient(135deg, #FFB74D 0%, #f57c00 100%); }
      .moderate-icon { background: linear-gradient(135deg, #FFE451 0%, #f9a825 100%); }
      .intact-icon { background: linear-gradient(135deg, #4CAF50 0%, #2e7d32 100%); }
      
      .stat-content {
        flex-grow: 1;
      }
      
      .stat-number {
        font-size: 1.5rem;
        font-weight: 700;
        color: #1e293b;
        line-height: 1.2;
      }
      
      .stat-label {
        font-size: 0.875rem;
        color: #64748b;
        font-weight: 500;
        margin-top: 0.25rem;
      }
      
      /* Enhanced map container */
      .map-container {
        position: relative;
        border-radius: var(--border-radius);
        overflow: hidden;
        box-shadow: var(--card-shadow);
      }
      
      .map-overlay {
        position: absolute;
        top: 1rem;
        left: 1rem;
        right: 1rem;
        z-index: 1000;
        background: rgba(255, 255, 255, 0.95);
        backdrop-filter: blur(10px);
        padding: 0.75rem 1rem;
        border-radius: var(--border-radius);
        box-shadow: var(--card-shadow);
      }
      
      .map-title {
        font-weight: 600;
        color: #1e293b;
        font-size: 1.125rem;
      }
      
      /* Enhanced analysis panel */
      .analysis-panel {
        background: white;
        border-left: 1px solid rgba(148, 163, 184, 0.2);
        position: relative;
      }
      
      .analysis-header {
        background: linear-gradient(135deg, #f8fafc 0%, #f1f5f9 100%);
        border-bottom: 1px solid rgba(148, 163, 184, 0.1);
      }
      
      .analysis-title {
        font-weight: 700 !important;
        color: #1e293b !important;
        font-size: 1.25rem !important;
      }
      
      .analysis-subtitle {
        color: #64748b !important;
        font-weight: 500 !important;
      }
      
      /* Enhanced chart section */
      .chart-section {
        background: white;
        border-bottom: 1px solid rgba(148, 163, 184, 0.1);
      }
      
      .chart-container {
        background: white;
        border-radius: var(--border-radius);
        box-shadow: 0 1px 3px 0 rgba(0, 0, 0, 0.1);
        padding: 1rem;
        border: 1px solid rgba(148, 163, 184, 0.1);
      }
      
      /* Enhanced table section */
      .table-section {
        background: white;
      }
      
      .table-title {
        font-weight: 700 !important;
        color: #1e293b !important;
        font-size: 1rem !important;
      }
      
      .table-subtitle {
        color: #64748b !important;
        font-weight: 500 !important;
      }
      
      .table-container {
        background: white;
        border-radius: var(--border-radius);
        border: 1px solid rgba(148, 163, 184, 0.1);
        box-shadow: 0 1px 3px 0 rgba(0, 0, 0, 0.1);
      }
      
      /* sticky table headers */
      .table-container .gt_table thead th,
      .table-container .gt_column_spanner,
      .table-container .gt_col_heading {
        position: sticky !important;
        top: 0 !important;
        background: white !important;
        z-index: 10 !important;
        border-bottom: 2px solid #e5e7eb !important;
      }
      
      /* FontAwesome icons override */
      .fa, .fas, .far, .fab, .fal, .fad, .fa-solid, .fa-regular, 
      .fa-light, .fa-thin, .fa-duotone, .fa-brands,
      [class*='fa-'], [class^='fa-'] {
        font-family: 'Font Awesome 6 Free', 'Font Awesome 6 Pro', 'FontAwesome' !important;
      }
      
      /* Enhanced leaflet styling */
      .leaflet-container {
        background: #f8fafc;
        font-family: 'Inter', sans-serif !important;
      }
      
      .leaflet-popup-content-wrapper {
        border-radius: var(--border-radius);
        box-shadow: var(--card-shadow-hover);
      }
      
      /* Responsive adjustments */
      @media (max-width: 768px) {
        .app-title {
          font-size: 1.25rem !important;
        }
        
        .stat-card {
          padding: 1rem;
        }
        
        .stat-number {
          font-size: 1.25rem;
        }
      }
      
      /* Loading animations */
      @keyframes pulse {
        0%, 100% { opacity: 1; }
        50% { opacity: 0.5; }
      }
      
      .loading {
        animation: pulse 2s infinite;
      }
      
      /* full-width export button styling */
      .btn-export-full-width {
        width: 100% !important;
        background-color: white !important;
        border: 2px solid #e5e7eb !important;
        color: #374151 !important;
        font-weight: 600 !important;
        padding: 0.75rem 1rem !important;
        border-radius: var(--border-radius) !important;
        transition: var(--transition) !important;
        margin-top: 0.5rem !important;
        box-shadow: 0 1px 2px 0 rgba(0, 0, 0, 0.05) !important;
      }
      
      .btn-export-full-width:hover {
        background-color: #f8fafc !important;
        border-color: #3b82f6 !important;
        color: #1e293b !important;
        box-shadow: 0 0 0 3px rgba(59, 130, 246, 0.1) !important;
        transform: translateY(-1px) !important;
      }
      
      .btn-export-full-width:active {
        transform: translateY(0) !important;
      }
    "))
  ),
  
  # main layout
  fluidRow(
    # left sidebar
    column(
      width = 3,
      create_sidebar_ui()
    ),
    
    # center map
    column(
      width = 4,
      create_map_ui()
    ),
    
    # right analysis panel
    column(
      width = 5,
      create_chart_ui()
    )
  )
)