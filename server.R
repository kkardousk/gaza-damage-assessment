# server.R - enhanced gaza damage assessment server
# ===================================================

# load essential libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(leaflet)
library(scales)
library(qs)
library(tidyr)
library(stringr)
library(mapgl)
library(mapboxapi)
library(gt)
library(gtExtras)
library(purrr)
library(openxlsx)
library(sf)
# environment setup
mapbox_token <- Sys.getenv("MAPBOX_SECRET_TOKEN")

server <- function(input, output, session) {
  
  observe({
    showNotification(
      paste("Loading Data...", input$governorate),
      type = "message",
      duration = 5
    )}
  )
  # enhanced data loading with error handling
  spatial_index <- NULL
  ui_lookups <- NULL
  
  tryCatch({
    if(file.exists("data/spatial_index.qs")) {
      spatial_index <- qread("data/spatial_index.qs")
      showNotification("Data loaded successfully", type = "message", duration = 2)
    }
    
    if(file.exists("data/ui_lookups.qs")) {
      ui_lookups <- qread("data/ui_lookups.qs")
    }
  }, error = function(e) {
    showNotification(paste("Error loading data:", e$message), type = "error")
  })
  
  # fallback if ui_lookups not available
  if(is.null(ui_lookups) && !is.null(spatial_index)) {
    ui_lookups <- list(
      governorates = c("All", sort(unique(spatial_index$Governorate))),
      unique_dates = sort(unique(as.Date(spatial_index$sensor_date)))
    )
  }
  
  # update ui controls with animation
  observe({
    if(!is.null(ui_lookups)) {
      updateSelectInput(session, "governorate", choices = ui_lookups$governorates)
      updateSelectInput(session, "date_filter", 
                        choices = ui_lookups$unique_dates,
                        selected = max(ui_lookups$unique_dates))
    }
  })
  
  # enhanced reactive data filtering with caching
  filtered_data <- reactive({
    req(spatial_index, input$governorate, input$date_filter)
    
    # add loading notification for large datasets
    if(!is.null(spatial_index) && nrow(spatial_index) > 50000) {
      showNotification("Processing data...", id = "processing", duration = NULL)
      on.exit(removeNotification("processing"))
    }
    
    data_filtered <- spatial_index
    
    if(input$governorate != "All") {
      data_filtered <- data_filtered |> filter(Governorate == input$governorate)
    }
    
    data_filtered <- data_filtered |> 
      filter(as.Date(sensor_date) == as.Date(input$date_filter))
    
    return(data_filtered)
  })
  
  # enhanced status output with more detail
  output$status <- renderText({
    if(is.null(filtered_data()) || nrow(filtered_data()) == 0) {
      "No data available for selection"
    } else {
      record_count <- scales::comma(nrow(filtered_data()))
      if(input$governorate == "All") {
        paste("Analyzing", record_count, "records across all governorates")
      } else {
        paste("Analyzing", record_count, "records in", input$governorate)
      }
    }
  })
  
  # enhanced damage counts with animation support
  output$destroyed_count <- renderText({
    req(filtered_data())
    scales::comma(sum(filtered_data()$dmg_status == 1, na.rm = TRUE))
  })
  
  output$severely_damaged_count <- renderText({
    req(filtered_data())
    scales::comma(sum(filtered_data()$dmg_status == 2, na.rm = TRUE))
  })
  
  output$moderately_damaged_count <- renderText({
    req(filtered_data())
    scales::comma(sum(filtered_data()$dmg_status == 3, na.rm = TRUE))
  })
  
  output$no_dmg_cnt <- renderText({
    req(filtered_data())
    scales::comma(sum(filtered_data()$dmg_status > 3, na.rm = TRUE))
  })
  
  # enhanced map output with better styling
  output$map <- renderLeaflet({
    
    # create enhanced base map
    base_map <- leaflet() |> 
      addMapboxTiles(
        access_token = mapbox_token,
        style_id = "light-v10",  # updated to newer style
        username = "mapbox"
      ) |> 
      setView(lng = 34.41, lat = 31.4, zoom = 11) |>
      addScaleBar(position = "bottomright") |>
      addMiniMap(
        tiles = providers$OpenStreetMap.Mapnik,
        toggleDisplay = TRUE,
        minimized = TRUE
      )
    
    # add enhanced circle markers if data is available
    if(!is.null(filtered_data()) && nrow(filtered_data()) > 0) {
      
      # intelligent sampling for performance
      sample_size <- min(8000, nrow(filtered_data()))
      map_data <- filtered_data() |>
        slice_sample(n = sample_size) |>
        filter(!is.na(lat), !is.na(lon)) |>
        mutate(
          map_color = case_when(
            dmg_status == 1 ~ "#E57373",  # original red
            dmg_status == 2 ~ "#FFB74D",  # original orange  
            dmg_status == 3 ~ "#FFE451",  # original yellow
            TRUE ~ "#4CAF50"              # original green
          ),
          status_text = case_when(
            dmg_status == 1 ~ "Destroyed",
            dmg_status == 2 ~ "Severely Damaged",
            dmg_status == 3 ~ "Moderately Damaged",
            TRUE ~ "No Damage"
          )
        )
      
      if(nrow(map_data) > 0) {
        base_map <- base_map |>
          addCircleMarkers(
            data = map_data,
            lng = ~lon,
            lat = ~lat,
            radius = ~ifelse(dmg_status == 1, 5, 4),  # larger markers for destroyed
            opacity = 0.8,
            fillOpacity = 0.7,
            fillColor = ~map_color,
            color = "white",
            weight = 1,
            stroke = TRUE,
            popup = ~paste0(
              "<div style='font-family: Inter, sans-serif; padding: 8px;'>",
              "<h6 style='margin: 0 0 8px 0; color: #1f2937; font-weight: 600;'>", status_text, "</h6>",
              "<div style='margin: 4px 0;'><strong>Governorate:</strong> ", Governorate, "</div>",
              "<div style='margin: 4px 0;'><strong>Municipality:</strong> ", Municipality, "</div>", 
              "<div style='margin: 4px 0;'><strong>Neighborhood:</strong> ", Neighborhood, "</div>",
              "<div style='margin: 4px 0;'><strong>Date:</strong> ", sensor_date, "</div>",
              "</div>"
            ),
            popupOptions = popupOptions(
              className = "custom-popup",
              closeButton = TRUE,
              closeOnEscapeKey = TRUE
            )
          ) |>
          addLegend(
            position = "bottomleft",
            colors = c("#E57373", "#FFB74D", "#FFE451", "#4CAF50"),
            labels = c("Destroyed", "Severely Damaged", "Moderately Damaged", "No Damage"),
            title = "Damage Status",
            opacity = 0.8
          )
      }
    }
    
    base_map
  })
  
  # chart output - keeping original styling
  output$damage_chart <- renderPlot({
    req(filtered_data())
    
    if(nrow(filtered_data()) == 0) {
      return(ggplot() + geom_blank() + labs(title = "No data available"))
    }
    
    # create summary data
    summary_data <- filtered_data() |>
      summarise(
        Destroyed = sum(dmg_status == 1, na.rm = TRUE),
        `Severely Damaged` = sum(dmg_status == 2, na.rm = TRUE),
        `Moderately Damaged` = sum(dmg_status == 3, na.rm = TRUE),
        `No Damage` = sum(dmg_status > 3, na.rm = TRUE)
      ) |>
      pivot_longer(everything(), names_to = "Status", values_to = "Count")
    
    ggplot(summary_data, aes(x = Status, y = Count, fill = Status)) +
      geom_col() +
      scale_fill_manual(values = c(
        "Destroyed" = '#E57373', 
        "Severely Damaged" = '#FFB74D', 
        "Moderately Damaged" = '#FFE451',
        "No Damage" = '#4CAF50'
      )) +
      scale_y_continuous(labels = scales::comma) +
      labs(
        title = paste("Damage Status -", input$governorate),
        subtitle = paste("As of Date:", input$date_filter),
        y = "Number of Buildings",
        x = NULL
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 12, face = 'bold'),
        plot.title = element_text(hjust = 0.5, size = 14, face = 'bold'),
        axis.text = element_text(size = 12, face = 'bold'),
        axis.title.y = element_text(size = 12, face = 'bold'),
        legend.position = "none"
      )
  })
  
  # table output - keeping original gt implementation
  output$data_table <- render_gt({
    damage_summaries <- qread("data/damage_summaries.qs")
    
    plot_trends <- function(governorate) {
      
      p <- damage_summaries |> 
        arrange(desc(sensor_date)) |> 
        filter(name == 'ttl_destroyed' & Governorate == governorate) |> 
        ggplot(aes(x = sensor_date, y = dmg_split)) +
        geom_smooth(se = FALSE, colour = 'black', linewidth = 1.7) +
        theme_minimal() +
        theme_void()
      
    }
    
    tab <- damage_summaries |> 
      mutate(gov = Governorate) |> 
      arrange(desc(sensor_date)) |> 
      filter(name == 'ttl_destroyed') |> 
      gt(id = "sticky_headers") |> 
      cols_label_with(fn = str_to_title) |> 
      cols_hide(columns = c(name, ttl_buildings_captured)) |> 
      fmt_percent(columns = dmg_split, decimals = 2) |> 
      fmt_number(columns = value, decimals = 0) |> 
      # fmt_markdown(columns = temp) |> 
      text_transform(
        locations = cells_body(columns = 'gov'),
        fn = function(governorate) {
          map(governorate, plot_trends) |> 
            ggplot_image()
        }
      ) |> 
      gtExtras::gt_theme_nytimes() |>
      tab_options(
        table.font.names = "franklin-medium",
        table.font.weight = "bold",
        container.height = px(320),                    
        container.overflow.y = TRUE,                    
        container.width = pct(100),
        container.overflow.x = TRUE                   
      ) |>
      cols_label(
        sensor_date = 'Sensor Date',
        value = 'No. of Buildings',
        dmg_split = 'Destroyed (%)',
        gov = 'Destroyed (%) Across Sensor Dates'
      ) |> 
      opt_css(css = "
      #sticky_headers .gt_col_heading {
      position: sticky;
      top: 0;
      background: white;    
      z-index: 3;
      }
      
      #sticky_headers .gt_stub {
      position: sticky;
      left: 0;
      background: white;
      z-index: 4;
      }
      
      #sticky_headers .gt_table {
      border-collapse: separate;
      border-spacing: 0;
      }
    ")
    tab
    
  })
  # enhanced reactive for summary statistics
  summary_stats <- reactive({
    req(filtered_data())
    
    if(nrow(filtered_data()) == 0) return(NULL)
    list(
      total_buildings = nrow(filtered_data()),
      destroyed_pct = round(sum(filtered_data()$dmg_status == 1) / nrow(filtered_data()) * 100, 1),
      damaged_pct = round(sum(filtered_data()$dmg_status %in% c(2,3)) / nrow(filtered_data()) * 100, 1),
      intact_pct = round(sum(filtered_data()$dmg_status > 3) / nrow(filtered_data()) * 100, 1)
    )
  })
  
  # add notification system for user feedback
  observeEvent(input$governorate, {
    if(input$governorate != "All") {
      showNotification(
        paste("Filtering data for", input$governorate),
        type = "message",
        duration = 2
      )
    }
  })
  
  observeEvent(input$date_filter, {
    req(input$date_filter)
    formatted_date <- format(as.Date(input$date_filter), "%B %d, %Y")
    showNotification(
      paste("Updated to", formatted_date),
      type = "message", 
      duration = 2
    )
  })
  
  # enhanced error handling for map updates
  observe({
    req(filtered_data())
    
    if(nrow(filtered_data()) > 20000) {
      showNotification(
        "Large dataset detected. Map showing sample of data points for performance.",
        type = "warning",
        duration = 5
      )
    }
  })
  
  # excel export download handler - creates excel with 2 tibbles
  output$btn_export_excel <- downloadHandler(
    filename = function() {
      paste0("gaza_damage_summary_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      tryCatch({
        # read specific data files directly
        damage_summaries <- qread("data/damage_summaries.qs")
        
        # long_data <- qread("data/long_data_efficient.qs") |> 
        #   filter(!is.na(sensor_date), !is.na(dmg_status))
        # 
        # create named list with actual data (2 tibbles only)
        data_to_export <- list(
          "Damage_Summaries" = damage_summaries
          # "long_data" = long_data
        )
        # write to excel
        openxlsx::write.xlsx(data_to_export, file)
        
      }, error = function(e) {
        showNotification(paste("Export error:", e$message), type = "error")
        
        # fallback - just export damage summaries
        damage_summaries <- qread("data/damage_summaries.qs")
        openxlsx::write.xlsx(damage_summaries, file)
      })
    }
  )
  
  # sf object download handler for sample geometry
  output$btn_export_sf <- downloadHandler(
    filename = function() {
      paste0("sample_geometry_", Sys.Date(), ".geojson")
    },
    content = function(file) {
      tryCatch({
        sample_geometry <- qread("data/sample_geometry.qs")
        
        # write sf object as geojson
        st_write(sample_geometry, file, driver = "GeoJSON", quiet = TRUE)
        
      }, error = function(e) {
        showNotification(paste("GeoJSON export error:", e$message), type = "error")
      })
    }
  )
  
}