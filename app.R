library(shiny)
library(bslib)
library(leaflet)

ui <- bslib::page_navbar(
  title = "NYC EV Corridor Charging Screening Tool",
  
  bslib::nav_panel(
    "Instructions",
    fluidPage(
      h3("Upload a corridor file"),
      p("Accepted formats: zipped shapefile (.zip) or GeoJSON (.geojson)."),
      p("For this MVP, upload one corridor line feature only.")
    )
  ),
  
  bslib::nav_panel(
    "Upload & Validate",
    sidebarLayout(
      sidebarPanel(
        fileInput(
          "corridor_file",
          "Upload corridor",
          accept = c(".zip", ".geojson", ".json")
        ),
        actionButton("validate_btn", "Validate File"),
        
        br(),
        
        selectInput(
          "scenario",
          "Deployment scenario",
          choices = c("4 Level 2 Ports", "8 Level 2 Ports", "2 DC Fast Chargers")
        )
      ),
      mainPanel(
        h4("Validation Messages"),
        verbatimTextOutput("validation_text"),
        h4("Corridor Metadata"),
        tableOutput("meta_table"),
        h4("Estimated EV Charging Demand"),
        textOutput("ev_demand"),
        h4("Scenario Economics"),
        tableOutput("cost_table"),
        br(),
        downloadButton("download_excel", "Download Excel Results")
      )
    )
  ),
  
  bslib::nav_panel(
    "Map Review",
    fluidPage(
      leafletOutput("map", height = 700)
    )
  )
)

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    corridor_raw = NULL,
    corridor = NULL,
    validation = NULL
  )
  
  observeEvent(input$validate_btn, {
    req(input$corridor_file)
    
    uploaded <- tryCatch(
      read_corridor_upload(input$corridor_file$datapath),
      error = function(e) e
    )
    
    if (inherits(uploaded, "error")) {
      rv$validation <- list(
        valid = FALSE,
        messages = c(uploaded$message),
        data = NULL
      )
      rv$corridor_raw <- NULL
      rv$corridor <- NULL
      return()
    }
    
    rv$corridor_raw <- uploaded
    val <- validate_corridor(uploaded)
    rv$validation <- val
    
    if (isTRUE(val$valid)) {
      rv$corridor <- prepare_corridor_geometry(val$data)
    } else {
      rv$corridor <- NULL
    }
  })
  
  output$validation_text <- renderPrint({
    req(rv$validation)
    cat(paste(rv$validation$messages, collapse = "\n"))
  })
  
  output$meta_table <- renderTable({
    req(rv$corridor)
    
    len <- calc_corridor_length(rv$corridor)
    
    data.frame(
      Metric = c("Feature count", "Geometry type", "CRS EPSG", "Length (feet)", "Length (miles)"),
      Value = c(
        nrow(rv$corridor),
        paste(unique(as.character(sf::st_geometry_type(rv$corridor))), collapse = ", "),
        sf::st_crs(rv$corridor)$epsg,
        len$feet,
        len$miles
      )
    )
  })
  
  output$ev_demand <- renderText({
    req(rv$corridor)
    
    len <- calc_corridor_length(rv$corridor)
    demand <- estimate_ev_demand(len$miles)
    
    paste("Estimated daily charging demand:", demand, "vehicles/day")
  })
  
  output$cost_table <- renderTable({
    req(rv$corridor)
    
    len <- calc_corridor_length(rv$corridor)
    demand <- estimate_ev_demand(len$miles)
    
    scenarios <- c("4 Level 2 Ports", "8 Level 2 Ports", "2 DC Fast Chargers")
    
    do.call(rbind, lapply(scenarios, function(s) {
      run_cost_model(demand, s)
    }))
  })
  
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("nyc_ev_corridor_results_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(rv$corridor)
      
      len <- calc_corridor_length(rv$corridor)
      
      metrics_df <- data.frame(
        Metric = c("Feature count", "Geometry type", "CRS EPSG", "Length (feet)", "Length (miles)"),
        Value = c(
          nrow(rv$corridor),
          paste(unique(as.character(sf::st_geometry_type(rv$corridor))), collapse = ", "),
          sf::st_crs(rv$corridor)$epsg,
          len$feet,
          len$miles
        )
      )
      
      demand <- estimate_ev_demand(len$miles)
      scenarios <- c("4 Level 2 Ports", "8 Level 2 Ports", "2 DC Fast Chargers")
      
      cost_df <- do.call(rbind, lapply(scenarios, function(s) {
        run_cost_model(demand, s)
      }))
      
      export_results_workbook(
        path = file,
        corridor_sf = rv$corridor,
        metrics_df = metrics_df,
        cost_df = cost_df
      )
    }
  )
  
  output$map <- leaflet::renderLeaflet({
    req(rv$corridor)
    
    buffer <- create_corridor_buffer(rv$corridor)
    
    make_base_map() %>%
      add_corridor_layer(rv$corridor) %>%
      leaflet::addPolygons(
        data = buffer,
        fillColor = "blue",
        fillOpacity = 0.2,
        color = NA
      )
  })
}

shinyApp(ui, server)