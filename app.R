library(shiny)
library(shinydashboard)
library(dplyr)
library(purrr)
library(furrr)
library(promises)
library(future)
library(DT)
library(waiter)
library(logger)

# Configure parallel processing
plan(multisession)

# Configure logging
log_threshold(DEBUG)
log_appender(appender_file("scraper.log"))

# Scraper functions
source("R/scraper.R")
source("R/utils.R")

# UI Definition
ui <- dashboardPage(
  skin = "black",
  
  dashboardHeader(title = "Licitaciones Scraper"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Settings", tabName = "settings", icon = icon("gear")),
      menuItem("Logs", tabName = "logs", icon = icon("terminal"))
    )
  ),
  
  dashboardBody(
    use_waiter(),
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          box(
            width = 12,
            title = "Control Panel",
            status = "primary",
            solidHeader = TRUE,
            
            fluidRow(
              column(
                width = 3,
                actionButton(
                  "start_scraping",
                  "Start Scraping",
                  icon = icon("play"),
                  class = "btn-success btn-lg btn-block"
                )
              ),
              column(
                width = 3,
                actionButton(
                  "stop_scraping",
                  "Stop Scraping",
                  icon = icon("stop"),
                  class = "btn-danger btn-lg btn-block"
                )
              )
            )
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = "Progress",
            status = "info",
            solidHeader = TRUE,
            
            fluidRow(
              column(
                width = 12,
                uiOutput("progress_cards")
              )
            ),
            
            fluidRow(
              column(
                width = 12,
                DTOutput("results_table")
              )
            )
          )
        )
      ),
      
      tabItem(
        tabName = "settings",
        box(
          width = 12,
          title = "Scraper Configuration",
          status = "warning",
          solidHeader = TRUE,
          
          fluidRow(
            column(
              width = 6,
              numericInput(
                "parallel_instances",
                "Parallel Instances",
                value = 1,
                min = 1,
                max = 10
              ),
              numericInput(
                "batch_size",
                "Batch Size",
                value = 20,
                min = 1
              ),
              numericInput(
                "max_pages",
                "Max Pages (optional)",
                value = NA,
                min = 1
              )
            ),
            column(
              width = 6,
              textInput(
                "output_dir",
                "Output Directory",
                value = "licitaciones_batches"
              ),
              numericInput(
                "timeout",
                "Timeout (seconds)",
                value = 480,
                min = 1
              ),
              selectInput(
                "mode",
                "Scraping Mode",
                choices = c("simulated", "real"),
                selected = "simulated"
              )
            )
          )
        )
      ),
      
      tabItem(
        tabName = "logs",
        box(
          width = 12,
          title = "Debug Logs",
          status = "danger",
          solidHeader = TRUE,
          
          verbatimTextOutput("log_output")
        )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  # Reactive values
  rv <- reactiveValues(
    is_running = FALSE,
    progress = list(),
    results = data.frame(),
    logs = character()
  )
  
  # Initialize waiter
  w <- Waiter$new(
    html = spin_flower(),
    color = "#3c8dbc"
  )
  
  # Start scraping
  observeEvent(input$start_scraping, {
    req(!rv$is_running)
    
    # Update state
    rv$is_running <- TRUE
    rv$progress <- list()
    rv$results <- data.frame()
    
    # Show loading screen
    w$show()
    
    # Get config
    config <- list(
      parallel_instances = input$parallel_instances,
      batch_size = input$batch_size,
      max_pages = if(is.na(input$max_pages)) NULL else input$max_pages,
      output_dir = input$output_dir,
      timeout = input$timeout,
      mode = input$mode
    )
    
    # Log start
    log_info("Starting scraping with config:", config)
    
    # Start scraping process
    future_promise({
      scrape_licitaciones(
        config = config,
        on_progress = function(progress) {
          # Update progress in main thread
          session$sendCustomMessage(
            type = "update_progress",
            message = progress
          )
        }
      )
    }) %...>%
      (function(result) {
        # Update results
        rv$results <- bind_rows(rv$results, result)
        
        # Update state
        rv$is_running <- FALSE
        
        # Hide loading screen
        w$hide()
        
        # Log completion
        log_info("Scraping completed successfully")
      }) %...!%
      (function(error) {
        # Handle error
        log_error("Scraping error:", error)
        
        # Update state
        rv$is_running <- FALSE
        
        # Hide loading screen
        w$hide()
        
        # Show error
        showNotification(
          "An error occurred during scraping",
          type = "error"
        )
      })
  })
  
  # Stop scraping
  observeEvent(input$stop_scraping, {
    req(rv$is_running)
    
    # Update state
    rv$is_running <- FALSE
    
    # Log stop
    log_info("Scraping stopped by user")
    
    # Show notification
    showNotification(
      "Scraping stopped",
      type = "warning"
    )
  })
  
  # Progress cards
  output$progress_cards <- renderUI({
    req(length(rv$progress) > 0)
    
    map(rv$progress, function(p) {
      div(
        class = "progress-card",
        
        div(
          class = "progress-header",
          h4(paste("Instance", p$instanceId)),
          span(class = "badge", paste("Batch", p$currentBatch))
        ),
        
        div(
          class = "progress-body",
          div(
            class = "progress-stats",
            span(paste("Page", p$currentPage, "of", p$totalPages)),
            span(paste(p$licitacionesCount, "licitaciones"))
          ),
          
          div(
            class = "progress-bar",
            div(
              class = "progress-fill",
              style = paste0(
                "width: ",
                round(p$currentPage / p$totalPages * 100),
                "%"
              )
            )
          )
        ),
        
        if (!is.null(p$lastLicitacion)) {
          div(
            class = "last-licitacion",
            div(class = "expediente", p$lastLicitacion$expediente),
            div(class = "descripcion", p$lastLicitacion$descripcion),
            div(
              class = "timestamp",
              format(as.POSIXct(p$lastLicitacion$timestamp), "%H:%M:%S")
            )
          )
        }
      )
    })
  })
  
  # Results table
  output$results_table <- renderDT({
    req(nrow(rv$results) > 0)
    
    datatable(
      rv$results,
      options = list(
        pageLength = 10,
        dom = "tp"
      )
    )
  })
  
  # Log output
  output$log_output <- renderPrint({
    # Read last 100 lines from log file
    tail(readLines("scraper.log"), 100)
  })
}

# Run app
shinyApp(ui, server)