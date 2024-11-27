# # # Check and install missing packages
# required_packages <- c(
#   "parsnip", "dplyr", "h2o", "xgboost", "rpart", "tree", "randomForest",
#   "ranger", "e1071", "kernlab", "class", "kknn", "klaR", "brulee", "rstanarm",
#   "LiblineaR", "neuralnet", "infotheo", "glmnet", "foreach", "iterators", "shape", "torch")
# 
# installed_packages <- rownames(installed.packages())
# for (pkg in required_packages) {
#   if (!(pkg %in% installed_packages)) {
#     install.packages(pkg)
#   }
# }

# Source helper functions and modules
source("helpers.R")
source("modules/data_upload.R")
source("modules/data_processing.R")
source("modules/results_display.R")

library(shiny)
library(shinyjs)
library(DT)  # For rendering interactive tables

# UI setup
ui <- fluidPage(
  titlePanel("HLGen"),
  useShinyjs(),  # Enable shinyjs functionality
  
  sidebarLayout(
    sidebarPanel(
      fileInput("user_data", "Upload your data (.csv or .rds)"),
      actionButton("load_example_csv", "Load Example CSV"),
      actionButton("load_example_rds", "Load Example RDS"),
      numericInput("random_seed", "Random Seed", value = 1234),
      numericInput("n_runs", "Number of Runs", value = 1, min = 1),
      actionButton("process_data", "Process Data"),
      actionButton("cancel_processing", "Cancel Processing"),
      tags$div(id = "progress", style = "display: none;", "Processing..."),
      downloadButton("download_example_data", "Download Example Data")
    ),
    
    mainPanel(
      h3("App Description"),
      p("This Shiny app processes user-uploaded data to generate predictive outputs based on a pre-trained model. Users are encouraged to use the exact feature names for best results."),
      
      tabsetPanel(
        tabPanel("Results", DTOutput("results_table")),
        tabPanel("Download", downloadButton("download_data", "Download Table"))
      )
    )
  )
)

# Server-side logic
server <- function(input, output, session) {
  processed_data <- reactiveVal(NULL)
  cancel_flag <- reactiveVal(FALSE)  # Flag to track if the cancel button was pressed
  
  # Reactive to handle file upload or example data loading
  uploaded_data <- reactive({
    ext <- NULL
    
    if (!is.null(input$user_data)) {
      ext <- tools::file_ext(input$user_data$name)
      file_path <- input$user_data$datapath
    } else if (input$load_example_csv > 0) {
      ext <- "csv"
      file_path <- "data/example_data.csv"
    } else if (input$load_example_rds > 0) {
      ext <- "rds"
      file_path <- "data/example_data.rds"
    }
    
    if (!is.null(ext)) {
      tryCatch({
        if (ext == "csv") {
          return(read.csv(file_path, header = TRUE, row.names = 1, stringsAsFactors = FALSE))
        } else if (ext == "rds") {
          return(readRDS(file_path))
        } else {
          stop("Unsupported file type. Please upload a .csv or .rds file.")
        }
      }, error = function(e) {
        showNotification(paste("Error reading file:", e$message), type = "error")
        return(NULL)
      })
    }
    return(NULL)
  })
  
  # Load the training object once at the start
  trainObj <- reactive({
    tryCatch({
      readRDS("data/HLtraining.rds")
    }, error = function(e) {
      showNotification("Error loading training object. Ensure the file exists and is readable.", type = "error")
      return(NULL)
    })
  })
  
  # Observe the process data button
  observeEvent(input$process_data, {
    req(uploaded_data(), trainObj())
    showNotification("Processing started...", type = "message")
    show("progress")
    
    processed_result <- dataProcessingServer(
      "data_processing", 
      uploaded_data(), 
      input$process_data, 
      input$random_seed, 
      input$n_runs, 
      trainObj()
    )
    
    # After processing is done, render results
    observe({
      req(processed_result())
      if (is.list(processed_result()) && "predictions" %in% names(processed_result())) {
        processed_data(processed_result())
        output$results_table <- renderDT({
          datatable(head(processed_data()$predictions, 10))  # Show top 10 rows
        })
        showNotification("Processing complete!", type = "message")
      } else {
        showNotification("Error: Processed result is not valid or missing 'predictions'.", type = "error")
      }
      hide("progress")
    })
  })
  
  # Handle cancel button
  observeEvent(input$cancel_processing, {
    cancel_flag(TRUE)
    showNotification("Processing canceled.", type = "warning")
    hide("progress")
  })
  
  # Download processed data as CSV
  output$download_data <- downloadHandler(
    filename = function() { "predictions.csv" },
    content = function(file) {
      req(processed_data())
      write.csv(processed_data()$predictions, file)
    }
  )
  
  # Download example data as a ZIP file
  output$download_example_data <- downloadHandler(
    filename = function() { "example_data.zip" },
    content = function(file) {
      temp_file <- tempfile(fileext = ".zip")
      zip::zip(temp_file, c("data/example_data.csv", "data/example_data.rds"))
      file.copy(temp_file, file)
    }
  )
}

shinyApp(ui = ui, server = server)
