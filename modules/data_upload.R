# data_upload.R

dataUploadUI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("user_data"), "Upload your data (.csv or .rds)"),
    actionButton(ns("load_example_csv"), "Load Example CSV"),
    actionButton(ns("load_example_rds"), "Load Example RDS")
  )
}

dataUploadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Logic for file upload handling
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
    
    return(uploaded_data)
  })
}
