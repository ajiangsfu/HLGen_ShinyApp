mod_resultsDisplay_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Results Display"),
    p("This section displays the results of your data processing. Ensure your data matches the format of the provided example files for a smooth experience."),
    verbatimTextOutput(ns("results_summary"))
  )
}

mod_resultsDisplay_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$results_summary <- renderPrint({
      req(data())
      summary(data())
    })
  })
}
