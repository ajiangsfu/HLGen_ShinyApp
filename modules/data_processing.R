
dataProcessingServer <- function(id, uploaded_data, process_button, random_seed, n_runs, trainObj) {
  processed_result <- reactiveVal(NULL)
  moduleServer(id, function(input, output, session) {
    h2o.init(max_mem_size = "512m")

    observeEvent(process_button, {
      # Ensure we are working with actual data
      data_to_use <- uploaded_data

      # Perform data processing and store result in processed_result
      processed_data <- tryCatch({
        print("Starting data processing...")

        # Example of how the processing might work (update to your actual logic)
        data_out <- getPredClass(
          trainObj = trainObj,
          new_data = data_to_use,
          seed = random_seed,
          n_runs = n_runs
        )

        # Store result in reactive value
        processed_result(data_out)

        # Shut down H2O after processing
        h2o.shutdown(prompt = FALSE)

        return(data_out)
      }, error = function(e) {
        message("Error during processing: ", e$message)
        processed_result(NULL)  # Set to NULL on error
        return(NULL)
      })

      print("Processed result stored.")
      print(str(processed_result()))  # Debug print processed result

    })


  })
  # Return the reactive value to be accessed
  return(processed_result)
}

