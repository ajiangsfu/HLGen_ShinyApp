# packages
require(parsnip)
require(dplyr)
require(h2o)
require(xgboost)
require(rpart)
require(tree)
require(randomForest)
require(ranger)
require(e1071)
require(kernlab)
require(class)
require(kknn)
require(klaR)
require(rstanarm)
require(LiblineaR)
require(parsnip)
require(neuralnet)
require(dplyr)
require(infotheo)
require(glmnet)
require(foreach)
require(iterators)
require(shape)

softmax <- function(scores) {
  exp_scores <- exp(scores)
  return(exp_scores / rowSums(exp_scores))
}

compute_majority_vote <- function(class_matrix) {
  apply(class_matrix, 1, function(x) names(sort(table(x), decreasing = TRUE))[1])
}

analyze_concordance <- function(classifications) {
  concordance_results <- list()
  
  # Check if the input is a data frame with at least two columns
  if (!is.data.frame(classifications) || ncol(classifications) < 2) {
    stop("Input must be a data frame with at least two columns.")
  }
  
  # Extract class columns
  class_cols <- colnames(classifications)
  
  # Calculate concordance rates
  concordance_matrix <- sapply(class_cols, function(col1) {
    sapply(class_cols, function(col2) {
      mean(classifications[[col1]] == classifications[[col2]])
    })
  })
  rownames(concordance_matrix) <- colnames(concordance_matrix) <- class_cols
  
  # Compute average concordance for each column
  average_concordance <- sapply(class_cols, function(col) {
    mean(concordance_matrix[col, class_cols])
  })
  
  # Add results to the list
  concordance_results$concordance_matrix <- concordance_matrix
  concordance_results$average_concordance <- average_concordance
  
  return(concordance_results)
}

mutual_info_importance <- function(data, response) {
  
  ## Remove UNCLASS 
  tmp <- which(response == "UNCLASS")
  if(length(tmp) > 0){
    response <- response[-tmp]
    data <- data[-tmp,]
  }
  
  # Ensure response is a factor for mutual information calculation
  if (!is.factor(response)) {
    response <- as.factor(response)
  }
  
  levels_response <- levels(response)  # Get response levels
  
  num_levels <- length(levels_response)
  
  mutual_info_matrix <- sapply(names(data), function(feature) {
    feature_data <- data[[feature]]
    
    if (is.numeric(feature_data)) {
      # Check if the numeric feature is binary
      if (all(feature_data %in% c(0, 1))) {
        feature_data <- as.factor(feature_data)  # Binary features as factors
      } else {
        feature_data <- discretize(feature_data)  # Discretize continuous features
      }
    } else {
      feature_data <- as.factor(feature_data)  # Convert categorical features to factors
    }
  
    sapply(levels_response, function(level) {
      # Create a binary response for the current level (1 vs. all)
      binary_response <- as.factor(response == level)
      mutinformation(feature_data, binary_response, method = "mm") # change on 20241022: set method to mm
    })
    
  })
  
  ## act
  mutual_info_df <- as.data.frame(t(mutual_info_matrix))
  colnames(mutual_info_df) <- levels_response
  rownames(mutual_info_df) <- names(data)
  return(mutual_info_df)

}

compute_accuracy <- function(predictions, true_values) {
  # Ensure that predictions and true_values are both factors (or both characters)
  if (is.data.frame(predictions)) {
    predictions <- predictions[, 1]
  }
  predictions <- as.factor(predictions)
  true_values <- as.factor(true_values)
  
  # Calculate the proportion of correct predictions
  mean(predictions == true_values)
}

calculate_median_prob <- function(classifications, probs, majority) {
  # Initialize an empty list to store medianProbOut
  medianProbOut <- list()
  
  # interesting, use a small example to test between "for" loop and "mapply" version, approach with "for" loop is actually faster! 
  # this is because within for loop, I only call basic mean and median functions. 
  #    The simplicity of the calculations means that the additional overhead of function calls in mapply likely outweighs any potential benefit.
  for (i in seq_along(majority)) {
    name <- names(majority)[i]
    vote <- majority[i]
    
    # Find the columns that support this vote
    supporting_vote <- colnames(classifications)[classifications[i, ] == vote]
    
    # Find the probabilities for these methods
    probs_supporting_vote <- probs[i, supporting_vote, drop = FALSE]
    
    # Calculate the median probability
    majority_median_prob <- median(probs_supporting_vote)
    
    ## add one more item on 20241010
    confidence_score <- mean(probs_supporting_vote)*(1-sd(probs_supporting_vote))
    
    # Store the result
    medianProbOut[[name]] <- list(
      supports = supporting_vote,
      probs = probs_supporting_vote,
      majority_median_prob = majority_median_prob,
      confidence_score = confidence_score
    )
  }
  return(medianProbOut)
}

ensemble_predictions <- function(results) {
  if (length(results) == 0) return(NULL)
  
  # Extract classifications and probabilities from each method
  all_classifications <- do.call(cbind, lapply(results, function(result) result$classification))
  all_probs <- do.call(cbind, lapply(results, function(result) result$prob))
  
  # Calculate majority vote for each entry
  majority_vote <- apply(all_classifications, 1, function(row) {
    names(which.max(table(row)))
  })
  
  majority_Probs <- calculate_median_prob(all_classifications, all_probs, majority_vote)
  
  majority_median_probs <- sapply(majority_Probs, function(xx){
    xx[[3]]
  })
  
  confidence_score <- sapply(majority_Probs, function(xx){
    xx[[4]]
  })
  
  ## Need the supporting rate as well
  supporting_rate <- sapply(1:nrow(all_classifications), function(i) {
    # Number of methods supporting the majority vote
    num_methods_support <- sum(all_classifications[i, ] == majority_vote[i])
    return(num_methods_support/ncol(all_classifications))
  })
  
  # Determine the final classification considering UNCLASS
  confident_classification <- sapply(1:nrow(all_classifications), function(i) {
    # Number of methods supporting the majority vote
    num_methods_support <- sum(all_classifications[i, ] == majority_vote[i])
    
    if ( confidence_score[i] < 0.4|| num_methods_support < (ncol(all_classifications) / 2)) {
      return("UNCLASS")
    } else {
      return(majority_vote[i])
    }
  })
  
  # Create a data frame with the results, ## change on 20241015
  results_df <- list(
    majority_vote = majority_vote,
    supporting_rate = supporting_rate,
    all_probs = all_probs,
    majority_Probs =  majority_Probs,
    majority_median_prob = majority_median_probs,
    confident_classification = confident_classification,
    confidence_score = confidence_score
  )
  
  return(results_df)
}


## Main prediction function
predict_from_model <- function(methods, new_data, seed = 123, n_runs = 10) {
  
  # Set random seed for reproducibility
  set.seed(seed)
  
  # Ensure new_data is a data frame
  new_data <- as.data.frame(new_data)
  
  method_results <- lapply(names(methods), function(method_name) {
    method <- methods[[method_name]]
    
    # Store predictions for multiple runs
    nndata <- dim(new_data)[1]
    all_predictions <- vector("list", n_runs)
    all_probs <- vector("list", n_runs)
    
    # Tried lapply, not much run time difference, so keep my original code with for loop.
    #  since the loop involves heavy computations like model fitting, the slight difference between for and lapply is negligible compared to
    #   the overall runtime of the operations inside the loop.
    for (i in seq_len(n_runs)) {
      # Set a new seed for each run to capture randomness
      set.seed(seed + i)
      
      # Fit the model
      fit <- if (is.function(method$fit)) {
        method$fit()
      } else {
        method$fit(method$model)
      }
      
      # Get predictions
      predictions <- method$predict(fit, new_data)
      predictions <- unlist(predictions)  # Assuming predictions is not a list
      
      # Get probabilities if available
      probs <- if (is.function(method$predict_prob)) {
        method$predict_prob(fit, new_data)
      } else {
        NULL  # Handle the case where predict_prob is not defined
      }
      
      # Store predictions for this run
      all_predictions[[i]] <- predictions
      all_probs[[i]] <- probs
    }
    
    class_outs <- do.call(cbind, all_predictions)
    prob_outs <- do.call(cbind, all_probs)
    
    colnames(class_outs) <- paste(method_name, 1:n_runs, sep = "_")
    colnames(prob_outs) <- colnames(class_outs)
    
    # Return the results in a list to maintain structure
    list(
      Method = method_name,
      classification = class_outs,
      prob = prob_outs
    )
  })
  
  names(method_results) <- names(methods)
  
  ## combine all classification together
  predictions_df <- lapply(method_results, function(xx){
    xx$classification
  })
  predictions_df <- data.frame(do.call(cbind, predictions_df))
  
  ## combine all prob together
  prob_df <- lapply(method_results, function(xx){
    xx$prob
  })
  prob_df <- data.frame(do.call(cbind, prob_df))
  
  # Generate ensemble predictions
  final_ensemble <- ensemble_predictions(method_results)
  
  # Feature importance based on mutual information
  feature_mutual_info <- mutual_info_importance(new_data, final_ensemble$confident_classification)
  
  ## changed on 20241121-22, 20241204
  ensemble_outs <- data.frame(
    majority_vote = final_ensemble$majority_vote,
    supporting_rate = final_ensemble$supporting_rate,
    majority_median_prob = final_ensemble$majority_median_prob,
    confidence_score = final_ensemble$confidence_score,
    prediction = final_ensemble$confident_classification,
    stringsAsFactors = FALSE
  )
  
  ## change class names on 20241121
  mapping <- c("1" = "CST", "2" = "CN913", "3" = "STB", "4" = "CN2P", "UNCLASS" = "UNCLASS")
  ensemble_outs$majority_vote <- mapping[ensemble_outs$majority_vote]
  ensemble_outs$prediction <- mapping[ensemble_outs$prediction]
  predictions_df <- cbind(predictions_df, ensemble_outs)
  
  # Return the final results
  return(list(
    predictions = ensemble_outs,
    feature_mutual_infos = feature_mutual_info,
    all_probs = prob_df,
    all_predictions = predictions_df
  ))
}

# Before I call the prediction function, I need to make changes on features if necessary, so I actually need to write a wrap function
## change on 20241021
getPredClass <- function(trainObj, new_data, seed = 123, n_runs = 10){
  
  ncols <- colnames(new_data)
  ocols <- colnames(trainObj$input_data)
  ocols <- setdiff(ocols, "trueClass")
  tmp <- setdiff(ocols, ncols)
  df <- data.frame(matrix(0, nrow = length(rownames(new_data)), ncol = length(tmp)))
  
  # Assign column names and row names
  colnames(df) <- tmp
  rownames(df) <- rownames(new_data)
  new_data <- cbind(new_data, df)
  print(dim(new_data))
  new_data <- new_data[,ocols]
  
  outs <- predict_from_model(trainObj$filtered_methods, new_data = new_data, seed = seed, n_runs = n_runs)

  return(outs)
  
}
