# model_evaluation.R
# =============================================================================
# This script defines functions to evaluate the performance of fitted Bayesian
# models. It supports both regression and classification tasks.
#
# For regression, it computes metrics such as RMSE (Root Mean Squared Error)
# and MAE (Mean Absolute Error). For classification, it computes accuracy,
# precision, recall, and F1 Score.
#
# This script is designed to integrate seamlessly with the rest of your
# workflow (including prior distributions, model fitting, convergence checks,
# and sensitivity analysis).
#
# =============================================================================

# -----------------------------------------------------------------------------
# Evaluate Model Performance on Test Data
# -----------------------------------------------------------------------------
#' Evaluate Model Performance on Test Data
#'
#' This function evaluates the performance of a fitted Bayesian model using test
#' data and a specified formula. For regression tasks, it computes RMSE and MAE.
#' For classification tasks, it computes accuracy, precision, recall, and F1 Score.
#'
#' @param fit A fitted model object (e.g., from rstanarm).
#' @param data_test A data.frame containing the test data. All variables in the model
#'   formula must be present.
#' @param formula A formula specifying the model (e.g., y ~ x1 + x2). The response variable
#'   is assumed to be on the left-hand side.
#' @param threshold Numeric. A threshold for converting predicted values to class labels
#'   (default is 0.5) for classification tasks.
#' @param analysis_type Character. Specify "regression" or "classification".
#'
#' @return A list of performance metrics:
#'   For regression: RMSE and MAE.
#'   For classification: Accuracy, Precision, Recall, and F1 Score.
#'
#' @examples
#' \dontrun{
#'   # Regression evaluation
#'   metrics_reg <- evaluate_model_performance(fit, test_data, y ~ x1 + x2, analysis_type = "regression")
#'   print(metrics_reg)
#'
#'   # Classification evaluation
#'   metrics_class <- evaluate_model_performance(fit, test_data, y ~ x1 + x2,
#'                                               threshold = 0.5, analysis_type = "classification")
#'   print(metrics_class)
#' }
#'
#' @export
evaluate_model_performance <- function(fit, data_test, formula, threshold = 0.5, analysis_type = "regression") {
  # Ensure that test data contains all variables in the formula
  required_columns <- all.vars(formula)
  if (!all(required_columns %in% colnames(data_test))) {
    stop(sprintf("Test data does not contain necessary columns for formula: %s", deparse(formula)))
  }
  
  # Extract response variable name from the formula
  response_var <- as.character(formula[[2]])
  
  # Get actual response values from test data
  y_actual <- data_test[[response_var]]
  
  # Obtain predicted values using the model's predict method
  y_pred <- as.vector(predict(fit, newdata = data_test))
  
  # Evaluate performance based on the analysis type
  if (analysis_type == "regression") {
    rmse <- sqrt(mean((y_actual - y_pred)^2))
    mae  <- mean(abs(y_actual - y_pred))
    return(list(RMSE = rmse, MAE = mae))
    
  } else if (analysis_type == "classification") {
    # Convert predictions to binary class labels using the specified threshold
    y_pred_class <- ifelse(y_pred > threshold, 1, 0)
    
    # Convert actual values to binary class labels if not already numeric
    if (!is.numeric(y_actual)) {
      # Convert factor to numeric: assumes first level is 0, second is 1
      y_actual <- as.numeric(as.factor(y_actual)) - 1
    }
    
    # Compute Accuracy
    accuracy <- mean(y_actual == y_pred_class)
    
    # Compute Precision, Recall, and F1 Score
    TP <- sum(y_pred_class == 1 & y_actual == 1)
    FP <- sum(y_pred_class == 1 & y_actual == 0)
    FN <- sum(y_pred_class == 0 & y_actual == 1)
    
    precision <- if ((TP + FP) == 0) NA else TP / (TP + FP)
    recall    <- if ((TP + FN) == 0) NA else TP / (TP + FN)
    f1_score  <- if (is.na(precision) || is.na(recall) || (precision + recall) == 0) NA 
    else 2 * precision * recall / (precision + recall)
    
    return(list(Accuracy = accuracy, Precision = precision, Recall = recall, F1_Score = f1_score))
    
  } else {
    stop("Unsupported analysis_type. Use 'regression' or 'classification'.")
  }
}
