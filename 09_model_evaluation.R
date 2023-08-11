#' Evaluate Model Performance
#'
#' This function evaluates the performance of a fitted model on a test dataset 
#' using either regression or classification metrics, depending on the analysis type.
#'
#' @param fit A model object for which predictions are made on the test dataset.
#' @param data_test A dataframe containing the test dataset.
#' @param formula An object of class "formula" indicating the model specification.
#' @param threshold A numeric threshold for classification; predictions above this value are classified as '1', and below as '0'. Default is 0.5.
#' @param analysis_type A character string indicating the type of analysis, either 'regression' or 'classification'. Default is 'regression'.
#'
#' @return A list containing the root mean square error (RMSE) for regression or accuracy for classification.
#'
#' @examples
#' \dontrun{
#' data_train <- data.frame(y = rnorm(80), x = rnorm(80))
#' data_test <- data.frame(y = rnorm(20), x = rnorm(20))
#' formula <- y ~ x
#' fit <- lm(formula, data = data_train)
#' performance <- evaluate_model_performance(fit, data_test, formula)
#' }
#'
#' @export
evaluate_model_performance <- function(fit, data_test, formula, threshold = 0.5, analysis_type = "regression") {
  required_columns <- all.vars(formula)
  if (!all(required_columns %in% colnames(data_test))) {
    stop(paste("Test data does not contain necessary columns for formula:", formula))
  }
  y_pred <- as.vector(predict(fit, newdata = data_test))
  
  # Improved extraction of response variable
  y_actual_var <- all.vars(formula)[1]
  
  if (analysis_type == "regression") {
    y_actual <- data_test[[y_actual_var]]
    rmse <- sqrt(mean((y_actual - y_pred) ^ 2))
    return(list(RMSE = rmse))
  } else if (analysis_type == "classification") {
    y_pred <- ifelse(y_pred > threshold, 1, 0)
    y_actual <- as.factor(data_test[[y_actual_var]])
    acc <- mean(y_actual == y_pred)
    return(list(Accuracy = acc))
  } else {
    stop("Unsupported analysis_type. Use 'regression' or 'classification'.")
  }
}