evaluate_model_performance <- function(fit, data_test, formula, threshold = 0.5, analysis_type = "regression") {
  # Check if test data contains necessary columns
  required_columns <- all.vars(formula)
  if (!all(required_columns %in% colnames(data_test))) {
    stop(paste("Test data does not contain necessary columns for formula:", deparse(formula)))
  }
  
  # Efficient extraction of response variable
  y_actual_var <- formula[[2]]
  y_actual <- data_test[[as.character(y_actual_var)]]
  y_pred <- as.vector(predict(fit, newdata = data_test))
  
  # Calculate performance metrics
  if (analysis_type == "regression") {
    rmse <- sqrt(mean((y_actual - y_pred) ^ 2))
    mae <- mean(abs(y_actual - y_pred))
    return(list(RMSE = rmse, MAE = mae))
  } else if (analysis_type == "classification") {
    y_pred_class <- ifelse(y_pred > threshold, 1, 0)
    y_actual_class <- as.factor(y_actual > threshold)
    acc <- mean(y_actual_class == y_pred_class)
    precision <- mean(y_pred_class[y_actual_class == 1])
    recall <- mean(y_actual_class[y_pred_class == 1])
    f1_score <- 2 * (precision * recall) / (precision + recall)
    return(list(Accuracy = acc, Precision = precision, Recall = recall, F1_Score = f1_score))
  } else {
    stop("Unsupported analysis_type. Use 'regression' or 'classification'.")
  }
}
