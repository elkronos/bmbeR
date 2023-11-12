empirical_bayes_priors <- function(data, formula, dist_types = list(intercept = "student_t", slope = "student_t"), df = 3) {
  # Check for missing values in the data
  if (any(is.na(data))) {
    stop("Data contains missing values. Please remove or impute them before proceeding.")
  }
  
  # Validate the formula
  required_columns <- all.vars(formula)
  if (!all(required_columns %in% colnames(data))) {
    stop(paste("Data does not contain necessary columns for formula:", formula))
  }
  
  # Use memoisation for caching lm() results
  lm_cache <- memoise::memoise(lm)
  
  # Fit the linear model and extract coefficients and standard errors
  prior_model <- lm_cache(formula, data = data)
  prior_coefs <- coef(prior_model)
  prior_ses <- sqrt(diag(vcov(prior_model)))
  
  # Create prior distributions for each predictor
  prior_list <- list()
  for (predictor in names(prior_coefs)) {
    dist_type <- dist_types[[predictor]] %||% "student_t"
    params <- list(df = df, mean = prior_coefs[predictor], sd = prior_ses[predictor])
    prior_list[[predictor]] <- get_prior_distribution(dist_type, params)
  }
  
  return(prior_list)
}