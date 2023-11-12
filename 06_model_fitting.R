fit_model_with_prior <- function(prior = NULL, data, formula, family = gaussian(), 
                                 dist_types = list(intercept = "student_t", slope = "student_t"), 
                                 df = list(intercept = 3, slope = 3), mean = list(intercept = 0, slope = 0), 
                                 sd = list(intercept = 2.5, slope = 2.5),
                                 chains = 4, iter = 4000, seed = 1234, ...) {
  
  required_columns <- all.vars(formula)
  if (!all(required_columns %in% colnames(data))) {
    stop(paste("Data does not contain necessary columns for formula:", formula))
  }
  
  # Check if family is compatible with the dependent variable type
  dependent_var <- all.vars(formula)[1]
  dependent_var_type <- class(data[[dependent_var]])
  if (!compatible_family(family, dependent_var_type)) {
    stop(paste("Incompatible family for the type of", dependent_var, ":", dependent_var_type))
  }
  
  predictor_names <- all.vars(formula)[-1] # Exclude the response variable
  predictor_names <- c("intercept", predictor_names)
  
  if (is.null(prior)) {
    prior <- list()
    for (predictor in predictor_names) {
      prior[[predictor]] <- get_prior_distribution(dist_types[[predictor]], 
                                                   list(df = df[[predictor]], mean = mean[[predictor]], sd = sd[[predictor]]))
    }
  }
  
  # Enable parallel processing of chains
  fit <- stan_glm(formula, data = data, family = family,
                  prior = prior$intercept,
                  prior_intercept = prior$slope,
                  chains = chains, iter = iter, seed = seed,
                  control = list(adapt_delta = 0.95), cores = parallel::detectCores(), ...)
  converged <- check_convergence(fit)
  if (!converged) {
    stop("Model did not converge. Adjust model parameters or inspect data.")
  }
  
  return(fit)
}

compatible_family <- function(family, var_type) {
  # This function checks if the family is compatible with the dependent variable type
  # This is a simplified example; more thorough checks based on the specific model and data may be needed
  if ((family == gaussian()) & (var_type %in% c("numeric", "integer"))) {
    return(TRUE)
  } else if ((family == binomial()) & (var_type == "factor")) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
