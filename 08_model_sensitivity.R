
define_prior <- function(dist_type, intercept_params = NULL, slope_params = NULL, use_empirical_bayes = FALSE) {
  # Validate the distribution type
  if (!(dist_type %in% c("student_t", "normal", "cauchy", "uniform", "beta", "gamma", "binomial", "poisson", "lognormal", "bernoulli"))) {
    stop(paste("Distribution type", dist_type, "is not supported."))
  }
  
  # Function to set default parameters for a distribution type
  set_default_params <- function(dist_type) {
    switch(dist_type,
           "student_t" = list(nu = 1, mu = 0, sigma = 1),
           "normal" = list(mu = 0, sigma = 1),
           "cauchy" = list(location = 0, scale = 1),
           "uniform" = list(min = 0, max = 1),
           "beta" = list(shape1 = 1, shape2 = 1),
           "gamma" = list(shape = 1, rate = 1),
           "binomial" = list(size = 1, prob = 0.5),
           "poisson" = list(lambda = 1),
           "lognormal" = list(meanlog = 0, sdlog = 1),
           "bernoulli" = list(prob = 0.5),
           stop("Invalid distribution type"))
  }
  
  # Apply default params if none provided
  intercept_params <- if (is.null(intercept_params)) set_default_params(dist_type) else intercept_params
  slope_params <- if (is.null(slope_params)) set_default_params(dist_type) else slope_params
  
  if (use_empirical_bayes) {
    # If empirical Bayes is used, ignore other parameters
    list(dist_type = dist_type, use_empirical = TRUE)
  } else {
    list(dist_type = dist_type, intercept_params = intercept_params, slope_params = slope_params, use_empirical = FALSE)
  }
}


sensitivity_analysis <- function(data, formula, prior_combinations,
                                 elpd_column = 'elpd_loo', estimate_column = 'Estimate') {
  results <- list()
  
  # Loop through each combination of priors
  for(i in seq_along(prior_combinations)) {
    prior_config <- prior_combinations[[i]]
    dist_type <- prior_config$dist_type
    
    current_prior <- get_prior_distribution(dist_type, prior_config$params)
    name <- paste0(prior_config$label, "_intercept_", names(prior_config$params$intercept)[1], "_slope_", names(prior_config$params$slope)[1])
    
    fit <- fit_model_with_prior(current_prior, data, formula)
    waic <- loo(fit, k_threshold = 0.7)$estimates[elpd_column, estimate_column]
    
    results[[name]] <- waic
  }
  
  return(results)
}