# model_sensitivity.R
# =============================================================================
# This script defines a function to perform sensitivity analyses across 
# different prior configurations for Bayesian models. The sensitivity_analysis 
# function iterates through a list of prior configurations, fits a model for 
# each configuration, and computes a performance metric (using loo) for each 
# fitted model. This helps assess how changes in priors affect model outcomes.
#
# Dependencies:
#   - rstanarm (for model fitting via fit_model_with_prior())
#   - loo: For computing leave-one-out cross-validation metrics.
#   - 06_model_fitting.R: Contains the fit_model_with_prior() function.
#
# =============================================================================

# Ensure the loo package is available
if (!requireNamespace("loo", quietly = TRUE)) {
  install.packages("loo")
}
library(loo)

#' Run Sensitivity Analysis Across Prior Configurations
#'
#' This function fits Bayesian models using different prior configurations and 
#' computes a model performance metric (e.g., expected log predictive density from LOO) 
#' for each configuration. It helps assess how changes in prior specifications affect 
#' model outcomes.
#'
#' @param data A data.frame containing the dataset. All variables in the model 
#'   formula must be present.
#' @param formula A formula specifying the model (e.g., y ~ x1 + x2).
#' @param prior_configurations A list of prior configurations. Each element should be a 
#'   list with at least two elements:
#'     \item{label}{A descriptive name for the configuration (character).}
#'     \item{prior_config}{A list with two elements, \code{intercept} and \code{slope}, 
#'       each containing hyperparameter specifications. For example:
#'       \code{list(type = "student_t", nu = 3, mu = 0, sigma = 2.5)}.}
#' @param family A family object for the model (default is \code{gaussian()}).
#' @param chains Number of MCMC chains for model fitting (default is 4).
#' @param iter Number of iterations per chain for model fitting (default is 4000).
#' @param seed Random seed for reproducibility (default is 1234).
#' @param metric Character. The performance metric to extract from the LOO object 
#'   (default is "elpd_loo"). Other common metrics include "p_loo" or "looic".
#' @param ... Additional arguments passed to \code{fit_model_with_prior()}.
#'
#' @return A named list of performance metrics (one per prior configuration).
#'
#' @examples
#' \dontrun{
#'   # Define two prior configurations
#'   prior_config1 <- list(
#'     label = "StudentT_Default",
#'     prior_config = list(
#'       intercept = list(type = "student_t", nu = 3, mu = 0, sigma = 2.5),
#'       slope     = list(type = "student_t", nu = 3, mu = 0, sigma = 2.5)
#'     )
#'   )
#'
#'   prior_config2 <- list(
#'     label = "Normal_Stricter",
#'     prior_config = list(
#'       intercept = list(type = "normal", mu = 0, sigma = 1),
#'       slope     = list(type = "normal", mu = 0, sigma = 1)
#'     )
#'   )
#'
#'   # Run sensitivity analysis across the two configurations
#'   results <- sensitivity_analysis(data, y ~ x1 + x2,
#'                                   prior_configurations = list(prior_config1, prior_config2),
#'                                   family = gaussian())
#'   print(results)
#' }
#'
#' @export
sensitivity_analysis <- function(data, formula, prior_configurations, 
                                 family = gaussian(), 
                                 chains = 4, iter = 4000, seed = 1234,
                                 metric = "elpd_loo", ...) {
  
  results <- list()
  
  for (i in seq_along(prior_configurations)) {
    config <- prior_configurations[[i]]
    
    if (!("label" %in% names(config)) || !("prior_config" %in% names(config))) {
      stop("Each prior configuration must have 'label' and 'prior_config' elements.")
    }
    
    label <- config$label
    current_prior_config <- config$prior_config
    
    message(sprintf("Fitting model for configuration: '%s'", label))
    
    # Fit the Bayesian model using the current prior configuration.
    # This function is defined in model_fitting.R.
    fit <- fit_model_with_prior(data = data, formula = formula, family = family,
                                prior_config = current_prior_config,
                                chains = chains, iter = iter, seed = seed, ...)
    
    # Compute performance metric using LOO cross-validation.
    loo_obj <- loo::loo(fit, k_threshold = 0.7)
    
    # Extract the specified metric (e.g., "elpd_loo") from the LOO object.
    metric_value <- loo_obj$estimates[metric, "Estimate"]
    
    message(sprintf("Configuration '%s': %s = %0.2f", label, metric, metric_value))
    
    results[[label]] <- metric_value
  }
  
  return(results)
}
