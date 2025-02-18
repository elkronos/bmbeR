# model_fitting.R
# =============================================================================
# This script defines functions to fit Bayesian regression models using 
# rstanarm::stan_glm. It integrates prior specifications for the intercept 
# and slopes based on user-specified hyperparameters.
#
# Supported prior distributions for coefficients (for rstanarm use) are:
#   - "student_t": constructed via rstanarm::student_t()
#   - "normal":    constructed via rstanarm::normal()
#   - "cauchy":    constructed via rstanarm::cauchy()
#
# The script also checks convergence using the check_convergence() function.
#
# Dependencies:
#   - rstanarm, rstan, parallel
#   - model_convergence.R (for check_convergence())
#   - utilities.R and distributions.R (for any additional helpers)
# =============================================================================

# Load required packages
if (!requireNamespace("rstanarm", quietly = TRUE)) {
  install.packages("rstanarm")
}
library(rstanarm)

if (!requireNamespace("rstan", quietly = TRUE)) {
  install.packages("rstan")
}
library(rstan)

if (!requireNamespace("parallel", quietly = TRUE)) {
  install.packages("parallel")
}
library(parallel)

# It is assumed that check_convergence() is defined in model_convergence.R;
# ensure that script is sourced prior to calling fit_model_with_prior().
# For example: source("model_convergence.R")

# -----------------------------------------------------------------------------
# Build Stanarm Prior Objects from Hyperparameter Specifications
# -----------------------------------------------------------------------------
#' Build Prior Objects for rstanarm
#'
#' Converts user-specified hyperparameter lists for the intercept and slopes into
#' prior objects for rstanarm::stan_glm.
#'
#' @param intercept_config A list with elements:
#'   - type: one of "student_t", "normal", or "cauchy"
#'   - For "student_t": nu, mu, sigma
#'   - For "normal": mu, sigma
#'   - For "cauchy": location, scale
#' @param slope_config A list with the same structure as intercept_config.
#'
#' @return A list with elements:
#'   - prior_intercept: prior object for the intercept
#'   - prior: prior object for the slopes
#'
#' @export
build_stanarm_priors <- function(intercept_config, slope_config) {
  # Build prior for intercept
  if (intercept_config$type == "student_t") {
    prior_intercept <- rstanarm::student_t(df = intercept_config$nu,
                                           location = intercept_config$mu,
                                           scale = intercept_config$sigma)
  } else if (intercept_config$type == "normal") {
    prior_intercept <- rstanarm::normal(location = intercept_config$mu,
                                        scale = intercept_config$sigma)
  } else if (intercept_config$type == "cauchy") {
    prior_intercept <- rstanarm::cauchy(location = intercept_config$location,
                                        scale = intercept_config$scale)
  } else {
    stop(sprintf("Unsupported distribution type for intercept: '%s'. Supported types: 'student_t', 'normal', 'cauchy'.",
                 intercept_config$type))
  }
  
  # Build prior for slopes
  if (slope_config$type == "student_t") {
    prior <- rstanarm::student_t(df = slope_config$nu,
                                 location = slope_config$mu,
                                 scale = slope_config$sigma)
  } else if (slope_config$type == "normal") {
    prior <- rstanarm::normal(location = slope_config$mu,
                              scale = slope_config$sigma)
  } else if (slope_config$type == "cauchy") {
    prior <- rstanarm::cauchy(location = slope_config$location,
                              scale = slope_config$scale)
  } else {
    stop(sprintf("Unsupported distribution type for slopes: '%s'. Supported types: 'student_t', 'normal', 'cauchy'.",
                 slope_config$type))
  }
  
  list(prior_intercept = prior_intercept, prior = prior)
}


# -----------------------------------------------------------------------------
# Fit Bayesian Model with Specified Priors
# -----------------------------------------------------------------------------
#' Fit Bayesian Model with User-Specified Priors
#'
#' Fits a Bayesian regression model using rstanarm::stan_glm with user-specified 
#' prior distributions for the intercept and slopes. If no prior configuration is 
#' provided, default priors are used (student_t with df = 3, location = 0, scale = 2.5).
#'
#' @param data A data.frame containing the dataset.
#' @param formula A formula specifying the model (e.g., y ~ x1 + x2).
#' @param family A family object (default is gaussian()).
#' @param prior_config A list with two elements, "intercept" and "slope", each of 
#'   which is a list of hyperparameters (see details in build_stanarm_priors()). If 
#'   NULL, default priors are used.
#' @param chains Number of MCMC chains (default is 4).
#' @param iter Number of iterations per chain (default is 4000).
#' @param seed Random seed (default is 1234).
#' @param ... Additional arguments passed to rstanarm::stan_glm.
#'
#' @return A fitted stanreg model object.
#'
#' @export
fit_model_with_prior <- function(data, formula, family = gaussian(), 
                                 prior_config = NULL,
                                 chains = 4, iter = 4000, seed = 1234, ...) {
  # Ensure that all variables in the formula exist in data
  required_columns <- all.vars(formula)
  if (!all(required_columns %in% colnames(data))) {
    stop(sprintf("Data does not contain necessary columns for formula: %s", deparse(formula)))
  }
  
  # Set default prior configuration if not provided
  if (is.null(prior_config)) {
    default_prior <- list(type = "student_t", nu = 3, mu = 0, sigma = 2.5)
    prior_config <- list(intercept = default_prior, slope = default_prior)
  } else {
    if (!("intercept" %in% names(prior_config)) || !("slope" %in% names(prior_config))) {
      stop("prior_config must be a list with 'intercept' and 'slope' elements.")
    }
  }
  
  # Build the prior objects using the helper function
  priors <- build_stanarm_priors(prior_config$intercept, prior_config$slope)
  
  # Basic compatibility check between family and dependent variable type
  dependent_var <- all.vars(formula)[1]
  dependent_type <- class(data[[dependent_var]])
  if (family$family == "gaussian" && !(dependent_type %in% c("numeric", "integer"))) {
    stop(sprintf("Dependent variable '%s' must be numeric/integer for gaussian family.", dependent_var))
  } else if (family$family == "binomial" && !(dependent_type %in% c("numeric", "factor"))) {
    stop(sprintf("Dependent variable '%s' is not appropriate for binomial family.", dependent_var))
  }
  
  # Fit the Bayesian model using stan_glm
  fit <- rstanarm::stan_glm(formula, data = data, family = family,
                            prior_intercept = priors$prior_intercept,
                            prior = priors$prior,
                            chains = chains, iter = iter, seed = seed,
                            cores = parallel::detectCores(), 
                            ...)
  
  # Check convergence using the check_convergence() function.
  # It is assumed that check_convergence() is available in the session.
  if (!check_convergence(fit)) {
    stop("Model did not converge. Please review diagnostics and adjust parameters.")
  }
  
  return(fit)
}
