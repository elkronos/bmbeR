# empirical_bayes.R
# =============================================================================
# This script defines the function `empirical_bayes_priors`, which computes 
# empirical Bayesian hyperparameter specifications for priors from a given 
# dataset and model formula.
#
# It fits a linear model (using memoisation for efficiency), extracts the 
# regression coefficients and their standard errors, and then, for each predictor,
# maps these estimates to hyperparameters for a specified prior distribution.
#
# Supported distribution types and their mappings:
#  - "student_t": returns list(mu, sigma, nu = df)
#  - "normal":    returns list(mu, sigma)
#  - "cauchy":    returns list(location, scale)
#  - "uniform":   returns list(min = coef - 2*se, max = coef + 2*se)
#  - "beta":      requires coef in [0,1], returns list(shape1, shape2) where
#                 shape1 = coef * k, shape2 = (1 - coef) * k (k = 10 by default)
#  - "gamma":     requires coef > 0, returns list(shape, rate) via moment matching
#  - "binomial":  requires coef in [0,1], returns list(size, prob) where size is 
#                 derived from se (size = round(1/se^2))
#  - "poisson":   requires coef > 0, returns list(lambda = coef)
#  - "lognormal": requires coef > 0, returns list(meanlog = log(coef), sdlog = se/coef)
#  - "bernoulli": requires coef in [0,1], returns list(prob = coef)
#
# Dependencies:
#   - memoise (for caching lm() results)
#   - utilities.R (for validation functions)
#
# =============================================================================

# Ensure memoise is available
if (!requireNamespace("memoise", quietly = TRUE)) {
  install.packages("memoise")
}
library(memoise)

#' Compute Empirical Bayesian Hyperparameters for Priors
#'
#' This function fits a linear model using the supplied data and formula,
#' then extracts the coefficients and their standard errors to generate a list
#' of hyperparameter specifications for each predictor. The output hyperparameters
#' are meant to serve as inputs for constructing prior distributions.
#'
#' @param data A data.frame containing the dataset. All variables in the model 
#'   formula must be present.
#' @param formula A formula specifying the linear model (e.g., y ~ x1 + x2).
#' @param dist_types A named list specifying the desired distribution type for 
#'   each predictor. For example, \code{list(`(Intercept)` = "student_t", x1 = "normal")}.
#'   If a predictor is not specified, it defaults to "student_t".
#' @param df Numeric. Degrees of freedom for the Student's t prior (default is 3).
#'
#' @return A named list where each element corresponds to a predictor from the 
#'   linear model. Each element is itself a list of hyperparameters that can be 
#'   used to construct a prior specification.
#'
#' @examples
#' \dontrun{
#'   set.seed(123)
#'   data <- data.frame(y = rnorm(100), x = rnorm(100))
#'   priors <- empirical_bayes_priors(data, y ~ x,
#'                                   dist_types = list(`(Intercept)` = "student_t", x = "normal"),
#'                                   df = 3)
#'   print(priors)
#' }
#'
#' @export
empirical_bayes_priors <- function(data, formula, 
                                   dist_types = list(`(Intercept)` = "student_t"),
                                   df = 3) {
  # Check for missing values in the data
  if (any(is.na(data))) {
    stop("Data contains missing values. Please remove or impute them before proceeding.")
  }
  
  # Ensure that all variables in the formula are present in the data
  required_columns <- all.vars(formula)
  if (!all(required_columns %in% colnames(data))) {
    stop(sprintf("Data does not contain necessary columns for formula: %s", deparse(formula)))
  }
  
  # Cache the lm() function using memoise for efficiency
  lm_cache <- memoise(lm)
  
  # Fit the linear model using the provided formula and data
  prior_model <- lm_cache(formula, data = data)
  prior_coefs <- coef(prior_model)
  prior_ses   <- sqrt(diag(vcov(prior_model)))
  
  # Create a list to store hyperparameter specifications for each predictor
  hyperparams <- list()
  
  for (predictor in names(prior_coefs)) {
    coef_val <- prior_coefs[predictor]
    se_val   <- prior_ses[predictor]
    
    # Determine the distribution type for this predictor (default to "student_t")
    current_dist <- if (!is.null(dist_types[[predictor]])) {
      dist_types[[predictor]]
    } else {
      "student_t"
    }
    
    hyperparams[[predictor]] <- switch(current_dist,
                                       student_t = {
                                         list(mu = coef_val, sigma = se_val, nu = df)
                                       },
                                       normal = {
                                         list(mu = coef_val, sigma = se_val)
                                       },
                                       cauchy = {
                                         list(location = coef_val, scale = se_val)
                                       },
                                       uniform = {
                                         list(min = coef_val - 2 * se_val, max = coef_val + 2 * se_val)
                                       },
                                       beta = {
                                         if (coef_val < 0 || coef_val > 1) {
                                           stop(sprintf("For beta prior, coefficient for %s must be in [0, 1].", predictor))
                                         }
                                         k <- 10  # Arbitrary scaling factor; adjust as needed
                                         list(shape1 = coef_val * k, shape2 = (1 - coef_val) * k)
                                       },
                                       gamma = {
                                         if (coef_val <= 0) {
                                           stop(sprintf("For gamma prior, coefficient for %s must be positive.", predictor))
                                         }
                                         shape <- (coef_val / se_val)^2
                                         rate <- coef_val / (se_val^2)
                                         list(shape = shape, rate = rate)
                                       },
                                       binomial = {
                                         if (coef_val < 0 || coef_val > 1) {
                                           stop(sprintf("For binomial prior, coefficient for %s must be in [0, 1].", predictor))
                                         }
                                         size <- round(1 / (se_val^2))
                                         list(size = size, prob = coef_val)
                                       },
                                       poisson = {
                                         if (coef_val <= 0) {
                                           stop(sprintf("For poisson prior, coefficient for %s must be positive.", predictor))
                                         }
                                         list(lambda = coef_val)
                                       },
                                       lognormal = {
                                         if (coef_val <= 0) {
                                           stop(sprintf("For lognormal prior, coefficient for %s must be positive.", predictor))
                                         }
                                         list(meanlog = log(coef_val), sdlog = se_val / coef_val)
                                       },
                                       bernoulli = {
                                         if (coef_val < 0 || coef_val > 1) {
                                           stop(sprintf("For bernoulli prior, coefficient for %s must be in [0, 1].", predictor))
                                         }
                                         list(prob = coef_val)
                                       },
                                       {
                                         stop(sprintf("Distribution type '%s' is not supported for empirical Bayes priors.", current_dist))
                                       }
    )
  }
  
  return(hyperparams)
}
