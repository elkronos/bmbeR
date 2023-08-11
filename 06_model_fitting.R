#' Fit a Bayesian Linear Model with Specified Priors
#'
#' Fits a Bayesian generalized linear model using the `stan_glm` function with specified priors. Checks for convergence using the `check_convergence` function.
#'
#' @param prior List of prior distributions for predictors. If NULL, priors are generated based on specified parameters.
#' @param data Data frame containing the data to be used for the model fit.
#' @param formula Formula specifying the model to be fitted.
#' @param family A description of the error distribution and link function to be used in the model. Default is gaussian().
#' @param dist_types A named list indicating the type of prior distribution for each predictor. Default is student_t for both intercept and slope.
#' @param df A named list of degrees of freedom for the prior distributions. Default is 3 for both intercept and slope.
#' @param mean A named list of means for the prior distributions. Default is 0 for both intercept and slope.
#' @param sd A named list of standard deviations for the prior distributions. Default is 2.5 for both intercept and slope.
#' @param chains The number of Markov chains. Default is 4.
#' @param iter The total number of iterations for each chain. Default is 4000.
#' @param seed The random seed for reproducibility. Default is 1234.
#' @param ... Additional arguments to pass to `stan_glm`.
#'
#' @return A fitted model object if convergence is achieved. The function stops with an error message otherwise.
#'
#' @examples
#' \dontrun{
#' # Assuming a dataset 'data' and a formula 'response ~ predictor'
#' fit <- fit_model_with_prior(data = data, formula = response ~ predictor)
#' print(fit)
#' }
#'
#' @seealso \code{\link[rstanarm]{stan_glm}} for the main model fitting function.
#' 
#' @importFrom rstanarm stan_glm
#' @importFrom stats all.vars
#' 
#' @export
fit_model_with_prior <- function(prior = NULL, data, formula, family = gaussian(), 
                                 dist_types = list(intercept = "student_t", slope = "student_t"), 
                                 df = list(intercept = 3, slope = 3), mean = list(intercept = 0, slope = 0), 
                                 sd = list(intercept = 2.5, slope = 2.5),
                                 chains = 4, iter = 4000, seed = 1234, ...) {
  
  required_columns <- all.vars(formula)
  if (!all(required_columns %in% colnames(data))) {
    stop(paste("Data does not contain necessary columns for formula:", formula))
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
  
  fit <- stan_glm(formula, data = data, family = family,
                  prior = prior$intercept,
                  prior_intercept = prior$slope,
                  chains = chains, iter = iter, seed = seed,
                  control = list(adapt_delta = 0.95), ...)
  converged <- check_convergence(fit)
  if (!converged) {
    stop("Model did not converge. Adjust model parameters or inspect data.")
  }
  
  return(fit)
}