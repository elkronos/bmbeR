#' Generate Diagnostic Plots for Stan Model Fit
#'
#' This function produces diagnostic plots for a Stan model fit object, either as a traceplot or a histogram.
#'
#' @param model_fit A fitted Stan model object.
#' @param type The type of plot to generate. Can be "trace" (default) or "hist".
#'
#' @return Generates a diagnostic plot for the specified `model_fit`. If `type` is "trace", a traceplot is generated. If `type` is "hist", a histogram is generated.
#'
#' @examples
#' \dontrun{
#' # Assuming a fitted Stan model 'fit'
#' generate_plot(fit)
#' generate_plot(fit, type = "hist")
#' }
#'
#' @seealso \code{\link[rstan]{traceplot}}, \code{\link[rstan]{stan_hist}}
#' 
#' @importFrom rstan traceplot stan_hist rhat
#' 
#' @export
generate_plot <- function(model_fit, type = "trace") {
  if (!inherits(model_fit, "stanfit")) {
    stop("The provided model_fit is not a Stan model fit object.")
  }
  rhat_values <- rstan::rhat(model_fit)
  if (any(rhat_values > 1.1)) {
    warning("Some R-hat values are above 1.1, which might indicate lack of convergence.")
  }
  if (type == "trace") {
    rstan::traceplot(model_fit)
  } else if (type == "hist") {
    rstan::stan_hist(model_fit)
  } else {
    stop("Type should be either 'trace' or 'hist'.")
  }
}

#' Plot Posterior Distributions for Stan Model Fit
#'
#' This function visualizes the posterior distributions of a fitted Stan model using 95% credible intervals.
#'
#' @param model_fit A fitted Stan model object.
#'
#' @return Plots the 95% credible intervals for the parameters in the fitted Stan model.
#'
#' @examples
#' \dontrun{
#' # Assuming a fitted Stan model 'fit'
#' plot_posterior_distributions(fit)
#' }
#'
#' @seealso \code{\link[bayesplot]{mcmc_intervals}}, \code{\link[rstan]{extract}}
#' 
#' @importFrom rstan extract
#' @importFrom bayesplot mcmc_intervals
#' 
#' @export
plot_posterior_distributions <- function(model_fit) {
  posterior_samples <- rstan::extract(model_fit)
  bayesplot::mcmc_intervals(posterior_samples, prob = 0.95)
}