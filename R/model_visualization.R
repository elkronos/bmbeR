# model_visualization.R
# =============================================================================
# This script defines functions for generating diagnostic plots and visualizations 
# of posterior distributions for Bayesian models. The functions support both rstan 
# and rstanarm fitted objects.
#
# Dependencies:
#   - rstan: Provides functions such as traceplot, stan_hist, stan_dens, stan_ac.
#   - bayesplot: Provides mcmc_intervals for visualizing posterior intervals.
#   - ggplot2: Used for generating and printing ggplot objects.
#
# =============================================================================

# Ensure necessary packages are installed and loaded
if (!requireNamespace("rstan", quietly = TRUE)) {
  install.packages("rstan")
}
library(rstan)

if (!requireNamespace("bayesplot", quietly = TRUE)) {
  install.packages("bayesplot")
}
library(bayesplot)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)


# -----------------------------------------------------------------------------
# Generate Diagnostic Plots for a Fitted Model
# -----------------------------------------------------------------------------
#' Generate Diagnostic Plots for a Fitted Bayesian Model
#'
#' This function generates a series of diagnostic plots for a fitted Bayesian 
#' model. Available plot types include:
#'   - "trace": Trace plots for MCMC chains.
#'   - "hist": Histograms of posterior samples.
#'   - "density": Density plots of the posterior samples.
#'   - "autocorrelation": Autocorrelation plots of the MCMC chains.
#'
#' The function supports both rstan (stanfit objects) and rstanarm (stanreg 
#' objects) by extracting the underlying stanfit object when needed.
#'
#' @param model_fit A fitted Bayesian model object (stanfit or stanreg).
#' @param types A character vector specifying which plot types to generate.
#'   Available options: "trace", "hist", "density", "autocorrelation". Default is 
#'   c("trace", "hist", "density", "autocorrelation").
#' @param ... Additional arguments passed to the underlying plotting functions.
#'
#' @return No return value; the function displays the requested plots.
#' @export
generate_plot <- function(model_fit, 
                          types = c("trace", "hist", "density", "autocorrelation"), 
                          ...) {
  # If model_fit is a stanreg object (from rstanarm), extract the underlying stanfit
  if (inherits(model_fit, "stanreg")) {
    model_fit <- model_fit$stanfit
  }
  
  # Verify that model_fit is a stanfit object
  if (!inherits(model_fit, "stanfit")) {
    stop("The provided model_fit object is not a stanfit object or does not contain a stanfit object.")
  }
  
  # Generate each requested plot type
  for (plot_type in types) {
    if (plot_type == "trace") {
      message("Generating trace plots...")
      rstan::traceplot(model_fit, ...)
    } else if (plot_type == "hist") {
      message("Generating histogram plots...")
      rstan::stan_hist(model_fit, ...)
    } else if (plot_type == "density") {
      message("Generating density plots...")
      rstan::stan_dens(model_fit, ...)
    } else if (plot_type == "autocorrelation") {
      message("Generating autocorrelation plots...")
      rstan::stan_ac(model_fit, ...)
    } else {
      stop(sprintf("Invalid plot type: '%s'. Available types: 'trace', 'hist', 'density', 'autocorrelation'.", plot_type))
    }
  }
}


# -----------------------------------------------------------------------------
# Visualize Posterior Distributions with 95% Credible Intervals
# -----------------------------------------------------------------------------
#' Plot Posterior Distributions with 95% Credible Intervals
#'
#' This function visualizes the posterior distributions of a fitted Bayesian model
#' by extracting the MCMC samples and generating interval plots using bayesplot.
#'
#' @param model_fit A fitted Bayesian model object (stanfit or stanreg).
#' @param prob Numeric. The probability mass to display in the intervals (default is 0.95).
#' @param ... Additional arguments passed to bayesplot::mcmc_intervals.
#'
#' @return A ggplot object showing the posterior intervals.
#' @export
plot_posterior_distributions <- function(model_fit, prob = 0.95, ...) {
  # If model_fit is a stanreg object (from rstanarm), extract the underlying stanfit
  if (inherits(model_fit, "stanreg")) {
    model_fit <- model_fit$stanfit
  }
  
  # Verify that model_fit is a stanfit object
  if (!inherits(model_fit, "stanfit")) {
    stop("The provided model_fit object is not a stanfit object or does not contain a stanfit object.")
  }
  
  # Extract posterior samples
  posterior_samples <- rstan::extract(model_fit)
  
  # Generate and print the intervals plot using bayesplot
  p <- bayesplot::mcmc_intervals(posterior_samples, prob = prob, ...)
  print(p)
  invisible(p)
}
