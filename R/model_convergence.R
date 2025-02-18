# model_convergence.R
# =============================================================================
# This script defines a function to check convergence diagnostics for a fitted
# Bayesian model. It supports models fitted with rstan (stanfit objects) and 
# rstanarm (stanreg objects) by extracting the underlying stanfit when needed.
#
# The function calculates Rhat and effective sample size (ESS) ratios and 
# provides options to generate trace plots for diagnosing convergence issues.
#
# Dependencies:
#   - rstan: Provides rstan::rhat(), rstan::neff_ratio(), and rstan::traceplot().
#
# =============================================================================

# Ensure that rstan is installed and loaded
if (!requireNamespace("rstan", quietly = TRUE)) {
  install.packages("rstan")
}
library(rstan)

#' Check Model Convergence
#'
#' Checks convergence diagnostics for a fitted Bayesian model. The function
#' computes Rhat and effective sample size (ESS) ratios and issues a warning if
#' any Rhat values exceed a specified threshold or if any ESS ratios fall below a
#' specified threshold. Optionally, it can generate and save trace plots.
#'
#' @param model_fit A fitted Bayesian model object. This can be either a 
#'   stanfit object (from rstan) or a stanreg object (from rstanarm).
#' @param rhat_threshold Numeric. The threshold above which Rhat values are 
#'   considered problematic (default is 1.1).
#' @param ess_threshold Numeric. The threshold below which effective sample size
#'   (ESS) ratios are considered problematic (default is 0.1).
#' @param save_plots Logical. If TRUE, trace plots will be saved as a PDF file.
#' @param plot_path Character. The file path where trace plots will be saved 
#'   (default is "trace_plot.pdf").
#'
#' @return Logical. Returns TRUE if convergence diagnostics are acceptable;
#'   otherwise, returns FALSE.
#'
#' @examples
#' \dontrun{
#'   # Example usage with a fitted model 'fit'
#'   is_converged <- check_convergence(fit, rhat_threshold = 1.1, ess_threshold = 0.1,
#'                                     save_plots = TRUE, plot_path = "my_trace_plots.pdf")
#'   if (!is_converged) {
#'     warning("Model did not converge. Please review trace plots and diagnostics.")
#'   }
#' }
#'
#' @export
check_convergence <- function(model_fit, 
                              rhat_threshold = 1.1, 
                              ess_threshold = 0.1, 
                              save_plots = FALSE, 
                              plot_path = "trace_plot.pdf") {
  # If model_fit is a rstanarm object (class "stanreg"), extract the underlying stanfit
  if (inherits(model_fit, "stanreg")) {
    model_fit <- model_fit$stanfit
  }
  
  # Compute Rhat and effective sample size (ESS) ratios
  rhat_values <- rstan::rhat(model_fit)
  ess_values  <- rstan::neff_ratio(model_fit)
  
  convergence_issue <- FALSE
  msg <- "Potential lack of convergence detected:"
  
  # Check Rhat values
  if (any(rhat_values > rhat_threshold, na.rm = TRUE)) {
    msg <- paste(msg, "Rhat values exceed", rhat_threshold, ".")
    convergence_issue <- TRUE
  }
  
  # Check ESS values
  if (any(ess_values < ess_threshold, na.rm = TRUE)) {
    msg <- paste(msg, "ESS values below", ess_threshold, ".")
    convergence_issue <- TRUE
  }
  
  if (convergence_issue) {
    warning(msg, " Review trace plots and diagnostics.")
    if (save_plots) {
      pdf(plot_path)
      rstan::traceplot(model_fit)
      dev.off()
      message("Trace plots saved to: ", plot_path)
    } else {
      rstan::traceplot(model_fit)
    }
    return(FALSE)
  }
  
  return(TRUE)
}
