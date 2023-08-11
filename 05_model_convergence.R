#' Check Convergence of a Stan Model Fit
#'
#' Reviews the convergence of a Stan model by examining the $\hat{R}$ and effective sample size (ESS) values.
#' Provides a trace plot and issues a warning if potential lack of convergence is detected.
#'
#' @param model_fit Stan model fit object to evaluate.
#' @param rhat_threshold $\hat{R}$ threshold above which the model is considered as potentially not converged. Default is 1.1.
#' @param ess_threshold Threshold for the ESS ratio below which the model is considered as potentially not converged. Default is 0.1.
#'
#' @return Logical indicating if the model converged or not (`TRUE` if converged, `FALSE` otherwise).
#'
#' @examples
#' \dontrun{
#' # Assuming a Stan model fit called 'fit'
#' is_converged <- check_convergence(fit)
#' print(is_converged)
#' }
#'
#' @seealso \code{\link[rstan]{rhat}}, \code{\link[rstan]{neff_ratio}}, and \code{\link[rstan]{traceplot}} for related Stan diagnostic functions.
#' 
#' @importFrom rstan rhat neff_ratio traceplot
#' 
#' @export
check_convergence <- function(model_fit, rhat_threshold = 1.1, ess_threshold = 0.1) {
  rhat_values <- rstan::rhat(model_fit)
  ess_values <- rstan::neff_ratio(model_fit)
  
  if (any(rhat_values > rhat_threshold) || any(ess_values < ess_threshold)) {
    warning("Potential lack of convergence detected. Review trace plots and diagnostics.")
    rstan::traceplot(model_fit)
    return(FALSE)
  }
  return(TRUE)
}