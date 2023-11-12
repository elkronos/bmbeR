check_convergence <- function(model_fit, rhat_threshold = 1.1, ess_threshold = 0.1, save_plots = FALSE, plot_path = "trace_plot.pdf") {
  rhat_values <- rstan::rhat(model_fit)
  ess_values <- rstan::neff_ratio(model_fit)
  
  # Initialize a variable to track if there is a convergence issue
  convergence_issue <- FALSE
  message <- "Potential lack of convergence detected. "
  
  if (any(rhat_values > rhat_threshold)) {
    message <- paste(message, "Rhat values exceed threshold. ")
    convergence_issue <- TRUE
  }
  
  if (any(ess_values < ess_threshold)) {
    message <- paste(message, "ESS values below threshold. ")
    convergence_issue <- TRUE
  }
  
  if (convergence_issue) {
    warning(message, "Review trace plots and diagnostics.")
    
    # Option to save trace plots
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