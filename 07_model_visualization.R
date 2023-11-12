generate_plot <- function(model_fit, types = c("trace", "hist", "density", "autocorrelation"), ...) {
  # Check if the provided model fit is a Stan model fit object
  if (!inherits(model_fit, "stanfit")) {
    stop("The provided model_fit is not a Stan model fit object.")
  }
  
  # Check for potential lack of convergence using Rhat values
  rhat_values <- rstan::rhat(model_fit)
  if (any(rhat_values > 1.1)) {
    warning("Some R-hat values are above 1.1, which might indicate lack of convergence.")
  }
  
  # Generate the requested plot types
  for (type in types) {
    switch(type,
           "trace" = rstan::traceplot(model_fit, ...),
           "hist" = rstan::stan_hist(model_fit, ...),
           "density" = rstan::stan_dens(model_fit, ...),
           "autocorrelation" = rstan::stan_ac(model_fit, ...),
           stop("Invalid plot type. Available types: 'trace', 'hist', 'density', 'autocorrelation'.")
    )
  }
}

plot_posterior_distributions <- function(model_fit, ...) {
  posterior_samples <- rstan::extract(model_fit)
  bayesplot::mcmc_intervals(posterior_samples, prob = 0.95, ...)
}
