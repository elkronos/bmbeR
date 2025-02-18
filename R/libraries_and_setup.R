# libraries_and_setup.R
# =============================================================================
# This script loads the necessary libraries and sets up global options 
# for the entire Bayesian model building and evaluation workflow.
#
# Libraries Loaded:
#   - rstanarm
#   - bayesplot
#   - ggplot2
#   - cowplot
#   - purrr
#   - rstan
#   - loo
#
# Global Settings:
#   - Options for parallel processing (using all available cores)
#   - Reproducibility settings (default seed)
#   - ggplot2 theme (minimal theme)
#
# =============================================================================

# List of required packages
required_packages <- c("rstanarm", "bayesplot", "ggplot2", "cowplot", "purrr", "rstan", "loo")

# Function to install (if necessary) and load packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Install and load each required package
invisible(sapply(required_packages, install_if_missing))

# Set global options
options(mc.cores = parallel::detectCores())  # Use all available cores for MCMC sampling
rstan::rstan_options(auto_write = TRUE)         # Cache compiled Stan models for reuse

# Set a default random seed for reproducibility
set.seed(1234)

# Set a default ggplot2 theme
ggplot2::theme_set(ggplot2::theme_minimal())

# Print session information for reproducibility and debugging purposes
message("Libraries loaded and global options set:")
sessionInfo()
