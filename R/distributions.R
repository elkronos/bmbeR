# distributions.R
# =============================================================================
# This script defines various prior distribution generators for Bayesian 
# modeling. It also provides a global list of these functions along with 
# helper utilities to add, reset, and retrieve distributions.
#
# Dependencies: 
#   - validate_positive_integer() and validate_numeric() should be defined 
#     (see utilities.R).
# =============================================================================

# -----------------------------------------------------------------------------
# Student's t Prior Distribution Generator
# -----------------------------------------------------------------------------
#' Generate Samples from a Scaled and Shifted Student's t-Distribution
#'
#' Generates random samples from a Student's t-distribution that has been
#' scaled and shifted. Note: This function uses a noncentral t formulation;
#' if a pure location-scale transformation is preferred, consider using
#' `mu + sigma * rt(sample_size, df = nu)`.
#'
#' @param sample_size Positive integer. Number of samples to generate.
#' @param nu Numeric. Degrees of freedom for the t-distribution (default is 1).
#' @param mu Numeric. Location parameter to shift the distribution (default is 0).
#' @param sigma Numeric. Scaling parameter for the distribution (default is 1).
#'
#' @return A numeric vector of random samples.
#' @export
student_t_prior <- function(sample_size, nu = 1, mu = 0, sigma = 1) {
  # Validate inputs (assumes these functions are defined in utilities.R)
  validate_positive_integer(sample_size, "sample_size")
  validate_numeric(nu, "nu")
  validate_numeric(mu, "mu")
  validate_numeric(sigma, "sigma")
  
  # Generate samples: note the use of noncentral t (ncp = mu) scaled by sigma.
  rt(sample_size, df = nu, ncp = mu) * sigma
}


# -----------------------------------------------------------------------------
# Normal Distribution Generator
# -----------------------------------------------------------------------------
#' Generate Random Numbers from a Normal Distribution
#'
#' Draws samples from a normal distribution with the specified mean and standard deviation.
#'
#' @param sample_size Positive integer. Number of samples to generate.
#' @param mu Numeric. Mean of the normal distribution (default is 0).
#' @param sigma Numeric. Standard deviation of the normal distribution (default is 1).
#'
#' @return A numeric vector of random samples.
#' @export
normal_prior <- function(sample_size, mu = 0, sigma = 1) {
  validate_positive_integer(sample_size, "sample_size")
  validate_numeric(mu, "mu")
  validate_numeric(sigma, "sigma")
  
  rnorm(sample_size, mean = mu, sd = sigma)
}


# -----------------------------------------------------------------------------
# Cauchy Distribution Generator
# -----------------------------------------------------------------------------
#' Generate a Cauchy Prior Distribution Sample
#'
#' Generates random samples from a Cauchy distribution with the specified 
#' location and scale.
#'
#' @param sample_size Positive integer. Number of samples to generate.
#' @param location Numeric. The location parameter (default is 0).
#' @param scale Numeric. The scale parameter (default is 1).
#'
#' @return A numeric vector of random samples.
#' @export
cauchy_prior <- function(sample_size, location = 0, scale = 1) {
  validate_positive_integer(sample_size, "sample_size")
  validate_numeric(location, "location")
  validate_numeric(scale, "scale")
  
  rcauchy(sample_size, location = location, scale = scale)
}


# -----------------------------------------------------------------------------
# Uniform Distribution Generator
# -----------------------------------------------------------------------------
#' Generate Random Numbers from a Uniform Distribution
#'
#' Draws samples from a uniform distribution between the specified minimum and maximum.
#'
#' @param sample_size Positive integer. Number of samples to generate.
#' @param min Numeric. Lower bound of the uniform distribution (default is 0).
#' @param max Numeric. Upper bound of the uniform distribution (default is 1). Must be greater than `min`.
#'
#' @return A numeric vector of random samples.
#' @export
uniform_prior <- function(sample_size, min = 0, max = 1) {
  validate_positive_integer(sample_size, "sample_size")
  validate_numeric(min, "min")
  validate_numeric(max, "max")
  
  if (min >= max) {
    stop("'min' must be less than 'max' in uniform_prior.")
  }
  
  runif(sample_size, min, max)
}


# -----------------------------------------------------------------------------
# Beta Distribution Generator
# -----------------------------------------------------------------------------
#' Generate Beta Prior Distribution Samples
#'
#' Draws samples from a Beta distribution defined by two shape parameters.
#'
#' @param sample_size Positive integer. Number of samples to generate.
#' @param shape1 Numeric. The first shape parameter (default is 1).
#' @param shape2 Numeric. The second shape parameter (default is 1).
#'
#' @return A numeric vector of random samples.
#' @export
beta_prior <- function(sample_size, shape1 = 1, shape2 = 1) {
  validate_positive_integer(sample_size, "sample_size")
  validate_numeric(shape1, "shape1")
  validate_numeric(shape2, "shape2")
  
  if (shape1 <= 0 || shape2 <= 0) {
    stop("Shape parameters for beta distribution must be positive.")
  }
  
  rbeta(sample_size, shape1, shape2)
}


# -----------------------------------------------------------------------------
# Gamma Distribution Generator
# -----------------------------------------------------------------------------
#' Gamma Prior Distribution Generator
#'
#' Draws samples from a gamma distribution with the specified shape and rate.
#'
#' @param sample_size Positive integer. Number of samples to generate.
#' @param shape Numeric. The shape parameter (default is 1).
#' @param rate Numeric. The rate parameter (default is 1).
#'
#' @return A numeric vector of random samples.
#' @export
gamma_prior <- function(sample_size, shape = 1, rate = 1) {
  validate_positive_integer(sample_size, "sample_size")
  validate_numeric(shape, "shape")
  validate_numeric(rate, "rate")
  
  if (shape <= 0 || rate <= 0) {
    stop("Shape and rate parameters for gamma distribution must be positive.")
  }
  
  rgamma(sample_size, shape, rate)
}


# -----------------------------------------------------------------------------
# Binomial Distribution Generator
# -----------------------------------------------------------------------------
#' Generate Random Numbers from a Binomial Distribution
#'
#' Draws samples from a binomial distribution representing the number of successes 
#' in a given number of trials.
#'
#' @param sample_size Positive integer. Number of experiments (trials) to simulate.
#' @param size Positive integer. Number of trials in each experiment (default is 1).
#' @param prob Numeric. Probability of success on each trial (default is 0.5; must be between 0 and 1).
#'
#' @return A numeric vector of simulated counts.
#' @export
binomial_prior <- function(sample_size, size = 1, prob = 0.5) {
  validate_positive_integer(sample_size, "sample_size")
  validate_positive_integer(size, "size")
  validate_numeric(prob, "prob")
  
  if (prob < 0 || prob > 1) {
    stop("Probability for binomial distribution must be between 0 and 1.")
  }
  
  rbinom(sample_size, size, prob)
}


# -----------------------------------------------------------------------------
# Poisson Distribution Generator
# -----------------------------------------------------------------------------
#' Generate Poisson Distributed Samples
#'
#' Draws samples from a Poisson distribution with the specified rate parameter.
#'
#' @param sample_size Positive integer. Number of samples to generate.
#' @param lambda Numeric. The rate (mean) of the distribution (default is 1; must be positive).
#'
#' @return A numeric vector of random samples.
#' @export
poisson_prior <- function(sample_size, lambda = 1) {
  validate_positive_integer(sample_size, "sample_size")
  validate_numeric(lambda, "lambda")
  
  if (lambda <= 0) {
    stop("Lambda parameter for Poisson distribution must be positive.")
  }
  
  rpois(sample_size, lambda)
}


# -----------------------------------------------------------------------------
# Lognormal Distribution Generator
# -----------------------------------------------------------------------------
#' Generate Log-Normal Distributed Random Numbers
#'
#' Draws samples from a log-normal distribution defined by the log-scale parameters.
#'
#' @param sample_size Positive integer. Number of samples to generate.
#' @param meanlog Numeric. Mean on the logarithmic scale (default is 0).
#' @param sdlog Numeric. Standard deviation on the logarithmic scale (default is 1).
#'
#' @return A numeric vector of random samples.
#' @export
lognormal_prior <- function(sample_size, meanlog = 0, sdlog = 1) {
  validate_positive_integer(sample_size, "sample_size")
  validate_numeric(meanlog, "meanlog")
  validate_numeric(sdlog, "sdlog")
  
  rlnorm(sample_size, meanlog, sdlog)
}


# -----------------------------------------------------------------------------
# Bernoulli Distribution Generator
# -----------------------------------------------------------------------------
#' Generate a Sample from a Bernoulli Distribution
#'
#' Simulates binary outcomes (0 or 1) for a series of trials based on a given success probability.
#'
#' @param sample_size Positive integer. Number of trials to simulate.
#' @param prob Numeric. Probability of success on each trial (default is 0.5; must be between 0 and 1).
#'
#' @return A numeric vector of 0s and 1s.
#' @export
bernoulli_prior <- function(sample_size, prob = 0.5) {
  validate_positive_integer(sample_size, "sample_size")
  validate_numeric(prob, "prob")
  
  if (prob < 0 || prob > 1) {
    stop("Probability for Bernoulli distribution must be between 0 and 1.")
  }
  
  rbinom(sample_size, 1, prob)
}


# =============================================================================
# Global Distribution Management
# =============================================================================

#' Global List of Original Distribution Functions
#'
#' This list holds the original set of prior distribution generator functions.
#'
#' @export
original_distributions <- list(
  student_t = student_t_prior,
  normal    = normal_prior,
  cauchy    = cauchy_prior,
  uniform   = uniform_prior,
  beta      = beta_prior,
  gamma     = gamma_prior,
  binomial  = binomial_prior,
  poisson   = poisson_prior,
  lognormal = lognormal_prior,
  bernoulli = bernoulli_prior
)

# Working copy of distributions (modifiable via add_distribution/reset_distributions)
#' @export
distributions <- original_distributions


# -----------------------------------------------------------------------------
# Add a New Distribution to the Global List
# -----------------------------------------------------------------------------
#' Add a New Distribution Function
#'
#' Adds a user-defined distribution generator function to the global distributions list.
#'
#' @param distributions A list of distribution functions (usually the global `distributions`).
#' @param name Character. The name to assign to the new distribution.
#' @param func Function. The distribution generator function.
#'
#' @return An updated list of distribution functions.
#' @export
add_distribution <- function(distributions, name, func) {
  if (!is.function(func)) {
    stop("The provided argument 'func' is not a valid function. Please provide a function that generates random samples.")
  }
  if (name %in% names(distributions)) {
    stop(sprintf("The distribution name '%s' already exists. Use a different name or remove the existing distribution first.", name))
  }
  distributions[[name]] <- func
  distributions
}


# -----------------------------------------------------------------------------
# Reset Distributions List to Original State
# -----------------------------------------------------------------------------
#' Reset the Distributions List
#'
#' Resets the global distributions list to its original state.
#'
#' @return A list of the original distribution generator functions.
#' @export
reset_distributions <- function() {
  original_distributions
}


# -----------------------------------------------------------------------------
# Retrieve a Prior Distribution
# -----------------------------------------------------------------------------
#' Retrieve a Prior Distribution
#'
#' Retrieves a prior distribution sample by invoking the appropriate distribution
#' generator function from the global list.
#'
#' @param dist_type Character. The type of distribution (must match a name in the global list).
#' @param params List. A list of parameters to pass to the distribution function.
#'
#' @return A numeric vector of samples generated by the specified distribution function.
#' @export
get_prior_distribution <- function(dist_type, params) {
  if (!(dist_type %in% names(distributions))) {
    stop(sprintf("Distribution type '%s' not supported.", dist_type))
  }
  do.call(distributions[[dist_type]], params)
}
