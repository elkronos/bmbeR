# Random-number generators for the distributions used in prior simulation.
#
# These are thin, validated wrappers around base R's r* functions. They are
# useful for simulating from priors by hand (e.g. to plot what a prior
# implies). For regression coefficients in rstanarm only location-scale
# families (normal, Student-t, Cauchy, Laplace) are valid priors; the
# discrete generators (binomial, Poisson, Bernoulli) simulate *data*, not
# coefficient priors, and are kept for prior predictive simulation.

#' Simulate from common prior distributions
#'
#' Validated random-number generators for distributions that are commonly
#' used as priors or for prior predictive simulation. All generators take the
#' number of draws as their first argument, `sample_size`.
#'
#' `student_t_prior()` draws from the *location-scale* Student-t
#' distribution, `mu + sigma * T` with `T ~ t(nu)`, which is the distribution
#' that [rstanarm::student_t()] places on a coefficient. (Versions of bmbeR
#' before 2.0.0 used a non-central t multiplied by `sigma`, whose mean is not
#' `mu`; see `vignette("bmbeR")` and the design review.)
#'
#' `binomial_prior()`, `poisson_prior()` and `bernoulli_prior()` generate
#' discrete *data*. They are not valid priors for regression coefficients,
#' which are continuous and unbounded, but they are handy when simulating
#' outcomes by hand.
#'
#' @param sample_size Positive integer: number of draws.
#' @param nu Degrees of freedom of the Student-t (must be positive).
#' @param mu,location,meanlog Location parameter.
#' @param sigma,scale,sdlog Scale parameter (must be positive).
#' @param min,max Bounds of the uniform distribution (`min < max`).
#' @param shape1,shape2 Positive shape parameters of the beta distribution.
#' @param shape,rate Positive shape and rate of the gamma distribution.
#' @param size Number of trials of the binomial distribution.
#' @param prob Success probability in `[0, 1]`.
#' @param lambda Positive Poisson rate.
#'
#' @return A numeric vector of length `sample_size`.
#' @examples
#' set.seed(1)
#' x <- student_t_prior(1e4, nu = 5, mu = 2, sigma = 0.5)
#' mean(x) # close to 2
#' summary(normal_prior(1000, mu = 0, sigma = 2.5))
#' @name prior_generators
NULL

#' @rdname prior_generators
#' @export
student_t_prior <- function(sample_size, nu = 1, mu = 0, sigma = 1) {
  validate_positive_integer(sample_size, "sample_size")
  validate_numeric(nu, "nu", positive = TRUE)
  validate_numeric(mu, "mu")
  validate_numeric(sigma, "sigma", positive = TRUE)
  mu + sigma * rt(sample_size, df = nu)
}

#' @rdname prior_generators
#' @export
normal_prior <- function(sample_size, mu = 0, sigma = 1) {
  validate_positive_integer(sample_size, "sample_size")
  validate_numeric(mu, "mu")
  validate_numeric(sigma, "sigma", positive = TRUE)
  rnorm(sample_size, mean = mu, sd = sigma)
}

#' @rdname prior_generators
#' @export
cauchy_prior <- function(sample_size, location = 0, scale = 1) {
  validate_positive_integer(sample_size, "sample_size")
  validate_numeric(location, "location")
  validate_numeric(scale, "scale", positive = TRUE)
  rcauchy(sample_size, location = location, scale = scale)
}

#' @rdname prior_generators
#' @export
uniform_prior <- function(sample_size, min = 0, max = 1) {
  validate_positive_integer(sample_size, "sample_size")
  validate_numeric(min, "min")
  validate_numeric(max, "max")
  if (min >= max) stop("`min` must be less than `max`.", call. = FALSE)
  runif(sample_size, min, max)
}

#' @rdname prior_generators
#' @export
beta_prior <- function(sample_size, shape1 = 1, shape2 = 1) {
  validate_positive_integer(sample_size, "sample_size")
  validate_numeric(shape1, "shape1", positive = TRUE)
  validate_numeric(shape2, "shape2", positive = TRUE)
  rbeta(sample_size, shape1, shape2)
}

#' @rdname prior_generators
#' @export
gamma_prior <- function(sample_size, shape = 1, rate = 1) {
  validate_positive_integer(sample_size, "sample_size")
  validate_numeric(shape, "shape", positive = TRUE)
  validate_numeric(rate, "rate", positive = TRUE)
  rgamma(sample_size, shape = shape, rate = rate)
}

#' @rdname prior_generators
#' @export
binomial_prior <- function(sample_size, size = 1, prob = 0.5) {
  validate_positive_integer(sample_size, "sample_size")
  validate_positive_integer(size, "size")
  validate_probability(prob, "prob")
  rbinom(sample_size, size, prob)
}

#' @rdname prior_generators
#' @export
poisson_prior <- function(sample_size, lambda = 1) {
  validate_positive_integer(sample_size, "sample_size")
  validate_numeric(lambda, "lambda", positive = TRUE)
  rpois(sample_size, lambda)
}

#' @rdname prior_generators
#' @export
lognormal_prior <- function(sample_size, meanlog = 0, sdlog = 1) {
  validate_positive_integer(sample_size, "sample_size")
  validate_numeric(meanlog, "meanlog")
  validate_numeric(sdlog, "sdlog", positive = TRUE)
  rlnorm(sample_size, meanlog, sdlog)
}

#' @rdname prior_generators
#' @export
bernoulli_prior <- function(sample_size, prob = 0.5) {
  validate_positive_integer(sample_size, "sample_size")
  validate_probability(prob, "prob")
  rbinom(sample_size, 1L, prob)
}

# =============================================================================
# Registry of generators
# =============================================================================

#' Registry of prior generators
#'
#' `original_distributions` is the built-in, named list of generator
#' functions (see [prior_generators]). `distributions` is an alias kept for
#' backwards compatibility. Both are constants: to extend the registry, create
#' your own copy with [add_distribution()] and pass it to
#' [get_prior_distribution()].
#'
#' @format A named list of functions.
#' @examples
#' names(original_distributions)
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

#' @rdname original_distributions
#' @export
distributions <- original_distributions

#' Manage and draw from a registry of prior generators
#'
#' `add_distribution()` returns a copy of a registry with a new generator
#' added; `reset_distributions()` returns the built-in registry;
#' `get_prior_distribution()` draws from a named generator in a registry.
#'
#' The registry is an ordinary list, so these functions have no side effects:
#' keep the list returned by `add_distribution()` and pass it to
#' `get_prior_distribution()` via its `distributions` argument.
#'
#' @param distributions A named list of generator functions, such as
#'   [original_distributions].
#' @param name Name for the new generator.
#' @param func A function whose first argument is the number of draws.
#' @param dist_type Name of the generator to use.
#' @param params A named list of arguments passed to the generator,
#'   including `sample_size`.
#'
#' @return `add_distribution()` and `reset_distributions()` return a named
#'   list of functions; `get_prior_distribution()` returns the draws.
#' @examples
#' my_dists <- add_distribution(original_distributions, "exponential",
#'                              function(sample_size, rate = 1) rexp(sample_size, rate))
#' get_prior_distribution("exponential", list(sample_size = 5, rate = 2),
#'                        distributions = my_dists)
#' @export
add_distribution <- function(distributions, name, func) {
  if (!is.list(distributions)) {
    stop("`distributions` must be a named list of generator functions.", call. = FALSE)
  }
  if (!is.character(name) || length(name) != 1L || !nzchar(name)) {
    stop("`name` must be a single non-empty string.", call. = FALSE)
  }
  if (!is.function(func)) {
    stop("`func` must be a function that generates random draws.", call. = FALSE)
  }
  if (name %in% names(distributions)) {
    stop(sprintf("A distribution named '%s' already exists.", name), call. = FALSE)
  }
  distributions[[name]] <- func
  distributions
}

#' @rdname add_distribution
#' @export
reset_distributions <- function() {
  original_distributions
}

#' @rdname add_distribution
#' @export
get_prior_distribution <- function(dist_type, params,
                                   distributions = original_distributions) {
  if (!is.character(dist_type) || length(dist_type) != 1L) {
    stop("`dist_type` must be a single string.", call. = FALSE)
  }
  if (!dist_type %in% names(distributions)) {
    stop(sprintf("Distribution '%s' is not in the registry. Available: %s.",
                 dist_type, paste(names(distributions), collapse = ", ")),
         call. = FALSE)
  }
  if (!is.list(params)) stop("`params` must be a named list.", call. = FALSE)
  do.call(distributions[[dist_type]], params)
}
