#' Generate Samples from a Student's t-distribution
#'
#' This function generates random samples from a non-central Student's t-distribution
#' with the given parameters. It adjusts the returned values by multiplying with a 
#' given scale parameter (\code{sigma}).
#'
#' @param sample_size Integer, number of samples to generate.
#' @param nu Numeric, degrees of freedom for the Student's t-distribution.
#' @param mu Numeric, non-centrality parameter.
#' @param sigma Numeric, scale factor to adjust the output.
#'
#' @return A numeric vector of random samples from the specified Student's t-distribution.
#'
#' @importFrom stats rt
#'
#' @examples 
#' \dontrun{
#' # Generate 100 samples from a standard t-distribution
#' samples <- student_t_prior(100)
#'
#' # Generate 100 samples from a non-central t-distribution with df=2, mu=3, sigma=2
#' samples2 <- student_t_prior(100, nu=2, mu=3, sigma=2)
#' }
#'
#' @export
student_t_prior <- function(sample_size, nu=1, mu=0, sigma=1) {
  rt(sample_size, df=nu, ncp=mu) * sigma
}

#' Generate Samples from a Normal Distribution
#'
#' This function generates random samples from a normal distribution with the
#' specified mean (\code{mu}) and standard deviation (\code{sigma}).
#'
#' @param sample_size Integer, number of samples to generate.
#' @param mu Numeric, mean of the normal distribution.
#' @param sigma Numeric, standard deviation of the normal distribution.
#'
#' @return A numeric vector of random samples from the specified normal distribution.
#'
#' @importFrom stats rnorm
#'
#' @examples 
#' \dontrun{
#' # Generate 100 samples from a standard normal distribution
#' samples <- normal_prior(100)
#'
#' # Generate 100 samples from a normal distribution with mean=3 and sd=2
#' samples2 <- normal_prior(100, mu=3, sigma=2)
#' }
#'
#' @export
normal_prior <- function(sample_size, mu=0, sigma=1) {
  rnorm(sample_size, mean=mu, sd=sigma)
}

#' Generate Samples from a Cauchy Distribution
#'
#' This function generates random samples from a Cauchy distribution with the
#' specified location parameter (\code{location}) and scale parameter (\code{scale}).
#'
#' @param sample_size Integer, number of samples to generate.
#' @param location Numeric, location parameter of the Cauchy distribution.
#' @param scale Numeric, scale parameter of the Cauchy distribution.
#'
#' @return A numeric vector of random samples from the specified Cauchy distribution.
#'
#' @importFrom stats rcauchy
#'
#' @examples 
#' \dontrun{
#' # Generate 100 samples from a standard Cauchy distribution
#' samples <- cauchy_prior(100)
#'
#' # Generate 100 samples from a Cauchy distribution with location=2 and scale=3
#' samples2 <- cauchy_prior(100, location=2, scale=3)
#' }
#'
#' @export
cauchy_prior <- function(sample_size, location=0, scale=1) {
  rcauchy(sample_size, location, scale)
}

#' Generate Samples from a Uniform Distribution
#'
#' This function generates random samples from a Uniform distribution between
#' the specified minimum (\code{min}) and maximum (\code{max}) values.
#'
#' @param sample_size Integer, number of samples to generate.
#' @param min Numeric, minimum value of the Uniform distribution.
#' @param max Numeric, maximum value of the Uniform distribution.
#'
#' @return A numeric vector of random samples from the specified Uniform distribution.
#'
#' @importFrom stats runif
#'
#' @examples 
#' \dontrun{
#' # Generate 100 samples from a standard Uniform distribution between 0 and 1
#' samples <- uniform_prior(100)
#'
#' # Generate 100 samples from a Uniform distribution between 5 and 10
#' samples2 <- uniform_prior(100, min=5, max=10)
#' }
#'
#' @export
uniform_prior <- function(sample_size, min=0, max=1) {
  runif(sample_size, min, max)
}

#' Generate Samples from a Beta Distribution
#'
#' This function generates random samples from a Beta distribution specified by 
#' its two shape parameters, \code{shape1} and \code{shape2}.
#'
#' @param sample_size Integer, number of samples to generate.
#' @param shape1 Numeric, the first shape parameter for the Beta distribution.
#' @param shape2 Numeric, the second shape parameter for the Beta distribution.
#'
#' @return A numeric vector of random samples from the specified Beta distribution.
#'
#' @importFrom stats rbeta
#'
#' @examples 
#' \dontrun{
#' # Generate 100 samples from a Beta distribution with shape parameters 1 and 1
#' samples <- beta_prior(100)
#'
#' # Generate 100 samples from a Beta distribution with shape parameters 2 and 5
#' samples2 <- beta_prior(100, shape1=2, shape2=5)
#' }
#'
#' @export
beta_prior <- function(sample_size, shape1=1, shape2=1) {
  rbeta(sample_size, shape1, shape2)
}

#' Generate Samples from a Gamma Distribution
#'
#' This function generates random samples from a Gamma distribution specified by 
#' its shape and rate parameters.
#'
#' @param sample_size Integer, number of samples to generate.
#' @param shape Numeric, the shape parameter for the Gamma distribution.
#' @param rate Numeric, the rate parameter for the Gamma distribution.
#'
#' @return A numeric vector of random samples from the specified Gamma distribution.
#'
#' @importFrom stats rgamma
#'
#' @examples 
#' \dontrun{
#' # Generate 100 samples from a Gamma distribution with shape 1 and rate 1
#' samples <- gamma_prior(100)
#'
#' # Generate 100 samples from a Gamma distribution with shape 2 and rate 3
#' samples2 <- gamma_prior(100, shape=2, rate=3)
#' }
#'
#' @export
gamma_prior <- function(sample_size, shape=1, rate=1) {
  rgamma(sample_size, shape, rate)
}

#' Generate Samples from a Binomial Distribution
#'
#' This function generates random samples from a Binomial distribution based on 
#' specified size and probability parameters.
#'
#' @param sample_size Integer, number of samples to generate.
#' @param size Numeric, number of trials for the Binomial distribution.
#' @param prob Numeric, probability of success in a single trial for the Binomial distribution.
#'
#' @return A numeric vector of random samples from the specified Binomial distribution.
#'
#' @importFrom stats rbinom
#'
#' @examples 
#' \dontrun{
#' # Generate 100 samples from a Binomial distribution with 1 trial and success probability of 0.5
#' samples <- binomial_prior(100)
#'
#' # Generate 100 samples from a Binomial distribution with 5 trials and success probability of 0.7
#' samples2 <- binomial_prior(100, size=5, prob=0.7)
#' }
#'
#' @export
binomial_prior <- function(sample_size, size = 1, prob = 0.5) {
  rbinom(sample_size, size, prob)
}

#' Generate Samples from a Poisson Distribution
#'
#' This function generates random samples from a Poisson distribution based on 
#' a specified lambda parameter.
#'
#' @param sample_size Integer, number of samples to generate.
#' @param lambda Numeric, the rate parameter for the Poisson distribution.
#'
#' @return A numeric vector of random samples from the specified Poisson distribution.
#'
#' @importFrom stats rpois
#'
#' @examples 
#' \dontrun{
#' # Generate 100 samples from a Poisson distribution with lambda = 1
#' samples <- poisson_prior(100)
#'
#' # Generate 100 samples from a Poisson distribution with lambda = 5
#' samples2 <- poisson_prior(100, lambda=5)
#' }
#'
#' @export
poisson_prior <- function(sample_size, lambda = 1) {
  rpois(sample_size, lambda)
}

#' Generate Samples from a Lognormal Distribution
#'
#' This function generates random samples from a Lognormal distribution using
#' specified mean and standard deviation for the log of the distribution.
#'
#' @param sample_size Integer, number of samples to generate.
#' @param meanlog Numeric, mean of the log of the desired lognormal distribution.
#' @param sdlog Numeric, standard deviation of the log of the desired lognormal distribution.
#'
#' @return A numeric vector of random samples from the specified lognormal distribution.
#'
#' @importFrom stats rlnorm
#'
#' @examples 
#' \dontrun{
#' # Generate 100 samples from a lognormal distribution with default parameters
#' samples <- lognormal_prior(100)
#'
#' # Generate 100 samples with a mean log of 0.5 and standard deviation log of 1.2
#' samples2 <- lognormal_prior(100, meanlog=0.5, sdlog=1.2)
#' }
#'
#' @export
lognormal_prior <- function(sample_size, meanlog=0, sdlog=1) {
  rlnorm(sample_size, meanlog, sdlog)
}

#' Generate Samples from a Bernoulli Distribution
#'
#' This function generates random samples from a Bernoulli distribution using a 
#' specified probability.
#'
#' @param sample_size Integer, number of samples to generate.
#' @param prob Numeric, probability of success for the Bernoulli distribution. 
#' Must be between 0 and 1.
#'
#' @return A numeric vector of random samples (0s or 1s) from the specified 
#' Bernoulli distribution.
#'
#' @importFrom stats rbinom
#'
#' @examples 
#' \dontrun{
#' # Generate 100 samples from a Bernoulli distribution with a 0.5 probability
#' samples <- bernoulli_prior(100)
#'
#' # Generate 100 samples with a success probability of 0.3
#' samples2 <- bernoulli_prior(100, prob=0.3)
#' }
#'
#' @export
bernoulli_prior <- function(sample_size, prob = 0.5) {
  rbinom(sample_size, 1, prob)
}

#' List of Original Prior Distribution Functions
#'
#' This list contains a collection of functions that generate random samples from 
#' various probability distributions. These are considered as the "original" or 
#' base set of distributions.
#'
#' The `distributions` list is a clone of this list and can be modified during 
#' runtime without affecting the base set.
#'
#' @name original_distributions
#' @format A list of functions.
#' 
#' @details The following distributions are included in the list:
#' \itemize{
#'  \item \strong{student_t}: Generates samples from a Student's t-distribution.
#'  \item \strong{normal}: Generates samples from a normal distribution.
#'  \item \strong{cauchy}: Generates samples from a Cauchy distribution.
#'  \item \strong{uniform}: Generates samples from a uniform distribution.
#'  \item \strong{beta}: Generates samples from a beta distribution.
#'  \item \strong{gamma}: Generates samples from a gamma distribution.
#'  \item \strong{binomial}: Generates samples from a binomial distribution.
#'  \item \strong{poisson}: Generates samples from a Poisson distribution.
#'  \item \strong{lognormal}: Generates samples from a lognormal distribution.
#'  \item \strong{bernoulli}: Generates samples from a Bernoulli distribution.
#' }
#'
#' @examples 
#' \dontrun{
#' # Access the normal distribution function from the list
#' normal_function <- original_distributions$normal
#'
#' # Generate 100 samples using the normal distribution function
#' samples <- normal_function(100)
#' }
#'
#' @seealso \code{\link{distributions}}, which is a clone of this list for working purposes.
#'
original_distributions <- list(
  student_t = student_t_prior,
  normal = normal_prior,
  cauchy = cauchy_prior,
  uniform = uniform_prior,
  beta = beta_prior,
  gamma = gamma_prior,
  binomial = binomial_prior,
  poisson = poisson_prior,
  lognormal = lognormal_prior,
  bernoulli = bernoulli_prior
)

#' Clone of the Original Prior Distribution Functions
#'
#' This list is a clone of the `original_distributions` list and can be modified 
#' during runtime without affecting the base set of distributions.
#'
#' @name distributions
#' @format A list of functions.
#'
#' @seealso \code{\link{original_distributions}}, which is the base set of these distribution functions.
#'
distributions <- original_distributions