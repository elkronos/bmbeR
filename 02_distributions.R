#' Validate Positive Integer
#'
#' This function is used to ensure that a given parameter is a positive integer. It is primarily used for input validation in functions where a positive integer is required. The function checks whether the input is numeric, of length one, positive, and an integer. If these conditions are not met, the function stops with an error message.
#'
#' @param n The numeric value to be validated. It must be a positive integer. There is no default value, and this parameter is required.
#' @param param_name A string representing the name of the parameter being checked. This is used in the error message to identify which parameter failed validation. This parameter is required and has no default value.
#'
#' @importFrom base is.numeric
#' @importFrom base stop
#' @importFrom base paste
#' @importFrom stats round
#'
#' @return No return value; this function is used for its side effect of stopping execution with an error if conditions are not met.
#' 
#' @examples 
#' \dontrun{
#'   # Correct usage
#'   validate_positive_integer(5, "exampleParam")
#'
#'   # Incorrect usage - this will cause an error
#'   validate_positive_integer(-3, "exampleParam")
#' }
#' 
#' @export
validate_positive_integer <- function(n, param_name) {
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || n != round(n)) {
    stop(paste(param_name, "must be a positive integer."))
  }
}


#' Validate Numeric Parameter
#'
#' This function is used to ensure that a given parameter is a numeric value. It is particularly useful 
#' in scenarios where a function expects a numeric argument and needs to validate it before proceeding 
#' with further calculations or operations. The function throws an error if the provided argument is 
#' not numeric or if it is numeric but has a length different from 1, indicating that it is not a single 
#' numeric value.
#'
#' @param x The parameter to be validated. This should be a numeric value.
#' @param param_name A character string representing the name of the parameter. 
#'   This is used to customize the error message if the validation fails.
#' 
#' @return Invisible NULL. This function is used for validation only and does not 
#'   return any meaningful value. It will stop the execution and throw an error 
#'   if the validation fails.
#'
#' @examples 
#' \dontrun{
#'   some_function <- function(a) {
#'     validate_numeric(a, "a")
#'     # Further processing assuming 'a' is valid...
#'   }
#'   some_function(5)    # Valid usage
#'   some_function("5")  # This will cause an error
#' }
#'
#' @importFrom stats is.numeric
#' @export
validate_numeric <- function(x, param_name) {
  if (!is.numeric(x) || length(x) != 1) {
    stop(paste(param_name, "must be a numeric value."))
  }
}


#' Generate Samples from a Scaled and Shifted Student's t-Distribution
#'
#' This function generates random samples from a Student's t-distribution that has been
#' scaled and shifted. It is primarily used in Bayesian statistics for setting up priors 
#' with heavier tails than a normal distribution. This can be useful in robust statistical
#' modelling.
#'
#' @param sample_size An integer indicating the number of samples to generate. Must be a 
#'        positive integer. This is a required parameter with no default value.
#' @param nu Degrees of freedom for the t-distribution, a positive value. Default is 1.
#' @param mu The location parameter (mean) to shift the distribution. Default is 0.
#' @param sigma The scaling parameter (standard deviation) for the distribution. Default is 1.
#' @importFrom stats rt
#' @importFrom yourPackageName validate_positive_integer validate_numeric
#' @return A numeric vector of random samples from the specified t-distribution.
#' @examples
#' \dontrun{
#'   # Generate 100 samples from a Student's t-distribution with default parameters
#'   samples <- student_t_prior(sample_size = 100)
#'   
#'   # Generate 50 samples with specific nu, mu, and sigma
#'   samples_custom <- student_t_prior(sample_size = 50, nu = 2, mu = 5, sigma = 3)
#' }
#' @export
#'
#' @details The function validates the input parameters using `validate_positive_integer`
#' and `validate_numeric` functions which should be part of your package or imported from
#' another package. It then uses the `rt` function from the `stats` package to generate
#' samples from a standard t-distribution, which are then scaled and shifted according to
#' the `mu` and `sigma` parameters.
#'
student_t_prior <- function(sample_size, nu = 1, mu = 0, sigma = 1) {
  validate_positive_integer(sample_size, "sample_size")
  validate_numeric(nu, "nu")
  validate_numeric(mu, "mu")
  validate_numeric(sigma, "sigma")
  
  rt(sample_size, df = nu, ncp = mu) * sigma
}


#' Generate Random Numbers from a Normal Distribution with Specified Parameters
#'
#' This function generates a vector of random numbers drawn from a normal distribution. 
#' It is useful for simulations or generating test data. The function requires specifying 
#' the sample size and optionally allows setting the mean and standard deviation of the 
#' distribution. All parameters are validated for appropriate values.
#'
#' @param sample_size An integer specifying the number of random numbers to generate. 
#'        Must be a positive integer. This is a required parameter.
#' @param mu A numeric value specifying the mean of the normal distribution. 
#'        Default is 0. This parameter is optional.
#' @param sigma A numeric value specifying the standard deviation of the normal distribution. 
#'        Default is 1. This parameter is optional.
#' @importFrom stats rnorm
#' @importFrom yourPackage validate_positive_integer
#' @importFrom yourPackage validate_numeric
#' @return A numeric vector of random numbers drawn from a specified normal distribution.
#' @examples
#' \dontrun{
#'   # Generate 100 random numbers from a standard normal distribution
#'   random_numbers <- normal_prior(100)
#'
#'   # Generate 50 random numbers from a normal distribution with mean = 10 and sd = 2
#'   random_numbers <- normal_prior(50, mu = 10, sigma = 2)
#' }
#' @export
#'
#' @seealso \code{\link[stats]{rnorm}} for the function used to generate random numbers.
normal_prior <- function(sample_size, mu = 0, sigma = 1) {
  validate_positive_integer(sample_size, "sample_size")
  validate_numeric(mu, "mu")
  validate_numeric(sigma, "sigma")
  
  rnorm(sample_size, mean = mu, sd = sigma)
}


#' Generate a Cauchy Prior Distribution
#'
#' This function generates a sample from a Cauchy distribution, which is often used
#' in Bayesian statistics as a prior due to its heavy tails. The function allows
#' specification of the sample size, location parameter, and scale parameter.
#'
#' @param sample_size A positive integer indicating the number of samples to generate.
#' @param location The location parameter (median) of the Cauchy distribution. 
#'                 Default is 0.
#' @param scale The scale parameter (half the interquartile range) of the Cauchy 
#'              distribution. Default is 1.
#' @return A numeric vector of samples from the Cauchy distribution.
#'
#' @importFrom stats rcauchy
#' @useDynLib yourPackageName
#' @export
#'
#' @examples
#' \dontrun{
#'   # Generate a sample of size 100 from a Cauchy distribution with default parameters
#'   samples <- cauchy_prior(sample_size = 100)
#'
#'   # Generate a sample with specified location and scale
#'   samples_custom <- cauchy_prior(sample_size = 50, location = 2, scale = 0.5)
#' }
cauchy_prior <- function(sample_size, location = 0, scale = 1) {
  validate_positive_integer(sample_size, "sample_size")
  validate_numeric(location, "location")
  validate_numeric(scale, "scale")
  
  rcauchy(sample_size, location, scale)
}


#' Generate Random Numbers from a Uniform Distribution
#'
#' This function generates a vector of random numbers from a uniform distribution
#' defined by minimum and maximum values. It is primarily used in statistical simulations
#' and analyses where a uniform prior distribution is needed.
#'
#' @param sample_size An integer indicating the number of random numbers to generate.
#'   Must be a positive integer.
#' @param min The lower bound of the uniform distribution (default = 0). Must be numeric.
#' @param max The upper bound of the uniform distribution (default = 1). Must be numeric
#'   and greater than `min`.
#'
#' @return A numeric vector of random numbers from a uniform distribution.
#'
#' @importFrom stats runif
#' @export
#'
#' @examples
#' \dontrun{
#'   # Generate 10 random numbers from a uniform distribution between 0 and 1
#'   uniform_prior(10)
#'   # Generate 5 random numbers from a uniform distribution between 10 and 20
#'   uniform_prior(5, min = 10, max = 20)
#' }
uniform_prior <- function(sample_size, min = 0, max = 1) {
  validate_positive_integer(sample_size, "sample_size")
  validate_numeric(min, "min")
  validate_numeric(max, "max")
  
  if (min >= max) {
    stop("'min' must be less than 'max' in uniform_prior.")
  }
  
  runif(sample_size, min, max)
}


#' Generate Beta Prior Distribution Samples
#'
#' This function generates a sample from a Beta distribution, which is often used in Bayesian 
#' statistics as a prior distribution for binomial proportions. The Beta distribution is defined 
#' by two shape parameters. This function is useful in scenarios where a user needs to understand 
#' the behavior of proportions or probabilities before observing data, such as in Bayesian A/B testing.
#'
#' @param sample_size A positive integer indicating the number of samples to be drawn from the Beta distribution.
#' @param shape1 The first shape parameter of the Beta distribution, a positive number. Default is 1.
#' @param shape2 The second shape parameter of the Beta distribution, a positive number. Default is 1.
#' @return A numeric vector of random numbers drawn from a Beta distribution.
#' @importFrom stats rbeta
#' @examples
#' \donttest{
#'   # Generate 10 samples from a Beta distribution with shape parameters 2 and 5
#'   beta_samples <- beta_prior(10, 2, 5)
#' }
#' @export
#' @references
#' John K. Kruschke (2010). Doing Bayesian Data Analysis: A Tutorial with R and BUGS.
#' Academic Press / Elsevier.
#' 
#' @seealso
#' \code{\link[stats]{rbeta}}
#'
#' @note
#' This function requires the user to input positive numbers for `sample_size`, `shape1`, and `shape2`.
#' It internally validates these inputs to ensure they meet the requirements.
#'
#' @importFrom utils stop
beta_prior <- function(sample_size, shape1 = 1, shape2 = 1) {
  validate_positive_integer(sample_size, "sample_size")
  validate_numeric(shape1, "shape1")
  validate_numeric(shape2, "shape2")
  
  if (shape1 <= 0 || shape2 <= 0) {
    stop("Shape parameters for beta distribution must be positive.")
  }
  
  rbeta(sample_size, shape1, shape2)
}


#' Gamma Prior Distribution Generator
#'
#' This function generates a sample from a gamma distribution, which is commonly
#' used as a prior in Bayesian statistics. It is particularly useful for modelling
#' variables that are always positive and may be skewed, such as rates or scales.
#'
#' @param sample_size An integer indicating the number of random values to generate.
#'   Must be a positive integer. This is a required parameter with no default value.
#' @param shape A numeric value representing the shape parameter of the gamma distribution.
#'   Must be a positive number. Defaults to 1 if not specified.
#' @param rate A numeric value representing the rate parameter (inverse scale) of the
#'   gamma distribution. Must be a positive number. Defaults to 1 if not specified.
#' @return A numeric vector of random values drawn from a gamma distribution.
#' @importFrom stats rgamma
#' @examples
#' \dontrun{
#'   # Generate 10 random values from a gamma distribution with shape = 2 and rate = 1
#'   gamma_samples <- gamma_prior(sample_size = 10, shape = 2, rate = 1)
#'   print(gamma_samples)
#' }
#' @note This function requires the input parameters to be positive. It will stop
#'   and produce an error message if negative or zero values are provided for shape
#'   and rate parameters.
#' @seealso \code{\link[stats]{rgamma}} for the underlying gamma distribution function.
#' @export
#'
#' Dependencies: This function requires the 'stats' package for the rgamma function.
#' Additionally, it relies on custom functions `validate_positive_integer` and
#' `validate_numeric` to check the input arguments. These functions need to be
#' defined elsewhere in your package or script.
gamma_prior <- function(sample_size, shape = 1, rate = 1) {
  validate_positive_integer(sample_size, "sample_size")
  validate_numeric(shape, "shape")
  validate_numeric(rate, "rate")
  
  if (shape <= 0 || rate <= 0) {
    stop("Shape and rate parameters for gamma distribution must be positive.")
  }
  
  rgamma(sample_size, shape, rate)
}


#' Generate Random Numbers Based on a Binomial Distribution
#'
#' This function generates a vector of random numbers following a binomial distribution,
#' which describes the number of successes in a sequence of independent experiments.
#' It is useful for simulations or probabilistic calculations where binomial outcomes are expected.
#'
#' @param sample_size The number of random values to generate. Must be a positive integer.
#' @param size The number of trials in each experiment. Defaults to 1.
#'        Must be a positive integer.
#' @param prob The probability of success on each trial. Must be a numeric value between 0 and 1.
#'        Defaults to 0.5.
#' @return A numeric vector of length equal to `sample_size`, each element representing
#'         the number of successes in the respective binomial experiment.
#' @importFrom stats rbinom
#' @examples
#' \dontrun{
#'   # Generate 10 random values with default size and probability
#'   random_values <- binomial_prior(sample_size = 10)
#'   
#'   # Generate 15 random values, each based on 5 trials with a success probability of 0.3
#'   random_values_custom <- binomial_prior(sample_size = 15, size = 5, prob = 0.3)
#' }
#' 
#' @note This function requires validation functions for input parameters, which should be
#'       defined or imported separately. The validation ensures `sample_size`, `size`, and
#'       `prob` meet their respective criteria. It stops execution with an informative error
#'       message if validations fail.
#'
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


#' Generate Poisson Distributed Samples
#'
#' This function creates a vector of random numbers following a Poisson distribution.
#' It is particularly useful in statistical contexts where modeling the frequency of events 
#' over a fixed interval of time or space is required. The function ensures that input parameters
#' are valid for a Poisson distribution.
#'
#' @param sample_size A positive integer specifying the number of random samples to generate.
#' @param lambda The rate parameter of the Poisson distribution, with a default value of 1.
#'   Must be a positive number.
#' @importFrom stats rpois
#' @importFrom yourPackage validate_positive_integer validate_numeric
#' @return A numeric vector of length `sample_size` containing Poisson-distributed random numbers.
#' @note Lambda is the expected number of events in an interval and should be greater than 0.
#'   The `sample_size` parameter indicates the number of independent samples to draw from the distribution.
#' @examples
#' \dontrun{
#'   # Generate 10 random samples from a Poisson distribution with lambda = 2
#'   poisson_samples <- poisson_prior(10, 2)
#' }
#' @export
#' @references
#' For more information on the Poisson distribution and its applications in statistical modeling,
#' refer to: [Statistical Inference by Casella and Berger].
poisson_prior <- function(sample_size, lambda = 1) {
  validate_positive_integer(sample_size, "sample_size")
  validate_numeric(lambda, "lambda")
  
  if (lambda <= 0) {
    stop("Lambda parameter for Poisson distribution must be positive.")
  }
  
  rpois(sample_size, lambda)
}


#' Generate Log-Normal Distributed Random Numbers
#'
#' This function creates a vector of random numbers following a log-normal distribution. 
#' It is primarily used in statistical analysis and simulations where log-normally distributed 
#' data is required. The function ensures input parameters are valid and then leverages the 
#' `rlnorm` function to generate the data.
#'
#' @param sample_size A positive integer indicating the number of random numbers to generate.
#' @param meanlog The mean of the logarithm of the distribution, with a default value of 0.
#' @param sdlog The standard deviation of the logarithm of the distribution, with a default value of 1.
#' @return A numeric vector of log-normally distributed random numbers.
#' @importFrom stats rlnorm
#' @importFrom yourPackage validate_positive_integer
#' @importFrom yourPackage validate_numeric
#' @examples
#' \dontrun{
#'   # Generate 100 log-normally distributed random numbers
#'   # with a log mean of 0 and log standard deviation of 1
#'   random_numbers <- lognormal_prior(100)
#' }
#' 
#' @export
lognormal_prior <- function(sample_size, meanlog = 0, sdlog = 1) {
  validate_positive_integer(sample_size, "sample_size")
  validate_numeric(meanlog, "meanlog")
  validate_numeric(sdlog, "sdlog")
  
  rlnorm(sample_size, meanlog, sdlog)
}


#' Generate a Sample from a Bernoulli Distribution
#'
#' This function creates a sample of specified size from a Bernoulli distribution,
#' using a given probability of success. It is useful for simulations and
#' probabilistic analyses where binary outcomes are involved.
#'
#' @param sample_size An integer representing the number of trials. It must be
#'   a positive integer. There is no default value, and this parameter is required.
#' @param prob A numeric value indicating the probability of success on each trial.
#'   The value must be between 0 and 1, inclusive. Default is 0.5.
#'
#' @importFrom stats rbinom
#' @importFrom yourPackage validate_positive_integer
#' @importFrom yourPackage validate_numeric
#'
#' @return A numeric vector of length equal to `sample_size`, with each element
#'   representing the outcome of a trial (0 or 1).
#'
#' @examples
#' \dontrun{
#'   # Generate a sample of 10 trials with a success probability of 0.7
#'   sample <- bernoulli_prior(sample_size = 10, prob = 0.7)
#'   print(sample)
#' }
#'
#' @note Replace 'yourPackage' with the actual package name where
#'   `validate_positive_integer` and `validate_numeric` are defined.
#'
#' @export
bernoulli_prior <- function(sample_size, prob = 0.5) {
  validate_positive_integer(sample_size, "sample_size")
  validate_numeric(prob, "prob")
  
  if (prob < 0 || prob > 1) {
    stop("Probability for Bernoulli distribution must be between 0 and 1.")
  }
  
  rbinom(sample_size, 1, prob)
}


# List of Original Prior Distribution Functions
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


# Reset
distributions <- original_distributions