# Simulate from common prior distributions

Validated random-number generators for distributions that are commonly
used as priors or for prior predictive simulation. All generators take
the number of draws as their first argument, `sample_size`.

## Usage

``` r
student_t_prior(sample_size, nu = 1, mu = 0, sigma = 1)

normal_prior(sample_size, mu = 0, sigma = 1)

cauchy_prior(sample_size, location = 0, scale = 1)

uniform_prior(sample_size, min = 0, max = 1)

beta_prior(sample_size, shape1 = 1, shape2 = 1)

gamma_prior(sample_size, shape = 1, rate = 1)

binomial_prior(sample_size, size = 1, prob = 0.5)

poisson_prior(sample_size, lambda = 1)

lognormal_prior(sample_size, meanlog = 0, sdlog = 1)

bernoulli_prior(sample_size, prob = 0.5)
```

## Arguments

- sample_size:

  Positive integer: number of draws.

- nu:

  Degrees of freedom of the Student-t (must be positive).

- mu, location, meanlog:

  Location parameter.

- sigma, scale, sdlog:

  Scale parameter (must be positive).

- min, max:

  Bounds of the uniform distribution (`min < max`).

- shape1, shape2:

  Positive shape parameters of the beta distribution.

- shape, rate:

  Positive shape and rate of the gamma distribution.

- size:

  Number of trials of the binomial distribution.

- prob:

  Success probability in `[0, 1]`.

- lambda:

  Positive Poisson rate.

## Value

A numeric vector of length `sample_size`.

## Details

`student_t_prior()` draws from the *location-scale* Student-t
distribution, `mu + sigma * T` with `T ~ t(nu)`, which is the
distribution that
[`rstanarm::student_t()`](https://mc-stan.org/rstanarm/reference/priors.html)
places on a coefficient. (Versions of bmbeR before 2.0.0 used a
non-central t multiplied by `sigma`, whose mean is not `mu`; see
[`vignette("bmbeR")`](https://elkronos.github.io/bmbeR/articles/bmbeR.md)
and the design review.)

`binomial_prior()`, `poisson_prior()` and `bernoulli_prior()` generate
discrete *data*. They are not valid priors for regression coefficients,
which are continuous and unbounded, but they are handy when simulating
outcomes by hand.

## Examples

``` r
set.seed(1)
x <- student_t_prior(1e4, nu = 5, mu = 2, sigma = 0.5)
mean(x) # close to 2
#> [1] 2.004395
summary(normal_prior(1000, mu = 0, sigma = 2.5))
#>     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#> -7.70907 -1.69622  0.01117  0.03723  1.74479  7.12879 
```
