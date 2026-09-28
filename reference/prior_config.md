# Combine priors for a regression model

Bundles the priors for the intercept, the regression coefficients and
the auxiliary parameter. Any component left as `NULL` uses rstanarm's
default, weakly informative, autoscaled prior (see
[`vignette("priors", package = "rstanarm")`](https://mc-stan.org/rstanarm/articles/priors.html)).
To request an improper flat prior explicitly, use `prior_spec("flat")`.

## Usage

``` r
prior_config(intercept = NULL, slope = NULL, aux = NULL)
```

## Arguments

- intercept, slope, aux:

  Priors created with
  [`prior_spec()`](https://elkronos.github.io/bmbeR/reference/prior_spec.md).
  Native rstanarm prior objects (e.g. `rstanarm::normal(0, 1)`) and the
  list format used by bmbeR 1.x (e.g.
  `list(type = "normal", mu = 0, sigma = 1)`) are also accepted.

## Value

An object of class `bmb_prior_config`.

## Details

The intercept prior applies to the intercept *after centring the
predictors* (i.e. the expected outcome, on the link scale, at the
predictor means). This is how rstanarm parameterises the model; you do
not need to centre the predictors yourself.

## Examples

``` r
prior_config(
  intercept = prior_spec("normal", location = 80, scale = 20),
  slope     = prior_spec("normal", location = 0, scale = c(1, 10))
)
#> <bmb_prior_config>
#>   intercept : normal(location = 80, scale = 20)
#>   slope     : normal(location = 0, scale = [1, 10])
#>   aux       : rstanarm default
```
