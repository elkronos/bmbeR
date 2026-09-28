# Specify a prior distribution

Creates a validated prior specification that can be used for the
intercept, the regression coefficients ("slopes") or the auxiliary
parameter (e.g. the residual standard deviation) of a model fitted with
[`fit_model_with_prior()`](https://elkronos.github.io/bmbeR/reference/fit_model_with_prior.md).
Argument names follow rstanarm.

## Usage

``` r
prior_spec(
  type = c("normal", "student_t", "cauchy", "laplace", "exponential", "flat"),
  location = 0,
  scale = 2.5,
  df = 3,
  rate = 1,
  autoscale = FALSE
)
```

## Arguments

- type:

  One of `"normal"`, `"student_t"`, `"cauchy"`, `"laplace"`
  (coefficients only), `"exponential"` (auxiliary parameter only) or
  `"flat"` (an improper uniform prior).

- location:

  Prior location. For slopes, either a single value or one value per
  coefficient.

- scale:

  Prior scale (positive). For slopes, a single value or one per
  coefficient.

- df:

  Degrees of freedom for `"student_t"`.

- rate:

  Rate for `"exponential"`.

- autoscale:

  Logical. If `TRUE`, rstanarm rescales the prior using the standard
  deviations of the outcome and predictors.

## Value

An object of class `bmb_prior`.

## Choosing a scale

A prior scale is only meaningful relative to the units of the outcome
and the predictors. A fixed scale such as `normal(0, 2.5)` is weakly
informative when variables are standardised but can be overwhelmingly
informative when they are not (for example, an outcome measured in
dollars). Either standardise your variables, use `autoscale = TRUE`
(rstanarm then rescales by the standard deviations of the data), derive
scales from substantive knowledge, or use
[`empirical_bayes_priors()`](https://elkronos.github.io/bmbeR/reference/empirical_bayes_priors.md).
See Gelman et al. (2008) and
[`vignette("bmbeR")`](https://elkronos.github.io/bmbeR/articles/bmbeR.md).

## References

Gelman, A., Jakulin, A., Pittau, M. G., & Su, Y.-S. (2008). A weakly
informative default prior distribution for logistic and other regression
models. *The Annals of Applied Statistics*, 2(4), 1360–1383.
[doi:10.1214/08-AOAS191](https://doi.org/10.1214/08-AOAS191)

## See also

[`prior_config()`](https://elkronos.github.io/bmbeR/reference/prior_config.md)
to combine priors for a model.

## Examples

``` r
prior_spec("normal", location = 0, scale = 1)
#> <bmb_prior> normal(location = 0, scale = 1) 
prior_spec("student_t", df = 3, location = 0, scale = 2.5)
#> <bmb_prior> student_t(df = 3, location = 0, scale = 2.5) 
prior_spec("exponential", rate = 1)
#> <bmb_prior> exponential(rate = 1) 
```
