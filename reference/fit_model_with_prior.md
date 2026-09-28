# Fit a Bayesian regression model with explicit priors

Fits a generalised linear model with
[`rstanarm::stan_glm()`](https://mc-stan.org/rstanarm/reference/stan_glm.html)
using priors specified with
[`prior_config()`](https://elkronos.github.io/bmbeR/reference/prior_config.md)
(or derived with
[`empirical_bayes_priors()`](https://elkronos.github.io/bmbeR/reference/empirical_bayes_priors.md)),
then runs
[`check_convergence()`](https://elkronos.github.io/bmbeR/reference/check_convergence.md)
on the result.

## Usage

``` r
fit_model_with_prior(
  data,
  formula,
  family = gaussian(),
  prior_config = NULL,
  chains = 4,
  iter = 2000,
  seed = 1234,
  on_nonconvergence = c("warn", "error", "ignore"),
  convergence_args = list(),
  ...
)
```

## Arguments

- data:

  A data frame containing all variables in `formula`.

- formula:

  A model formula, e.g. `y ~ x1 + x2`.

- family:

  A family object, family function or family name: `gaussian`,
  `binomial`, `poisson`, `Gamma`, `inverse.gaussian` or
  `neg_binomial_2`.

- prior_config:

  Priors as a
  [`prior_config()`](https://elkronos.github.io/bmbeR/reference/prior_config.md)
  object, the output of
  [`empirical_bayes_priors()`](https://elkronos.github.io/bmbeR/reference/empirical_bayes_priors.md),
  a bmbeR 1.x list with `intercept` and `slope` elements, or `NULL` for
  rstanarm's defaults.

- chains:

  Number of Markov chains (at least 4 are recommended).

- iter:

  Iterations per chain, including warm-up (half by default).

- seed:

  Random seed passed to Stan, for reproducibility.

- on_nonconvergence:

  What to do if
  [`check_convergence()`](https://elkronos.github.io/bmbeR/reference/check_convergence.md)
  fails: `"warn"` (default), `"error"` or `"ignore"`.

- convergence_args:

  A list of further arguments for
  [`check_convergence()`](https://elkronos.github.io/bmbeR/reference/check_convergence.md),
  e.g. `list(rhat_threshold = 1.01)`.

- ...:

  Further arguments passed to
  [`rstanarm::stan_glm()`](https://mc-stan.org/rstanarm/reference/stan_glm.html),
  e.g. `cores`, `refresh = 0`, `adapt_delta` or `weights`.

## Value

A `stanreg` object with attribute `bmb_convergence` (a
[`check_convergence()`](https://elkronos.github.io/bmbeR/reference/check_convergence.md)
result).

## Details

**Default priors.** Components of `prior_config` that are `NULL` use
rstanarm's weakly informative defaults, which are *autoscaled* to the
data (Gelman et al., 2008). bmbeR 1.x instead used an unscaled
`student_t(3, 0, 2.5)` prior for all coefficients, which can dominate
the likelihood when variables are not on a unit scale.

**Convergence.** Diagnostics are always computed and stored in
`attr(fit, "bmb_convergence")`. By default a failed check issues a
warning but still returns the fit, so that the (expensive) model is not
lost and can be inspected; use `on_nonconvergence = "error"` to stop
instead.

**Parallel chains.** bmbeR does not choose the number of cores. Set
`options(mc.cores = parallel::detectCores())` or pass `cores = ` through
`...`.

## References

Gelman, A., Jakulin, A., Pittau, M. G., & Su, Y.-S. (2008). A weakly
informative default prior distribution for logistic and other regression
models. *The Annals of Applied Statistics*, 2(4), 1360–1383.
[doi:10.1214/08-AOAS191](https://doi.org/10.1214/08-AOAS191)

## See also

[`prior_predictive_check()`](https://elkronos.github.io/bmbeR/reference/prior_predictive_check.md)
to check priors before fitting,
[`prior_sensitivity()`](https://elkronos.github.io/bmbeR/reference/prior_sensitivity.md)
to check their influence afterwards.

## Examples

``` r
# \donttest{
data(kidiq, package = "rstanarm")
fit <- fit_model_with_prior(
  kidiq, kid_score ~ mom_iq + mom_hs,
  prior_config = prior_config(
    intercept = prior_spec("normal", location = 80, scale = 20),
    slope     = prior_spec("normal", location = 0, scale = c(1, 10))
  ),
  chains = 4, iter = 1000, refresh = 0
)
attr(fit, "bmb_convergence")
#> <bmb_convergence> PASSED  (4 chains, 2000 post-warm-up draws)
#>   Criteria: R-hat <= 1.01; bulk/tail-ESS >= 400; divergences <= 0; E-BFMI >= 0.3
#>   Max R-hat: 1.003 | min bulk-ESS: 1974 | min tail-ESS: 1359
#>   Divergences: 0 | max-treedepth hits: 0 | min E-BFMI: 0.96
# }
```
