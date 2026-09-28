# Plot posterior intervals

Shows posterior medians with inner (default 50%) and outer (default 95%)
central credible intervals for each parameter.

## Usage

``` r
plot_posterior_distributions(
  model_fit,
  prob = 0.95,
  prob_inner = 0.5,
  pars = NULL,
  ...
)
```

## Arguments

- model_fit:

  A `stanreg` or `stanfit` object.

- prob:

  Probability mass of the outer interval.

- prob_inner:

  Probability mass of the inner interval.

- pars:

  Parameters to show. Defaults to all model parameters (for `stanfit`
  objects, all except `lp__`). Parameters on very different scales are
  easier to read when plotted separately.

- ...:

  Passed to
  [`bayesplot::mcmc_intervals()`](https://mc-stan.org/bayesplot/reference/MCMC-intervals.html).

## Value

A ggplot object.

## Examples

``` r
# \donttest{
data(kidiq, package = "rstanarm")
fit <- rstanarm::stan_glm(kid_score ~ mom_iq + mom_hs, data = kidiq,
                          chains = 2, iter = 1000, refresh = 0)
plot_posterior_distributions(fit, pars = c("mom_iq", "mom_hs"))

# }
```
