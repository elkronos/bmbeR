# Compare each prior with its posterior

Overlays the prior density that rstanarm actually used (after any
autoscaling) on the posterior density of each parameter. A posterior
that looks like its prior indicates the data carry little information
about that parameter; a posterior squeezed against a region the prior
considers implausible suggests prior-data conflict. Unlike
[`rstanarm::posterior_vs_prior()`](https://mc-stan.org/rstanarm/reference/posterior_vs_prior.html),
no refitting is needed.

## Usage

``` r
plot_prior_posterior(fit, pars = NULL, n_grid = 300L)
```

## Arguments

- fit:

  A `stanreg` object from
  [`rstanarm::stan_glm()`](https://mc-stan.org/rstanarm/reference/stan_glm.html).

- pars:

  Parameters to show (default: all).

- n_grid:

  Number of grid points for the prior density.

## Value

A ggplot object.

## Details

The intercept is shown at the predictor means, which is the parameter
rstanarm places its prior on.

## Examples

``` r
# \donttest{
data(kidiq, package = "rstanarm")
fit <- rstanarm::stan_glm(kid_score ~ mom_iq, data = kidiq,
                          chains = 2, iter = 1000, refresh = 0)
plot_prior_posterior(fit)

# }
```
