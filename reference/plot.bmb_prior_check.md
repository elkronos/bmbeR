# Plot a prior predictive check

For continuous and count outcomes, overlays densities of simulated data
sets on the observed outcome density (shown only for scale). For binary
outcomes, shows the distribution of simulated success proportions.

## Usage

``` r
# S3 method for class 'bmb_prior_check'
plot(x, ndraws = 50L, ...)
```

## Arguments

- x:

  A `bmb_prior_check` object.

- ndraws:

  Number of simulated data sets to overlay.

- ...:

  Unused.

## Value

A ggplot object.
