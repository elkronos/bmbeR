# Plot power-scaling sensitivity

Shows how far each posterior mean moves (in posterior SDs) when the
prior or the likelihood is power-scaled by each `alpha`. Hollow points
mark importance-sampling estimates flagged as unreliable by Pareto
\\\hat{k}\\.

## Usage

``` r
# S3 method for class 'bmb_prior_sensitivity'
plot(x, ...)
```

## Arguments

- x:

  A `bmb_prior_sensitivity` object.

- ...:

  Unused.

## Value

A ggplot object.
