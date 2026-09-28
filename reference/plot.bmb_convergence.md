# Plot convergence diagnostics

Rank plots (recommended by Vehtari et al., 2021) or trace plots for the
parameters in a
[`check_convergence()`](https://elkronos.github.io/bmbeR/reference/check_convergence.md)
result. Parameters that failed a check are shown first.

## Usage

``` r
# S3 method for class 'bmb_convergence'
plot(x, type = c("rank", "trace"), max_pars = 6L, ...)
```

## Arguments

- x:

  A `bmb_convergence` object.

- type:

  `"rank"` (rank-histogram overlay) or `"trace"`.

- max_pars:

  Maximum number of parameters to show.

- ...:

  Passed to the bayesplot function.

## Value

A ggplot object.
