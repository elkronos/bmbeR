# Compare models on the same held-out data

Compares the held-out expected log predictive density (ELPD) of several
models evaluated with
[`evaluate_model_performance()`](https://elkronos.github.io/bmbeR/reference/evaluate_model_performance.md)
on the *same* test observations. Differences are paired by observation,
and their standard errors are computed from the per-observation
differences, as in
[`loo::loo_compare()`](https://mc-stan.org/loo/reference/loo_compare.html).

## Usage

``` r
compare_performance(...)
```

## Arguments

- ...:

  Named `bmb_performance` objects, or a single named list of them.

## Value

A data frame sorted from best to worst with columns `model`, `elpd`,
`elpd_diff` (relative to the best model) and `se_diff`.

## Examples

``` r
# \donttest{
data(wells, package = "rstanarm")
wells$dist100 <- wells$dist / 100
set.seed(1)
test <- sample(nrow(wells), 1000)
fit1 <- rstanarm::stan_glm(switch ~ dist100, family = binomial(),
                           data = wells[-test, ], refresh = 0)
fit2 <- rstanarm::stan_glm(switch ~ dist100 + arsenic, family = binomial(),
                           data = wells[-test, ], refresh = 0)
compare_performance(
  distance = evaluate_model_performance(fit1, wells[test, ]),
  distance_arsenic = evaluate_model_performance(fit2, wells[test, ])
)
#>              model      elpd elpd_diff  se_diff
#> 1 distance_arsenic -644.7463   0.00000 0.000000
#> 2         distance -671.6709 -26.92464 6.313672
# }
```
