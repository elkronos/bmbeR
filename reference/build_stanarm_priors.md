# Translate prior specifications into rstanarm prior arguments

Converts bmbeR prior specifications into the `prior_intercept`, `prior`
and `prior_aux` arguments of
[`rstanarm::stan_glm()`](https://mc-stan.org/rstanarm/reference/stan_glm.html).
Components that are `NULL` are omitted so that rstanarm's defaults
apply; flat priors are returned as explicit `NULL` entries, which is how
rstanarm requests them.

## Usage

``` r
build_stanarm_priors(
  intercept_config = NULL,
  slope_config = NULL,
  aux_config = NULL
)
```

## Arguments

- intercept_config, slope_config, aux_config:

  Priors for the intercept, the coefficients and the auxiliary
  parameter, in any format accepted by
  [`prior_config()`](https://elkronos.github.io/bmbeR/reference/prior_config.md).

## Value

A named list suitable for `do.call(rstanarm::stan_glm, ...)`.

## Examples

``` r
str(build_stanarm_priors(prior_spec("normal", 0, 10), prior_spec("normal", 0, 1)))
#> List of 2
#>  $ prior_intercept:List of 5
#>   ..$ dist     : chr "normal"
#>   ..$ df       : logi NA
#>   ..$ location : num 0
#>   ..$ scale    : num 10
#>   ..$ autoscale: logi FALSE
#>  $ prior          :List of 5
#>   ..$ dist     : chr "normal"
#>   ..$ df       : logi NA
#>   ..$ location : num 0
#>   ..$ scale    : num 1
#>   ..$ autoscale: logi FALSE
```
