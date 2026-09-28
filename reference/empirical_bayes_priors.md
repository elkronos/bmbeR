# Data-informed priors: unit-information, empirical Bayes and power priors

Derives priors for the intercept and regression coefficients of a
generalised linear model from data, using one of three published methods
that avoid the "double-dipping" problem of centring a tight prior on
estimates from the same data that will be used to fit the model (which
counts the data twice and produces overconfident posteriors).

## Usage

``` r
empirical_bayes_priors(
  data,
  formula,
  family = gaussian(),
  method = c("unit_information", "eb_shrinkage", "power"),
  type = c("normal", "student_t", "cauchy"),
  a0 = 0.5,
  df = 3,
  dist_types = NULL
)
```

## Arguments

- data:

  A data frame. For `method = "power"`, the historical data. Rows with
  missing values in the variables of `formula` are dropped (with a
  message); other columns are ignored.

- formula:

  Model formula, identical to the one you will fit.

- family:

  Model family (object, function or name), identical to the one you will
  fit.

- method:

  One of `"unit_information"`, `"eb_shrinkage"` or `"power"`.

- type:

  Prior family for the output: `"normal"`, `"student_t"` or `"cauchy"`.

- a0:

  Power-prior discount in `(0, 1]` (only for `method = "power"`).

- df:

  Degrees of freedom when `type = "student_t"`.

- dist_types:

  Deprecated (bmbeR 1.x). rstanarm requires a single prior family for
  all coefficients; use `type`.

## Value

A
[`prior_config()`](https://elkronos.github.io/bmbeR/reference/prior_config.md)
object (class `bmb_prior_config`) that can be passed to
[`fit_model_with_prior()`](https://elkronos.github.io/bmbeR/reference/fit_model_with_prior.md),
with attributes `table` (a data frame of estimates, standard errors and
prior parameters), `method`, `terms` and, for `"eb_shrinkage"`, `tau`.

## Details

A maximum-likelihood GLM with the same formula and family is fitted
first. Coefficients are on the link scale of `family`, so the priors are
directly usable in
[`fit_model_with_prior()`](https://elkronos.github.io/bmbeR/reference/fit_model_with_prior.md).
The intercept prior is placed on the intercept at the predictor means,
which is how rstanarm parameterises the model.

- `method = "unit_information"` (default; Kass & Wasserman, 1995): each
  prior is centred at the estimate with variance `n` times the sampling
  variance, i.e. the prior carries the information of a single
  observation. It is the prior implicit in BIC and is suitable when the
  same data are used to fit the model, because it adds only `1/n` of the
  data's information.

- `method = "eb_shrinkage"` (parametric empirical Bayes; Efron & Morris,
  1973; Morris, 1983): coefficients are placed on a common scale by
  multiplying by the predictor standard deviations, and a shared prior
  standard deviation `tau` is estimated by maximising the marginal
  likelihood of `b_j ~ N(0, tau^2 + se_j^2)`. Slopes then receive
  zero-centred priors with scale `tau / sd(x_j)`, which shrink noisy
  estimates towards zero. Requires at least three slopes. When the
  estimate of `tau` falls below the median standard error it is floored
  at that value (with a warning), because a boundary estimate of zero
  would give a degenerate prior. The intercept uses the unit-information
  prior.

- `method = "power"` (Ibrahim & Chen, 2000): `data` must be *historical
  or external* data, not the data you will analyse. The prior is the
  normal approximation to the historical likelihood raised to the power
  `a0`: centred at the historical estimates, with variances inflated by
  `1 / a0`. `a0 = 1` borrows the historical data at full weight; smaller
  values discount it.

With `type = "student_t"` or `"cauchy"`, the same locations and scales
are used with heavier tails, which lets the data override the prior if
the two conflict (O'Hagan & Pericchi, 2012).

## References

Kass, R. E., & Wasserman, L. (1995). A reference Bayesian test for
nested hypotheses and its relationship to the Schwarz criterion. *JASA*,
90(431), 928–934.
[doi:10.1080/01621459.1995.10476592](https://doi.org/10.1080/01621459.1995.10476592)

Efron, B., & Morris, C. (1973). Stein's estimation rule and its
competitors—an empirical Bayes approach. *JASA*, 68(341), 117–130.
[doi:10.1080/01621459.1973.10481350](https://doi.org/10.1080/01621459.1973.10481350)

Morris, C. N. (1983). Parametric empirical Bayes inference: Theory and
applications. *JASA*, 78(381), 47–55.
[doi:10.1080/01621459.1983.10477914](https://doi.org/10.1080/01621459.1983.10477914)

Ibrahim, J. G., & Chen, M.-H. (2000). Power prior distributions for
regression models. *Statistical Science*, 15(1), 46–60.
[doi:10.1214/ss/1009212673](https://doi.org/10.1214/ss/1009212673)

O'Hagan, A., & Pericchi, L. (2012). Bayesian heavy-tailed models and
conflict resolution: A review. *Brazilian Journal of Probability and
Statistics*, 26(4), 372–401.
[doi:10.1214/11-BJPS164](https://doi.org/10.1214/11-BJPS164)

## Examples

``` r
data(kidiq, package = "rstanarm")
empirical_bayes_priors(kidiq, kid_score ~ mom_iq + mom_hs)
#> <bmb_prior_config> data-informed priors
#>   method : unit-information prior (Kass & Wasserman, 1995) 
#>   family : gaussian  (link scale); n = 434 
#>   type   : normal 
#> 
#>                   term estimate std_error prior_location prior_scale
#>  (Intercept) [centred]   86.800    0.8710         86.800       18.10
#>                 mom_iq    0.564    0.0606          0.564        1.26
#>                 mom_hs    5.950    2.2100          5.950       46.10

# Power prior from a (here simulated) historical study, discounted by half
historical <- kidiq[sample(nrow(kidiq), 200), ]
empirical_bayes_priors(historical, kid_score ~ mom_iq + mom_hs,
                       method = "power", a0 = 0.5)
#> method = "power": `data` is treated as historical data. Do not fit the resulting priors to the same data.
#> <bmb_prior_config> data-informed priors
#>   method : power prior from historical data, a0 = 0.5 (Ibrahim & Chen, 2000) 
#>   family : gaussian  (link scale); n = 200 
#>   type   : normal 
#> 
#>                   term estimate std_error prior_location prior_scale
#>  (Intercept) [centred]   85.600      1.29         85.600       1.820
#>                 mom_iq    0.614      0.09          0.614       0.127
#>                 mom_hs    5.460      3.18          5.460       4.500
```
