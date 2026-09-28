# Bayesian analysis report (BARG checklist)

Assembles the information that the Bayesian Analysis Reporting
Guidelines (BARG; Kruschke, 2021) ask authors to report, marks each item
as `ok`, `check` (needs attention) or `missing` (not yet done), and
computes the pieces that are cheap to obtain from the fit itself:
convergence diagnostics, a posterior summary, posterior predictive
p-values, PSIS-LOO and power-scaling prior sensitivity.

## Usage

``` r
workflow_report(
  fit,
  prior_check = NULL,
  sensitivity = NULL,
  performance = NULL,
  prob = 0.95
)
```

## Arguments

- fit:

  A `stanreg` object from
  [`rstanarm::stan_glm()`](https://mc-stan.org/rstanarm/reference/stan_glm.html)
  or
  [`fit_model_with_prior()`](https://elkronos.github.io/bmbeR/reference/fit_model_with_prior.md).

- prior_check:

  Optional result of
  [`prior_predictive_check()`](https://elkronos.github.io/bmbeR/reference/prior_predictive_check.md).

- sensitivity:

  Optional result of
  [`prior_sensitivity()`](https://elkronos.github.io/bmbeR/reference/prior_sensitivity.md)
  or
  [`sensitivity_analysis()`](https://elkronos.github.io/bmbeR/reference/sensitivity_analysis.md).
  If `NULL`,
  [`prior_sensitivity()`](https://elkronos.github.io/bmbeR/reference/prior_sensitivity.md)
  is run.

- performance:

  Optional result of
  [`evaluate_model_performance()`](https://elkronos.github.io/bmbeR/reference/evaluate_model_performance.md)
  on held-out data.

- prob:

  Probability mass of the reported credible intervals.

## Value

An object of class `bmb_report` with elements `items` (a data frame with
one row per checklist item: `item`, `status`, `detail`), `posterior`
(posterior summary table), `priors`, `convergence`, `loo`, `ppc` and
`sensitivity`.

## Details

Supply the results of steps that require extra work, such as
[`prior_predictive_check()`](https://elkronos.github.io/bmbeR/reference/prior_predictive_check.md)
and
[`evaluate_model_performance()`](https://elkronos.github.io/bmbeR/reference/evaluate_model_performance.md),
to complete the checklist.

## References

Kruschke, J. K. (2021). Bayesian analysis reporting guidelines. *Nature
Human Behaviour*, 5, 1282–1291.
[doi:10.1038/s41562-021-01177-7](https://doi.org/10.1038/s41562-021-01177-7)

Gelman, A., Meng, X.-L., & Stern, H. (1996). Posterior predictive
assessment of model fitness via realized discrepancies. *Statistica
Sinica*, 6(4), 733–760.

## Examples

``` r
# \donttest{
data(kidiq, package = "rstanarm")
fit <- fit_model_with_prior(kidiq, kid_score ~ mom_iq + mom_hs,
                            chains = 4, iter = 1000, refresh = 0)
workflow_report(fit)
#> Bayesian analysis report (BARG checklist; Kruschke, 2021)
#> ============================================================
#> [ok]    1. Model
#>           kid_score ~ mom_iq + mom_hs; gaussian family, identity link; 434
#>           observations; 3 coefficients
#> [ok]    2. Priors
#>           rstanarm defaults (weakly informative, autoscaled): (Intercept) ~
#>           normal(86.8, 51); mom_iq ~ normal(0, 3.4); mom_hs ~ normal(0,
#>           124); sigma ~ exponential(rate = 0.049)
#> [ -- ]  3. Prior predictive check
#>           Not supplied: run prior_predictive_check().
#> [ok]    4. MCMC computation
#>           4 chains x 1000 iterations (500 warm-up), seed 1234. Max R-hat
#>           1.006, min bulk-ESS 2120, min tail-ESS 1080, 0 divergences.
#> [ok]    5. Posterior summary
#>           Means, medians, SDs and 95% central credible intervals for 4
#>           parameters (see $posterior).
#> [ok]    6. Posterior predictive check
#>           Posterior predictive p-values: mean 0.51, sd 0.52, min 0.80, max
#>           0.67
#> [ok]    7. Cross-validation (PSIS-LOO)
#>           elpd_loo = -1876.1 (SE 14.3), p_loo = 4.1; 0 observation(s) with
#>           Pareto k > 0.70
#> [ok]    8. Prior sensitivity
#>           Power-scaling: no parameter is sensitive to the prior.
#> [ -- ]  9. Held-out predictive performance
#>           Not supplied: run evaluate_model_performance() on test data
#>           (optional if LOO suffices).
#> [ok]    10. Software
#>           R 4.6.1; rstanarm 2.32.2; rstan 2.32.7; bmbeR 2.0.0
#> 
#> Posterior summary:
#>     variable   mean median     sd   q2.5  q97.5 p_positive
#>  (Intercept) 25.700 25.800 5.9000 13.900 37.200      1.000
#>       mom_iq  0.564  0.563 0.0603  0.449  0.683      1.000
#>       mom_hs  5.980  6.040 2.2300  1.660 10.400      0.998
#>        sigma 18.200 18.200 0.6460 16.900 19.500      1.000
#> 
#> 0 item(s) need attention, 2 not yet done.
# }
```
