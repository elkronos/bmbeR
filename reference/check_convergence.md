# Check MCMC convergence with current best-practice diagnostics

Assesses whether the Markov chains of a fitted model can be trusted,
following Vehtari et al. (2021) and the Stan developers' guidance:

## Usage

``` r
check_convergence(
  model_fit,
  rhat_threshold = 1.01,
  min_ess = NULL,
  max_divergent = 0,
  min_bfmi = 0.3,
  pars = NULL,
  save_plots = FALSE,
  plot_path = "trace_plot.pdf",
  ...
)
```

## Arguments

- model_fit:

  A `stanreg` (rstanarm) or `stanfit` (rstan) object fitted by MCMC.

- rhat_threshold:

  Maximum acceptable R-hat.

- min_ess:

  Minimum acceptable bulk- and tail-ESS. Defaults to
  `100 * number of chains`.

- max_divergent:

  Maximum acceptable number of divergent transitions.

- min_bfmi:

  Minimum acceptable E-BFMI per chain.

- pars:

  Optional character vector of parameters to check. By default all model
  parameters are checked.

- save_plots:

  If `TRUE`, save rank and trace plots to `plot_path`.

- plot_path:

  Path of the PDF file for `save_plots = TRUE`.

- ...:

  Deprecated arguments. `ess_threshold`, an ESS *ratio* used in bmbeR
  1.x, is converted to an absolute `min_ess` with a warning.

## Value

An object of class `bmb_convergence`: a list with elements `converged`
(logical), `parameters` (a data frame with R-hat, bulk-ESS and tail-ESS
per parameter), `sampler` (divergences, tree-depth hits, E-BFMI),
`issues` (character vector of problems), `thresholds`, and the draws
needed by the [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
method. Use `isTRUE(x$converged)` in code.

## Details

- **Rank-normalised split-R-hat** below `rhat_threshold` (default 1.01;
  the older 1.1 threshold is too lenient to detect many failures).

- **Bulk-ESS and tail-ESS** of at least `min_ess` (default 100 per
  chain), so that posterior means *and* interval endpoints are estimated
  reliably.

- **Divergent transitions** after warm-up: any divergence indicates the
  sampler could not explore part of the posterior and results may be
  biased (Betancourt, 2017).

- **E-BFMI** below `min_bfmi` (default 0.3) indicates poor exploration
  of the energy distribution.

- Iterations that **saturate the maximum tree depth** are reported as an
  efficiency warning; they do not by themselves invalidate the draws.

## References

Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Bürkner, P.-C.
(2021). Rank-normalization, folding, and localization: An improved R-hat
for assessing convergence of MCMC. *Bayesian Analysis*, 16(2), 667–718.
[doi:10.1214/20-BA1221](https://doi.org/10.1214/20-BA1221)

Betancourt, M. (2017). A conceptual introduction to Hamiltonian Monte
Carlo. *arXiv:1701.02434*.

## See also

[`generate_plot()`](https://elkronos.github.io/bmbeR/reference/generate_plot.md)
for more diagnostic plots.

## Examples

``` r
# \donttest{
data(kidiq, package = "rstanarm")
fit <- rstanarm::stan_glm(kid_score ~ mom_iq, data = kidiq,
                          chains = 2, iter = 1000, refresh = 0)
conv <- check_convergence(fit)
conv
#> <bmb_convergence> PASSED  (2 chains, 1000 post-warm-up draws)
#>   Criteria: R-hat <= 1.01; bulk/tail-ESS >= 200; divergences <= 0; E-BFMI >= 0.3
#>   Max R-hat: 1.001 | min bulk-ESS: 973 | min tail-ESS: 561
#>   Divergences: 0 | max-treedepth hits: 0 | min E-BFMI: 1.05
isTRUE(conv$converged)
#> [1] TRUE
# }
```
