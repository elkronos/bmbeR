# Adversarial review of bmbeR

This document reviews bmbeR as it stood at commit `3f55fb6` (version "1.01"),
records the evidence for each finding, and maps each finding to its resolution
in version 2.0.0. The review asked four questions: does the code **work**, is
it **statistically correct**, is it **easy to use and well documented**, and
does it offer **anything beyond existing R packages**?

## Verdict

**Before 2.0.0 the code did not work.** The central function,
`fit_model_with_prior()`, failed on every call because the convergence check
it calls used a function that rstan does not export; `sensitivity_analysis()`
depends on it and failed too. Of the remaining functions, one plotting
function always errored, another silently displayed nothing, and several
produced statistically wrong results without any warning:

* a prior generator whose mean was double the requested value;
* a default prior that ignored the units of the data and could override them
  completely;
* "empirical Bayes" priors that counted the data twice (about 83% coverage
  for nominal 95% intervals) and were on the wrong scale for non-Gaussian models;
* classification metrics computed on log-odds instead of probabilities.

There was no package structure, no tests, no licence and no documentation
beyond a list of function names. The code offered nothing that rstanarm,
bayesplot and loo do not already do better.

Version 2.0.0 is a rewrite that fixes every finding below, adds regression
tests for each, and reorients the package around a gap in the existing
ecosystem: a principled, prior-focused workflow for rstanarm models (see
[What bmbeR adds](#what-bmber-adds-over-existing-packages)).

## Method

* **Static review** of all 1,215 lines of R and the README.
* **Dynamic review**: every exported function was run against real data
  (`kidiq`, `wells` from rstanarm; `mtcars`) and adversarial inputs on R 4.3.3
  with rstanarm 2.32.1, rstan 2.32.5, bayesplot 1.11.1 and loo 2.6.0. Where a
  crash hid later behaviour, the crashing call was patched locally so that the
  downstream code could be examined too.
* **Statistical checks** against closed-form results and simulation.

The script `review/reproduce-review.R` re-runs the probes against the original
code (retrieved from git history), so every number quoted below can be
reproduced.

## Findings

Severity: **Critical** = the function cannot be used; **High** = silently
wrong results; **Medium** = usability or design defect; **Low** = hygiene.

### Critical

| # | Finding | Evidence | Resolution in 2.0.0 |
|---|---|---|---|
| C1 | `check_convergence()` calls `rstan::rhat()` and `rstan::neff_ratio()`, which rstan does not export (they live in bayesplot). `fit_model_with_prior()` calls `check_convergence()`, so **every model fit failed**, and so did `sensitivity_analysis()`. | `Error: 'rhat' is not an exported object from 'namespace:rstan'` | Rewritten on `posterior::rhat()`, `ess_bulk()` and `ess_tail()`; covered by unit tests on synthetic chains and integration tests on real fits. |
| C2 | `plot_posterior_distributions()` passes the list returned by `rstan::extract()` to `bayesplot::mcmc_intervals()`, which needs a matrix or array. It **always errored**. It also combined `prob = 0.95` with bayesplot's default `prob_outer = 0.9`. | `Error: Arrays should have 2 or 3 dimensions` | Uses a draws array; inner/outer interval probabilities are explicit and validated. |
| C3 | `generate_plot()` creates ggplot objects inside a loop and never prints or returns them: **nothing is displayed**. `check_convergence(save_plots = TRUE)` has the same bug and writes an **empty PDF** (0 pages). | PDF of 3,611 bytes containing no pages. | Plots are returned as a named list (and printed on request); saved PDFs contain rank and trace plots (tested: file size > 10 kB). |
| C4 | Not an R package: no `DESCRIPTION`/`NAMESPACE`, so `@export` tags did nothing and no help pages existed. Sourcing any script called `install.packages()` and `library()`; `libraries_and_setup.R` changed global options, set the global seed, changed the ggplot2 theme and printed `sessionInfo()`. | By inspection. | Proper package: roxygen documentation, declared dependencies, no side effects on load, GPL-3 licence. |

### High: silently wrong results

| # | Finding | Evidence | Resolution in 2.0.0 |
|---|---|---|---|
| H1 | `student_t_prior()` used a *non-central* t (`rt(n, nu, ncp = mu) * sigma`), not the location-scale t that rstanarm's `student_t()` prior uses. | `mu = 5, sigma = 2, nu = 30` gave mean **10.25** (expected 5) and SD 2.49 (expected 2.07). | `mu + sigma * rt(n, nu)`; tested against the theoretical mean and SD. |
| H2 | The default prior, `student_t(3, 0, 2.5)` on every coefficient **without autoscaling**, ignores the units of the data. With the outcome in different units it overrides the likelihood, and the chains converge, so nothing warns. | `mtcars`: slope of `mpg ~ wt` = −5.34 (OLS) vs −5.13 (bmbeR). Slope of `mpg*100 ~ wt` = −534 (OLS) vs **about −0.1** (bmbeR); intercept 3729 vs **about 0**. | Defaults are rstanarm's autoscaled weakly informative priors; a regression test checks that rescaling the outcome by 100 rescales the slope by 100. The *Choosing priors* article demonstrates the failure and how prior predictive checks catch it. |
| H3 | `empirical_bayes_priors()` centred a prior with scale = SE on the OLS estimate from the *same* data that are then used to fit the model. This counts the data twice: posterior variance is halved. | Simulation (2,000 data sets, n = 50): coverage of nominal 95% intervals **0.841** (vs 0.950 with a flat prior); the exact value is P(\|t₄₈\| < 1.96/√2) ≈ 0.83. | Replaced by three published methods: unit-information priors (Kass & Wasserman, 1995), parametric empirical Bayes shrinkage (Morris, 1983), and power priors for *historical* data (Ibrahim & Chen, 2000). The *Data-informed priors* article reproduces the coverage failure. |
| H4 | `empirical_bayes_priors()` always fitted `lm()`, even for logistic or Poisson models, so priors were on the wrong scale. | `wells`, `switch ~ dist100 + arsenic`: prior N(**−0.20**, 0.023²) for a coefficient whose logit-scale estimate is **−0.90**: an extremely tight prior in the wrong place. | A GLM in the model's own family is fitted, so priors are on the link scale. Tested. |
| H5 | The empirical-Bayes intercept prior was centred on the *uncentred* OLS intercept, but rstanarm applies `prior_intercept` to the intercept *at the predictor means*. | By inspection of rstanarm's parameterisation. | Priors are derived for the centred intercept. An integration test confirms that a tight intercept prior acts on the centred intercept. |
| H6 | `evaluate_model_performance()` thresholded `predict()` output at 0.5, but for rstanarm models `predict()` returns the **linear predictor (log-odds)**, not probabilities. | `wells`: 33% of "predictions" outside [0, 1]; accuracy **0.555** vs 0.618 when probabilities are used. | Uses the posterior mean of `posterior_epred()` (probabilities). Adds proper scoring rules (held-out ELPD, CRPS, Brier), AUC and interval coverage. Tested against a manual computation. |
| H7 | Convergence criteria were outdated: R-hat ≤ 1.1 (current recommendation 1.01), an ESS *ratio* ≥ 0.1 instead of bulk- and tail-ESS, and no check for divergent transitions or E-BFMI, the most important HMC-specific diagnostics. | Vehtari et al. (2021); Betancourt (2017). | Rank-normalised R-hat ≤ 1.01, bulk/tail-ESS ≥ 100 per chain, divergences, E-BFMI and tree-depth saturation. |
| H8 | `empirical_bayes_priors()` offered uniform, beta, gamma, binomial, Poisson, log-normal and Bernoulli "priors" for regression coefficients. Coefficients are real-valued and unbounded; discrete distributions cannot be priors for them; the beta mapping ignored the SE (`k = 10`); binomial used `size = round(1/se^2)`. None of these can be passed to rstanarm, which also requires a single prior family for all slopes, so per-predictor `dist_types` was unusable. | By inspection. | Output types limited to normal, Student-t and Cauchy, the families rstanarm supports. `dist_types` is deprecated with an informative message. |

### Medium: usability and design

| # | Finding | Resolution in 2.0.0 |
|---|---|---|
| M1 | The pipeline did not connect: the output of `empirical_bayes_priors()` (named list per predictor, no `type`) could not be passed to `fit_model_with_prior()` (`Error: argument is of length zero`). | All prior sources produce a `prior_config` object accepted by every fitting function; coefficient order is checked. |
| M2 | A failed convergence check called `stop()`, discarding the (expensive) fit, so it could not be inspected. | Warns and returns the fit with diagnostics attached (`on_nonconvergence = "warn"`); `"error"` and `"ignore"` are available. |
| M3 | Integer 0/1 outcomes were rejected for logistic regression; `family = binomial` (unevaluated, as `glm()` allows) crashed; `log(y) ~ x` crashed evaluation. | Families accepted as object, function or name; responses validated by family; transformed and `cbind()` responses supported. |
| M4 | `add_distribution()` returned a modified list that `get_prior_distribution()` could never see (it read a global). | `get_prior_distribution(..., distributions = )` takes the registry explicitly. |
| M5 | `cores = parallel::detectCores()` was hard-coded, unfriendly on shared machines and a clash with a user-supplied `cores`. | Cores left to the user (`options(mc.cores)` or `cores =`). |
| M6 | `sensitivity_analysis()` returned only one LOO number per prior: no posterior summaries, no standard errors of differences. LOO measures predictive performance, not prior influence. `k_threshold = 0.7` silently triggered refits. | Returns posterior shifts (in reference SDs) and `loo_compare()` differences with SEs. Adds `prior_sensitivity()` (power-scaling, no refit). |
| M7 | Missing values in *any* column, even unused ones, stopped `empirical_bayes_priors()`. | Only model variables are checked; dropped rows are reported. |
| M8 | Inconsistent argument names (`mu`/`sigma`/`nu` vs `location`/`scale`). | `prior_spec()` uses rstanarm's names; the 1.x format is still accepted. |
| M9 | Input validation let through `Inf`, `NaN` and negative scales (NaN draws with a warning); `validate_positive_integer(NA)` gave a cryptic error. | Validation checks finiteness and positivity with clear messages. |

### Low: hygiene

* `memoise(lm)` was re-created on every call, so it never cached anything; it
  added a dependency for no benefit. `cowplot` and `purrr` were loaded but
  never used. `install.packages("parallel")` targets a base package.
* `%||%` was exported, masking the base R (≥ 4.4) and rlang operators.
* Validation helpers were exported as part of the public API.
* A stale reference to `06_model_fitting.R`; roxygen text contradicting the
  code (the "scaled and shifted" t that was non-central).

All resolved: unused dependencies removed, helpers internal.

### Documentation

The README listed function names but had no installation instructions, no
example, no explanation of how the scripts fitted together and no references.
There were no help pages, vignettes, tests, continuous integration, website
or licence (so the code could not legally be reused).

Version 2.0.0 has help pages with runnable examples for every exported
function; a *Get started* vignette; five articles (choosing priors,
data-informed priors, convergence diagnostics, prior sensitivity, predictive
evaluation); a pkgdown website deployed by GitHub Actions; R CMD check on
Linux, macOS and Windows; and a test suite of real Stan fits plus
closed-form checks of the statistical methods.

## What bmbeR adds over existing packages

The original code wrapped rstanarm, bayesplot and loo, with nothing of its
own, and less reliably than calling those packages directly. Version 2.0.0
focuses on what the ecosystem lacks for rstanarm users:

| Capability | Existing tools | bmbeR 2.0.0 |
|---|---|---|
| Prior predictive check with a plausibility summary | `rstanarm` (`prior_PD = TRUE`) + `bayesplot`, assembled by hand | `prior_predictive_check()`: share of implausible values; share of extreme probabilities for logistic models |
| Power-scaling prior sensitivity | `priorsense` (brms, cmdstanr, rstan, JAGS, NIMBLE; not rstanarm fits) | `prior_sensitivity()` for rstanarm fits, reconstructing log-priors from `prior_summary()` (autoscaling, centred intercept) |
| Distinguishing an informative prior from a conflicting one | not available as a single diagnostic | `conflict_z`, exactly the Box (1980) prior predictive z-score in the normal model |
| Posterior contraction | computed by hand | reported per parameter |
| Prior vs posterior plot without refitting | `rstanarm::posterior_vs_prior()` refits the model | `plot_prior_posterior()` uses the analytic prior |
| Data-informed priors that avoid double-dipping | not packaged for rstanarm | unit-information, empirical Bayes shrinkage, power priors |
| Held-out proper scoring (ELPD, CRPS, Brier, coverage) with paired model comparison | `loo` (in-sample LOO), `scoringRules` (manual) | `evaluate_model_performance()`, `compare_performance()` |
| Modern convergence summary in one object | `posterior`, `rstan` (separate calls) | `check_convergence()` |
| Reporting checklist | BARG (a paper) | `workflow_report()` assembles and audits each item |

## Remaining limitations

These are deliberate scope limits or open questions, not known bugs:

* **Model class.** Only `rstanarm::stan_glm()` models are supported. Multilevel
  models (`stan_glmer()`) and brms are out of scope; for brms, priorsense
  provides power-scaling diagnostics.
* **Local diagnostics are local.** Power-scaling describes the posterior you
  obtained. A posterior stuck in a self-consistent but wrong explanation can
  look like "weak likelihood" rather than "conflict" (the *Choosing priors*
  article shows one); the prior predictive check is the global safeguard.
* **Normal-model interpretations.** The interpretations of the sensitivities
  and of `conflict_z` are exact for a normal posterior and approximate
  otherwise.
* **Thresholds are heuristics.** The defaults (`threshold = 0.05`,
  `z_threshold = 2`, `shift_threshold = 0.5`) are adjustable.
* **Empirical Bayes** uses the normal approximation to the GLM likelihood and
  does not support `neg_binomial_2`.
* **Priors unsupported by the sensitivity tools**: horseshoe, lasso,
  product-normal and R² priors, and models fitted with `QR = TRUE`. These
  produce informative errors.

## References

Betancourt, M. (2017). A conceptual introduction to Hamiltonian Monte Carlo. *arXiv:1701.02434*.

Box, G. E. P. (1980). Sampling and Bayes' inference in scientific modelling and robustness. *JRSS A*, 143(4), 383–430.

Gelman, A., Vehtari, A., Simpson, D., et al. (2020). Bayesian workflow. *arXiv:2011.01808*.

Ibrahim, J. G., & Chen, M.-H. (2000). Power prior distributions for regression models. *Statistical Science*, 15(1), 46–60.

Kallioinen, N., Paananen, T., Bürkner, P.-C., & Vehtari, A. (2023). Detecting and diagnosing prior and likelihood sensitivity with power-scaling. *Statistics and Computing*, 34, 57.

Kass, R. E., & Wasserman, L. (1995). A reference Bayesian test for nested hypotheses and its relationship to the Schwarz criterion. *JASA*, 90(431), 928–934.

Kruschke, J. K. (2021). Bayesian analysis reporting guidelines. *Nature Human Behaviour*, 5, 1282–1291.

Morris, C. N. (1983). Parametric empirical Bayes inference: Theory and applications. *JASA*, 78(381), 47–55.

Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Bürkner, P.-C. (2021). Rank-normalization, folding, and localization: An improved R-hat for assessing convergence of MCMC. *Bayesian Analysis*, 16(2), 667–718.
