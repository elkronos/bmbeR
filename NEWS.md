# bmbeR 2.1.0

Fixes from a second review of 2.0.0, each with a regression test.

## Bug fixes

* Power priors from `empirical_bayes_priors(method = "power")` placed the
  intercept prior at the historical data's predictor means, but rstanarm
  applies it at the analysed data's means. With different means the
  intercept was badly wrong. `fit_model_with_prior()` and
  `prior_predictive_check()` now re-express the intercept prior at the means
  of the data being fitted.
* `evaluate_model_performance()` ignored offsets in test data, so every metric
  was wrong for models with an exposure. Formula offsets are now computed from
  the test data, and the new `offset` argument supplies offsets for models
  fitted with `stan_glm(offset = )`.
* Unit-information priors counted rows instead of Bernoulli trials for
  `cbind(successes, failures)` responses, making the prior as informative as
  several observations rather than one. `n` is now the number of trials, or
  the sum of the new `weights` argument.
* Formulas with `.` and formulas that use variables from their environment
  (e.g. `poly(x, k)`) are accepted.
* `weights`, `subset`, `offset` and `na.action` passed through `...` are
  evaluated inside the data, as in `stan_glm()`, so `weights = w` works. The
  stored model call refers to the data by name instead of containing a copy
  of it.
* `sensitivity_analysis()` validates every configuration before fitting,
  keeps going when one configuration fails (recording the error in
  `$errors`), and reports the number of observations with high Pareto k.
* R's random number stream is no longer changed by `fit_model_with_prior()`,
  `prior_predictive_check()`, `evaluate_model_performance()`,
  `sensitivity_analysis()` or `workflow_report()`.
* `compare_performance()` checks that models were scored on the same test
  observations, not only the same number of them.
* `prior_predictive_check()` rejects flat priors, validates `chains`, `iter`
  and `plausible_range`, and summarises sampler warnings instead of hiding
  them.
* `workflow_report()`: held-out performance is marked `check` when interval
  coverage is off by more than two standard errors; rstanarm's defaults are
  recognised for intercept-only models; binary outcomes get a binned
  calibration check and a prior predictive summary based on success
  proportions; QR fits get an accurate message about prior sensitivity.
* `check_convergence()` no longer fails parameters with constant draws,
  validates its thresholds, and reports the R-hat of the failing parameters.
* `empirical_bayes_priors()` reports separation and other warnings from the
  maximum-likelihood fit.
* Smaller fixes: rstanarm priors with default arguments (e.g.
  `rstanarm::normal()`) are translated; Student-t `df` lengths are checked;
  legacy exponential priors given by `scale` keep it; family names are looked
  up in the caller's environment; `seed` is validated; `add_distribution()`
  rejects a missing name; `evaluate_model_performance(formula = )` must match
  the model.

## New features

* `prior_table()` returns the priors a fit used, after autoscaling.
* `sensitivity_analysis(cv = "kfold")` compares configurations by K-fold
  cross-validation, `cv = "none"` compares posteriors only (needed for
  weighted models, which rstanarm cannot cross-validate), and
  `parallel = TRUE` fits the configurations in parallel with future.apply.
* `prior_table()`, `prior_sensitivity()`, `plot_prior_posterior()` and
  `workflow_report()` accept `stan_glmer()` fits. The population-level priors
  are analysed; the group-level covariance prior is held fixed.
* `as_priorsense_draws()` exports a fit, with its log prior and
  log-likelihood, for the priorsense package.
* `as.data.frame()` methods for convergence, sensitivity and prior check
  results.
* `format(report, type = "markdown")`, and knitting a report in R Markdown or
  Quarto, give a Markdown checklist.
* `workflow_report()` gains a `seed` argument.
* `citation("bmbeR")`.

## Internal

* withr moved from Suggests to Imports; future, future.apply and priorsense
  added to Suggests.
* GitHub Actions are pinned to commit SHAs and kept current by Dependabot; a
  test-coverage workflow was added.

# bmbeR 2.0.0

A rewrite following an adversarial review of version 1.01; see `REVIEW.md`
for the findings and the evidence behind them. bmbeR is now an installable R
package with documentation, tests, continuous integration and a website.

## Bug fixes

* `fit_model_with_prior()` and `sensitivity_analysis()` failed on every call
  because `check_convergence()` used functions that rstan does not export.
* `plot_posterior_distributions()` always errored; `generate_plot()` displayed
  nothing; `check_convergence(save_plots = TRUE)` wrote an empty PDF.
* `student_t_prior()` sampled a non-central t whose mean was not `mu`; it now
  samples the location-scale t that rstanarm uses.
* `evaluate_model_performance()` thresholded log-odds instead of
  probabilities for logistic models.
* `empirical_bayes_priors()` fitted `lm()` for all families (wrong scale for
  GLMs), centred the intercept prior on the uncentred intercept, and rejected
  data with missing values in unused columns.
* Integer 0/1 outcomes, unevaluated family functions (`family = binomial`),
  family names and transformed responses (`log(y) ~ x`) now work.
* `add_distribution()` and `get_prior_distribution()` now work together via
  the `distributions` argument.
* Generators validate that scales are positive and parameters finite.

## Breaking changes

* Default priors are now rstanarm's autoscaled weakly informative priors,
  rather than an unscaled `student_t(3, 0, 2.5)` that could override the
  data when variables were not on a unit scale.
* `empirical_bayes_priors()` no longer centres a prior with scale equal to
  the standard error on estimates from the same data (which counts the data
  twice). It offers `method = "unit_information"` (default),
  `"eb_shrinkage"` and `"power"`, and returns a `prior_config` object that
  `fit_model_with_prior()` accepts directly. The per-predictor `dist_types`
  argument is deprecated because rstanarm uses one prior family for all
  coefficients.
* `check_convergence()` returns a `bmb_convergence` object (use
  `isTRUE(x$converged)`) and uses current thresholds: rank-normalised R-hat
  ≤ 1.01, bulk- and tail-ESS ≥ 100 per chain, no divergences, E-BFMI ≥ 0.3.
  The ESS-ratio argument `ess_threshold` is deprecated.
* `fit_model_with_prior()` warns instead of stopping when convergence checks
  fail (see `on_nonconvergence`), no longer sets `cores`, and defaults to
  `iter = 2000`.
* `sensitivity_analysis()` returns a `bmb_sensitivity` object with posterior
  shifts and LOO comparisons instead of a list of numbers, and no longer
  refits observations with high Pareto k unless `k_threshold` is given.
* `evaluate_model_performance()` returns a `bmb_performance` object; the
  metric names from 1.x (`RMSE`, `MAE`, `Accuracy`, ...) are unchanged.
* `generate_plot()` returns a list of ggplot objects.
* The helpers `validate_positive_integer()`, `validate_numeric()` and `%||%`
  are no longer exported. The `libraries_and_setup.R` script was removed: the
  package no longer installs packages or changes global options, the random
  seed or the ggplot2 theme.

## New features

* `prior_spec()` and `prior_config()`: validated prior specifications that
  also accept rstanarm prior objects and the 1.x list format.
* `prior_predictive_check()`: prior predictive simulation with plausibility
  summaries, including the share of extreme probabilities in logistic models.
* `prior_sensitivity()`: power-scaling prior and likelihood sensitivity for
  rstanarm fits without refitting, with posterior contraction, a prior-data
  conflict z-score and importance-sampled finite perturbations.
* `plot_prior_posterior()`: prior vs posterior densities without refitting.
* `compare_performance()`: paired ELPD comparison of models on the same test
  data.
* `evaluate_model_performance()` adds held-out ELPD, CRPS, Brier score, AUC
  and predictive interval coverage.
* `workflow_report()`: a checklist aligned with the Bayesian Analysis
  Reporting Guidelines.
* Rank plots in `generate_plot()` and `plot.bmb_convergence()`.
