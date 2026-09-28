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
