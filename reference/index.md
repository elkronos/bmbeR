# Package index

## Specify priors

- [`prior_spec()`](https://elkronos.github.io/bmbeR/reference/prior_spec.md)
  : Specify a prior distribution
- [`prior_config()`](https://elkronos.github.io/bmbeR/reference/prior_config.md)
  : Combine priors for a regression model
- [`empirical_bayes_priors()`](https://elkronos.github.io/bmbeR/reference/empirical_bayes_priors.md)
  : Data-informed priors: unit-information, empirical Bayes and power
  priors
- [`build_stanarm_priors()`](https://elkronos.github.io/bmbeR/reference/build_stanarm_priors.md)
  : Translate prior specifications into rstanarm prior arguments

## Check priors before fitting

- [`prior_predictive_check()`](https://elkronos.github.io/bmbeR/reference/prior_predictive_check.md)
  : Prior predictive check
- [`plot(`*`<bmb_prior_check>`*`)`](https://elkronos.github.io/bmbeR/reference/plot.bmb_prior_check.md)
  : Plot a prior predictive check

## Fit and check the computation

- [`fit_model_with_prior()`](https://elkronos.github.io/bmbeR/reference/fit_model_with_prior.md)
  : Fit a Bayesian regression model with explicit priors
- [`check_convergence()`](https://elkronos.github.io/bmbeR/reference/check_convergence.md)
  : Check MCMC convergence with current best-practice diagnostics
- [`plot(`*`<bmb_convergence>`*`)`](https://elkronos.github.io/bmbeR/reference/plot.bmb_convergence.md)
  : Plot convergence diagnostics
- [`generate_plot()`](https://elkronos.github.io/bmbeR/reference/generate_plot.md)
  : Diagnostic plots for MCMC draws

## Quantify prior influence

- [`prior_sensitivity()`](https://elkronos.github.io/bmbeR/reference/prior_sensitivity.md)
  : Prior and likelihood sensitivity by power-scaling
- [`plot(`*`<bmb_prior_sensitivity>`*`)`](https://elkronos.github.io/bmbeR/reference/plot.bmb_prior_sensitivity.md)
  : Plot power-scaling sensitivity
- [`sensitivity_analysis()`](https://elkronos.github.io/bmbeR/reference/sensitivity_analysis.md)
  : Sensitivity analysis across alternative priors
- [`plot(`*`<bmb_sensitivity>`*`)`](https://elkronos.github.io/bmbeR/reference/plot.bmb_sensitivity.md)
  : Plot posterior intervals across prior configurations
- [`plot_prior_posterior()`](https://elkronos.github.io/bmbeR/reference/plot_prior_posterior.md)
  : Compare each prior with its posterior
- [`plot_posterior_distributions()`](https://elkronos.github.io/bmbeR/reference/plot_posterior_distributions.md)
  : Plot posterior intervals

## Evaluate and report

- [`evaluate_model_performance()`](https://elkronos.github.io/bmbeR/reference/evaluate_model_performance.md)
  : Evaluate predictive performance on test data
- [`compare_performance()`](https://elkronos.github.io/bmbeR/reference/compare_performance.md)
  : Compare models on the same held-out data
- [`workflow_report()`](https://elkronos.github.io/bmbeR/reference/workflow_report.md)
  : Bayesian analysis report (BARG checklist)

## Simulate from distributions

- [`student_t_prior()`](https://elkronos.github.io/bmbeR/reference/prior_generators.md)
  [`normal_prior()`](https://elkronos.github.io/bmbeR/reference/prior_generators.md)
  [`cauchy_prior()`](https://elkronos.github.io/bmbeR/reference/prior_generators.md)
  [`uniform_prior()`](https://elkronos.github.io/bmbeR/reference/prior_generators.md)
  [`beta_prior()`](https://elkronos.github.io/bmbeR/reference/prior_generators.md)
  [`gamma_prior()`](https://elkronos.github.io/bmbeR/reference/prior_generators.md)
  [`binomial_prior()`](https://elkronos.github.io/bmbeR/reference/prior_generators.md)
  [`poisson_prior()`](https://elkronos.github.io/bmbeR/reference/prior_generators.md)
  [`lognormal_prior()`](https://elkronos.github.io/bmbeR/reference/prior_generators.md)
  [`bernoulli_prior()`](https://elkronos.github.io/bmbeR/reference/prior_generators.md)
  : Simulate from common prior distributions
- [`original_distributions`](https://elkronos.github.io/bmbeR/reference/original_distributions.md)
  [`distributions`](https://elkronos.github.io/bmbeR/reference/original_distributions.md)
  : Registry of prior generators
- [`add_distribution()`](https://elkronos.github.io/bmbeR/reference/add_distribution.md)
  [`reset_distributions()`](https://elkronos.github.io/bmbeR/reference/add_distribution.md)
  [`get_prior_distribution()`](https://elkronos.github.io/bmbeR/reference/add_distribution.md)
  : Manage and draw from a registry of prior generators

## Package

- [`bmbeR`](https://elkronos.github.io/bmbeR/reference/bmbeR-package.md)
  [`bmbeR-package`](https://elkronos.github.io/bmbeR/reference/bmbeR-package.md)
  : bmbeR: Prior-Focused Bayesian Model Building and Evaluation
