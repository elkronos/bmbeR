# bmbeR: Prior-Focused Bayesian Model Building and Evaluation

bmbeR wraps a principled Bayesian workflow (Gelman et al., 2020) around
regression models fitted with
[`rstanarm::stan_glm()`](https://mc-stan.org/rstanarm/reference/stan_glm.html),
with an emphasis on the part of the workflow that existing packages
leave to the analyst: the prior.

The functions map onto the stages of the workflow:

|  |  |
|----|----|
| Stage | Functions |
| Specify priors | [`prior_spec()`](https://elkronos.github.io/bmbeR/reference/prior_spec.md), [`prior_config()`](https://elkronos.github.io/bmbeR/reference/prior_config.md), [`empirical_bayes_priors()`](https://elkronos.github.io/bmbeR/reference/empirical_bayes_priors.md) |
| Check priors before fitting | [`prior_predictive_check()`](https://elkronos.github.io/bmbeR/reference/prior_predictive_check.md) |
| Fit and check computation | [`fit_model_with_prior()`](https://elkronos.github.io/bmbeR/reference/fit_model_with_prior.md), [`check_convergence()`](https://elkronos.github.io/bmbeR/reference/check_convergence.md) |
| Quantify prior influence | [`prior_sensitivity()`](https://elkronos.github.io/bmbeR/reference/prior_sensitivity.md), [`sensitivity_analysis()`](https://elkronos.github.io/bmbeR/reference/sensitivity_analysis.md), [`plot_prior_posterior()`](https://elkronos.github.io/bmbeR/reference/plot_prior_posterior.md) |
| Visualise | [`generate_plot()`](https://elkronos.github.io/bmbeR/reference/generate_plot.md), [`plot_posterior_distributions()`](https://elkronos.github.io/bmbeR/reference/plot_posterior_distributions.md) |
| Evaluate predictions | [`evaluate_model_performance()`](https://elkronos.github.io/bmbeR/reference/evaluate_model_performance.md) |
| Report | [`workflow_report()`](https://elkronos.github.io/bmbeR/reference/workflow_report.md) |

## Global options

bmbeR never changes global state (options, the random seed, or the
ggplot2 theme). To run chains in parallel, set `options(mc.cores = 4)`
(or
[`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html))
yourself before fitting.

## References

Gelman, A., Vehtari, A., Simpson, D., Margossian, C. C., Carpenter, B.,
Yao, Y., Kennedy, L., Gabry, J., Bürkner, P.-C., & Modrák, M. (2020).
Bayesian workflow. *arXiv:2011.01808*.

## See also

Useful links:

- <https://elkronos.github.io/bmbeR/>

- <https://github.com/elkronos/bmbeR>

- Report bugs at <https://github.com/elkronos/bmbeR/issues>

## Author

**Maintainer**: elkronos <jchase.msu@gmail.com>
