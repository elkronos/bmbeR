#' bmbeR: Prior-Focused Bayesian Model Building and Evaluation
#'
#' @description
#' bmbeR wraps a principled Bayesian workflow (Gelman et al., 2020) around
#' regression models fitted with [rstanarm::stan_glm()], with an emphasis on
#' the part of the workflow that existing packages leave to the analyst: the
#' prior.
#'
#' The functions map onto the stages of the workflow:
#'
#' | Stage | Functions |
#' |---|---|
#' | Specify priors | [prior_spec()], [prior_config()], [empirical_bayes_priors()] |
#' | Check priors before fitting | [prior_predictive_check()] |
#' | Fit and check computation | [fit_model_with_prior()], [check_convergence()] |
#' | Quantify prior influence | [prior_sensitivity()], [sensitivity_analysis()], [plot_prior_posterior()] |
#' | Visualise | [generate_plot()], [plot_posterior_distributions()] |
#' | Evaluate predictions | [evaluate_model_performance()] |
#' | Report | [workflow_report()] |
#'
#' @section Global options:
#' bmbeR never changes global state (options, the random seed, or the ggplot2
#' theme). To run chains in parallel, set `options(mc.cores = 4)` (or
#' `parallel::detectCores()`) yourself before fitting.
#'
#' @references
#' Gelman, A., Vehtari, A., Simpson, D., Margossian, C. C., Carpenter, B.,
#' Yao, Y., Kennedy, L., Gabry, J., Bürkner, P.-C., & Modrák, M. (2020).
#' Bayesian workflow. *arXiv:2011.01808*.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom stats sd var cov quantile median model.frame model.response
#'   model.matrix glm gaussian binomial poisson Gamma inverse.gaussian coef
#'   vcov nobs dnorm dt dcauchy dexp rnorm rt rcauchy runif rbeta rgamma
#'   rbinom rpois rlnorm optimize formula na.omit setNames family terms
#'   delete.response
#' @importFrom utils packageVersion
#' @importFrom ggplot2 .data
## usethis namespace: end
NULL
