#' Fit a Bayesian regression model with explicit priors
#'
#' Fits a generalised linear model with [rstanarm::stan_glm()] using priors
#' specified with [prior_config()] (or derived with
#' [empirical_bayes_priors()]), then runs [check_convergence()] on the
#' result.
#'
#' @details
#' **Default priors.** Components of `prior_config` that are `NULL` use
#' rstanarm's weakly informative defaults, which are *autoscaled* to the
#' data (Gelman et al., 2008). bmbeR 1.x instead used an unscaled
#' `student_t(3, 0, 2.5)` prior for all coefficients, which can dominate the
#' likelihood when variables are not on a unit scale.
#'
#' **Convergence.** Diagnostics are always computed and stored in
#' `attr(fit, "bmb_convergence")`. By default a failed check issues a
#' warning but still returns the fit, so that the (expensive) model is not
#' lost and can be inspected; use `on_nonconvergence = "error"` to stop
#' instead.
#'
#' **Parallel chains.** bmbeR does not choose the number of cores. Set
#' `options(mc.cores = parallel::detectCores())` or pass `cores = ` through
#' `...`.
#'
#' @param data A data frame containing all variables in `formula`.
#' @param formula A model formula, e.g. `y ~ x1 + x2`.
#' @param family A family object, family function or family name:
#'   `gaussian`, `binomial`, `poisson`, `Gamma`, `inverse.gaussian` or
#'   `neg_binomial_2`.
#' @param prior_config Priors as a [prior_config()] object, the output of
#'   [empirical_bayes_priors()], a bmbeR 1.x list with `intercept` and `slope`
#'   elements, or `NULL` for rstanarm's defaults.
#' @param chains Number of Markov chains (at least 4 are recommended).
#' @param iter Iterations per chain, including warm-up (half by default).
#' @param seed Random seed passed to Stan, for reproducibility.
#' @param on_nonconvergence What to do if [check_convergence()] fails:
#'   `"warn"` (default), `"error"` or `"ignore"`.
#' @param convergence_args A list of further arguments for
#'   [check_convergence()], e.g. `list(rhat_threshold = 1.01)`.
#' @param ... Further arguments passed to [rstanarm::stan_glm()], e.g.
#'   `cores`, `refresh = 0`, `adapt_delta` or `weights`.
#'
#' @return A `stanreg` object with attribute `bmb_convergence` (a
#'   [check_convergence()] result).
#' @references
#' Gelman, A., Jakulin, A., Pittau, M. G., & Su, Y.-S. (2008). A weakly
#' informative default prior distribution for logistic and other regression
#' models. *The Annals of Applied Statistics*, 2(4), 1360–1383.
#' \doi{10.1214/08-AOAS191}
#' @seealso [prior_predictive_check()] to check priors before fitting,
#'   [prior_sensitivity()] to check their influence afterwards.
#' @examples
#' \donttest{
#' data(kidiq, package = "rstanarm")
#' fit <- fit_model_with_prior(
#'   kidiq, kid_score ~ mom_iq + mom_hs,
#'   prior_config = prior_config(
#'     intercept = prior_spec("normal", location = 80, scale = 20),
#'     slope     = prior_spec("normal", location = 0, scale = c(1, 10))
#'   ),
#'   chains = 4, iter = 1000, refresh = 0
#' )
#' attr(fit, "bmb_convergence")
#' }
#' @export
fit_model_with_prior <- function(data, formula, family = gaussian(),
                                 prior_config = NULL,
                                 chains = 4, iter = 2000, seed = 1234,
                                 on_nonconvergence = c("warn", "error", "ignore"),
                                 convergence_args = list(), ...) {
  on_nonconvergence <- match.arg(on_nonconvergence)
  validate_data_formula(data, formula)
  validate_positive_integer(chains, "chains")
  validate_positive_integer(iter, "iter")
  family <- normalize_family(family)
  check_response_family(get_response(formula, data), family)
  cfg <- as_prior_config(prior_config)
  check_prior_terms(cfg, formula, data)

  fit <- run_stan_glm(data, formula, family, cfg, chains = chains, iter = iter,
                      seed = seed, ...)

  conv <- do.call(check_convergence, c(list(fit), convergence_args))
  attr(fit, "bmb_convergence") <- conv
  attr(fit, "bmb_prior_config") <- cfg
  if (!conv$converged) {
    msg <- paste0("MCMC convergence checks failed:\n  - ",
                  paste(conv$issues, collapse = "\n  - "),
                  "\nInspect attr(fit, \"bmb_convergence\"). Do not interpret the results until resolved.")
    if (on_nonconvergence == "error") stop(msg, call. = FALSE)
    if (on_nonconvergence == "warn") warning(msg, call. = FALSE)
  }
  fit
}

# Shared by fit_model_with_prior() and prior_predictive_check().
run_stan_glm <- function(data, formula, family, cfg, chains, iter, seed, ...) {
  args <- c(
    list(formula = formula, data = data, family = family,
         chains = chains, iter = iter, seed = seed),
    prior_args_from_config(cfg),
    list(...)
  )
  do.call(rstanarm::stan_glm, args)
}

# For priors derived by empirical_bayes_priors(), make sure the coefficient
# order matches the model that will be fitted (vector priors are positional).
check_prior_terms <- function(cfg, formula, data) {
  terms_prior <- attr(cfg, "terms")
  slope <- cfg$slope
  mm_names <- colnames(model.matrix(formula, model.frame(formula, data, na.action = na.omit)))
  k <- length(setdiff(mm_names, "(Intercept)"))
  if (!is.null(terms_prior) && !identical(terms_prior, mm_names)) {
    stop(sprintf(paste0("The priors were derived for coefficients [%s] but the model has [%s]. ",
                        "Use the same formula (and factor levels) in both steps."),
                 paste(terms_prior, collapse = ", "), paste(mm_names, collapse = ", ")),
         call. = FALSE)
  }
  if (!is.null(slope) && slope$type != "flat") {
    len <- max(length(slope$location), length(slope$scale))
    if (len > 1L && len != k) {
      stop(sprintf("The slope prior has %d values but the model has %d coefficients (%s).",
                   len, k, paste(setdiff(mm_names, "(Intercept)"), collapse = ", ")),
           call. = FALSE)
    }
  }
  invisible(TRUE)
}
