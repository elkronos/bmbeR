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
#' **Arguments in `...`.** `weights`, `subset`, `offset` and `na.action` are
#' evaluated inside `data`, as in [rstanarm::stan_glm()], so
#' `weights = w` refers to the column `w`. The stored model call refers to
#' the data by name, so [stats::update()], [loo::loo()] and [loo::kfold()]
#' work on the result.
#'
#' **Data-informed priors.** The intercept prior from
#' [empirical_bayes_priors()] is placed on the intercept at the predictor
#' means. When `data` has different predictor means from the data the priors
#' were derived from (always the case for power priors), the intercept prior
#' is re-expressed at the means of `data` before fitting.
#'
#' **Random numbers.** The `seed` is passed to Stan. The default is fixed, so
#' repeated calls give identical chains unless you change it. R's own random
#' number stream is left unchanged.
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
#' @param seed Random seed passed to Stan, for reproducibility (a single
#'   non-negative whole number).
#' @param on_nonconvergence What to do if [check_convergence()] fails:
#'   `"warn"` (default), `"error"` or `"ignore"`.
#' @param convergence_args A list of further arguments for
#'   [check_convergence()], e.g. `list(rhat_threshold = 1.01)`.
#' @param ... Further arguments passed to [rstanarm::stan_glm()], e.g.
#'   `cores`, `refresh = 0`, `adapt_delta` or `weights`.
#'
#' @return A `stanreg` object with attributes `bmb_convergence` (a
#'   [check_convergence()] result) and `bmb_prior_config` (the priors used,
#'   after any re-expression of the intercept prior).
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
  env <- parent.frame()
  data_expr <- substitute(data)
  family <- normalize_family(family, env)
  dots <- capture_dots(...)
  fit_glm_with_priors(data, formula, family, prior_config, chains = chains,
                      iter = iter, seed = seed, dots = dots,
                      data_expr = data_expr, env = env,
                      on_nonconvergence = on_nonconvergence,
                      convergence_args = convergence_args)
}

# Shared by fit_model_with_prior() and sensitivity_analysis(). `dots` comes
# from capture_dots(); `data_expr` and `env` are the user's data expression
# and calling environment.
fit_glm_with_priors <- function(data, formula, family, prior_config, chains,
                                iter, seed, dots, data_expr, env,
                                on_nonconvergence = "warn",
                                convergence_args = list()) {
  validate_data_formula(data, formula)
  validate_positive_integer(chains, "chains")
  validate_positive_integer(iter, "iter")
  validate_seed(seed)
  if (!is.list(convergence_args)) {
    stop("`convergence_args` must be a list.", call. = FALSE)
  }
  check_response_family(get_response(formula, data), family)
  cfg <- prepare_prior_config(as_prior_config(prior_config), formula, data)

  fit <- run_stan_glm(data, formula, family, cfg, chains = chains, iter = iter,
                      seed = seed, dots = dots, data_expr = data_expr, env = env)

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

# Builds and evaluates the rstanarm::stan_glm() call. The call is built as a
# language object so that data-masked arguments (weights = w) work and the
# stored call stays small and can be re-evaluated by update().
run_stan_glm <- function(data, formula, family, cfg, chains, iter, seed,
                         dots = list(), data_expr = NULL, env = parent.frame(),
                         extra = list()) {
  data_arg <- if (is.null(data_expr)) data else data_call_arg(data, data_expr, env)
  args <- c(
    list(formula = formula, data = data_arg, family = family_call(family),
         chains = chains, iter = iter, seed = seed),
    prior_args_from_config(cfg),
    extra,
    dots
  )
  dup <- intersect(names(dots), c(names(args)[seq_len(length(args) - length(dots))]))
  if (length(dup) > 0L) {
    stop(sprintf("Argument(s) %s are set by bmbeR and cannot be passed through `...`.",
                 paste(dup, collapse = ", ")), call. = FALSE)
  }
  cl <- as.call(c(list(quote(rstanarm::stan_glm)), args))
  fit <- preserve_seed(eval(cl, env))
  if (!inherits(fit, "stanreg")) {
    stop("rstanarm::stan_glm() did not return a fitted model; see the messages above.",
         call. = FALSE)
  }
  fit
}

# Checks that a prior configuration fits the model's coefficients and, for
# data-informed priors, re-expresses the intercept prior at the predictor
# means of `data` (rstanarm places the intercept prior there).
prepare_prior_config <- function(cfg, formula, data) {
  check_prior_terms(cfg, formula, data)
  recentre_intercept_prior(cfg, formula, data)
}

recentre_intercept_prior <- function(cfg, formula, data) {
  basis <- attr(cfg, "intercept_basis")
  if (is.null(basis) || is.null(cfg$intercept)) return(cfg)
  X <- model.matrix(formula, model.frame(formula, data, na.action = na.omit))
  slopes <- setdiff(colnames(X), "(Intercept)")
  xbar <- colMeans(X[, slopes, drop = FALSE])
  if (isTRUE(all.equal(unname(xbar), unname(basis$xbar)))) return(cfg)
  terms_x <- names(basis$beta)
  cvec <- c(1, xbar)[match(terms_x, c("(Intercept)", slopes))]
  loc <- sum(cvec * basis$beta)
  scale <- sqrt(drop(t(cvec) %*% basis$vcov %*% cvec) * basis$inflate)
  old <- cfg$intercept
  cfg$intercept <- prior_spec(old$type, location = loc, scale = scale,
                              df = old$df %||% 3)
  cfg
}

# Make sure vector priors match the coefficients of the model that will be
# fitted (vector priors are positional), and that priors derived by
# empirical_bayes_priors() were derived for the same coefficients.
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
    len <- max(length(slope$location), length(slope$scale), length(slope$df))
    if (len > 1L && len != k) {
      stop(sprintf("The slope prior has %d values but the model has %d coefficients (%s).",
                   len, k, paste(setdiff(mm_names, "(Intercept)"), collapse = ", ")),
           call. = FALSE)
    }
  }
  invisible(TRUE)
}
