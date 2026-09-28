#' Bayesian analysis report (BARG checklist)
#'
#' Assembles the information that the Bayesian Analysis Reporting
#' Guidelines (BARG; Kruschke, 2021) ask authors to report, marks each item
#' as `ok`, `check` (needs attention) or `missing` (not yet done), and
#' computes the pieces that are cheap to obtain from the fit itself:
#' convergence diagnostics, a posterior summary, posterior predictive
#' p-values, PSIS-LOO and power-scaling prior sensitivity.
#'
#' Supply the results of steps that require extra work, such as
#' [prior_predictive_check()] and [evaluate_model_performance()], to
#' complete the checklist.
#'
#' @param fit A `stanreg` object from [rstanarm::stan_glm()] or
#'   [fit_model_with_prior()].
#' @param prior_check Optional result of [prior_predictive_check()].
#' @param sensitivity Optional result of [prior_sensitivity()] or
#'   [sensitivity_analysis()]. If `NULL`, [prior_sensitivity()] is run.
#' @param performance Optional result of [evaluate_model_performance()] on
#'   held-out data.
#' @param prob Probability mass of the reported credible intervals.
#'
#' @return An object of class `bmb_report` with elements `items` (a data
#'   frame with one row per checklist item: `item`, `status`, `detail`),
#'   `posterior` (posterior summary table), `priors`, `convergence`, `loo`,
#'   `ppc` and `sensitivity`.
#' @references
#' Kruschke, J. K. (2021). Bayesian analysis reporting guidelines. *Nature
#' Human Behaviour*, 5, 1282–1291. \doi{10.1038/s41562-021-01177-7}
#'
#' Gelman, A., Meng, X.-L., & Stern, H. (1996). Posterior predictive
#' assessment of model fitness via realized discrepancies. *Statistica
#' Sinica*, 6(4), 733–760.
#' @examples
#' \donttest{
#' data(kidiq, package = "rstanarm")
#' fit <- fit_model_with_prior(kidiq, kid_score ~ mom_iq + mom_hs,
#'                             chains = 4, iter = 1000, refresh = 0)
#' workflow_report(fit)
#' }
#' @export
workflow_report <- function(fit, prior_check = NULL, sensitivity = NULL,
                            performance = NULL, prob = 0.95) {
  check_stanreg(fit, glm_only = TRUE)
  validate_probability(prob, "prob", open = TRUE)
  items <- list()
  add <- function(item, status, detail) {
    items[[length(items) + 1L]] <<- data.frame(item = item, status = status,
                                                detail = detail, stringsAsFactors = FALSE)
  }
  fam <- stats::family(fit)

  # 1. Model -------------------------------------------------------------
  X <- rstanarm::get_x(fit)
  add("Model", "ok", sprintf("%s; %s family, %s link; %d observations; %d coefficients",
                             paste(deparse(stats::formula(fit)), collapse = " "),
                             fam$family, fam$link, nrow(X), ncol(X)))

  # 2. Priors --------------------------------------------------------------
  priors <- tryCatch(resolve_priors(fit), error = function(e) NULL)
  cfg <- attr(fit, "bmb_prior_config")
  ps <- rstanarm::prior_summary(fit)
  if (is.null(priors)) {
    add("Priors", "check", "Priors could not be summarised (unsupported prior type or QR = TRUE).")
  } else {
    flat <- priors$variable[priors$dist == "flat"]
    src <- if (!is.null(attr(cfg, "method"))) {
      sprintf("data-informed (%s)", attr(cfg, "method"))
    } else if (is_default_priors(ps)) {
      "rstanarm defaults (weakly informative, autoscaled)"
    } else "user-specified"
    detail <- sprintf("%s: %s", src, paste(sprintf("%s ~ %s", priors$variable,
                                                   describe_prior_rows(priors)), collapse = "; "))
    status <- "ok"
    if (length(flat) > 0L) {
      status <- "check"
      detail <- paste0(detail, sprintf(". Improper flat prior(s) on %s: BARG asks for proper, justified priors.",
                                       paste(flat, collapse = ", ")))
    }
    if (identical(attr(cfg, "method"), "power")) {
      detail <- paste(detail, "(power prior: make sure the historical data differ from the analysed data)")
    }
    add("Priors", status, detail)
  }

  # 3. Prior predictive check -----------------------------------------------
  if (is.null(prior_check)) {
    add("Prior predictive check", "missing", "Not supplied: run prior_predictive_check().")
  } else {
    s <- prior_check$summary
    problems <- character()
    if (!is.null(s$share_outside) && s$share_outside > 0.05) {
      problems <- c(problems, sprintf("%.0f%% of simulated values outside the plausible range",
                                      100 * s$share_outside))
    }
    if (!is.null(s$share_extreme) && s$share_extreme > 0.5) {
      problems <- c(problems, sprintf("%.0f%% of prior success probabilities are below 5%% or above 95%%",
                                      100 * s$share_extreme))
    }
    q <- signif(s$yrep_quantiles[c(2, 4)], 3)
    detail <- sprintf("90%% of prior predictive values in [%s, %s]; observed range [%s, %s]",
                      q[1], q[2], signif(s$observed_range[1], 3), signif(s$observed_range[2], 3))
    if (length(problems) > 0L) detail <- paste0(detail, ". ", paste(problems, collapse = "; "))
    add("Prior predictive check", if (length(problems) > 0L) "check" else "ok", detail)
  }

  # 4. Computation ---------------------------------------------------------
  conv <- attr(fit, "bmb_convergence") %||% check_convergence(fit)
  sf <- fit$stanfit
  add("MCMC computation", if (conv$converged) "ok" else "check",
      sprintf("%d chains x %d iterations (%d warm-up), seed %s. Max R-hat %.3f, min bulk-ESS %.0f, min tail-ESS %.0f, %d divergences.%s",
              conv$n_chains, sf@sim$iter, sf@sim$warmup,
              format(sf@stan_args[[1]]$seed %||% NA),
              max(conv$parameters$rhat, na.rm = TRUE),
              min(conv$parameters$ess_bulk, na.rm = TRUE),
              min(conv$parameters$ess_tail, na.rm = TRUE),
              conv$sampler$divergent %||% NA_integer_,
              if (conv$converged) "" else paste0(" Issues: ", paste(conv$issues, collapse = " "))))

  # 5. Posterior summary -----------------------------------------------------
  draws <- as.matrix(fit)
  a <- (1 - prob) / 2
  post <- data.frame(
    variable = colnames(draws),
    mean = colMeans(draws),
    median = apply(draws, 2L, stats::median),
    sd = apply(draws, 2L, sd),
    lower = apply(draws, 2L, stats::quantile, a),
    upper = apply(draws, 2L, stats::quantile, 1 - a),
    p_positive = colMeans(draws > 0),
    stringsAsFactors = FALSE
  )
  rownames(post) <- NULL
  names(post)[5:6] <- sprintf("q%g", 100 * c(a, 1 - a))
  add("Posterior summary", "ok",
      sprintf("Means, medians, SDs and %g%% central credible intervals for %d parameters (see $posterior).",
              100 * prob, nrow(post)))

  # 6. Posterior predictive check -------------------------------------------
  ppc <- ppc_pvalues(fit)
  extreme <- ppc$statistic[ppc$p_value < 0.025 | ppc$p_value > 0.975]
  add("Posterior predictive check",
      if (length(extreme) > 0L) "check" else "ok",
      sprintf("Posterior predictive p-values: %s%s",
              paste(sprintf("%s %.2f", ppc$statistic, ppc$p_value), collapse = ", "),
              if (length(extreme) > 0L)
                sprintf(". Extreme for %s: the model does not reproduce this feature of the data (see rstanarm::pp_check()).",
                        paste(extreme, collapse = ", ")) else ""))

  # 7. LOO -------------------------------------------------------------------
  loo_obj <- suppressWarnings(loo::loo(fit))
  k <- loo::pareto_k_values(loo_obj)
  kthr <- pareto_k_threshold(nrow(draws))
  nbad <- sum(k > kthr)
  add("Cross-validation (PSIS-LOO)", if (nbad > 0L) "check" else "ok",
      sprintf("elpd_loo = %.1f (SE %.1f), p_loo = %.1f; %d observation(s) with Pareto k > %.2f%s",
              loo_obj$estimates["elpd_loo", 1], loo_obj$estimates["elpd_loo", 2],
              loo_obj$estimates["p_loo", 1], nbad, kthr,
              if (nbad > 0L) " (use loo(fit, k_threshold = 0.7) or K-fold CV)" else ""))

  # 8. Sensitivity -----------------------------------------------------------
  if (is.null(sensitivity) && !is.null(priors)) {
    sensitivity <- tryCatch(prior_sensitivity(fit), error = function(e) NULL)
  }
  if (is.null(sensitivity)) {
    add("Prior sensitivity", "missing", "Not available: run prior_sensitivity() or sensitivity_analysis().")
  } else if (inherits(sensitivity, "bmb_prior_sensitivity")) {
    d <- sensitivity$summary
    serious <- d$variable[d$diagnosis %in% c("prior-data conflict", "weak likelihood")]
    info <- d$variable[d$diagnosis == "informative prior"]
    detail <- if (length(serious) + length(info) == 0L) {
      "Power-scaling: no parameter is sensitive to the prior."
    } else {
      paste0("Power-scaling: ", paste(sprintf("%s (%s)", d$variable[d$diagnosis != "-"],
                                             d$diagnosis[d$diagnosis != "-"]), collapse = "; "))
    }
    add("Prior sensitivity", if (length(serious) > 0L) "check" else "ok", detail)
  } else if (inherits(sensitivity, "bmb_sensitivity")) {
    flagged <- unique(sensitivity$summary$variable[sensitivity$summary$flag])
    add("Prior sensitivity", if (length(flagged) > 0L) "check" else "ok",
        sprintf("Refitted under %d prior configurations; %s",
                length(sensitivity$metric),
                if (length(flagged) > 0L) paste("posterior shifts for", paste(flagged, collapse = ", "))
                else "no notable posterior shifts"))
  }

  # 9. Held-out performance -----------------------------------------------
  if (is.null(performance)) {
    add("Held-out predictive performance", "missing",
        "Not supplied: run evaluate_model_performance() on test data (optional if LOO suffices).")
  } else {
    vals <- unlist(unclass(performance))
    keep <- intersect(c("ELPD", "RMSE", "CRPS", "Coverage", "Accuracy", "Brier", "AUC"), names(vals))
    add("Held-out predictive performance", "ok",
        paste(sprintf("%s = %s", keep, signif(vals[keep], 3)), collapse = ", "))
  }

  # 10. Reproducibility ---------------------------------------------------
  add("Software", "ok", sprintf("R %s; rstanarm %s; rstan %s; bmbeR %s",
                                getRversion(), packageVersion("rstanarm"),
                                packageVersion("rstan"), packageVersion("bmbeR")))

  structure(list(items = do.call(rbind, items), posterior = post, priors = priors,
                 convergence = conv, loo = loo_obj, ppc = ppc,
                 sensitivity = sensitivity, prob = prob),
            class = "bmb_report")
}

is_default_priors <- function(ps) {
  isTRUE(ps$prior$dist == "normal") && all(ps$prior$scale == 2.5) &&
    !is.null(ps$prior$adjusted_scale) &&
    (is.null(ps$prior_intercept) ||
       (isTRUE(ps$prior_intercept$dist == "normal") && ps$prior_intercept$scale == 2.5))
}

describe_prior_rows <- function(priors) {
  vapply(seq_len(nrow(priors)), function(i) {
    p <- priors[i, ]
    s <- function(v) signif(v, 3)
    switch(p$dist,
      normal = sprintf("normal(%s, %s)", s(p$location), s(p$scale)),
      student_t = sprintf("student_t(%s, %s, %s)", s(p$df), s(p$location), s(p$scale)),
      cauchy = sprintf("cauchy(%s, %s)", s(p$location), s(p$scale)),
      laplace = sprintf("laplace(%s, %s)", s(p$location), s(p$scale)),
      exponential = sprintf("exponential(rate = %s)", s(1 / p$scale)),
      flat = "flat"
    )
  }, character(1))
}

# Posterior predictive p-values P(T(yrep) >= T(y)) for simple statistics.
ppc_pvalues <- function(fit, ndraws = 1000L) {
  y <- rstanarm::get_y(fit)
  if (is.matrix(y)) y <- y[, 1L]
  y <- if (is.factor(y) || is.logical(y)) binary_to_01(y) else as.numeric(y)
  yrep <- rstanarm::posterior_predict(fit, draws = min(ndraws, nrow(as.matrix(fit))), seed = 1)
  binary <- all(y %in% c(0, 1))
  stats_fun <- if (binary) list(mean = mean) else list(mean = mean, sd = sd, min = min, max = max)
  data.frame(
    statistic = names(stats_fun),
    observed = vapply(stats_fun, function(f) f(y), numeric(1)),
    p_value = vapply(stats_fun, function(f) mean(apply(yrep, 1L, f) >= f(y)), numeric(1)),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

#' @export
print.bmb_report <- function(x, digits = 3, ...) {
  cat("Bayesian analysis report (BARG checklist; Kruschke, 2021)\n")
  cat(strrep("=", 60), "\n", sep = "")
  it <- x$items
  marks <- c(ok = "[ok]   ", check = "[check]", missing = "[ -- ] ")
  for (i in seq_len(nrow(it))) {
    cat(sprintf("%s %d. %s\n", marks[[it$status[i]]], i, it$item[i]))
    wrapped <- strwrap(it$detail[i], width = 76, indent = 10, exdent = 10)
    cat(paste(wrapped, collapse = "\n"), "\n", sep = "")
  }
  cat("\nPosterior summary:\n")
  post <- x$posterior
  num <- vapply(post, is.numeric, logical(1))
  post[num] <- lapply(post[num], signif, digits = digits)
  print(post, row.names = FALSE)
  n_check <- sum(it$status == "check")
  n_missing <- sum(it$status == "missing")
  cat(sprintf("\n%d item(s) need attention, %d not yet done.\n", n_check, n_missing))
  invisible(x)
}

#' @export
as.data.frame.bmb_report <- function(x, ...) x$items
