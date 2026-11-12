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
#' Posterior predictive p-values use the mean, SD, minimum and maximum of the
#' outcome. For binary outcomes they use the mean and, with at least 20
#' observations, a binned calibration statistic, because any model with an
#' intercept reproduces the mean. The calibration statistic groups
#' observations by fitted probability and sums the squared standardised
#' differences between observed and expected successes, a posterior
#' predictive version of the Hosmer-Lemeshow statistic; only large values
#' indicate misfit.
#'
#' Held-out performance is marked `check` when the coverage of the predictive
#' intervals differs from their nominal level (`prob` in
#' [evaluate_model_performance()]) by more than two binomial standard errors.
#'
#' Use `format(report, type = "markdown")` (or knit the object in R Markdown
#' or Quarto) for a Markdown version of the checklist.
#'
#' @param fit A `stanreg` object from [rstanarm::stan_glm()],
#'   [rstanarm::stan_glmer()] or
#'   [fit_model_with_prior()].
#' @param prior_check Optional result of [prior_predictive_check()].
#' @param sensitivity Optional result of [prior_sensitivity()] or
#'   [sensitivity_analysis()]. If `NULL`, [prior_sensitivity()] is run.
#' @param performance Optional result of [evaluate_model_performance()] on
#'   held-out data.
#' @param prob Probability mass of the reported credible intervals.
#' @param seed Seed for the posterior predictive simulations. R's random
#'   number stream is left unchanged.
#'
#' @return An object of class `bmb_report` with elements `items` (a data
#'   frame with one row per checklist item: `item`, `status`, `detail`),
#'   `posterior` (posterior summary table), `priors`, `convergence`, `loo`,
#'   `ppc`, `sensitivity` and `prob`.
#' @references
#' Kruschke, J. K. (2021). Bayesian analysis reporting guidelines. *Nature
#' Human Behaviour*, 5, 1282–1291. \doi{10.1038/s41562-021-01177-7}
#'
#' Gelman, A., Meng, X.-L., & Stern, H. (1996). Posterior predictive
#' assessment of model fitness via realized discrepancies. *Statistica
#' Sinica*, 6(4), 733–760.
#' \url{https://www3.stat.sinica.edu.tw/statistica/j6n4/j6n41/j6n41.htm}
#' @examples
#' \donttest{
#' data(kidiq, package = "rstanarm")
#' fit <- fit_model_with_prior(kidiq, kid_score ~ mom_iq + mom_hs,
#'                             chains = 4, iter = 1000, refresh = 0)
#' workflow_report(fit)
#' }
#' @export
workflow_report <- function(fit, prior_check = NULL, sensitivity = NULL,
                            performance = NULL, prob = 0.95, seed = 1234) {
  check_stanreg(fit, glm_only = TRUE)
  validate_probability(prob, "prob", open = TRUE)
  validate_seed(seed)
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
  priors_error <- NULL
  priors <- tryCatch(resolve_priors(fit), error = function(e) {
    priors_error <<- conditionMessage(e)
    NULL
  })
  cfg <- attr(fit, "bmb_prior_config")
  ps <- rstanarm::prior_summary(fit)
  if (is.null(priors)) {
    add("Priors", "check", paste("Priors could not be summarised:", priors_error))
  } else {
    flat <- priors$variable[priors$dist == "flat"]
    src <- prior_source(cfg, ps, X)
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
    if (!is.null(ps$prior_covariance)) {
      pc <- ps$prior_covariance
      detail <- paste0(detail, sprintf("; group-level covariance ~ %s(regularization = %s, concentration = %s, shape = %s, scale = %s)",
                                       pc$dist, format(pc$regularization), format(pc$concentration),
                                       format(pc$shape), format(pc$scale)))
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
    detail <- if (isTRUE(prior_check$binary)) {
      pq <- signif(s$proportion_quantiles[c(1, 3)], 3)
      sprintf(paste0("90%% of simulated success proportions in [%s, %s]; %.0f%% of prior ",
                     "success probabilities below 5%% or above 95%%"),
              pq[1], pq[2], 100 * s$share_extreme)
    } else {
      q <- signif(s$yrep_quantiles[c(2, 4)], 3)
      sprintf("90%% of prior predictive values in [%s, %s]; observed range [%s, %s]",
              q[1], q[2], signif(s$observed_range[1], 3), signif(s$observed_range[2], 3))
    }
    if (length(problems) > 0L) detail <- paste0(detail, ". ", paste(problems, collapse = "; "))
    add("Prior predictive check", if (length(problems) > 0L) "check" else "ok", detail)
  }

  # 4. Computation ---------------------------------------------------------
  conv <- attr(fit, "bmb_convergence") %||% check_convergence(fit)
  sf <- fit$stanfit
  add("MCMC computation", if (conv$converged) "ok" else "check",
      sprintf("%d chains x %d iterations (%d warm-up), seed %s. Max R-hat %s, min bulk-ESS %s, min tail-ESS %s, %s divergences.%s",
              conv$n_chains, sf@sim$iter, sf@sim$warmup,
              format(sf@stan_args[[1]]$seed %||% NA),
              safe_extreme(conv$parameters$rhat, max, "%.3f"),
              safe_extreme(conv$parameters$ess_bulk, min, "%.0f"),
              safe_extreme(conv$parameters$ess_tail, min, "%.0f"),
              format(conv$sampler$divergent %||% NA_integer_),
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
  ppc <- ppc_pvalues(fit, seed = seed)
  extreme <- ppc$statistic[ppc$p_value < 0.025 | (!ppc$one_sided & ppc$p_value > 0.975)]
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
  sens_error <- priors_error
  if (is.null(sensitivity) && !is.null(priors)) {
    sensitivity <- tryCatch(prior_sensitivity(fit), error = function(e) {
      sens_error <<- conditionMessage(e)
      NULL
    })
  }
  if (is.null(sensitivity)) {
    msg <- if (is.null(sens_error)) {
      "Not available: run prior_sensitivity() or sensitivity_analysis()."
    } else {
      paste0("Power-scaling is not available for this model (", sens_error,
             "). Use sensitivity_analysis() to refit under alternative priors.")
    }
    add("Prior sensitivity", "missing", msg)
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
    detail <- paste(sprintf("%s = %s", keep, signif(vals[keep], 3)), collapse = ", ")
    status <- "ok"
    if ("Coverage" %in% names(vals) && !is.na(vals[["Coverage"]])) {
      target <- attr(performance, "prob")
      se <- sqrt(target * (1 - target) / attr(performance, "n"))
      if (abs(vals[["Coverage"]] - target) > 2 * se) {
        status <- "check"
        detail <- paste0(detail, sprintf(". Coverage differs from the nominal %g%% by more than two standard errors: the predictive intervals are %s.",
                                         100 * target,
                                         if (vals[["Coverage"]] < target) "too narrow" else "too wide"))
      }
    }
    add("Held-out predictive performance", status, detail)
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

# Where the priors came from: the bmbeR configuration when the model was
# fitted with fit_model_with_prior(), otherwise inferred from prior_summary().
prior_source <- function(cfg, ps, X) {
  if (!is.null(attr(cfg, "method"))) {
    return(sprintf("data-informed (%s)", attr(cfg, "method")))
  }
  if (!is.null(cfg)) {
    given <- c("intercept", "slope", "aux")[!vapply(cfg[c("intercept", "slope", "aux")], is.null, logical(1))]
    if (length(given) == 0L) return("rstanarm defaults (weakly informative, autoscaled)")
    if (length(given) == 3L) return("user-specified")
    return(sprintf("user-specified for %s; rstanarm defaults otherwise",
                   paste(given, collapse = " and ")))
  }
  if (is_default_priors(ps, X)) "rstanarm defaults (weakly informative, autoscaled)" else "user-specified"
}

is_default_priors <- function(ps, X) {
  has_slopes <- length(setdiff(colnames(X), "(Intercept)")) > 0L
  slope_default <- !has_slopes ||
    (isTRUE(ps$prior$dist == "normal") && all(ps$prior$scale == 2.5) &&
       !is.null(ps$prior$adjusted_scale))
  intercept_default <- is.null(ps$prior_intercept) ||
    (isTRUE(ps$prior_intercept$dist == "normal") && isTRUE(ps$prior_intercept$scale == 2.5))
  aux_default <- is.null(ps$prior_aux) ||
    (isTRUE(ps$prior_aux$dist == "exponential") && isTRUE(ps$prior_aux$rate == 1))
  slope_default && intercept_default && aux_default
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
# For binary outcomes, a binned calibration statistic replaces the SD, min and
# max (which are determined by the mean for 0/1 data).
ppc_pvalues <- function(fit, ndraws = 1000L, seed = 1234) {
  y <- rstanarm::get_y(fit)
  if (is.matrix(y)) y <- y[, 1L]
  y <- if (is.factor(y) || is.logical(y)) binary_to_01(y) else as.numeric(y)
  nd <- min(ndraws, nrow(as.matrix(fit)))
  yrep <- preserve_seed(rstanarm::posterior_predict(fit, draws = nd, seed = seed))
  binary <- all(y %in% c(0, 1))
  stats_fun <- if (binary) list(mean = mean) else list(mean = mean, sd = sd, min = min, max = max)
  out <- data.frame(
    statistic = names(stats_fun),
    observed = vapply(stats_fun, function(f) f(y), numeric(1)),
    p_value = vapply(stats_fun, function(f) mean(apply(yrep, 1L, f) >= f(y)), numeric(1)),
    one_sided = FALSE,
    stringsAsFactors = FALSE, row.names = NULL
  )
  if (binary && length(y) >= 20L) {
    p_hat <- colMeans(preserve_seed(rstanarm::posterior_epred(fit, draws = nd)))
    calib <- binned_calibration(p_hat, n_bins = min(10L, length(y) %/% 20L))
    t_obs <- calib(y)
    t_rep <- apply(yrep, 1L, calib)
    out <- rbind(out, data.frame(statistic = "calibration", observed = t_obs,
                                 p_value = mean(t_rep >= t_obs), one_sided = TRUE,
                                 stringsAsFactors = FALSE))
  }
  out
}

# Returns a function computing sum_b (O_b - E_b)^2 / (n_b pbar_b (1 - pbar_b))
# over bins of the fitted probabilities.
binned_calibration <- function(p_hat, n_bins) {
  n_bins <- max(2L, n_bins)
  breaks <- unique(stats::quantile(p_hat, seq(0, 1, length.out = n_bins + 1L)))
  bin <- if (length(breaks) < 3L) rep(1L, length(p_hat)) else
    cut(p_hat, breaks, include.lowest = TRUE, labels = FALSE)
  expected <- tapply(p_hat, bin, sum)
  n_b <- tabulate(bin)
  pbar <- expected / n_b
  denom <- pmax(n_b * pbar * (1 - pbar), 1e-8)
  function(y) sum((tapply(y, bin, sum) - expected)^2 / denom)
}

#' @rdname workflow_report
#' @param x A `bmb_report` object.
#' @param type `"text"` (as printed) or `"markdown"`.
#' @param digits Significant digits in the posterior summary.
#' @param ... Unused.
#' @export
format.bmb_report <- function(x, type = c("text", "markdown"), digits = 3, ...) {
  type <- match.arg(type)
  it <- x$items
  post <- x$posterior
  num <- vapply(post, is.numeric, logical(1))
  post[num] <- lapply(post[num], signif, digits = digits)
  n_check <- sum(it$status == "check")
  n_missing <- sum(it$status == "missing")
  footer <- sprintf("%d item(s) need attention, %d not yet done.", n_check, n_missing)
  if (type == "markdown") {
    esc <- function(v) gsub("|", "\\|", v, fixed = TRUE)
    rows <- sprintf("| %d | %s | %s | %s |", seq_len(nrow(it)), esc(it$item), it$status, esc(it$detail))
    post_chr <- lapply(post, function(v) esc(as.character(v)))
    post_rows <- do.call(paste, c(post_chr, sep = " | "))
    return(c(
      "## Bayesian analysis report (BARG checklist; Kruschke, 2021)", "",
      "| # | Item | Status | Detail |", "|---|---|---|---|", rows, "",
      "### Posterior summary", "",
      paste0("| ", paste(names(post), collapse = " | "), " |"),
      paste0("|", paste(rep("---", ncol(post)), collapse = "|"), "|"),
      paste0("| ", post_rows, " |"), "",
      footer
    ))
  }
  marks <- c(ok = "[ok]   ", check = "[check]", missing = "[ -- ] ")
  lines <- c("Bayesian analysis report (BARG checklist; Kruschke, 2021)", strrep("=", 60))
  for (i in seq_len(nrow(it))) {
    lines <- c(lines, sprintf("%s %d. %s", marks[[it$status[i]]], i, it$item[i]),
               strwrap(it$detail[i], width = 76, indent = 10, exdent = 10))
  }
  c(lines, "", "Posterior summary:", utils::capture.output(print(post, row.names = FALSE)),
    "", footer)
}

#' @export
print.bmb_report <- function(x, digits = 3, ...) {
  cat(format(x, type = "text", digits = digits), sep = "\n")
  invisible(x)
}

#' @exportS3Method knitr::knit_print
knit_print.bmb_report <- function(x, ...) {
  knitr::asis_output(paste(format(x, type = "markdown"), collapse = "\n"))
}

#' @export
as.data.frame.bmb_report <- function(x, ...) x$items
