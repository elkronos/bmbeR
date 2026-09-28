#' Evaluate predictive performance on test data
#'
#' Scores a fitted model's predictions for new data using the full posterior
#' predictive distribution. Proper scoring rules (Gneiting & Raftery, 2007)
#' reward both accuracy and honest uncertainty, and are reported alongside
#' the familiar point-prediction metrics.
#'
#' @details
#' Always reported:
#' * `ELPD`: expected log predictive density on the test data, the sum over
#'   observations of \eqn{\log \frac{1}{S}\sum_s p(y_i \mid \theta_s)}
#'   (higher is better), with its standard error `ELPD_SE` and per-observation
#'   mean `ELPD_mean`. This is the out-of-sample analogue of `elpd_loo`
#'   (Vehtari et al., 2017).
#'
#' Regression (continuous or count outcomes):
#' * `RMSE`, `MAE` of the posterior mean prediction (on the scale of the
#'   model's response, e.g. `log(y)` for `log(y) ~ x`).
#' * `CRPS`: continuous ranked probability score of the posterior predictive
#'   distribution (lower is better), estimated from draws.
#' * `Coverage` and `Interval_Width`: share of test outcomes inside the
#'   central `prob` posterior predictive interval, and its mean width. For a
#'   well-calibrated model coverage should be close to `prob`.
#'
#' Classification (binary outcomes of a binomial model):
#' * `Accuracy`, `Precision`, `Recall`, `F1_Score` after classifying the
#'   posterior mean *probability* at `threshold`.
#' * `Brier`: mean squared error of the predicted probabilities (lower is
#'   better; Brier, 1950).
#' * `AUC`: area under the ROC curve.
#'
#' In bmbeR 1.x, classification thresholded `predict()` output, which for
#' rstanarm is on the link (log-odds) scale; probabilities are now used.
#'
#' @param fit A `stanreg` object.
#' @param data_test A data frame of test data containing the model's
#'   variables. Rows with missing values in those variables are dropped.
#' @param formula Optional. Defaults to the model's formula; retained for
#'   compatibility with bmbeR 1.x.
#' @param threshold Probability threshold for classification.
#' @param analysis_type `"auto"` (classification for binary outcomes of a
#'   binomial model, regression otherwise), `"regression"` or
#'   `"classification"`.
#' @param prob Probability mass of the predictive intervals used for
#'   `Coverage`.
#' @param ndraws Number of posterior draws to use (default: all).
#' @param seed Seed for posterior predictive simulation.
#'
#' @return An object of class `bmb_performance`: a named list of metrics
#'   (see Details) with attributes `type` and `n`. Access metrics with `$`,
#'   e.g. `perf$RMSE`, or convert with `as.data.frame()`.
#' @references
#' Gneiting, T., & Raftery, A. E. (2007). Strictly proper scoring rules,
#' prediction, and estimation. *JASA*, 102(477), 359–378.
#' \doi{10.1198/016214506000001437}
#'
#' Brier, G. W. (1950). Verification of forecasts expressed in terms of
#' probability. *Monthly Weather Review*, 78(1), 1–3.
#'
#' Vehtari, A., Gelman, A., & Gabry, J. (2017). Practical Bayesian model
#' evaluation using leave-one-out cross-validation and WAIC. *Statistics and
#' Computing*, 27(5), 1413–1432. \doi{10.1007/s11222-016-9696-4}
#' @examples
#' \donttest{
#' data(wells, package = "rstanarm")
#' wells$dist100 <- wells$dist / 100
#' set.seed(1)
#' test <- sample(nrow(wells), 500)
#' fit <- rstanarm::stan_glm(switch ~ dist100 + arsenic, family = binomial(),
#'                           data = wells[-test, ], chains = 2, iter = 1000,
#'                           refresh = 0)
#' evaluate_model_performance(fit, wells[test, ])
#' }
#' @export
evaluate_model_performance <- function(fit, data_test, formula = NULL,
                                       threshold = 0.5,
                                       analysis_type = c("auto", "regression", "classification"),
                                       prob = 0.9, ndraws = NULL, seed = 1234) {
  check_stanreg(fit)
  analysis_type <- match.arg(analysis_type)
  validate_probability(threshold, "threshold", open = TRUE)
  validate_probability(prob, "prob", open = TRUE)
  formula <- formula %||% stats::formula(fit)
  validate_data_formula(data_test, formula, data_arg = "data_test")

  vars <- intersect(all.vars(stats::formula(fit)), colnames(data_test))
  complete <- stats::complete.cases(data_test[, vars, drop = FALSE])
  if (any(!complete)) {
    message(sprintf("Dropped %d test row(s) with missing values.", sum(!complete)))
    data_test <- data_test[complete, , drop = FALSE]
  }
  if (nrow(data_test) == 0L) stop("No complete test observations.", call. = FALSE)

  y <- get_response(formula, data_test)
  fam <- stats::family(fit)$family
  binary <- fam == "binomial" && is_binary_outcome(y)
  type <- if (analysis_type == "auto") {
    if (binary) "classification" else "regression"
  } else analysis_type
  if (type == "classification" && !binary) {
    stop("Classification metrics need a binary outcome from a binomial model.", call. = FALSE)
  }

  draws <- if (is.null(ndraws)) NULL else min(ndraws, nrow(as.matrix(fit)))
  ll <- rstanarm::log_lik(fit, newdata = data_test, draws = draws)
  elpd_i <- log_mean_exp_cols(ll)
  n <- length(elpd_i)
  out <- list(
    ELPD = sum(elpd_i),
    ELPD_SE = sqrt(n) * sd(elpd_i),
    ELPD_mean = mean(elpd_i)
  )

  epred <- rstanarm::posterior_epred(fit, newdata = data_test, draws = draws)
  if (type == "classification") {
    y01 <- binary_to_01(y)
    p <- colMeans(epred)
    cls <- as.integer(p > threshold)
    out <- c(out, classification_metrics(y01, p, cls))
  } else {
    yrep <- rstanarm::posterior_predict(fit, newdata = data_test, draws = draws, seed = seed)
    if (is.matrix(y)) {
      # cbind(successes, failures): score the number of successes.
      trials <- rowSums(y)
      y_obs <- y[, 1L]
      point <- colMeans(epred) * trials
    } else {
      y_obs <- if (binary) binary_to_01(y) else as.numeric(y)
      point <- colMeans(epred)
    }
    lo <- apply(yrep, 2L, stats::quantile, (1 - prob) / 2)
    hi <- apply(yrep, 2L, stats::quantile, 1 - (1 - prob) / 2)
    out <- c(out, list(
      RMSE = sqrt(mean((y_obs - point)^2)),
      MAE = mean(abs(y_obs - point)),
      CRPS = mean(crps_draws(yrep, y_obs)),
      Coverage = mean(y_obs >= lo & y_obs <= hi),
      Interval_Width = mean(hi - lo)
    ))
  }
  structure(out, class = "bmb_performance", type = type, n = n, prob = prob,
            threshold = threshold, elpd_pointwise = elpd_i)
}

#' Compare models on the same held-out data
#'
#' Compares the held-out expected log predictive density (ELPD) of several
#' models evaluated with [evaluate_model_performance()] on the *same* test
#' observations. Differences are paired by observation, and their standard
#' errors are computed from the per-observation differences, as in
#' [loo::loo_compare()].
#'
#' @param ... Named `bmb_performance` objects, or a single named list of them.
#' @return A data frame sorted from best to worst with columns `model`,
#'   `elpd`, `elpd_diff` (relative to the best model) and `se_diff`.
#' @examples
#' \donttest{
#' data(wells, package = "rstanarm")
#' wells$dist100 <- wells$dist / 100
#' set.seed(1)
#' test <- sample(nrow(wells), 1000)
#' fit1 <- rstanarm::stan_glm(switch ~ dist100, family = binomial(),
#'                            data = wells[-test, ], refresh = 0)
#' fit2 <- rstanarm::stan_glm(switch ~ dist100 + arsenic, family = binomial(),
#'                            data = wells[-test, ], refresh = 0)
#' compare_performance(
#'   distance = evaluate_model_performance(fit1, wells[test, ]),
#'   distance_arsenic = evaluate_model_performance(fit2, wells[test, ])
#' )
#' }
#' @export
compare_performance <- function(...) {
  perfs <- list(...)
  if (length(perfs) == 1L && is.list(perfs[[1L]]) && !inherits(perfs[[1L]], "bmb_performance")) {
    perfs <- perfs[[1L]]
  }
  if (length(perfs) < 2L) stop("Supply at least two models to compare.", call. = FALSE)
  if (is.null(names(perfs)) || any(!nzchar(names(perfs)))) {
    stop("Name each model, e.g. compare_performance(a = perf_a, b = perf_b).", call. = FALSE)
  }
  if (!all(vapply(perfs, inherits, logical(1), "bmb_performance"))) {
    stop("All inputs must come from evaluate_model_performance().", call. = FALSE)
  }
  pw <- lapply(perfs, attr, "elpd_pointwise")
  n <- vapply(pw, length, integer(1))
  if (length(unique(n)) != 1L) {
    stop("All models must be evaluated on the same test observations.", call. = FALSE)
  }
  elpd <- vapply(pw, sum, numeric(1))
  best <- which.max(elpd)
  diffs <- lapply(pw, function(x) x - pw[[best]])
  out <- data.frame(
    model = names(perfs),
    elpd = unname(elpd),
    elpd_diff = vapply(diffs, sum, numeric(1)),
    se_diff = vapply(diffs, function(d) sqrt(length(d)) * sd(d), numeric(1)),
    stringsAsFactors = FALSE
  )
  out <- out[order(-out$elpd), ]
  rownames(out) <- NULL
  out
}

classification_metrics <- function(y, p, cls) {
  tp <- sum(cls == 1 & y == 1)
  fp <- sum(cls == 1 & y == 0)
  fn <- sum(cls == 0 & y == 1)
  precision <- if (tp + fp == 0) NA_real_ else tp / (tp + fp)
  recall <- if (tp + fn == 0) NA_real_ else tp / (tp + fn)
  f1 <- if (is.na(precision) || is.na(recall) || precision + recall == 0) NA_real_ else
    2 * precision * recall / (precision + recall)
  list(
    Accuracy = mean(cls == y),
    Precision = precision,
    Recall = recall,
    F1_Score = f1,
    Brier = mean((p - y)^2),
    AUC = auc_rank(y, p)
  )
}

# CRPS for each observation from a draws x observations matrix, using
# CRPS = E|X - y| - 0.5 E|X - X'| with the sorted-sample identity for the
# second term (Gneiting & Raftery, 2007).
crps_draws <- function(yrep, y) {
  yrep <- as.matrix(yrep)
  m <- nrow(yrep)
  w <- (2 * seq_len(m) - m - 1) / m^2
  vapply(seq_along(y), function(i) {
    x <- yrep[, i]
    mean(abs(x - y[i])) - sum(w * sort(x))
  }, numeric(1))
}

# Area under the ROC curve via the Mann-Whitney statistic.
auc_rank <- function(y, p) {
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  r <- rank(p)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

#' @export
print.bmb_performance <- function(x, digits = 3, ...) {
  type <- attr(x, "type")
  cat(sprintf("<bmb_performance> %s, %d test observations\n", type, attr(x, "n")))
  vals <- unlist(unclass(x))
  labels <- c(
    ELPD = "ELPD (log score, higher = better)",
    ELPD_SE = "  SE of ELPD",
    ELPD_mean = "  ELPD per observation",
    RMSE = "RMSE of posterior mean",
    MAE = "MAE of posterior mean",
    CRPS = "CRPS (lower = better)",
    Coverage = sprintf("Coverage of %g%% predictive intervals", 100 * attr(x, "prob")),
    Interval_Width = "Mean interval width",
    Accuracy = sprintf("Accuracy (threshold %g)", attr(x, "threshold")),
    Precision = "Precision",
    Recall = "Recall",
    F1_Score = "F1 score",
    Brier = "Brier score (lower = better)",
    AUC = "AUC"
  )
  for (nm in names(vals)) {
    cat(sprintf("  %-40s %s\n", labels[[nm]] %||% nm, format(signif(vals[[nm]], digits))))
  }
  invisible(x)
}

#' @export
as.data.frame.bmb_performance <- function(x, ...) {
  vals <- unlist(unclass(x))
  data.frame(metric = names(vals), value = unname(vals), stringsAsFactors = FALSE)
}
