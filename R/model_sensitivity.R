# Two complementary approaches to prior sensitivity:
#  * prior_sensitivity(): local and importance-sampled power-scaling of the
#    prior and likelihood of a single fit (no refitting).
#  * sensitivity_analysis(): refit the model under alternative priors and
#    compare posteriors and predictive performance.

# =============================================================================
# Power-scaling sensitivity (no refit)
# =============================================================================

#' Prior and likelihood sensitivity by power-scaling
#'
#' Diagnoses how strongly the posterior depends on the prior, and whether
#' prior and data conflict, without refitting the model. The prior (or the
#' likelihood) is "power-scaled", i.e. raised to a power `alpha`, and the
#' resulting change in the posterior is measured (Kallioinen et al., 2023).
#'
#' @details
#' **Local sensitivity.** The derivative of a posterior expectation with
#' respect to `log(alpha)` at `alpha = 1` equals a posterior covariance
#' (Giordano et al., 2018). For each parameter \eqn{\theta} with posterior
#' mean \eqn{\mu} and standard deviation \eqn{\sigma}, bmbeR reports
#' * `prior_mean_sens` = \eqn{Cov(\theta, \log p(\theta)) / \sigma}: how many
#'   posterior SDs the mean moves per unit change in `log(alpha)`;
#' * `prior_sd_sens` = \eqn{Cov((\theta-\mu)^2, \log p(\theta)) / (2\sigma^2)}:
#'   the relative change in the posterior SD;
#' * `lik_mean_sens`, `lik_sd_sens`: the same with the log-likelihood.
#'
#' In a conjugate normal model with prior \eqn{N(m_0, s_0^2)} and likelihood
#' \eqn{N(\bar y, s^2)}, `-2 * prior_sd_sens` is the share of posterior
#' precision contributed by the prior, `-2 * lik_sd_sens` the share
#' contributed by the data, and
#' `conflict_z = prior_mean_sens / (2 * sqrt(prior_sd_sens * lik_sd_sens))`
#' equals \eqn{(m_0 - \bar y)/\sqrt{s_0^2 + s^2}}, the classic prior
#' predictive check of prior-data conflict (Box, 1980). Outside that model
#' these are approximations. See the "Prior sensitivity" article on the
#' package website for the derivations.
#'
#' **Diagnosis** (per parameter). The prior's *influence* is
#' `max(|prior_mean_sens|, |prior_sd_sens|)`: a prior matters if it moves the
#' posterior mean or adds precision. The likelihood's *informativeness* is
#' `|lik_sd_sens|`. (`lik_mean_sens` is reported but not used: for a
#' symmetric posterior, tempering prior and likelihood together leaves the
#' mean unchanged, so `lik_mean_sens = -prior_mean_sens`.)
#' * `"-"`: the prior's influence is below `threshold`; the likelihood
#'   dominates.
#' * `"weak likelihood"`: the prior is influential and the likelihood is
#'   not informative; the data say little about this parameter.
#' * `"prior-data conflict"`: prior influential, likelihood informative, and
#'   `|conflict_z| >= z_threshold`; prior and data disagree.
#' * `"informative prior"`: prior influential and likelihood informative,
#'   but they agree; fine if the prior is justified.
#'
#' This refines the diagnosis of Kallioinen et al. (2023), which flags
#' potential conflict whenever both prior and likelihood sensitivity are
#' high, by separating informative-but-compatible priors from conflicting
#' ones.
#'
#' **Finite perturbations.** For each value in `alpha`, the posterior under
#' the power-scaled prior (and likelihood) is approximated by Pareto-smoothed
#' importance sampling (Vehtari et al., 2024). The Pareto \eqn{\hat{k}}
#' diagnostic indicates whether the approximation is reliable. Weakening
#' (`alpha < 1`) is the harder direction: for an approximately normal
#' posterior the importance weights have a Pareto tail with \eqn{k \approx
#' 1 - \alpha} or heavier, so values much below 0.8 are often unreliable.
#' Use [sensitivity_analysis()] to assess large perturbations by refitting.
#'
#' **Posterior contraction** is \eqn{1 - \sigma^2_{post}/\sigma^2_{prior}}
#' (Schad et al., 2021): values near 0 mean the data barely updated the
#' prior. It is `NA` when the prior variance is infinite (flat, Cauchy,
#' Student-t with `df <= 2`).
#'
#' The intercept is analysed at the predictor means, which is the parameter
#' rstanarm places its prior on. Supported: models from
#' [rstanarm::stan_glm()] with normal, Student-t, Cauchy, Laplace, flat or
#' exponential priors, fitted without `QR = TRUE`.
#'
#' @param fit A `stanreg` object from [rstanarm::stan_glm()] or
#'   [fit_model_with_prior()].
#' @param alpha Power-scaling factors for the finite perturbations. Values
#'   below 1 weaken, above 1 strengthen the prior (or likelihood). The
#'   default, `c(0.8, 1.25)`, is a symmetric change on the log scale.
#' @param threshold Influence threshold used for the diagnosis. The default
#'   0.05 corresponds to a prior (or likelihood) contributing about 10% of
#'   the posterior precision in a normal model.
#' @param z_threshold Absolute `conflict_z` at or above which an influential
#'   prior is diagnosed as conflicting with the data.
#' @param pars Parameters to report (default: all).
#' @param scale_priors Parameters whose priors are power-scaled (default:
#'   all). Use this to isolate one prior, e.g. `scale_priors = "sigma"`.
#'
#' @return An object of class `bmb_prior_sensitivity` with elements
#'   `summary` (one row per parameter), `perturbation` (one row per
#'   component, alpha and parameter, with Pareto \eqn{\hat{k}}), `priors`
#'   (the priors used, after autoscaling), and settings. Has `print()` and
#'   `plot()` methods.
#' @references
#' Kallioinen, N., Paananen, T., Bürkner, P.-C., & Vehtari, A. (2023).
#' Detecting and diagnosing prior and likelihood sensitivity with
#' power-scaling. *Statistics and Computing*, 34, 57.
#' \doi{10.1007/s11222-023-10366-5}
#'
#' Giordano, R., Broderick, T., & Jordan, M. I. (2018). Covariances,
#' robustness, and variational Bayes. *Journal of Machine Learning Research*,
#' 19(51), 1–49.
#'
#' Vehtari, A., Simpson, D., Gelman, A., Yao, Y., & Gabry, J. (2024).
#' Pareto smoothed importance sampling. *Journal of Machine Learning
#' Research*, 25(72), 1–58.
#'
#' Box, G. E. P. (1980). Sampling and Bayes' inference in scientific
#' modelling and robustness. *JRSS A*, 143(4), 383–430.
#' \doi{10.2307/2982063}
#'
#' Schad, D. J., Betancourt, M., & Vasishth, S. (2021). Toward a principled
#' Bayesian workflow in cognitive science. *Psychological Methods*, 26(1),
#' 103–126. \doi{10.1037/met0000275}
#' @examples
#' \donttest{
#' data(kidiq, package = "rstanarm")
#' fit <- rstanarm::stan_glm(kid_score ~ mom_iq, data = kidiq,
#'                           prior = rstanarm::normal(2, 0.05),
#'                           chains = 2, iter = 1000, refresh = 0)
#' ps <- prior_sensitivity(fit)
#' ps
#' plot(ps)
#' }
#' @export
prior_sensitivity <- function(fit, alpha = c(0.8, 1.25), threshold = 0.05,
                              z_threshold = 2, pars = NULL, scale_priors = NULL) {
  check_stanreg(fit, glm_only = TRUE)
  validate_numeric(alpha, "alpha", positive = TRUE, allow_vector = TRUE)
  validate_numeric(threshold, "threshold", positive = TRUE)
  validate_numeric(z_threshold, "z_threshold", positive = TRUE)
  priors <- resolve_priors(fit)
  theta <- prior_scale_draws(fit, priors)
  ld <- prior_log_density(theta, priors)
  scale_priors <- scale_priors %||% priors$variable
  unknown <- setdiff(c(scale_priors, pars), priors$variable)
  if (length(unknown) > 0L) {
    stop(sprintf("Unknown parameter(s): %s. Available: %s",
                 paste(unknown, collapse = ", "), paste(priors$variable, collapse = ", ")),
         call. = FALSE)
  }
  log_prior <- rowSums(ld[, scale_priors, drop = FALSE])
  log_lik <- rowSums(rstanarm::log_lik(fit))
  pars <- pars %||% priors$variable

  res <- power_scaling_core(theta[, pars, drop = FALSE], log_prior, log_lik,
                            alpha = alpha, threshold = threshold,
                            z_threshold = z_threshold)
  psd <- prior_sd(priors)[match(pars, priors$variable)]
  res$summary$contraction <- ifelse(is.finite(psd), 1 - res$summary$sd^2 / psd^2, NA_real_)
  res$summary <- res$summary[, c("variable", "mean", "sd", "contraction",
                                 "prior_mean_sens", "prior_sd_sens",
                                 "lik_mean_sens", "lik_sd_sens", "conflict_z",
                                 "diagnosis")]
  flat_scaled <- priors$variable[priors$dist == "flat" & priors$variable %in% scale_priors]
  structure(c(res, list(priors = priors, scale_priors = scale_priors,
                        flat = flat_scaled)),
            class = "bmb_prior_sensitivity")
}

# Core computation on draws, separated from Stan objects for testing.
# theta: draws x parameters; log_prior, log_lik: per-draw totals.
power_scaling_core <- function(theta, log_prior, log_lik, alpha = c(0.8, 1.25),
                               threshold = 0.05, z_threshold = 2) {
  theta <- as.matrix(theta)
  S <- nrow(theta)
  mu <- colMeans(theta)
  sdv <- apply(theta, 2L, sd)
  local <- function(lw) {
    if (sd(lw) == 0) return(list(mean = rep(0, ncol(theta)), sd = rep(0, ncol(theta))))
    centred <- sweep(theta, 2L, mu)
    list(
      mean = as.numeric(cov(theta, lw)) / sdv,
      sd = as.numeric(cov(centred^2, lw)) / (2 * sdv^2)
    )
  }
  lp <- local(log_prior)
  ll <- local(log_lik)
  summ <- data.frame(
    variable = colnames(theta), mean = unname(mu), sd = unname(sdv),
    prior_mean_sens = lp$mean, prior_sd_sens = lp$sd,
    lik_mean_sens = ll$mean, lik_sd_sens = ll$sd,
    stringsAsFactors = FALSE
  )
  summ$conflict_z <- conflict_z(summ)
  summ$diagnosis <- diagnose_sensitivity(summ, threshold, z_threshold)

  kmax <- pareto_k_threshold(S)
  pert <- list()
  for (comp in c("prior", "likelihood")) {
    base <- if (comp == "prior") log_prior else log_lik
    for (a in alpha) {
      if (sd(base) == 0 || a == 1) {
        w <- rep(1 / S, S)
        k <- -Inf
      } else {
        ps <- suppressWarnings(loo::psis((a - 1) * base, r_eff = NA))
        w <- as.numeric(stats::weights(ps, log = FALSE, normalize = TRUE))
        k <- as.numeric(loo::pareto_k_values(ps))
      }
      m_w <- colSums(theta * w)
      sd_w <- sqrt(colSums(sweep(theta, 2L, m_w)^2 * w))
      pert[[length(pert) + 1L]] <- data.frame(
        component = comp, alpha = a, variable = colnames(theta),
        mean = unname(m_w), sd = unname(sd_w),
        shift_sd = unname((m_w - mu) / sdv), sd_ratio = unname(sd_w / sdv),
        pareto_k = k, reliable = k <= kmax, stringsAsFactors = FALSE
      )
    }
  }
  pert <- do.call(rbind, pert)
  rownames(pert) <- NULL
  list(summary = summ, perturbation = pert, threshold = threshold,
       alpha = alpha, n_draws = S)
}

# Prior-data conflict statistic. In a conjugate normal model with prior
# N(m0, s0^2) and likelihood N(ybar, s^2) this equals the prior predictive
# z-score (m0 - ybar) / sqrt(s0^2 + s^2) (Box, 1980), because
# prior_sd_sens = -w/2, lik_sd_sens = -(1 - w)/2 and
# prior_mean_sens = w (1 - w) (m0 - ybar) / sigma, with w the prior's share of
# posterior precision.
conflict_z <- function(s) {
  denom <- 2 * sqrt(abs(s$prior_sd_sens * s$lik_sd_sens))
  ifelse(denom > 0, s$prior_mean_sens / denom, NA_real_)
}

# The likelihood's informativeness is measured by |lik_sd_sens| (half the
# data's share of posterior precision in a normal model). Its mean
# sensitivity is not used: jointly tempering prior and likelihood leaves the
# mean of a symmetric posterior unchanged, so lik_mean_sens = -prior_mean_sens
# there and carries no separate information.
diagnose_sensitivity <- function(s, threshold, z_threshold = 2) {
  influence <- pmax(abs(s$prior_mean_sens), abs(s$prior_sd_sens))
  lik <- abs(s$lik_sd_sens)
  z <- abs(s$conflict_z)
  ifelse(influence < threshold, "-",
         ifelse(lik < threshold, "weak likelihood",
                ifelse(!is.na(z) & z >= z_threshold, "prior-data conflict",
                       "informative prior")))
}

#' @export
print.bmb_prior_sensitivity <- function(x, digits = 3, ...) {
  cat(sprintf("<bmb_prior_sensitivity> power-scaling diagnostics (%d draws, threshold %s)\n",
              x$n_draws, x$threshold))
  s <- x$summary
  num <- vapply(s, is.numeric, logical(1))
  s[num] <- lapply(s[num], function(v) round(v, digits))
  print(s, row.names = FALSE)
  flagged <- x$summary$diagnosis != "-"
  if (any(flagged)) {
    cat("\nFlagged parameters:\n")
    for (i in which(flagged)) {
      cat(sprintf("  %-20s %s\n", x$summary$variable[i], x$summary$diagnosis[i]))
    }
  } else {
    cat("\nNo parameter is sensitive to the prior at this threshold.\n")
  }
  p <- x$perturbation
  cat("\nFinite perturbations: posterior mean shift, in posterior SDs, when the prior or\nlikelihood is raised to the power alpha (* = unreliable importance sampling;\nrefit with sensitivity_analysis() instead):\n")
  p$label <- sprintf("%s%s", sprintf("%+.2f", p$shift_sd), ifelse(p$reliable, "", "*"))
  print_perturbation_table(p)
  if (length(x$flat) > 0L) {
    cat(sprintf("\nNote: flat priors on %s do not change under power-scaling.\n",
                paste(x$flat, collapse = ", ")))
  }
  cat("Intercept diagnostics refer to the intercept at the predictor means.\n")
  invisible(x)
}

print_perturbation_table <- function(p) {
  p$col <- sprintf("%s a=%s", ifelse(p$component == "prior", "prior", "lik"), p$alpha)
  vars <- unique(p$variable)
  cols <- unique(p$col)
  m <- matrix("", length(vars), length(cols), dimnames = list(vars, cols))
  for (i in seq_len(nrow(p))) m[p$variable[i], p$col[i]] <- p$label[i]
  print(noquote(m))
}

#' Plot power-scaling sensitivity
#'
#' Shows how far each posterior mean moves (in posterior SDs) when the prior
#' or the likelihood is power-scaled by each `alpha`. Hollow points mark
#' importance-sampling estimates flagged as unreliable by Pareto
#' \eqn{\hat{k}}.
#'
#' @param x A `bmb_prior_sensitivity` object.
#' @param ... Unused.
#' @return A ggplot object.
#' @export
plot.bmb_prior_sensitivity <- function(x, ...) {
  p <- x$perturbation
  base <- unique(p[, c("component", "variable")])
  base$alpha <- 1
  base$shift_sd <- 0
  base$reliable <- TRUE
  d <- rbind(p[, names(base)], base)
  d$reliability <- ifelse(d$reliable, "reliable", "unreliable (Pareto k)")
  ggplot2::ggplot(d, ggplot2::aes(x = .data$alpha, y = .data$shift_sd,
                                  colour = .data$variable, group = .data$variable)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60") +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes(shape = .data$reliability), size = 2.5) +
    ggplot2::scale_shape_manual(values = c(reliable = 16, `unreliable (Pareto k)` = 1)) +
    ggplot2::scale_x_log10(breaks = sort(unique(d$alpha))) +
    ggplot2::facet_wrap(~component, labeller = ggplot2::as_labeller(
      c(prior = "Prior power-scaled", likelihood = "Likelihood power-scaled"))) +
    ggplot2::labs(x = "Power-scaling factor alpha (log scale)",
                  y = "Shift of posterior mean (posterior SDs)",
                  colour = "Parameter", shape = NULL,
                  title = "Power-scaling sensitivity") +
    ggplot2::theme_bw()
}

# =============================================================================
# Multi-prior refit
# =============================================================================

#' Sensitivity analysis across alternative priors
#'
#' Refits the model under each of several prior configurations and compares
#' (1) the posterior of every parameter with a reference configuration and
#' (2) out-of-sample predictive performance by PSIS-LOO (Vehtari et al.,
#' 2017), including the standard error of each difference. This is the
#' "global" complement to [prior_sensitivity()], which is local and needs no
#' refitting.
#'
#' A shift of `shift_sd` means the posterior mean under a configuration
#' differs from the reference by that many reference posterior SDs;
#' `sd_ratio` compares posterior SDs. Shifts at or above `shift_threshold`
#' are flagged (a heuristic; choose it for your context).
#'
#' LOO compares predictive performance, not prior plausibility: a prior can
#' change the estimate of a parameter substantially while barely changing
#' predictions, so both parts of the output should be examined.
#'
#' @inheritParams fit_model_with_prior
#' @param prior_configurations A *named* list of prior configurations (each
#'   anything accepted by `prior_config` in [fit_model_with_prior()], with
#'   `NULL` meaning rstanarm's defaults), or the bmbeR 1.x format: an
#'   unnamed list of `list(label = , prior_config = )`.
#' @param reference Name or index of the reference configuration.
#' @param metric LOO estimate reported per configuration: `"elpd_loo"`,
#'   `"p_loo"` or `"looic"`.
#' @param shift_threshold Posterior-mean shift (in reference SDs) that is
#'   flagged.
#' @param k_threshold Passed to [loo::loo()]: if not `NULL`, observations
#'   with Pareto \eqn{\hat{k}} above it are refitted exactly (slow).
#' @param keep_fits Keep the fitted models in the result?
#' @param ... Further arguments passed to [fit_model_with_prior()].
#'
#' @return An object of class `bmb_sensitivity` with elements `summary`
#'   (posterior summaries by configuration and parameter), `comparison` (the
#'   [loo::loo_compare()] table), `metric` (named vector of the chosen LOO
#'   estimate), `converged` (named logical), `loo` (list of loo objects) and,
#'   if `keep_fits = TRUE`, `fits`.
#' @references
#' Vehtari, A., Gelman, A., & Gabry, J. (2017). Practical Bayesian model
#' evaluation using leave-one-out cross-validation and WAIC. *Statistics and
#' Computing*, 27(5), 1413–1432. \doi{10.1007/s11222-016-9696-4}
#' @examples
#' \donttest{
#' data(kidiq, package = "rstanarm")
#' sens <- sensitivity_analysis(
#'   kidiq, kid_score ~ mom_iq,
#'   prior_configurations = list(
#'     default = NULL,
#'     tight   = prior_config(slope = prior_spec("normal", 0, 0.1))
#'   ),
#'   chains = 2, iter = 1000, refresh = 0
#' )
#' sens
#' plot(sens)
#' }
#' @export
sensitivity_analysis <- function(data, formula, prior_configurations,
                                 family = gaussian(),
                                 chains = 4, iter = 2000, seed = 1234,
                                 reference = 1L,
                                 metric = c("elpd_loo", "p_loo", "looic"),
                                 shift_threshold = 0.5, k_threshold = NULL,
                                 keep_fits = TRUE, ...) {
  metric <- match.arg(metric)
  configs <- normalize_configurations(prior_configurations)
  labels <- names(configs)
  ref <- if (is.character(reference)) match(reference, labels) else as.integer(reference)
  if (is.na(ref) || ref < 1L || ref > length(configs)) {
    stop("`reference` must name or index one of the prior configurations.", call. = FALSE)
  }

  fits <- vector("list", length(configs))
  names(fits) <- labels
  loos <- fits
  converged <- setNames(logical(length(configs)), labels)
  for (i in seq_along(configs)) {
    message(sprintf("Fitting configuration %d/%d: '%s'", i, length(configs), labels[i]))
    fit <- fit_model_with_prior(data = data, formula = formula, family = family,
                                prior_config = configs[[i]], chains = chains,
                                iter = iter, seed = seed, ...)
    converged[i] <- attr(fit, "bmb_convergence")$converged
    loos[[i]] <- if (is.null(k_threshold)) {
      suppressWarnings(loo::loo(fit))
    } else {
      loo::loo(fit, k_threshold = k_threshold)
    }
    fits[[i]] <- fit
  }

  summ <- do.call(rbind, lapply(seq_along(fits), function(i) {
    d <- as.matrix(fits[[i]])
    data.frame(config = labels[i], variable = colnames(d),
               mean = colMeans(d), sd = apply(d, 2L, sd),
               q2.5 = apply(d, 2L, stats::quantile, 0.025),
               q97.5 = apply(d, 2L, stats::quantile, 0.975),
               stringsAsFactors = FALSE)
  }))
  ref_s <- summ[summ$config == labels[ref], ]
  idx <- match(summ$variable, ref_s$variable)
  summ$shift_sd <- (summ$mean - ref_s$mean[idx]) / ref_s$sd[idx]
  summ$sd_ratio <- summ$sd / ref_s$sd[idx]
  summ$flag <- !is.na(summ$shift_sd) & abs(summ$shift_sd) >= shift_threshold
  rownames(summ) <- NULL

  metric_vals <- vapply(loos, function(l) l$estimates[metric, "Estimate"], numeric(1))
  comparison <- if (length(loos) > 1L) loo::loo_compare(loos) else NULL

  structure(list(
    summary = summ, comparison = comparison, metric = metric_vals,
    metric_name = metric, converged = converged, loo = loos,
    fits = if (keep_fits) fits else NULL, reference = labels[ref],
    shift_threshold = shift_threshold
  ), class = "bmb_sensitivity")
}

normalize_configurations <- function(x) {
  if (!is.list(x) || length(x) == 0L) {
    stop("`prior_configurations` must be a non-empty list.", call. = FALSE)
  }
  legacy <- vapply(x, function(el) is.list(el) && all(c("label", "prior_config") %in% names(el)),
                   logical(1))
  if (all(legacy)) {
    labels <- vapply(x, function(el) as.character(el$label), character(1))
    configs <- lapply(x, function(el) el$prior_config)
  } else if (any(legacy)) {
    stop("Mixing the bmbeR 1.x list(label, prior_config) format with named ",
         "configurations is not supported.", call. = FALSE)
  } else {
    labels <- names(x)
    if (is.null(labels) || any(!nzchar(labels))) {
      stop("`prior_configurations` must be a named list, e.g. ",
           "list(default = NULL, tight = prior_config(...)).", call. = FALSE)
    }
    configs <- x
  }
  if (anyDuplicated(labels)) stop("Configuration names must be unique.", call. = FALSE)
  configs <- lapply(configs, as_prior_config)
  names(configs) <- labels
  configs
}

#' @export
print.bmb_sensitivity <- function(x, digits = 3, ...) {
  cat(sprintf("<bmb_sensitivity> %d prior configurations (reference: '%s')\n",
              length(x$metric), x$reference))
  if (any(!x$converged)) {
    cat("  ! Convergence failed for:", paste(names(x$converged)[!x$converged], collapse = ", "), "\n")
  }
  s <- x$summary
  cat("\nPosterior means by configuration (shift in reference posterior SDs):\n")
  vars <- unique(s$variable)
  cfgs <- unique(s$config)
  m <- matrix("", length(vars), length(cfgs), dimnames = list(vars, cfgs))
  for (i in seq_len(nrow(s))) {
    lab <- sprintf("%s", signif(s$mean[i], digits))
    if (s$config[i] != x$reference) {
      lab <- sprintf("%s (%+.2f%s)", lab, s$shift_sd[i], if (s$flag[i]) "!" else "")
    }
    m[s$variable[i], s$config[i]] <- lab
  }
  print(noquote(m))
  if (any(s$flag)) {
    cat(sprintf("  ! = shift of at least %s posterior SDs\n", x$shift_threshold))
  }
  if (!is.null(x$comparison)) {
    cat("\nPredictive comparison (PSIS-LOO; differences relative to the best):\n")
    cmp <- x$comparison[, c("elpd_diff", "se_diff", "elpd_loo", "p_loo"), drop = FALSE]
    print(round(cmp, 1))
    cat("  |elpd_diff| smaller than ~2 se_diff is not a meaningful difference.\n")
  }
  invisible(x)
}

#' Plot posterior intervals across prior configurations
#'
#' @param x A `bmb_sensitivity` object.
#' @param pars Parameters to show (default: all).
#' @param ... Unused.
#' @return A ggplot object.
#' @export
plot.bmb_sensitivity <- function(x, pars = NULL, ...) {
  s <- x$summary
  if (!is.null(pars)) s <- s[s$variable %in% pars, ]
  s$config <- factor(s$config, levels = rev(unique(x$summary$config)))
  ggplot2::ggplot(s, ggplot2::aes(y = .data$config, x = .data$mean,
                                  xmin = .data$q2.5, xmax = .data$q97.5,
                                  colour = .data$config)) +
    ggplot2::geom_pointrange() +
    ggplot2::facet_wrap(~variable, scales = "free_x") +
    ggplot2::labs(x = "Posterior mean and 95% interval", y = NULL,
                  title = "Posterior sensitivity to the prior") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")
}
