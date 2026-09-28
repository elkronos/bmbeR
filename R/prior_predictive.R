#' Prior predictive check
#'
#' Simulates outcomes from the prior predictive distribution, i.e. data the
#' model considers plausible *before* seeing the observed outcomes, by
#' fitting the model with `prior_PD = TRUE` (the likelihood is ignored).
#' Comparing these simulations with what is substantively plausible is the
#' recommended way to check that priors encode sensible assumptions (Gabry
#' et al., 2019; Gelman et al., 2020).
#'
#' For binomial models the check reports the share of prior predictive
#' success probabilities that are extreme (below 5% or above 95%). Wide
#' "non-informative" priors on the logit scale put most of their mass on such
#' extremes, i.e. they claim outcomes are nearly deterministic, which is
#' rarely intended (Gelman et al., 2020).
#'
#' @inheritParams fit_model_with_prior
#' @param ndraws Number of prior predictive data sets to keep.
#' @param plausible_range Optional numeric vector `c(lower, upper)` giving the
#'   range of outcome values you consider possible. The share of simulated
#'   values outside it is reported.
#' @param chains,iter MCMC settings for sampling from the prior (defaults are
#'   smaller than for posterior fitting because the prior is simple).
#' @param ... Further arguments passed to [rstanarm::stan_glm()].
#'
#' @return An object of class `bmb_prior_check` with elements `yrep` (a
#'   draws x observations matrix), `y` (the observed outcome, for scale),
#'   `summary` (a list of summary statistics), `fit` (the prior-only model)
#'   and `binary`. Has `print()` and `plot()` methods.
#' @references
#' Gabry, J., Simpson, D., Vehtari, A., Betancourt, M., & Gelman, A. (2019).
#' Visualization in Bayesian workflow. *Journal of the Royal Statistical
#' Society: Series A*, 182(2), 389–402. \doi{10.1111/rssa.12378}
#'
#' Gelman, A., Vehtari, A., Simpson, D., et al. (2020). Bayesian workflow.
#' *arXiv:2011.01808*.
#' @examples
#' \donttest{
#' data(kidiq, package = "rstanarm")
#' pc <- prior_predictive_check(kidiq, kid_score ~ mom_iq,
#'                              plausible_range = c(0, 200), refresh = 0)
#' pc
#' plot(pc)
#' }
#' @export
prior_predictive_check <- function(data, formula, family = gaussian(),
                                   prior_config = NULL, ndraws = 200,
                                   plausible_range = NULL,
                                   chains = 2, iter = 1000, seed = 1234, ...) {
  validate_data_formula(data, formula)
  validate_positive_integer(ndraws, "ndraws")
  family <- normalize_family(family)
  y <- get_response(formula, data)
  check_response_family(y, family)
  if (!is.null(plausible_range)) {
    if (!is.numeric(plausible_range) || length(plausible_range) != 2L ||
        plausible_range[1] >= plausible_range[2]) {
      stop("`plausible_range` must be c(lower, upper) with lower < upper.", call. = FALSE)
    }
  }
  cfg <- as_prior_config(prior_config)
  check_prior_terms(cfg, formula, data)
  fit <- suppressWarnings(
    run_stan_glm(data, formula, family, cfg, chains = chains, iter = iter,
                 seed = seed, prior_PD = TRUE, ...)
  )
  n_avail <- nrow(as.matrix(fit))
  yrep <- rstanarm::posterior_predict(fit, draws = min(ndraws, n_avail), seed = seed)

  binary <- family$family == "binomial" && is_binary_outcome(y)
  y_num <- if (is.matrix(y)) y[, 1L] else if (binary) binary_to_01(y) else as.numeric(y)
  if (is.matrix(y)) {
    trials <- rowSums(y)
    yrep_prop <- sweep(yrep, 2L, trials, "/")
  }
  summ <- list(
    observed_range = range(y_num),
    yrep_quantiles = stats::quantile(yrep, c(0.01, 0.05, 0.5, 0.95, 0.99), names = TRUE)
  )
  if (family$family == "binomial") {
    # Prior predictive success probabilities for each observation.
    pprob <- rstanarm::posterior_epred(fit, draws = nrow(yrep))
    summ$share_extreme <- mean(pprob < 0.05 | pprob > 0.95)
    prop <- if (is.matrix(y)) rowMeans(yrep_prop) else rowMeans(yrep)
    summ$proportion_quantiles <- stats::quantile(prop, c(0.05, 0.5, 0.95))
  } else {
    summ$yrep_mean_quantiles <- stats::quantile(rowMeans(yrep), c(0.05, 0.5, 0.95))
    summ$yrep_sd_quantiles <- stats::quantile(apply(yrep, 1L, sd), c(0.05, 0.5, 0.95))
  }
  if (!is.null(plausible_range)) {
    summ$plausible_range <- plausible_range
    summ$share_outside <- mean(yrep < plausible_range[1] | yrep > plausible_range[2])
  }
  structure(list(yrep = yrep, y = y_num, summary = summ, fit = fit,
                 binary = binary, family = family$family,
                 prior_summary = rstanarm::prior_summary(fit)),
            class = "bmb_prior_check")
}

#' @export
print.bmb_prior_check <- function(x, ...) {
  s <- x$summary
  cat(sprintf("<bmb_prior_check> %d simulated data sets of %d observations (%s family)\n",
              nrow(x$yrep), ncol(x$yrep), x$family))
  cat(sprintf("  Observed outcome range     : [%s, %s]\n",
              signif(s$observed_range[1], 4), signif(s$observed_range[2], 4)))
  q <- signif(s$yrep_quantiles, 4)
  cat(sprintf("  Prior predictive quantiles : 1%%: %s | 5%%: %s | 50%%: %s | 95%%: %s | 99%%: %s\n",
              q[1], q[2], q[3], q[4], q[5]))
  if (!is.null(s$share_extreme)) {
    pq <- signif(s$proportion_quantiles, 3)
    cat(sprintf("  Simulated success proportion (5%%, 50%%, 95%%): %s, %s, %s\n", pq[1], pq[2], pq[3]))
    cat(sprintf("  Prior success probabilities below 5%% or above 95%%: %.1f%%\n",
                100 * s$share_extreme))
    if (s$share_extreme > 0.5) {
      cat("  ! The priors make most outcomes all-or-nothing (probabilities near 0 or 1):\n",
          "   they are probably too wide on the logit scale (Gelman et al., 2020).\n")
    }
  } else {
    mq <- signif(s$yrep_mean_quantiles, 4)
    cat(sprintf("  Mean of simulated data (5%%, 50%%, 95%%): %s, %s, %s\n", mq[1], mq[2], mq[3]))
  }
  if (!is.null(s$share_outside)) {
    cat(sprintf("  Share of simulated values outside plausible range [%s, %s]: %.1f%%\n",
                s$plausible_range[1], s$plausible_range[2], 100 * s$share_outside))
    if (s$share_outside > 0.05) {
      cat("  ! More than 5% of prior predictive values are implausible: consider tighter\n",
          "   or better-located priors.\n")
    }
  }
  invisible(x)
}

#' Plot a prior predictive check
#'
#' For continuous and count outcomes, overlays densities of simulated data
#' sets on the observed outcome density (shown only for scale). For binary
#' outcomes, shows the distribution of simulated success proportions.
#'
#' @param x A `bmb_prior_check` object.
#' @param ndraws Number of simulated data sets to overlay.
#' @param ... Unused.
#' @return A ggplot object.
#' @export
plot.bmb_prior_check <- function(x, ndraws = 50L, ...) {
  yrep <- x$yrep[seq_len(min(ndraws, nrow(x$yrep))), , drop = FALSE]
  if (x$binary) {
    p <- bayesplot::ppc_stat(x$y, x$yrep, stat = "mean", binwidth = 0.02) +
      ggplot2::labs(x = "Proportion of successes",
                    title = "Prior predictive distribution of the success proportion")
  } else {
    p <- bayesplot::ppc_dens_overlay(x$y, yrep) +
      ggplot2::labs(title = "Prior predictive check",
                    subtitle = "Light: simulated from the prior; dark: observed data (for scale)")
    if (!is.null(x$summary$plausible_range)) {
      p <- p + ggplot2::geom_vline(xintercept = x$summary$plausible_range,
                                   linetype = "dashed")
    }
  }
  p
}
