#' Check MCMC convergence with current best-practice diagnostics
#'
#' Assesses whether the Markov chains of a fitted model can be trusted,
#' following Vehtari et al. (2021) and the Stan developers' guidance:
#'
#' * **Rank-normalised split-R-hat** below `rhat_threshold` (default 1.01;
#'   the older 1.1 threshold is too lenient to detect many failures).
#' * **Bulk-ESS and tail-ESS** of at least `min_ess` (default 100 per chain),
#'   so that posterior means *and* interval endpoints are estimated reliably.
#' * **Divergent transitions** after warm-up: any divergence indicates the
#'   sampler could not explore part of the posterior and results may be
#'   biased (Betancourt, 2017).
#' * **E-BFMI** below `min_bfmi` (default 0.3) indicates poor exploration of
#'   the energy distribution.
#' * Iterations that **saturate the maximum tree depth** are reported as an
#'   efficiency warning; they do not by themselves invalidate the draws.
#'
#' @param model_fit A `stanreg` (rstanarm) or `stanfit` (rstan) object fitted
#'   by MCMC.
#' @param rhat_threshold Maximum acceptable R-hat.
#' @param min_ess Minimum acceptable bulk- and tail-ESS. Defaults to
#'   `100 * number of chains`.
#' @param max_divergent Maximum acceptable number of divergent transitions.
#' @param min_bfmi Minimum acceptable E-BFMI per chain.
#' @param pars Optional character vector of parameters to check. By default
#'   all model parameters are checked.
#' @param save_plots If `TRUE`, save rank and trace plots to `plot_path`.
#' @param plot_path Path of the PDF file for `save_plots = TRUE`.
#' @param ... Deprecated arguments. `ess_threshold`, an ESS *ratio* used in
#'   bmbeR 1.x, is converted to an absolute `min_ess` with a warning.
#'
#' @return An object of class `bmb_convergence`: a list with elements
#'   `converged` (logical), `parameters` (a data frame with R-hat, bulk-ESS
#'   and tail-ESS per parameter), `sampler` (divergences, tree-depth hits,
#'   E-BFMI), `issues` (character vector of problems), `thresholds`, and the
#'   draws needed by the `plot()` method. Use `isTRUE(x$converged)` in code.
#'
#' @references
#' Vehtari, A., Gelman, A., Simpson, D., Carpenter, B., & Bürkner, P.-C.
#' (2021). Rank-normalization, folding, and localization: An improved R-hat
#' for assessing convergence of MCMC. *Bayesian Analysis*, 16(2), 667–718.
#' \doi{10.1214/20-BA1221}
#'
#' Betancourt, M. (2017). A conceptual introduction to Hamiltonian Monte
#' Carlo. *arXiv:1701.02434*.
#'
#' @seealso [generate_plot()] for more diagnostic plots.
#' @examples
#' \donttest{
#' data(kidiq, package = "rstanarm")
#' fit <- rstanarm::stan_glm(kid_score ~ mom_iq, data = kidiq,
#'                           chains = 2, iter = 1000, refresh = 0)
#' conv <- check_convergence(fit)
#' conv
#' isTRUE(conv$converged)
#' }
#' @export
check_convergence <- function(model_fit,
                              rhat_threshold = 1.01,
                              min_ess = NULL,
                              max_divergent = 0,
                              min_bfmi = 0.3,
                              pars = NULL,
                              save_plots = FALSE,
                              plot_path = "trace_plot.pdf",
                              ...) {
  dots <- list(...)
  sf <- extract_stanfit(model_fit)
  arr <- draws_array(model_fit, pars)
  n_chains <- dim(arr)[2L]
  if (!is.null(dots$ess_threshold)) {
    min_ess <- dots$ess_threshold * dim(arr)[1L] * n_chains
    warning(sprintf(paste0("`ess_threshold` (an ESS ratio) is deprecated; using the equivalent ",
                           "absolute `min_ess = %.0f`. Vehtari et al. (2021) recommend ",
                           "min_ess = 100 * chains."), min_ess), call. = FALSE)
  }
  unknown <- setdiff(names(dots), "ess_threshold")
  if (length(unknown) > 0L) {
    stop(sprintf("Unknown argument(s): %s", paste(unknown, collapse = ", ")), call. = FALSE)
  }
  sampler <- sampler_diagnostics(sf)
  out <- convergence_from_draws(arr, sampler,
                                rhat_threshold = rhat_threshold,
                                min_ess = min_ess,
                                max_divergent = max_divergent,
                                min_bfmi = min_bfmi)
  if (isTRUE(save_plots)) {
    grDevices::pdf(plot_path, width = 9, height = 6)
    on.exit(grDevices::dev.off(), add = TRUE)
    print(plot(out, type = "rank"))
    print(plot(out, type = "trace"))
    message("Diagnostic plots saved to: ", plot_path)
  }
  out
}

# Sampler-level diagnostics from a stanfit.
sampler_diagnostics <- function(sf) {
  args <- sf@stan_args[[1L]]
  max_td <- args$control$max_treedepth %||% 10L
  list(
    divergent = rstan::get_num_divergent(sf),
    treedepth_hits = rstan::get_num_max_treedepth(sf),
    max_treedepth = max_td,
    bfmi = suppressWarnings(as.numeric(rstan::get_bfmi(sf))),
    n_warmup = sf@sim$warmup,
    n_iter = sf@sim$iter
  )
}

# Core logic, separated from Stan objects so it can be tested directly.
# `arr` is an iterations x chains x parameters array; `sampler` is a list
# as returned by sampler_diagnostics() (or NULL when unavailable).
convergence_from_draws <- function(arr, sampler = NULL, rhat_threshold = 1.01,
                                   min_ess = NULL, max_divergent = 0,
                                   min_bfmi = 0.3) {
  validate_numeric(rhat_threshold, "rhat_threshold", positive = TRUE)
  n_chains <- dim(arr)[2L]
  n_draws <- dim(arr)[1L] * n_chains
  min_ess <- min_ess %||% (100 * n_chains)
  validate_numeric(min_ess, "min_ess", positive = TRUE)

  draws <- posterior::as_draws_array(arr)
  summ <- posterior::summarise_draws(
    draws,
    rhat = posterior::rhat, ess_bulk = posterior::ess_bulk, ess_tail = posterior::ess_tail
  )
  params <- data.frame(
    variable = summ$variable,
    rhat = summ$rhat,
    ess_bulk = summ$ess_bulk,
    ess_tail = summ$ess_tail,
    stringsAsFactors = FALSE
  )
  params$rhat_ok <- !is.na(params$rhat) & params$rhat <= rhat_threshold
  params$ess_ok <- !is.na(params$ess_bulk) & !is.na(params$ess_tail) &
    params$ess_bulk >= min_ess & params$ess_tail >= min_ess

  issues <- character()
  notes <- character()
  if (n_chains < 2L) {
    issues <- c(issues, "Only one chain: R-hat cannot detect non-convergence between chains. Run at least 4 chains.")
  }
  if (any(!params$rhat_ok)) {
    bad <- params$variable[!params$rhat_ok]
    issues <- c(issues, sprintf("R-hat > %s for %d parameter(s): %s (max %.3f).",
                                rhat_threshold, length(bad), paste(utils::head(bad, 5L), collapse = ", "),
                                max(params$rhat, na.rm = TRUE)))
  }
  if (any(!params$ess_ok)) {
    bad <- params$variable[!params$ess_ok]
    issues <- c(issues, sprintf("Bulk- or tail-ESS < %.0f for %d parameter(s): %s (min bulk %.0f, min tail %.0f).",
                                min_ess, length(bad), paste(utils::head(bad, 5L), collapse = ", "),
                                min(params$ess_bulk, na.rm = TRUE), min(params$ess_tail, na.rm = TRUE)))
  }
  if (!is.null(sampler)) {
    if (sampler$divergent > max_divergent) {
      issues <- c(issues, sprintf("%d divergent transition(s) after warm-up. Increase adapt_delta or reparameterise; do not ignore divergences.",
                                  sampler$divergent))
    }
    low_bfmi <- which(!is.na(sampler$bfmi) & sampler$bfmi < min_bfmi)
    if (length(low_bfmi) > 0L) {
      issues <- c(issues, sprintf("E-BFMI < %s in chain(s) %s.", min_bfmi,
                                  paste(low_bfmi, collapse = ", ")))
    }
    if (sampler$treedepth_hits > 0L) {
      notes <- c(notes, sprintf("%d iteration(s) hit the maximum tree depth (%d): an efficiency concern, not a validity one.",
                                sampler$treedepth_hits, sampler$max_treedepth))
    }
  }

  structure(list(
    converged = length(issues) == 0L,
    parameters = params,
    sampler = sampler,
    issues = issues,
    notes = notes,
    thresholds = list(rhat = rhat_threshold, min_ess = min_ess,
                      max_divergent = max_divergent, min_bfmi = min_bfmi),
    n_chains = n_chains,
    n_draws = n_draws,
    draws = arr
  ), class = "bmb_convergence")
}

#' @export
print.bmb_convergence <- function(x, digits = 3, ...) {
  status <- if (x$converged) "PASSED" else "FAILED"
  cat(sprintf("<bmb_convergence> %s  (%d chains, %d post-warm-up draws)\n",
              status, x$n_chains, x$n_draws))
  cat(sprintf("  Criteria: R-hat <= %s; bulk/tail-ESS >= %.0f; divergences <= %d; E-BFMI >= %s\n",
              x$thresholds$rhat, x$thresholds$min_ess, x$thresholds$max_divergent,
              x$thresholds$min_bfmi))
  p <- x$parameters
  cat(sprintf("  Max R-hat: %.3f | min bulk-ESS: %.0f | min tail-ESS: %.0f\n",
              max(p$rhat, na.rm = TRUE), min(p$ess_bulk, na.rm = TRUE),
              min(p$ess_tail, na.rm = TRUE)))
  if (!is.null(x$sampler)) {
    cat(sprintf("  Divergences: %d | max-treedepth hits: %d | min E-BFMI: %.2f\n",
                x$sampler$divergent, x$sampler$treedepth_hits,
                suppressWarnings(min(x$sampler$bfmi, na.rm = TRUE))))
  }
  if (length(x$issues) > 0L) {
    cat("  Issues:\n")
    for (m in x$issues) cat("   -", m, "\n")
    cat("  Inspect with plot(<this object>) and see https://mc-stan.org/misc/warnings.html\n")
  }
  if (length(x$notes) > 0L) {
    cat("  Notes:\n")
    for (m in x$notes) cat("   -", m, "\n")
  }
  invisible(x)
}

#' Plot convergence diagnostics
#'
#' Rank plots (recommended by Vehtari et al., 2021) or trace plots for the
#' parameters in a [check_convergence()] result. Parameters that failed a
#' check are shown first.
#'
#' @param x A `bmb_convergence` object.
#' @param type `"rank"` (rank-histogram overlay) or `"trace"`.
#' @param max_pars Maximum number of parameters to show.
#' @param ... Passed to the bayesplot function.
#' @return A ggplot object.
#' @export
plot.bmb_convergence <- function(x, type = c("rank", "trace"), max_pars = 6L, ...) {
  type <- match.arg(type)
  p <- x$parameters
  ord <- order(p$rhat_ok & p$ess_ok, -p$rhat)
  pars <- utils::head(p$variable[ord], max_pars)
  arr <- x$draws[, , pars, drop = FALSE]
  if (type == "rank") {
    bayesplot::mcmc_rank_overlay(arr, ...)
  } else {
    bayesplot::mcmc_trace(arr, ...)
  }
}
