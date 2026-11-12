#' Export a fit for the priorsense package
#'
#' Returns the posterior draws of a [rstanarm::stan_glm()] or
#' [rstanarm::stan_glmer()] fit together with the log prior and
#' log-likelihood draws that the priorsense package needs for its
#' power-scaling diagnostics (Kallioinen et al., 2023), such as
#' `priorsense::powerscale_sensitivity()` and `priorsense::powerscale_plot_dens()`.
#' priorsense does not read rstanarm fits directly; bmbeR rebuilds the log
#' prior from [rstanarm::prior_summary()] in the same way as
#' [prior_sensitivity()].
#'
#' The draws contain:
#' * every model parameter, with `(Intercept)` evaluated at the predictor
#'   means (the parameter rstanarm places its prior on);
#' * `lprior[1]`, ..., `lprior[p]`: the log density of each population-level
#'   prior, in the row order of [prior_table()], so that priorsense's
#'   `prior_selection` argument can pick individual priors;
#' * `log_lik[1]`, ..., `log_lik[n]`: the pointwise log-likelihood.
#'
#' For multilevel models the group-level covariance prior has no `lprior`
#' entry, so priorsense power-scales only the population-level priors.
#'
#' @inheritParams prior_table
#' @return A `posterior::draws_array` (iterations x chains x variables).
#' @references
#' Kallioinen, N., Paananen, T., Bürkner, P.-C., & Vehtari, A. (2023).
#' Detecting and diagnosing prior and likelihood sensitivity with
#' power-scaling. *Statistics and Computing*, 34, 57.
#' \doi{10.1007/s11222-023-10366-5}
#' @examples
#' \donttest{
#' data(kidiq, package = "rstanarm")
#' fit <- rstanarm::stan_glm(kid_score ~ mom_iq, data = kidiq,
#'                           chains = 2, iter = 1000, refresh = 0)
#' draws <- as_priorsense_draws(fit)
#' posterior::variables(draws)[1:6]
#' if (requireNamespace("priorsense", quietly = TRUE)) {
#'   priorsense::powerscale_sensitivity(draws)
#' }
#' }
#' @export
as_priorsense_draws <- function(fit) {
  priors <- resolve_priors(fit)
  arr <- as.array(fit)
  d <- dim(arr)
  # Chain by chain, the same order as as.matrix(fit) and log_lik(fit).
  draws <- matrix(arr, d[1L] * d[2L], d[3L], dimnames = list(NULL, dimnames(arr)[[3L]]))
  theta <- prior_scale_draws(fit, priors, draws = draws, keep_all = TRUE)
  lprior <- prior_log_density(theta[, priors$variable, drop = FALSE], priors)
  colnames(lprior) <- sprintf("lprior[%d]", seq_len(ncol(lprior)))
  ll <- rstanarm::log_lik(fit)
  colnames(ll) <- sprintf("log_lik[%d]", seq_len(ncol(ll)))
  vals <- cbind(theta, lprior, ll)
  out <- array(vals, c(d[1L], d[2L], ncol(vals)),
               dimnames = list(NULL, NULL, colnames(vals)))
  posterior::as_draws_array(out)
}
