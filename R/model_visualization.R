#' Diagnostic plots for MCMC draws
#'
#' Creates trace, rank, histogram, density and autocorrelation plots with
#' bayesplot. Rank plots are the most sensitive visual check of mixing
#' (Vehtari et al., 2021): with well-mixed chains each chain's rank histogram
#' is approximately uniform.
#'
#' @param model_fit A `stanreg` or `stanfit` object fitted by MCMC.
#' @param types Plot types: any of `"trace"`, `"rank"`, `"hist"`,
#'   `"density"`, `"autocorrelation"`.
#' @param pars Parameters to plot (default: all model parameters).
#' @param print Print each plot? Set to `FALSE` to only return them.
#' @param ... Passed to the underlying bayesplot functions.
#'
#' @return Invisibly, a named list of ggplot objects (one per type).
#' @references
#' Gabry, J., Simpson, D., Vehtari, A., Betancourt, M., & Gelman, A. (2019).
#' Visualization in Bayesian workflow. *JRSS A*, 182(2), 389–402.
#' \doi{10.1111/rssa.12378}
#' @examples
#' \donttest{
#' data(kidiq, package = "rstanarm")
#' fit <- rstanarm::stan_glm(kid_score ~ mom_iq, data = kidiq,
#'                           chains = 2, iter = 1000, refresh = 0)
#' plots <- generate_plot(fit, types = c("trace", "rank"), print = FALSE)
#' plots$rank
#' }
#' @export
generate_plot <- function(model_fit,
                          types = c("trace", "rank", "hist", "density", "autocorrelation"),
                          pars = NULL, print = TRUE, ...) {
  valid <- c("trace", "rank", "hist", "density", "autocorrelation")
  bad <- setdiff(types, valid)
  if (length(bad) > 0L) {
    stop(sprintf("Invalid plot type(s): %s. Available: %s.",
                 paste(bad, collapse = ", "), paste(valid, collapse = ", ")), call. = FALSE)
  }
  arr <- draws_array(model_fit, pars)
  plots <- lapply(types, function(type) {
    switch(type,
      trace = bayesplot::mcmc_trace(arr, ...),
      rank = bayesplot::mcmc_rank_overlay(arr, ...),
      hist = bayesplot::mcmc_hist(arr, ...),
      density = bayesplot::mcmc_dens_overlay(arr, ...),
      autocorrelation = bayesplot::mcmc_acf(arr, ...)
    )
  })
  names(plots) <- types
  if (isTRUE(print)) for (p in plots) print(p)
  invisible(plots)
}

#' Plot posterior intervals
#'
#' Shows posterior medians with inner (default 50%) and outer (default 95%)
#' central credible intervals for each parameter.
#'
#' @param model_fit A `stanreg` or `stanfit` object.
#' @param prob Probability mass of the outer interval.
#' @param prob_inner Probability mass of the inner interval.
#' @param pars Parameters to show. Defaults to all model parameters (for
#'   `stanfit` objects, all except `lp__`). Parameters on very different
#'   scales are easier to read when plotted separately.
#' @param ... Passed to [bayesplot::mcmc_intervals()].
#'
#' @return A ggplot object.
#' @examples
#' \donttest{
#' data(kidiq, package = "rstanarm")
#' fit <- rstanarm::stan_glm(kid_score ~ mom_iq + mom_hs, data = kidiq,
#'                           chains = 2, iter = 1000, refresh = 0)
#' plot_posterior_distributions(fit, pars = c("mom_iq", "mom_hs"))
#' }
#' @export
plot_posterior_distributions <- function(model_fit, prob = 0.95, prob_inner = 0.5,
                                         pars = NULL, ...) {
  validate_probability(prob, "prob", open = TRUE)
  validate_probability(prob_inner, "prob_inner", open = TRUE)
  if (prob_inner > prob) stop("`prob_inner` must not exceed `prob`.", call. = FALSE)
  arr <- draws_array(model_fit, pars)
  if (is.null(pars)) {
    keep <- setdiff(dimnames(arr)[[3]], "lp__")
    arr <- arr[, , keep, drop = FALSE]
  }
  bayesplot::mcmc_intervals(arr, prob = prob_inner, prob_outer = prob, ...) +
    ggplot2::labs(subtitle = sprintf("Posterior medians with %g%% and %g%% intervals",
                                     100 * prob_inner, 100 * prob))
}

#' Compare each prior with its posterior
#'
#' Overlays the prior density that rstanarm actually used (after any
#' autoscaling) on the posterior density of each parameter. A posterior that
#' looks like its prior indicates the data carry little information about
#' that parameter; a posterior squeezed against a region the prior considers
#' implausible suggests prior-data conflict. Unlike
#' [rstanarm::posterior_vs_prior()], no refitting is needed.
#'
#' The intercept is shown at the predictor means, which is the parameter
#' rstanarm places its prior on.
#'
#' @param fit A `stanreg` object from [rstanarm::stan_glm()].
#' @param pars Parameters to show (default: all).
#' @param n_grid Number of grid points for the prior density.
#' @return A ggplot object.
#' @examples
#' \donttest{
#' data(kidiq, package = "rstanarm")
#' fit <- rstanarm::stan_glm(kid_score ~ mom_iq, data = kidiq,
#'                           chains = 2, iter = 1000, refresh = 0)
#' plot_prior_posterior(fit)
#' }
#' @export
plot_prior_posterior <- function(fit, pars = NULL, n_grid = 300L) {
  priors <- resolve_priors(fit)
  theta <- prior_scale_draws(fit, priors)
  pars <- pars %||% priors$variable
  unknown <- setdiff(pars, priors$variable)
  if (length(unknown) > 0L) {
    stop(sprintf("Unknown parameter(s): %s", paste(unknown, collapse = ", ")), call. = FALSE)
  }
  post <- list()
  prior_d <- list()
  for (v in pars) {
    x <- theta[, v]
    pr <- priors[priors$variable == v, ]
    label <- if (v == "(Intercept)") "(Intercept) at predictor means" else v
    dens <- stats::density(x)
    post[[v]] <- data.frame(parameter = label, x = dens$x, density = dens$y,
                            stringsAsFactors = FALSE)
    if (pr$dist != "flat") {
      # Show the posterior region plus the prior's central 95% (for finite
      # scale priors), so both are visible.
      q <- stats::quantile(x, c(0.001, 0.999))
      span <- diff(q)
      centre <- if (is.na(pr$location)) 0 else pr$location
      # Cap the extension so a vague prior does not squash the posterior.
      lo <- max(min(q[1] - 0.5 * span, centre - 2 * pr$scale), q[1] - 3 * span)
      hi <- min(max(q[2] + 0.5 * span, centre + 2 * pr$scale), q[2] + 3 * span)
      if (pr$role == "aux") lo <- max(lo, 0)
      grid <- seq(lo, hi, length.out = n_grid)
      prior_d[[v]] <- data.frame(parameter = label, x = grid,
                                 density = prior_density_grid(pr, grid),
                                 stringsAsFactors = FALSE)
    }
  }
  post <- do.call(rbind, post)
  post$distribution <- "posterior"
  d <- post
  if (length(prior_d) > 0L) {
    prior_d <- do.call(rbind, prior_d)
    prior_d$distribution <- "prior"
    d <- rbind(post, prior_d)
  }
  ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$density,
                                  colour = .data$distribution,
                                  linetype = .data$distribution)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::facet_wrap(~parameter, scales = "free") +
    ggplot2::scale_colour_manual(values = c(posterior = "#1b6ca8", prior = "#d1495b")) +
    ggplot2::scale_linetype_manual(values = c(posterior = "solid", prior = "dashed")) +
    ggplot2::labs(x = NULL, y = "Density", colour = NULL, linetype = NULL,
                  title = "Prior vs posterior") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
}
