# Prior specification: one validated representation of a prior that can be
# translated to rstanarm, evaluated as a density, and summarised.

prior_types <- c("normal", "student_t", "cauchy", "laplace", "exponential", "flat")
allowed_types <- list(
  intercept = c("normal", "student_t", "cauchy", "flat"),
  slope     = c("normal", "student_t", "cauchy", "laplace", "flat"),
  aux       = c("exponential", "normal", "student_t", "cauchy", "flat")
)

#' Specify a prior distribution
#'
#' Creates a validated prior specification that can be used for the
#' intercept, the regression coefficients ("slopes") or the auxiliary
#' parameter (e.g. the residual standard deviation) of a model fitted with
#' [fit_model_with_prior()]. Argument names follow rstanarm.
#'
#' @section Choosing a scale:
#' A prior scale is only meaningful relative to the units of the outcome and
#' the predictors. A fixed scale such as `normal(0, 2.5)` is weakly
#' informative when variables are standardised but can be overwhelmingly
#' informative when they are not (for example, an outcome measured in
#' dollars). Either standardise your variables, use `autoscale = TRUE`
#' (rstanarm then rescales by the standard deviations of the data), derive
#' scales from substantive knowledge, or use [empirical_bayes_priors()]. See
#' Gelman et al. (2008) and `vignette("bmbeR")`.
#'
#' @param type One of `"normal"`, `"student_t"`, `"cauchy"`, `"laplace"`
#'   (coefficients only), `"exponential"` (auxiliary parameter only) or
#'   `"flat"` (an improper uniform prior).
#' @param location Prior location. For slopes, either a single value or one
#'   value per coefficient.
#' @param scale Prior scale (positive). For slopes, a single value or one per
#'   coefficient.
#' @param df Degrees of freedom for `"student_t"`.
#' @param rate Rate for `"exponential"`.
#' @param autoscale Logical. If `TRUE`, rstanarm rescales the prior using the
#'   standard deviations of the outcome and predictors.
#'
#' @return An object of class `bmb_prior`.
#' @references
#' Gelman, A., Jakulin, A., Pittau, M. G., & Su, Y.-S. (2008). A weakly
#' informative default prior distribution for logistic and other regression
#' models. *The Annals of Applied Statistics*, 2(4), 1360–1383.
#' \doi{10.1214/08-AOAS191}
#' @seealso [prior_config()] to combine priors for a model.
#' @examples
#' prior_spec("normal", location = 0, scale = 1)
#' prior_spec("student_t", df = 3, location = 0, scale = 2.5)
#' prior_spec("exponential", rate = 1)
#' @export
prior_spec <- function(type = c("normal", "student_t", "cauchy", "laplace",
                                "exponential", "flat"),
                       location = 0, scale = 2.5, df = 3, rate = 1,
                       autoscale = FALSE) {
  type <- match.arg(type)
  if (!is.logical(autoscale) || length(autoscale) != 1L || is.na(autoscale)) {
    stop("`autoscale` must be TRUE or FALSE.", call. = FALSE)
  }
  spec <- list(type = type, location = NULL, scale = NULL, df = NULL,
               rate = NULL, autoscale = autoscale)
  if (type %in% c("normal", "student_t", "cauchy", "laplace")) {
    validate_numeric(location, "location", allow_vector = TRUE)
    validate_numeric(scale, "scale", positive = TRUE, allow_vector = TRUE)
    if (length(location) > 1L && length(scale) > 1L &&
        length(location) != length(scale)) {
      stop("`location` and `scale` must have the same length (or length 1).",
           call. = FALSE)
    }
    spec$location <- unname(location)
    spec$scale <- unname(scale)
    if (type == "student_t") {
      validate_numeric(df, "df", positive = TRUE, allow_vector = TRUE)
      spec$df <- unname(df)
    }
  } else if (type == "exponential") {
    validate_numeric(rate, "rate", positive = TRUE)
    spec$rate <- rate
  }
  structure(spec, class = "bmb_prior")
}

#' @export
format.bmb_prior <- function(x, digits = 3, ...) {
  f <- function(v) {
    if (length(v) > 4L) {
      paste0("[", paste(signif(utils::head(v, 3L), digits), collapse = ", "), ", ...]")
    } else if (length(v) > 1L) {
      paste0("[", paste(signif(v, digits), collapse = ", "), "]")
    } else {
      as.character(signif(v, digits))
    }
  }
  args <- switch(x$type,
    normal = , cauchy = , laplace = sprintf("location = %s, scale = %s", f(x$location), f(x$scale)),
    student_t = sprintf("df = %s, location = %s, scale = %s", f(x$df), f(x$location), f(x$scale)),
    exponential = sprintf("rate = %s", f(x$rate)),
    flat = ""
  )
  out <- sprintf("%s(%s)", x$type, args)
  if (isTRUE(x$autoscale)) out <- paste(out, "[autoscaled]")
  out
}

#' @export
print.bmb_prior <- function(x, ...) {
  cat("<bmb_prior>", format(x, ...), "\n")
  invisible(x)
}

#' Combine priors for a regression model
#'
#' Bundles the priors for the intercept, the regression coefficients and the
#' auxiliary parameter. Any component left as `NULL` uses rstanarm's default,
#' weakly informative, autoscaled prior (see
#' `vignette("priors", package = "rstanarm")`). To request an improper flat
#' prior explicitly, use `prior_spec("flat")`.
#'
#' The intercept prior applies to the intercept *after centring the
#' predictors* (i.e. the expected outcome, on the link scale, at the
#' predictor means). This is how rstanarm parameterises the model; you do not
#' need to centre the predictors yourself.
#'
#' @param intercept,slope,aux Priors created with [prior_spec()]. Native
#'   rstanarm prior objects (e.g. `rstanarm::normal(0, 1)`) and the
#'   list format used by bmbeR 1.x (e.g.
#'   `list(type = "normal", mu = 0, sigma = 1)`) are also accepted.
#'
#' @return An object of class `bmb_prior_config`.
#' @examples
#' prior_config(
#'   intercept = prior_spec("normal", location = 80, scale = 20),
#'   slope     = prior_spec("normal", location = 0, scale = c(1, 10))
#' )
#' @export
prior_config <- function(intercept = NULL, slope = NULL, aux = NULL) {
  out <- list(
    intercept = as_prior_spec(intercept, "intercept"),
    slope     = as_prior_spec(slope, "slope"),
    aux       = as_prior_spec(aux, "aux")
  )
  structure(out, class = "bmb_prior_config")
}

# Convert user input to a bmb_prior (or NULL = rstanarm default).
as_prior_spec <- function(x, role = c("intercept", "slope", "aux")) {
  role <- match.arg(role)
  if (is.null(x)) return(NULL)
  if (inherits(x, "bmb_prior")) {
    spec <- x
  } else if (is.list(x) && !is.null(x$dist)) {
    # Native rstanarm prior object, e.g. rstanarm::normal(0, 1).
    dist <- if (identical(x$dist, "t")) "student_t" else x$dist
    if (!dist %in% prior_types) {
      stop(sprintf("rstanarm prior '%s' is not supported by bmbeR for the %s. Supported: %s.",
                   x$dist, role, paste(allowed_types[[role]], collapse = ", ")),
           call. = FALSE)
    }
    spec <- if (dist == "exponential") {
      prior_spec("exponential", rate = 1 / x$scale, autoscale = isTRUE(x$autoscale))
    } else {
      prior_spec(dist, location = x$location, scale = x$scale,
                 df = if (dist == "student_t") x$df else 3,
                 autoscale = isTRUE(x$autoscale))
    }
  } else if (is.list(x) && !is.null(x$type)) {
    # bmbeR 1.x format: list(type, mu/location, sigma/scale, nu/df).
    type <- x$type
    if (!type %in% prior_types) {
      stop(sprintf("Unsupported prior type '%s' for the %s. Supported: %s.",
                   type, role, paste(allowed_types[[role]], collapse = ", ")),
           call. = FALSE)
    }
    location <- x$location %||% x$mu %||% 0
    scale <- x$scale %||% x$sigma
    if (is.null(scale) && type %in% c("normal", "student_t", "cauchy", "laplace")) {
      stop(sprintf("The %s prior needs a scale (`scale` or `sigma`).", role), call. = FALSE)
    }
    spec <- prior_spec(type, location = location, scale = scale %||% 1,
                       df = x$df %||% x$nu %||% 3, rate = x$rate %||% 1,
                       autoscale = isTRUE(x$autoscale))
  } else {
    stop(sprintf("Could not interpret the %s prior. Use prior_spec().", role),
         call. = FALSE)
  }
  if (!spec$type %in% allowed_types[[role]]) {
    stop(sprintf("A '%s' prior cannot be used for the %s in rstanarm. Allowed: %s.",
                 spec$type, role, paste(allowed_types[[role]], collapse = ", ")),
         call. = FALSE)
  }
  if (role != "slope" && (length(spec$location) > 1L || length(spec$scale) > 1L)) {
    stop(sprintf("The %s prior must have a single location and scale.", role),
         call. = FALSE)
  }
  spec
}

# Convert user input to a bmb_prior_config.
as_prior_config <- function(x) {
  if (is.null(x)) return(prior_config())
  if (inherits(x, "bmb_prior_config")) return(x)
  if (inherits(x, "bmb_prior")) {
    stop("`prior_config` must combine priors with prior_config(intercept = , slope = , aux = ), ",
         "not a single prior_spec().", call. = FALSE)
  }
  if (is.list(x)) {
    bad <- setdiff(names(x), c("intercept", "slope", "aux"))
    if (length(bad) > 0L || is.null(names(x))) {
      stop("`prior_config` must be created with prior_config() or be a list with ",
           "elements named 'intercept', 'slope' and/or 'aux'.", call. = FALSE)
    }
    return(prior_config(intercept = x$intercept, slope = x$slope, aux = x$aux))
  }
  stop("`prior_config` must be NULL, a prior_config() object, or a named list.",
       call. = FALSE)
}

# Translate a bmb_prior into the corresponding rstanarm prior object.
to_rstanarm_prior <- function(spec) {
  switch(spec$type,
    normal      = rstanarm::normal(spec$location, spec$scale, autoscale = spec$autoscale),
    student_t   = rstanarm::student_t(spec$df, spec$location, spec$scale, autoscale = spec$autoscale),
    cauchy      = rstanarm::cauchy(spec$location, spec$scale, autoscale = spec$autoscale),
    laplace     = rstanarm::laplace(spec$location, spec$scale, autoscale = spec$autoscale),
    exponential = rstanarm::exponential(spec$rate, autoscale = spec$autoscale),
    flat        = NULL
  )
}

#' Translate prior specifications into rstanarm prior arguments
#'
#' Converts bmbeR prior specifications into the `prior_intercept`, `prior`
#' and `prior_aux` arguments of [rstanarm::stan_glm()]. Components that are
#' `NULL` are omitted so that rstanarm's defaults apply; flat priors are
#' returned as explicit `NULL` entries, which is how rstanarm requests them.
#'
#' @param intercept_config,slope_config,aux_config Priors for the intercept,
#'   the coefficients and the auxiliary parameter, in any format accepted by
#'   [prior_config()].
#'
#' @return A named list suitable for `do.call(rstanarm::stan_glm, ...)`.
#' @examples
#' str(build_stanarm_priors(prior_spec("normal", 0, 10), prior_spec("normal", 0, 1)))
#' @export
build_stanarm_priors <- function(intercept_config = NULL, slope_config = NULL,
                                 aux_config = NULL) {
  cfg <- prior_config(intercept = intercept_config, slope = slope_config,
                      aux = aux_config)
  prior_args_from_config(cfg)
}

prior_args_from_config <- function(cfg) {
  out <- list()
  map <- c(intercept = "prior_intercept", slope = "prior", aux = "prior_aux")
  for (nm in names(map)) {
    if (!is.null(cfg[[nm]])) out[map[[nm]]] <- list(to_rstanarm_prior(cfg[[nm]]))
  }
  out
}

# =============================================================================
# Priors as used by a fitted model (after rstanarm's autoscaling)
# =============================================================================

# Returns a data frame with one row per parameter carrying the prior that
# rstanarm actually used. The intercept row refers to the intercept at the
# predictor means (the parameter the prior is placed on).
resolve_priors <- function(fit) {
  check_stanreg(fit, glm_only = TRUE)
  ps <- rstanarm::prior_summary(fit)
  if (isTRUE(attr(ps, "QR"))) {
    stop("Models fitted with QR = TRUE place priors on transformed coefficients; ",
         "prior diagnostics are not available for them.", call. = FALSE)
  }
  if (isTRUE(attr(ps, "sparse"))) {
    stop("Models fitted with sparse = TRUE are not supported.", call. = FALSE)
  }
  coef_names <- colnames(rstanarm::get_x(fit))
  has_intercept <- "(Intercept)" %in% coef_names
  slopes <- setdiff(coef_names, "(Intercept)")
  rows <- list()
  norm_dist <- function(d) if (is.null(d) || is.na(d)) "flat" else if (d == "t") "student_t" else d
  if (has_intercept && !is.null(ps$prior_intercept)) {
    p <- ps$prior_intercept
    rows[[length(rows) + 1L]] <- data.frame(
      variable = "(Intercept)", role = "intercept", dist = norm_dist(p$dist),
      location = as.numeric(p$location %||% 0)[1L],
      scale = as.numeric(p$adjusted_scale %||% p$scale %||% NA)[1L],
      df = as.numeric(p$df %||% NA)[1L], stringsAsFactors = FALSE)
  }
  if (length(slopes) > 0L) {
    p <- ps$prior
    k <- length(slopes)
    if (is.null(p)) {
      p <- list(dist = NA)
    }
    rec <- function(v) if (is.null(v)) rep(NA_real_, k) else rep_len(as.numeric(v), k)
    rows[[length(rows) + 1L]] <- data.frame(
      variable = slopes, role = "slope", dist = norm_dist(p$dist),
      location = rec(p$location %||% 0), scale = rec(p$adjusted_scale %||% p$scale),
      df = rec(p$df), stringsAsFactors = FALSE)
  }
  if (!is.null(ps$prior_aux)) {
    p <- ps$prior_aux
    dist <- norm_dist(p$dist)
    if (dist == "exponential") {
      scale <- p$adjusted_scale %||% (1 / p$rate)
      loc <- NA_real_
    } else {
      scale <- p$adjusted_scale %||% p$scale %||% NA_real_
      loc <- 0
    }
    rows[[length(rows) + 1L]] <- data.frame(
      variable = p$aux_name, role = "aux", dist = dist, location = loc,
      scale = as.numeric(scale)[1L], df = as.numeric(p$df %||% NA)[1L],
      stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, rows)
  supported <- c("normal", "student_t", "cauchy", "laplace", "exponential", "flat")
  bad <- setdiff(unique(out$dist), supported)
  if (length(bad) > 0L) {
    stop(sprintf("Prior diagnostics are not implemented for rstanarm prior(s): %s.",
                 paste(bad, collapse = ", ")), call. = FALSE)
  }
  rownames(out) <- NULL
  out
}

# Draws of the parameters on which the priors are placed: identical to
# as.matrix(fit) except the intercept is evaluated at the predictor means.
prior_scale_draws <- function(fit, priors = resolve_priors(fit)) {
  draws <- as.matrix(fit)
  X <- rstanarm::get_x(fit)
  if ("(Intercept)" %in% colnames(X) && ncol(X) > 1L) {
    slopes <- setdiff(colnames(X), "(Intercept)")
    xbar <- colMeans(X[, slopes, drop = FALSE])
    draws[, "(Intercept)"] <- draws[, "(Intercept)"] +
      drop(draws[, slopes, drop = FALSE] %*% xbar)
  }
  draws[, priors$variable, drop = FALSE]
}

# Log density of each prior evaluated at each draw (draws x parameters).
# Constants are included so values are proper log densities (half-densities
# for positive auxiliary parameters).
prior_log_density <- function(draws, priors) {
  out <- matrix(0, nrow(draws), nrow(priors), dimnames = list(NULL, priors$variable))
  for (i in seq_len(nrow(priors))) {
    x <- draws[, priors$variable[i]]
    pr <- priors[i, ]
    half <- pr$role == "aux" && pr$dist != "exponential"
    ld <- switch(pr$dist,
      normal = dnorm(x, pr$location, pr$scale, log = TRUE),
      student_t = dt((x - pr$location) / pr$scale, pr$df, log = TRUE) - log(pr$scale),
      cauchy = dcauchy(x, pr$location, pr$scale, log = TRUE),
      laplace = -log(2 * pr$scale) - abs(x - pr$location) / pr$scale,
      exponential = dexp(x, 1 / pr$scale, log = TRUE),
      flat = rep(0, length(x))
    )
    if (half) ld <- ld + log(2)
    out[, i] <- ld
  }
  out
}

# Prior standard deviation of each parameter (Inf when it does not exist).
prior_sd <- function(priors) {
  vapply(seq_len(nrow(priors)), function(i) {
    pr <- priors[i, ]
    half <- pr$role == "aux" && pr$dist != "exponential"
    s <- pr$scale
    switch(pr$dist,
      normal = if (half) s * sqrt(1 - 2 / pi) else s,
      student_t = {
        if (pr$df <= 2) Inf else {
          v <- s^2 * pr$df / (pr$df - 2)
          if (half) {
            m <- s * 2 * sqrt(pr$df) * exp(lgamma((pr$df + 1) / 2) - lgamma(pr$df / 2)) /
              (sqrt(pi) * (pr$df - 1))
            sqrt(v - m^2)
          } else sqrt(v)
        }
      },
      cauchy = Inf,
      laplace = sqrt(2) * s,
      exponential = s,
      flat = Inf
    )
  }, numeric(1))
}

# Evaluate a prior's density on a grid (for plotting).
prior_density_grid <- function(pr, x) {
  half <- pr$role == "aux" && pr$dist != "exponential"
  d <- switch(pr$dist,
    normal = dnorm(x, pr$location, pr$scale),
    student_t = dt((x - pr$location) / pr$scale, pr$df) / pr$scale,
    cauchy = dcauchy(x, pr$location, pr$scale),
    laplace = exp(-abs(x - pr$location) / pr$scale) / (2 * pr$scale),
    exponential = dexp(x, 1 / pr$scale),
    flat = rep(NA_real_, length(x))
  )
  if (half) d <- ifelse(x < 0, 0, 2 * d)
  d
}
