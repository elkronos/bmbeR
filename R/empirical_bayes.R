#' Data-informed priors: unit-information, empirical Bayes and power priors
#'
#' Derives priors for the intercept and regression coefficients of a
#' generalised linear model from data, using one of three published methods
#' that avoid the "double-dipping" problem of centring a tight prior on
#' estimates from the same data that will be used to fit the model (which
#' counts the data twice and produces overconfident posteriors).
#'
#' @details
#' A maximum-likelihood GLM with the same formula and family is fitted first.
#' Coefficients are on the link scale of `family`, so the priors are directly
#' usable in [fit_model_with_prior()]. The intercept prior is placed on the
#' intercept at the predictor means, which is how rstanarm parameterises the
#' model.
#'
#' * `method = "unit_information"` (default; Kass & Wasserman, 1995):
#'   each prior is centred at the estimate with variance `n` times the
#'   sampling variance, i.e. the prior carries the information of a single
#'   observation. It is the prior implicit in BIC and is suitable when the
#'   same data are used to fit the model, because it adds only `1/n` of the
#'   data's information.
#' * `method = "eb_shrinkage"` (parametric empirical Bayes; Efron & Morris,
#'   1973; Morris, 1983): coefficients are placed on a common scale by
#'   multiplying by the predictor standard deviations, and a shared prior
#'   standard deviation `tau` is estimated by maximising the marginal
#'   likelihood of `b_j ~ N(0, tau^2 + se_j^2)`. Slopes then receive
#'   zero-centred priors with scale `tau / sd(x_j)`, which shrink noisy
#'   estimates towards zero. Requires at least three slopes. When the
#'   estimate of `tau` falls below the median standard error it is floored
#'   at that value (with a warning), because a boundary estimate of zero
#'   would give a degenerate prior. The intercept uses the unit-information
#'   prior.
#' * `method = "power"` (Ibrahim & Chen, 2000): `data` must be *historical
#'   or external* data, not the data you will analyse. The prior is the
#'   normal approximation to the historical likelihood raised to the power
#'   `a0`: centred at the historical estimates, with variances inflated by
#'   `1 / a0`. `a0 = 1` borrows the historical data at full weight; smaller
#'   values discount it.
#'
#' With `type = "student_t"` or `"cauchy"`, the same locations and scales are
#' used with heavier tails, which lets the data override the prior if the two
#' conflict (O'Hagan & Pericchi, 2012).
#'
#' @param data A data frame. For `method = "power"`, the historical data.
#'   Rows with missing values in the variables of `formula` are dropped (with
#'   a message); other columns are ignored.
#' @param formula Model formula, identical to the one you will fit.
#' @param family Model family (object, function or name), identical to the
#'   one you will fit.
#' @param method One of `"unit_information"`, `"eb_shrinkage"` or `"power"`.
#' @param type Prior family for the output: `"normal"`, `"student_t"` or
#'   `"cauchy"`.
#' @param a0 Power-prior discount in `(0, 1]` (only for `method = "power"`).
#' @param df Degrees of freedom when `type = "student_t"`.
#' @param dist_types Deprecated (bmbeR 1.x). rstanarm requires a single prior
#'   family for all coefficients; use `type`.
#'
#' @return A [prior_config()] object (class `bmb_prior_config`) that can be
#'   passed to [fit_model_with_prior()], with attributes `table` (a data frame
#'   of estimates, standard errors and prior parameters), `method`, `terms`
#'   and, for `"eb_shrinkage"`, `tau`.
#'
#' @references
#' Kass, R. E., & Wasserman, L. (1995). A reference Bayesian test for nested
#' hypotheses and its relationship to the Schwarz criterion. *JASA*, 90(431),
#' 928–934. \doi{10.1080/01621459.1995.10476592}
#'
#' Efron, B., & Morris, C. (1973). Stein's estimation rule and its
#' competitors—an empirical Bayes approach. *JASA*, 68(341), 117–130.
#' \doi{10.1080/01621459.1973.10481350}
#'
#' Morris, C. N. (1983). Parametric empirical Bayes inference: Theory and
#' applications. *JASA*, 78(381), 47–55. \doi{10.1080/01621459.1983.10477914}
#'
#' Ibrahim, J. G., & Chen, M.-H. (2000). Power prior distributions for
#' regression models. *Statistical Science*, 15(1), 46–60.
#' \doi{10.1214/ss/1009212673}
#'
#' O'Hagan, A., & Pericchi, L. (2012). Bayesian heavy-tailed models and
#' conflict resolution: A review. *Brazilian Journal of Probability and
#' Statistics*, 26(4), 372–401. \doi{10.1214/11-BJPS164}
#'
#' @examples
#' data(kidiq, package = "rstanarm")
#' empirical_bayes_priors(kidiq, kid_score ~ mom_iq + mom_hs)
#'
#' # Power prior from a (here simulated) historical study, discounted by half
#' historical <- kidiq[sample(nrow(kidiq), 200), ]
#' empirical_bayes_priors(historical, kid_score ~ mom_iq + mom_hs,
#'                        method = "power", a0 = 0.5)
#' @export
empirical_bayes_priors <- function(data, formula, family = gaussian(),
                                   method = c("unit_information", "eb_shrinkage", "power"),
                                   type = c("normal", "student_t", "cauchy"),
                                   a0 = 0.5, df = 3, dist_types = NULL) {
  method <- match.arg(method)
  if (!is.null(dist_types)) {
    types <- unique(unlist(dist_types))
    if (length(types) != 1L || !types %in% c("normal", "student_t", "cauchy")) {
      stop("`dist_types` is deprecated: rstanarm uses one prior family for all ",
           "coefficients. Use `type = \"normal\"`, \"student_t\" or \"cauchy\".",
           call. = FALSE)
    }
    warning("`dist_types` is deprecated; use `type` instead.", call. = FALSE)
    type <- types
  }
  type <- match.arg(type)
  validate_data_formula(data, formula)
  family <- normalize_family(family)
  validate_numeric(df, "df", positive = TRUE)
  if (method == "power") {
    validate_numeric(a0, "a0", positive = TRUE)
    if (a0 > 1) stop("`a0` must lie in (0, 1].", call. = FALSE)
  }

  mf <- model.frame(formula, data = data, na.action = na.omit)
  n_dropped <- nrow(data) - nrow(mf)
  if (n_dropped > 0L) {
    message(sprintf("Dropped %d row(s) with missing values in the model variables.", n_dropped))
  }
  check_response_family(model.response(mf), family)

  mle <- fit_mle(formula, data, family)
  X <- mle$X
  n <- nrow(X)
  terms_x <- colnames(X)
  has_intercept <- "(Intercept)" %in% terms_x
  slopes <- setdiff(terms_x, "(Intercept)")

  beta <- mle$coef
  V <- mle$vcov
  if (anyNA(beta)) {
    stop(sprintf("Coefficient(s) not estimable (collinear predictors?): %s",
                 paste(names(beta)[is.na(beta)], collapse = ", ")), call. = FALSE)
  }

  # Estimate and variance of the centred intercept.
  if (has_intercept) {
    xbar <- colMeans(X[, slopes, drop = FALSE])
    cvec <- c(1, xbar)[match(terms_x, c("(Intercept)", slopes))]
    int_est <- sum(cvec * beta)
    int_var <- drop(t(cvec) %*% V %*% cvec)
  }
  slope_est <- beta[slopes]
  slope_se <- sqrt(diag(V)[slopes])

  inflate <- switch(method, unit_information = n, eb_shrinkage = n, power = 1 / a0)
  tau <- NULL
  if (method == "eb_shrinkage") {
    if (length(slopes) < 3L) {
      stop("method = \"eb_shrinkage\" needs at least three coefficients to estimate a ",
           "common prior scale; use \"unit_information\".", call. = FALSE)
    }
    sx <- apply(X[, slopes, drop = FALSE], 2L, sd)
    if (any(sx == 0)) stop("A predictor has zero variance.", call. = FALSE)
    b_std <- slope_est * sx
    se_std <- slope_se * sx
    tau <- estimate_tau(b_std, se_std)
    slope_loc <- rep(0, length(slopes))
    slope_scale <- tau / sx
  } else {
    slope_loc <- unname(slope_est)
    slope_scale <- unname(slope_se * sqrt(inflate))
  }

  mk <- function(loc, scale) {
    prior_spec(type, location = loc, scale = scale, df = df)
  }
  intercept <- if (has_intercept) mk(int_est, sqrt(int_var * inflate)) else NULL
  slope <- if (length(slopes) > 0L) mk(slope_loc, slope_scale) else NULL

  cfg <- prior_config(intercept = intercept, slope = slope)
  tab <- data.frame(
    term = c(if (has_intercept) "(Intercept) [centred]", slopes),
    estimate = c(if (has_intercept) int_est, unname(slope_est)),
    std_error = c(if (has_intercept) sqrt(int_var), unname(slope_se)),
    prior_location = c(if (has_intercept) int_est, slope_loc),
    prior_scale = c(if (has_intercept) sqrt(int_var * inflate), slope_scale),
    stringsAsFactors = FALSE
  )
  attr(cfg, "table") <- tab
  attr(cfg, "method") <- method
  attr(cfg, "terms") <- terms_x
  attr(cfg, "n") <- n
  attr(cfg, "tau") <- tau
  attr(cfg, "a0") <- if (method == "power") a0
  attr(cfg, "family") <- family$family
  if (method == "power") {
    message("method = \"power\": `data` is treated as historical data. Do not fit the ",
            "resulting priors to the same data.")
  }
  cfg
}

#' @export
print.bmb_prior_config <- function(x, ...) {
  method <- attr(x, "method")
  if (is.null(method)) {
    cat("<bmb_prior_config>\n")
    for (nm in c("intercept", "slope", "aux")) {
      txt <- if (is.null(x[[nm]])) "rstanarm default" else format(x[[nm]])
      cat(sprintf("  %-9s : %s\n", nm, txt))
    }
    return(invisible(x))
  }
  label <- switch(method,
    unit_information = "unit-information prior (Kass & Wasserman, 1995)",
    eb_shrinkage = "parametric empirical Bayes shrinkage (Morris, 1983)",
    power = sprintf("power prior from historical data, a0 = %s (Ibrahim & Chen, 2000)",
                    format(attr(x, "a0"))))
  cat("<bmb_prior_config> data-informed priors\n")
  cat("  method :", label, "\n")
  cat("  family :", attr(x, "family"), " (link scale); n =", attr(x, "n"), "\n")
  prior_type <- (x$slope %||% x$intercept)$type
  cat("  type   :", prior_type, "\n")
  if (!is.null(attr(x, "tau"))) {
    cat("  tau    :", signif(attr(x, "tau"), 3), "(on the standardised-predictor scale)\n")
  }
  if (!is.null(x$aux)) cat("  aux    :", format(x$aux), "\n")
  tab <- attr(x, "table")
  num <- vapply(tab, is.numeric, logical(1))
  tab[num] <- lapply(tab[num], signif, digits = 3)
  cat("\n")
  print(tab, row.names = FALSE)
  invisible(x)
}

# Maximum-likelihood fit used to derive priors. Returns the design matrix,
# coefficients and covariance matrix.
fit_mle <- function(formula, data, family) {
  if (family$family == "neg_binomial_2") {
    stop("empirical_bayes_priors() does not support neg_binomial_2; ",
         "use a poisson family to derive priors for the coefficients.", call. = FALSE)
  }
  m <- suppressWarnings(glm(formula, data = data, family = family, na.action = na.omit))
  if (!m$converged) {
    warning("The maximum-likelihood fit used to derive priors did not converge ",
            "(separation in a logistic model?). Priors may be unreliable.", call. = FALSE)
  }
  X <- model.matrix(m)
  list(X = X, coef = coef(m), vcov = vcov(m))
}

# Marginal maximum-likelihood estimate of the common prior SD in the
# normal-normal model b_j ~ N(0, tau^2 + se_j^2).
estimate_tau <- function(b, se) {
  negll <- function(log_tau) {
    -sum(dnorm(b, 0, sqrt(exp(2 * log_tau) + se^2), log = TRUE))
  }
  upper <- log(10 * max(abs(b), se) + 1e-8)
  lower <- log(min(se) * 1e-4)
  tau <- exp(optimize(negll, c(lower, upper))$minimum)
  floor_val <- stats::median(se)
  if (tau < floor_val) {
    warning(sprintf(paste0("The empirical Bayes estimate of tau (%.3g) is below the median ",
                           "standard error; flooring it at %.3g to avoid a degenerate prior. ",
                           "The data carry little information about the spread of effects; ",
                           "consider method = \"unit_information\"."), tau, floor_val),
            call. = FALSE)
    tau <- floor_val
  }
  tau
}
