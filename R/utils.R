# Internal helpers shared across the package. None of these are exported.

`%||%` <- function(x, y) if (is.null(x)) y else x

# -----------------------------------------------------------------------------
# Input validation
# -----------------------------------------------------------------------------

validate_positive_integer <- function(n, param_name) {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || !is.finite(n) ||
      n <= 0 || n != round(n)) {
    stop(sprintf("`%s` must be a single positive integer.", param_name),
         call. = FALSE)
  }
  invisible(TRUE)
}

validate_numeric <- function(x, param_name, positive = FALSE,
                             allow_vector = FALSE) {
  ok <- is.numeric(x) && length(x) >= 1L && all(is.finite(x)) &&
    (allow_vector || length(x) == 1L)
  if (!ok) {
    what <- if (allow_vector) "a finite numeric vector" else "a single finite number"
    stop(sprintf("`%s` must be %s.", param_name, what), call. = FALSE)
  }
  if (positive && any(x <= 0)) {
    stop(sprintf("`%s` must be strictly positive.", param_name), call. = FALSE)
  }
  invisible(TRUE)
}

validate_probability <- function(x, param_name, open = FALSE) {
  validate_numeric(x, param_name)
  bad <- if (open) x <= 0 || x >= 1 else x < 0 || x > 1
  if (bad) {
    rng <- if (open) "(0, 1)" else "[0, 1]"
    stop(sprintf("`%s` must lie in %s.", param_name, rng), call. = FALSE)
  }
  invisible(TRUE)
}

validate_data_formula <- function(data, formula, data_arg = "data") {
  if (!is.data.frame(data)) {
    stop(sprintf("`%s` must be a data frame.", data_arg), call. = FALSE)
  }
  if (!inherits(formula, "formula")) {
    stop("`formula` must be a formula, e.g. `y ~ x1 + x2`.", call. = FALSE)
  }
  missing_cols <- setdiff(formula_vars(formula, data), colnames(data))
  # Variables that are not columns may come from the formula's environment
  # (e.g. `k` in `poly(x, k)`), as model.frame() allows.
  env <- environment(formula) %||% globalenv()
  in_env <- vapply(missing_cols, function(v) {
    exists(v, envir = env) && !is.function(get(v, envir = env))
  }, logical(1))
  missing_cols <- missing_cols[!in_env]
  if (length(missing_cols) > 0L) {
    stop(sprintf("`%s` is missing variable(s) used in the formula: %s",
                 data_arg, paste(missing_cols, collapse = ", ")),
         call. = FALSE)
  }
  invisible(TRUE)
}

# Variables used by a formula, with `.` expanded against the data.
formula_vars <- function(formula, data) {
  vars <- all.vars(formula)
  if ("." %in% vars) {
    vars <- all.vars(stats::terms(formula, data = data))
  }
  vars
}

validate_seed <- function(seed) {
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) || seed < 0 ||
      seed != round(seed) || seed > .Machine$integer.max) {
    stop("`seed` must be a single non-negative whole number.", call. = FALSE)
  }
  invisible(TRUE)
}

# -----------------------------------------------------------------------------
# Arguments forwarded to rstanarm::stan_glm()
# -----------------------------------------------------------------------------

# stan_glm() evaluates these arguments inside the data, as model.frame() does,
# so they are kept as unevaluated expressions (e.g. `weights = w`).
nse_args <- c("weights", "subset", "offset", "na.action")

# Capture the arguments in `...` for stan_glm(): data-masked arguments as
# expressions, everything else as values.
capture_dots <- function(...) {
  exprs <- as.list(substitute(list(...)))[-1L]
  if (length(exprs) == 0L) return(list())
  nms <- names(exprs) %||% rep("", length(exprs))
  if (any(!nzchar(nms))) {
    stop("Arguments passed through `...` to rstanarm::stan_glm() must be named.",
         call. = FALSE)
  }
  out <- vector("list", length(exprs))
  names(out) <- nms
  for (i in seq_along(exprs)) {
    out[i] <- list(if (nms[i] %in% nse_args) exprs[[i]] else ...elt(i))
  }
  out
}

# A language object for a family, so that the stored model call stays small
# and can be re-evaluated by update(), loo() and kfold().
family_call <- function(family) {
  fun <- if (family$family == "neg_binomial_2") {
    quote(rstanarm::neg_binomial_2)
  } else {
    call("::", as.name("stats"), as.name(family$family))
  }
  as.call(list(fun, link = family$link))
}

# The expression used for `data` in the stored model call: the name the user
# gave when it refers to the same object, otherwise the data itself.
data_call_arg <- function(data, data_expr, env) {
  if (is.name(data_expr) && exists(as.character(data_expr), envir = env) &&
      identical(get(as.character(data_expr), envir = env), data)) {
    return(data_expr)
  }
  data
}

# Run code that uses R's random number generator without changing the
# caller's random number stream.
preserve_seed <- function(expr) withr::with_preserve_seed(expr)

# -----------------------------------------------------------------------------
# Families and responses
# -----------------------------------------------------------------------------

# Accept a family object, a family function, or a family name, as glm() does.
# `env` is where a family name is looked up: the environment of the user's
# call.
normalize_family <- function(family, env = parent.frame(2)) {
  if (is.character(family)) {
    if (length(family) != 1L || is.na(family)) {
      stop("`family` must be a single family name, e.g. \"binomial\".", call. = FALSE)
    }
    fam_fun <- if (family == "neg_binomial_2") {
      rstanarm::neg_binomial_2
    } else {
      fun <- get0(family, envir = env, mode = "function")
      if (is.null(fun)) {
        stop(sprintf("Unknown family '%s'.", family), call. = FALSE)
      }
      fun
    }
    family <- fam_fun
  }
  if (is.function(family)) family <- family()
  if (!inherits(family, "family")) {
    stop("`family` must be a family object such as `gaussian()` or `binomial()`.",
         call. = FALSE)
  }
  supported <- c("gaussian", "binomial", "poisson", "Gamma",
                 "inverse.gaussian", "neg_binomial_2")
  if (!family$family %in% supported) {
    stop(sprintf("Family '%s' is not supported by rstanarm::stan_glm(). Supported: %s.",
                 family$family, paste(supported, collapse = ", ")),
         call. = FALSE)
  }
  family
}

# The response exactly as the model sees it (handles transformations such as
# log(y) and two-column binomial responses such as cbind(successes, failures)).
get_response <- function(formula, data) {
  mf <- model.frame(formula, data = data, na.action = na.omit)
  model.response(mf)
}

check_response_family <- function(y, family) {
  fam <- family$family
  if (is.numeric(y) && any(!is.finite(y))) {
    stop("The response contains infinite values.", call. = FALSE)
  }
  if (fam == "gaussian") {
    if (!is.numeric(y) || is.matrix(y)) {
      stop("A gaussian model needs a numeric response.", call. = FALSE)
    }
  } else if (fam == "binomial") {
    if (is.matrix(y)) {
      if (ncol(y) != 2L || any(y < 0) || any(y != round(y))) {
        stop("A two-column binomial response must be cbind(successes, failures) ",
             "with non-negative integer counts.", call. = FALSE)
      }
    } else if (is.factor(y) || is.logical(y)) {
      if (length(unique(stats::na.omit(y))) > 2L) {
        stop("A binomial response given as a factor must have at most two levels.",
             call. = FALSE)
      }
    } else if (is.numeric(y)) {
      if (any(y < 0 | y > 1)) {
        stop("A numeric binomial response must lie in [0, 1] (0/1 outcomes or ",
             "proportions with `weights`); use cbind(successes, failures) for counts.",
             call. = FALSE)
      }
    } else {
      stop("A binomial response must be 0/1, logical, a two-level factor, or ",
           "cbind(successes, failures).", call. = FALSE)
    }
  } else if (fam %in% c("poisson", "neg_binomial_2")) {
    if (!is.numeric(y) || any(y < 0) || any(y != round(y))) {
      stop(sprintf("A %s model needs a non-negative integer (count) response.", fam),
           call. = FALSE)
    }
  } else if (fam %in% c("Gamma", "inverse.gaussian")) {
    if (!is.numeric(y) || any(y <= 0)) {
      stop(sprintf("A %s model needs a strictly positive response.", fam),
           call. = FALSE)
    }
  }
  invisible(TRUE)
}

# Convert a binary response (0/1, logical, two-level factor) to 0/1 using the
# same convention as glm(): the first factor level is failure.
binary_to_01 <- function(y) {
  if (is.factor(y)) return(as.integer(y != levels(y)[1L]))
  if (is.logical(y)) return(as.integer(y))
  as.numeric(y)
}

is_binary_outcome <- function(y) {
  if (is.matrix(y)) return(FALSE)
  if (is.factor(y) || is.logical(y)) return(TRUE)
  is.numeric(y) && all(y %in% c(0, 1))
}

# -----------------------------------------------------------------------------
# Model objects
# -----------------------------------------------------------------------------

check_stanreg <- function(fit, arg = "fit", glm_only = FALSE) {
  if (!inherits(fit, "stanreg")) {
    stop(sprintf("`%s` must be a model fitted with rstanarm (a 'stanreg' object).", arg),
         call. = FALSE)
  }
  if (!identical(fit$algorithm, "sampling")) {
    stop(sprintf("`%s` was fitted with algorithm = '%s'; this function needs MCMC draws (algorithm = 'sampling').",
                 arg, fit$algorithm), call. = FALSE)
  }
  if (glm_only && !identical(fit$stan_function, "stan_glm")) {
    stop(sprintf("`%s` must come from rstanarm::stan_glm() (got %s()).",
                 arg, fit$stan_function), call. = FALSE)
  }
  invisible(TRUE)
}

# Extract the stanfit and an iterations x chains x parameters draws array
# from either a stanreg or a stanfit.
extract_stanfit <- function(model_fit) {
  if (inherits(model_fit, "stanreg")) {
    if (!identical(model_fit$algorithm, "sampling")) {
      stop("MCMC diagnostics need a model fitted with algorithm = 'sampling'.",
           call. = FALSE)
    }
    return(model_fit$stanfit)
  }
  if (inherits(model_fit, "stanfit")) return(model_fit)
  stop("`model_fit` must be a 'stanreg' (rstanarm) or 'stanfit' (rstan) object.",
       call. = FALSE)
}

draws_array <- function(model_fit, pars = NULL) {
  if (inherits(model_fit, "stanreg")) {
    arr <- as.array(model_fit)
  } else {
    arr <- as.array(extract_stanfit(model_fit))
  }
  if (!is.null(pars)) {
    unknown <- setdiff(pars, dimnames(arr)[[3]])
    if (length(unknown) > 0L) {
      stop(sprintf("Unknown parameter(s): %s. Available: %s",
                   paste(unknown, collapse = ", "),
                   paste(dimnames(arr)[[3]], collapse = ", ")), call. = FALSE)
    }
    arr <- arr[, , pars, drop = FALSE]
  }
  arr
}

# -----------------------------------------------------------------------------
# Numerics
# -----------------------------------------------------------------------------

# Column-wise log(mean(exp(x))) computed stably; x is draws x observations.
log_mean_exp_cols <- function(x) {
  x <- as.matrix(x)
  m <- apply(x, 2L, max)
  m + log(colMeans(exp(sweep(x, 2L, m))))
}

# Sample-size dependent Pareto-k threshold (Vehtari et al., 2024).
pareto_k_threshold <- function(S) min(1 - 1 / log10(S), 0.7)

fmt_num <- function(x, digits = 3) {
  formatC(x, digits = digits, format = "fg", flag = "#")
}
