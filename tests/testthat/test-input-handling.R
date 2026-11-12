# Regression tests for input handling that do not need Stan.

test_that("formulas with `.` and variables from the formula environment are accepted", {
  set.seed(1)
  d <- data.frame(y = rnorm(40), x1 = rnorm(40), x2 = rnorm(40))
  cfg <- empirical_bayes_priors(d, y ~ .)
  expect_identical(attr(cfg, "terms"), c("(Intercept)", "x1", "x2"))
  k <- 2
  expect_s3_class(empirical_bayes_priors(d, y ~ poly(x1, k)), "bmb_prior_config")
  expect_error(empirical_bayes_priors(d, y ~ nope), "missing variable\\(s\\) used in the formula: nope")
  # A function with the same name as a missing column is not a variable.
  expect_error(empirical_bayes_priors(d, y ~ t), "missing variable")
})

test_that("family names are looked up where the user called the function", {
  set.seed(1)
  d <- data.frame(y = rbinom(60, 1, 0.4), x = rnorm(60))
  f <- function() {
    mybin <- binomial
    empirical_bayes_priors(d, y ~ x, family = "mybin")
  }
  expect_equal(attr(f(), "family"), "binomial")
  expect_error(empirical_bayes_priors(d, y ~ x, family = c("gaussian", "binomial")), "single family name")
  expect_error(empirical_bayes_priors(d, y ~ x, family = "nope"), "Unknown family")
})

test_that("infinite responses are rejected", {
  expect_error(bmbeR:::check_response_family(c(1, Inf), gaussian()), "infinite")
})

test_that("rstanarm priors with default arguments are translated", {
  expect_equal(prior_config(slope = rstanarm::normal())$slope$scale, 2.5)
  st <- prior_config(slope = rstanarm::student_t(df = 5))$slope
  expect_equal(c(st$df, st$location, st$scale), c(5, 0, 2.5))
  expect_equal(prior_config(aux = rstanarm::exponential())$aux$rate, 1)
})

test_that("Student-t df must match the length of location and scale", {
  expect_error(prior_spec("student_t", location = c(0, 1), scale = 1, df = c(3, 4, 5)), "`df` must have length")
  expect_equal(prior_spec("student_t", location = c(0, 1), scale = 1, df = c(3, 4))$df, c(3, 4))
  d <- data.frame(y = rnorm(20), x1 = rnorm(20), x2 = rnorm(20))
  cfg <- prior_config(slope = prior_spec("student_t", 0, 1, df = c(3, 4, 5)))
  expect_error(bmbeR:::check_prior_terms(cfg, y ~ x1 + x2, d), "3 values")
})

test_that("legacy exponential priors given by scale keep the scale", {
  expect_equal(prior_config(aux = list(type = "exponential", scale = 4))$aux$rate, 0.25)
  expect_equal(prior_config(aux = list(type = "exponential", rate = 3))$aux$rate, 3)
})

test_that("add_distribution rejects a missing name", {
  expect_error(add_distribution(original_distributions, NA_character_, rnorm), "non-empty string")
})

test_that("a single configuration is not mistaken for a list of configurations", {
  expect_error(bmbeR:::normalize_configurations(prior_config()), "named list of configurations")
})

test_that("compare_performance requires the same test observations", {
  perf <- function(y, rows, elpd) {
    structure(list(ELPD = sum(elpd)), class = "bmb_performance", elpd_pointwise = elpd,
              test_signature = list(y = y, rows = rows))
  }
  a <- perf(c(1, 2, 3), c("1", "2", "3"), c(-1, -1, -1))
  b <- perf(c(1, 2, 3), c("1", "2", "3"), c(-2, -1, -1))
  c2 <- perf(c(4, 5, 6), c("4", "5", "6"), c(-2, -1, -1))
  expect_equal(nrow(compare_performance(a = a, b = b)), 2)
  expect_error(compare_performance(a = a, c = c2), "same test observations")
})

test_that("seeds are validated", {
  expect_error(bmbeR:::validate_seed("a"), "non-negative whole number")
  expect_error(bmbeR:::validate_seed(-1), "non-negative whole number")
  expect_error(bmbeR:::validate_seed(1.5), "non-negative whole number")
  expect_true(bmbeR:::validate_seed(0))
})

test_that("arguments for stan_glm keep data-masked expressions unevaluated", {
  f <- function(...) bmbeR:::capture_dots(...)
  out <- f(weights = w, subset = x > 1, refresh = 0)
  expect_identical(out$weights, quote(w))
  expect_identical(out$subset, quote(x > 1))
  expect_identical(out$refresh, 0)
  n_cores <- 3
  expect_identical(f(cores = n_cores)$cores, 3)
  expect_error(f(1), "must be named")
})

test_that("the binned calibration statistic is zero for perfectly calibrated counts", {
  p <- rep(c(0.2, 0.8), each = 50)
  calib <- bmbeR:::binned_calibration(p, n_bins = 2)
  y <- c(rep(c(1, 0, 0, 0, 0), 10), rep(c(1, 1, 1, 1, 0), 10))
  expect_equal(calib(y), 0)
  expect_gt(calib(rev(y)), 10)
})
