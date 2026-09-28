test_that("prior_spec validates its arguments", {
  expect_s3_class(prior_spec("normal", 0, 1), "bmb_prior")
  expect_error(prior_spec("normal", 0, -1), "strictly positive")
  expect_error(prior_spec("student_t", 0, 1, df = 0), "strictly positive")
  expect_error(prior_spec("normal", c(0, 1), c(1, 2, 3)), "same length")
  expect_error(prior_spec("exponential", rate = 0), "strictly positive")
  expect_error(prior_spec("gamma"), "should be one of")
  expect_output(print(prior_spec("student_t", 0, 2.5, df = 3)), "student_t\\(df = 3")
})

test_that("prior_config accepts bmbeR, rstanarm and legacy formats", {
  legacy <- list(type = "student_t", nu = 5, mu = 1, sigma = 2)
  cfg <- prior_config(intercept = legacy,
                      slope = rstanarm::normal(0, 3),
                      aux = rstanarm::exponential(2))
  expect_equal(cfg$intercept$df, 5)
  expect_equal(cfg$intercept$location, 1)
  expect_equal(cfg$intercept$scale, 2)
  expect_equal(cfg$slope$type, "normal")
  expect_equal(cfg$slope$scale, 3)
  expect_equal(cfg$aux$rate, 2)
  cauchy_legacy <- prior_config(slope = list(type = "cauchy", location = 0, scale = 2.5))
  expect_equal(cauchy_legacy$slope$scale, 2.5)
})

test_that("prior_config rejects priors rstanarm cannot use", {
  expect_error(prior_config(intercept = prior_spec("laplace", 0, 1)), "cannot be used")
  expect_error(prior_config(slope = prior_spec("exponential")), "cannot be used")
  expect_error(prior_config(intercept = prior_spec("normal", c(0, 1), 1)), "single location")
  expect_error(prior_config(slope = list(type = "normal", mu = 0)), "needs a scale")
})

test_that("build_stanarm_priors maps to rstanarm objects and keeps defaults", {
  p <- build_stanarm_priors(prior_spec("normal", 0, 10), prior_spec("student_t", 0, 1, df = 4))
  expect_equal(p$prior_intercept, rstanarm::normal(0, 10))
  expect_equal(p$prior, rstanarm::student_t(4, 0, 1))
  expect_false("prior_aux" %in% names(p))
  # NULL components are omitted so rstanarm defaults apply ...
  expect_length(build_stanarm_priors(), 0)
  # ... but flat priors are passed as explicit NULLs.
  flat <- build_stanarm_priors(slope_config = prior_spec("flat"))
  expect_true("prior" %in% names(flat))
  expect_null(flat$prior)
})

test_that("as_prior_config validates structure", {
  expect_s3_class(bmbeR:::as_prior_config(NULL), "bmb_prior_config")
  expect_error(bmbeR:::as_prior_config(list(slopes = prior_spec())), "named 'intercept'")
  expect_error(bmbeR:::as_prior_config(prior_spec()), "not a single prior_spec")
})

test_that("prior densities and SDs are correct", {
  pri <- data.frame(variable = c("a", "b", "c", "sigma"),
                    role = c("slope", "slope", "slope", "aux"),
                    dist = c("normal", "student_t", "laplace", "exponential"),
                    location = c(1, 0, 0, NA), scale = c(2, 1, 3, 0.5), df = c(NA, 5, NA, NA),
                    stringsAsFactors = FALSE)
  d <- matrix(c(0.3, 0.2, -1, 0.4), 1, 4, dimnames = list(NULL, pri$variable))
  ld <- bmbeR:::prior_log_density(d, pri)
  expect_equal(unname(ld[1, ]), c(dnorm(0.3, 1, 2, log = TRUE), dt(0.2, 5, log = TRUE),
                                  -log(6) - 1 / 3, dexp(0.4, 2, log = TRUE)))
  expect_equal(bmbeR:::prior_sd(pri), c(2, sqrt(5 / 3), 3 * sqrt(2), 0.5))
  # Half-normal and half-t standard deviations match simulation.
  half <- data.frame(variable = c("s1", "s2"), role = "aux", dist = c("normal", "student_t"),
                     location = 0, scale = 2, df = c(NA, 7), stringsAsFactors = FALSE)
  set.seed(3)
  expect_equal(bmbeR:::prior_sd(half),
               c(sd(abs(rnorm(4e5, 0, 2))), sd(abs(2 * rt(4e5, 7)))), tolerance = 0.01)
})
