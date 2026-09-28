sim_data <- function(n = 200, seed = 1) {
  set.seed(seed)
  d <- data.frame(x1 = rnorm(n), x2 = rnorm(n, 5, 2), x3 = rbinom(n, 1, 0.4),
                  x4 = runif(n))
  d$y <- 3 + 0.5 * d$x1 - 0.2 * d$x2 + 1 * d$x3 + rnorm(n)
  d$b <- rbinom(n, 1, plogis(-0.5 + 0.8 * d$x1))
  d
}

test_that("unit-information priors have variance n times the sampling variance", {
  d <- sim_data()
  cfg <- empirical_bayes_priors(d, y ~ x1 + x2)
  m <- lm(y ~ x1 + x2, d)
  se <- sqrt(diag(vcov(m)))
  expect_s3_class(cfg, "bmb_prior_config")
  expect_equal(cfg$slope$location, unname(coef(m)[-1]))
  expect_equal(cfg$slope$scale, unname(se[-1] * sqrt(nrow(d))))
  # The intercept prior is for the centred intercept, which equals mean(y) in OLS.
  expect_equal(cfg$intercept$location, mean(d$y))
  expect_equal(cfg$intercept$scale, sd(residuals(m)) * sqrt((nrow(d) - 1) / (nrow(d) - 3)),
               tolerance = 1e-8)
  expect_identical(attr(cfg, "terms"), c("(Intercept)", "x1", "x2"))
})

test_that("logistic models use the logit-scale GLM, not a linear probability model", {
  d <- sim_data(500)
  cfg <- empirical_bayes_priors(d, b ~ x1, family = binomial())
  expect_equal(cfg$slope$location, unname(coef(glm(b ~ x1, binomial, d))[2]))
  cfg2 <- empirical_bayes_priors(d, b ~ x1, family = "binomial")
  expect_equal(cfg$slope, cfg2$slope)
})

test_that("power priors scale the variance by 1 / a0", {
  d <- sim_data()
  expect_message(p1 <- empirical_bayes_priors(d, y ~ x1, method = "power", a0 = 1), "historical")
  expect_message(p4 <- empirical_bayes_priors(d, y ~ x1, method = "power", a0 = 0.25), "historical")
  expect_equal(p4$slope$scale, 2 * p1$slope$scale)
  expect_error(empirical_bayes_priors(d, y ~ x1, method = "power", a0 = 1.5), "\\(0, 1\\]")
})

test_that("empirical Bayes shrinkage recovers the spread of effects", {
  set.seed(10)
  n <- 3000; k <- 60; tau <- 0.3
  X <- matrix(rnorm(n * k), n, k, dimnames = list(NULL, paste0("x", 1:k)))
  beta <- rnorm(k, 0, tau)
  d <- data.frame(X, y = drop(X %*% beta) + rnorm(n, 0, 3))
  f <- reformulate(colnames(X), "y")
  cfg <- empirical_bayes_priors(d, f, method = "eb_shrinkage")
  expect_equal(attr(cfg, "tau"), tau, tolerance = 0.25)
  expect_true(all(cfg$slope$location == 0))
  expect_error(empirical_bayes_priors(d, y ~ x1 + x2, method = "eb_shrinkage"), "at least three")
})

test_that("EB shrinkage floors a boundary estimate of tau with a warning", {
  d <- sim_data(100)
  d$z1 <- rnorm(100); d$z2 <- rnorm(100); d$z3 <- rnorm(100)
  expect_warning(cfg <- empirical_bayes_priors(d, y ~ z1 + z2 + z3, method = "eb_shrinkage"),
                 "flooring")
  expect_true(all(cfg$slope$scale > 0))
})

test_that("missing values only matter in model variables", {
  d <- sim_data()
  d$unused <- NA
  d$x1[1:3] <- NA
  expect_message(cfg <- empirical_bayes_priors(d, y ~ x1), "Dropped 3 row")
  expect_equal(attr(cfg, "n"), nrow(d) - 3)
})

test_that("heavy-tailed output types and deprecated dist_types", {
  d <- sim_data()
  cfg <- empirical_bayes_priors(d, y ~ x1, type = "student_t", df = 4)
  expect_equal(cfg$slope$type, "student_t")
  expect_equal(cfg$slope$df, 4)
  expect_warning(empirical_bayes_priors(d, y ~ x1, dist_types = list(x1 = "normal")), "deprecated")
  expect_error(empirical_bayes_priors(d, y ~ x1, dist_types = list(x1 = "beta")), "deprecated")
})

test_that("invalid inputs give informative errors", {
  d <- sim_data()
  expect_error(empirical_bayes_priors(d, y ~ nope), "missing variable")
  expect_error(empirical_bayes_priors(as.list(d), y ~ x1), "data frame")
  d$x5 <- d$x1
  expect_error(empirical_bayes_priors(d, y ~ x1 + x5), "not estimable")
  expect_output(print(empirical_bayes_priors(d, y ~ x1)), "unit-information")
})
