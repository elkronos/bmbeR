test_that("CRPS estimator matches the closed form for a normal forecast", {
  set.seed(1)
  x <- rnorm(4e4)
  crps_normal <- function(y) y * (2 * pnorm(y) - 1) + 2 * dnorm(y) - 1 / sqrt(pi)
  est <- bmbeR:::crps_draws(cbind(x, x), c(0.7, -1.5))
  expect_equal(est, crps_normal(c(0.7, -1.5)), tolerance = 0.02)
  # A point forecast reduces to absolute error.
  expect_equal(bmbeR:::crps_draws(matrix(3, 10, 1), 1), 2)
})

test_that("AUC is computed correctly", {
  expect_equal(bmbeR:::auc_rank(c(0, 0, 1, 1), c(0.1, 0.2, 0.8, 0.9)), 1)
  expect_equal(bmbeR:::auc_rank(c(0, 0, 1, 1), c(0.9, 0.8, 0.2, 0.1)), 0)
  expect_equal(bmbeR:::auc_rank(c(0, 1, 0, 1), c(0.5, 0.5, 0.5, 0.5)), 0.5)
  expect_true(is.na(bmbeR:::auc_rank(c(1, 1), c(0.2, 0.3))))
})

test_that("classification metrics handle edge cases", {
  m <- bmbeR:::classification_metrics(c(1, 0, 1, 0), c(0.9, 0.2, 0.4, 0.6), c(1, 0, 0, 1))
  expect_equal(m$Accuracy, 0.5)
  expect_equal(m$Precision, 0.5)
  expect_equal(m$Recall, 0.5)
  expect_equal(m$Brier, mean(c(0.1, 0.2, 0.6, 0.6)^2))
  none <- bmbeR:::classification_metrics(c(0, 0), c(0.1, 0.2), c(0, 0))
  expect_true(is.na(none$Precision))
})

test_that("log_mean_exp is numerically stable", {
  x <- matrix(c(-1000, -1001, -1002, 0, 0, 0), 3)
  expect_equal(bmbeR:::log_mean_exp_cols(x),
               c(-1000 + log(mean(exp(c(0, -1, -2)))), 0))
})
