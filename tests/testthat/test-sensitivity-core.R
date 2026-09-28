# Conjugate normal model: prior N(m0, s0^2), likelihood N(ybar | theta, s^2).
conjugate <- function(m0, s0, ybar, s, n = 4e5, seed = 1) {
  set.seed(seed)
  P <- 1 / s0^2 + 1 / s^2
  mu <- (m0 / s0^2 + ybar / s^2) / P
  sig <- sqrt(1 / P)
  th <- rnorm(n, mu, sig)
  list(theta = cbind(theta = th), lp = dnorm(th, m0, s0, log = TRUE),
       ll = dnorm(ybar, th, s, log = TRUE), mu = mu, sig = sig)
}

test_that("local sensitivities match the conjugate-normal closed form", {
  cj <- conjugate(m0 = 0, s0 = 1, ybar = 2, s = 0.8)
  r <- bmbeR:::power_scaling_core(cj$theta, cj$lp, cj$ll)
  s <- r$summary
  expect_equal(s$prior_mean_sens, (0 - cj$mu) * cj$sig / 1, tolerance = 0.02)
  expect_equal(s$prior_sd_sens, -0.5 * cj$sig^2 / 1, tolerance = 0.02)
  expect_equal(s$lik_mean_sens, (2 - cj$mu) * cj$sig / 0.8^2, tolerance = 0.02)
  expect_equal(s$lik_sd_sens, -0.5 * cj$sig^2 / 0.8^2, tolerance = 0.02)
  # Prior and likelihood SD sensitivities sum to -1/2 in this model.
  expect_equal(s$prior_sd_sens + s$lik_sd_sens, -0.5, tolerance = 0.02)
  # Mean sensitivities mirror each other for a symmetric posterior.
  expect_equal(s$lik_mean_sens, -s$prior_mean_sens, tolerance = 0.02)
  # conflict_z recovers the prior predictive z-score (Box, 1980).
  expect_equal(s$conflict_z, (0 - 2) / sqrt(1 + 0.8^2), tolerance = 0.02)
})

test_that("importance-sampled perturbations match exact power-scaled posteriors", {
  cj <- conjugate(m0 = 0, s0 = 1, ybar = 2, s = 0.8)
  r <- bmbeR:::power_scaling_core(cj$theta, cj$lp, cj$ll, alpha = c(0.8, 1.25))
  for (a in c(0.8, 1.25)) {
    Pa <- a / 1 + 1 / 0.8^2
    row <- r$perturbation[r$perturbation$component == "prior" & r$perturbation$alpha == a, ]
    expect_equal(row$mean, (2 / 0.8^2) / Pa, tolerance = 0.005)
    expect_equal(row$sd, sqrt(1 / Pa), tolerance = 0.005)
    expect_true(row$reliable)
  }
})

test_that("diagnoses distinguish conflict, weak likelihood and informative priors", {
  diag <- function(m0, s0, ybar, s) {
    cj <- conjugate(m0, s0, ybar, s, n = 1e5)
    bmbeR:::power_scaling_core(cj$theta, cj$lp, cj$ll)$summary$diagnosis
  }
  expect_equal(diag(m0 = 0, s0 = 100, ybar = 1, s = 0.1), "-")                    # vague prior
  expect_equal(diag(m0 = 5, s0 = 0.3, ybar = 0, s = 0.3), "prior-data conflict")  # disagreement
  expect_equal(diag(m0 = 0, s0 = 0.3, ybar = 0, s = 0.3), "informative prior")    # agreement
  expect_equal(diag(m0 = 0.2, s0 = 0.3, ybar = 0, s = 0.3), "informative prior")  # mild difference
  expect_equal(diag(m0 = 0, s0 = 1, ybar = 0.5, s = 50), "weak likelihood")       # no data
  expect_equal(diag(m0 = 0.6, s0 = 0.05, ybar = 0.4, s = 0.2), "weak likelihood") # data ~6% of precision
})

test_that("a flat prior has zero prior sensitivity", {
  set.seed(1)
  th <- rnorm(1000)
  r <- bmbeR:::power_scaling_core(cbind(a = th), rep(0, 1000), dnorm(0, th, 1, log = TRUE))
  expect_equal(r$summary$prior_mean_sens, 0)
  expect_true(all(r$perturbation$shift_sd[r$perturbation$component == "prior"] == 0))
})
