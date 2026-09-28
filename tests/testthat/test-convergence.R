make_chains <- function(n_iter = 500, n_chains = 4, shift = 0, seed = 1) {
  set.seed(seed)
  arr <- array(rnorm(n_iter * n_chains * 2), c(n_iter, n_chains, 2),
               dimnames = list(NULL, NULL, c("a", "b")))
  arr[, 1, "b"] <- arr[, 1, "b"] + shift  # one chain stuck elsewhere
  arr
}
ok_sampler <- list(divergent = 0L, treedepth_hits = 0L, max_treedepth = 10L,
                   bfmi = c(1, 1, 1, 1), n_warmup = 500, n_iter = 1000)

test_that("well-mixed chains pass", {
  res <- bmbeR:::convergence_from_draws(make_chains(), ok_sampler)
  expect_s3_class(res, "bmb_convergence")
  expect_true(res$converged)
  expect_equal(res$thresholds$min_ess, 400)
  expect_output(print(res), "PASSED")
})

test_that("a stuck chain fails the R-hat check", {
  res <- bmbeR:::convergence_from_draws(make_chains(shift = 3), ok_sampler)
  expect_false(res$converged)
  expect_false(res$parameters$rhat_ok[res$parameters$variable == "b"])
  expect_match(res$issues, "R-hat", all = FALSE)
})

test_that("low ESS, divergences and low E-BFMI are reported", {
  set.seed(4)
  ar <- array(0, c(500, 4, 1), dimnames = list(NULL, NULL, "a"))
  for (ch in 1:4) ar[, ch, 1] <- stats::filter(rnorm(500), 0.99, method = "recursive")
  res <- bmbeR:::convergence_from_draws(ar, ok_sampler)
  expect_match(res$issues, "ESS", all = FALSE)

  bad <- ok_sampler
  bad$divergent <- 3L
  bad$bfmi <- c(0.1, 1, 1, 1)
  bad$treedepth_hits <- 5L
  res2 <- bmbeR:::convergence_from_draws(make_chains(), bad)
  expect_false(res2$converged)
  expect_match(res2$issues, "divergent", all = FALSE)
  expect_match(res2$issues, "E-BFMI", all = FALSE)
  expect_match(res2$notes, "tree depth")
})

test_that("a single chain is flagged", {
  res <- bmbeR:::convergence_from_draws(make_chains(n_chains = 1), NULL)
  expect_false(res$converged)
  expect_match(res$issues, "Only one chain", all = FALSE)
})

test_that("plot methods return ggplots", {
  res <- bmbeR:::convergence_from_draws(make_chains(), ok_sampler)
  expect_s3_class(plot(res), "ggplot")
  expect_s3_class(plot(res, type = "trace"), "ggplot")
})
