test_that("student_t_prior is the location-scale t used by rstanarm", {
  set.seed(1)
  x <- student_t_prior(2e5, nu = 30, mu = 5, sigma = 2)
  expect_equal(mean(x), 5, tolerance = 0.02)
  expect_equal(sd(x), 2 * sqrt(30 / 28), tolerance = 0.02)
})

test_that("generators reject invalid parameters", {
  expect_error(normal_prior(10, sigma = -1), "strictly positive")
  expect_error(lognormal_prior(10, sdlog = 0), "strictly positive")
  expect_error(cauchy_prior(10, scale = -2), "strictly positive")
  expect_error(student_t_prior(10, nu = 0), "strictly positive")
  expect_error(normal_prior(NA_real_), "positive integer")
  expect_error(normal_prior(2.5), "positive integer")
  expect_error(normal_prior(10, mu = Inf), "finite")
  expect_error(uniform_prior(10, min = 1, max = 1), "less than")
  expect_error(binomial_prior(10, prob = 1.5), "\\[0, 1\\]")
})

test_that("generators return the requested number of draws", {
  for (f in original_distributions) expect_length(f(7), 7)
})

test_that("a custom distribution can be registered and used", {
  my <- add_distribution(original_distributions, "exponential",
                         function(sample_size, rate = 1) rexp(sample_size, rate))
  set.seed(2)
  x <- get_prior_distribution("exponential", list(sample_size = 5, rate = 2),
                              distributions = my)
  expect_length(x, 5)
  expect_error(get_prior_distribution("exponential", list(sample_size = 5)),
               "not in the registry")
  expect_error(add_distribution(my, "normal", rnorm), "already exists")
  expect_error(add_distribution(my, "x", 1), "must be a function")
  expect_identical(reset_distributions(), original_distributions)
  expect_identical(distributions, original_distributions)
})
