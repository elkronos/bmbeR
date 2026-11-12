# Regression tests that fit Stan models, one per fix in bmbeR 2.1.0.

test_that("power priors from data with other predictor means give the right intercept", {
  skip_if_no_stan()
  set.seed(3)
  mk <- function(n, mx) {
    x <- rnorm(n, mx, 1)
    data.frame(x = x, y = 2 + 1.5 * x + rnorm(n))
  }
  hist <- mk(400, 0)
  cur <- mk(40, 6)
  cfg <- suppressMessages(empirical_bayes_priors(hist, y ~ x, method = "power", a0 = 1))
  fit <- fit_model_with_prior(cur, y ~ x, prior_config = cfg, chains = 2, iter = 1000,
                              refresh = 0, on_nonconvergence = "ignore")
  pooled <- coef(lm(y ~ x, rbind(hist, cur)))
  expect_equal(unname(coef(fit)), unname(pooled), tolerance = 0.05)
})

test_that("offsets are used when scoring test data", {
  skip_if_no_stan()
  set.seed(2)
  d <- data.frame(x = rnorm(200), t = runif(200, 1, 10))
  d$y <- rpois(200, d$t * exp(0.3 * d$x))
  truth <- d$t[1:50] * exp(0.3 * d$x[1:50])
  f1 <- fit_model_with_prior(d, y ~ x + offset(log(t)), family = poisson(), chains = 2,
                             iter = 600, refresh = 0, on_nonconvergence = "ignore")
  p1 <- evaluate_model_performance(f1, d[1:50, ])
  expect_lt(p1$RMSE, 1.2 * sqrt(mean((d$y[1:50] - truth)^2)))
  f2 <- fit_model_with_prior(d, y ~ x, family = poisson(), offset = log(t), chains = 2,
                             iter = 600, refresh = 0, on_nonconvergence = "ignore")
  expect_error(evaluate_model_performance(f2, d[1:50, ]), "supply `offset`")
  p2 <- evaluate_model_performance(f2, d[1:50, ], offset = log(d$t[1:50]))
  expect_equal(p2$RMSE, p1$RMSE, tolerance = 0.05)
  expect_error(evaluate_model_performance(f1, d[1:50, ], offset = log(d$t[1:50])), "must be NULL")
  expect_error(evaluate_model_performance(f1, d[1:50, ], formula = y ~ x), "differs from the model's formula")
})

test_that("data-masked arguments work and the stored call names the data", {
  skip_if_no_stan()
  data(kidiq, package = "rstanarm", envir = environment())
  kidiq$w <- rep(c(0.5, 2), length.out = nrow(kidiq))
  fit <- fit_model_with_prior(kidiq, kid_score ~ mom_iq, weights = w, chains = 2, iter = 400,
                              refresh = 0, on_nonconvergence = "ignore")
  expect_identical(fit$call$data, quote(kidiq))
  expect_identical(fit$call$weights, quote(w))
  expect_lt(as.numeric(object.size(fit$call)), 20000)
  expect_equal(unname(fit$weights), kidiq$w)
  # Short chains: rstanarm's own sampler warnings are expected here.
  sub <- suppressWarnings(fit_model_with_prior(kidiq, kid_score ~ mom_iq, subset = mom_hs == 1,
                                               chains = 2, iter = 300, refresh = 0,
                                               on_nonconvergence = "ignore"))
  expect_equal(nobs(sub), sum(kidiq$mom_hs == 1))
  expect_s3_class(suppressWarnings(update(fit, iter = 300)), "stanreg")
  k3 <- kidiq[, c("kid_score", "mom_iq", "mom_hs")]
  dot <- suppressWarnings(fit_model_with_prior(k3, kid_score ~ ., chains = 2, iter = 300,
                                               refresh = 0, on_nonconvergence = "ignore"))
  expect_named(coef(dot), c("(Intercept)", "mom_iq", "mom_hs"))
  expect_s3_class(evaluate_model_performance(dot, k3[1:30, ]), "bmb_performance")
  expect_error(fit_model_with_prior(kidiq, kid_score ~ mom_iq, seed = "a"), "seed")
  expect_error(fit_model_with_prior(kidiq, kid_score ~ mom_iq,
                                    prior_config = prior_config(slope = prior_spec("normal", 0, 1)),
                                    prior = rstanarm::normal(0, 2)),
               "set by bmbeR")
})

test_that("R's random number stream is left unchanged", {
  skip_if_no_stan()
  fit <- kidiq_fit()
  data(kidiq, package = "rstanarm", envir = environment())
  unchanged <- function(expr) {
    set.seed(99)
    runif(1)
    suppressWarnings(suppressMessages(force(expr)))
    after <- runif(1)
    set.seed(99)
    identical(after, runif(2)[2])
  }
  expect_true(unchanged(evaluate_model_performance(fit, kidiq[1:50, ])))
  expect_true(unchanged(workflow_report(fit)))
  expect_true(unchanged(prior_predictive_check(kidiq, kid_score ~ mom_iq, refresh = 0)))
  expect_true(unchanged(fit_model_with_prior(kidiq, kid_score ~ mom_iq, chains = 2, iter = 300,
                                             refresh = 0, on_nonconvergence = "ignore")))
})

test_that("prior predictive checks refuse flat priors", {
  skip_if_no_stan()
  data(kidiq, package = "rstanarm", envir = environment())
  expect_error(prior_predictive_check(kidiq, kid_score ~ mom_iq,
                                      prior_config = prior_config(slope = prior_spec("flat"))),
               "proper priors")
  expect_error(prior_predictive_check(kidiq, kid_score ~ mom_iq, chains = 0), "chains")
  expect_error(prior_predictive_check(kidiq, kid_score ~ mom_iq, plausible_range = c(NA, 1)),
               "plausible_range")
})

test_that("sensitivity_analysis validates first, reports Pareto k and keeps going after a failure", {
  skip_if_no_stan()
  data(kidiq, package = "rstanarm", envir = environment())
  expect_error(sensitivity_analysis(kidiq, kid_score ~ mom_iq + mom_hs,
                                    list(ok = NULL, bad = prior_config(slope = prior_spec("normal", 0, c(1, 2, 3))))),
               "configuration 'bad'")
  sens <- suppressMessages(sensitivity_analysis(
    kidiq, kid_score ~ mom_iq,
    list(a = NULL, b = prior_config(slope = prior_spec("normal", 0, 0.1))),
    chains = 2, iter = 600, refresh = 0, on_nonconvergence = "ignore"))
  expect_true("n_high_k" %in% names(sens$comparison))
  expect_s3_class(as.data.frame(sens), "data.frame")
  expect_length(sens$errors, 0)
  # A configuration that fails while fitting is left out with a warning.
  real_fit <- bmbeR:::fit_glm_with_priors
  local_mocked_bindings(fit_glm_with_priors = function(data, formula, family, prior_config, ...) {
    if (identical(prior_config$slope$scale, 0.2)) stop("sampler exploded")
    suppressWarnings(real_fit(data, formula, family, prior_config, ...))
  })
  expect_warning(
    sens2 <- suppressMessages(sensitivity_analysis(
      kidiq, kid_score ~ mom_iq,
      list(a = NULL, b = prior_config(slope = prior_spec("normal", 0, 0.2))),
      chains = 2, iter = 300, refresh = 0, on_nonconvergence = "ignore")),
    "Configuration 'b' failed")
  expect_named(sens2$errors, "b")
  expect_equal(unique(sens2$summary$config), "a")
  expect_output(print(sens2), "Failed and left out: b")
  local_mocked_bindings(fit_glm_with_priors = real_fit)
  kf <- suppressWarnings(suppressMessages(sensitivity_analysis(
    kidiq, kid_score ~ mom_iq, list(a = NULL, b = prior_config(slope = prior_spec("normal", 0, 0.1))),
    cv = "kfold", K = 2, chains = 2, iter = 400, refresh = 0, on_nonconvergence = "ignore")))
  expect_true("elpd_kfold" %in% names(kf$comparison))
  expect_output(print(kf), "K-fold")
  kidiq$w <- 1
  expect_error(sensitivity_analysis(kidiq, kid_score ~ mom_iq, list(a = NULL), cv = "kfold", weights = w),
               "K-fold")
})

test_that("workflow_report statuses reflect the evidence", {
  skip_if_no_stan()
  data(kidiq, package = "rstanarm", envir = environment())
  intercept_only <- suppressWarnings(fit_model_with_prior(kidiq, kid_score ~ 1, chains = 2,
                                                          iter = 600, refresh = 0,
                                                          on_nonconvergence = "ignore"))
  rep0 <- workflow_report(intercept_only)
  expect_match(rep0$items$detail[rep0$items$item == "Priors"], "^rstanarm defaults")

  narrow <- fit_model_with_prior(kidiq, kid_score ~ mom_iq,
                                 prior_config = prior_config(aux = prior_spec("normal", 0, 0.5)),
                                 chains = 2, iter = 600, refresh = 0, on_nonconvergence = "ignore")
  rep1 <- suppressWarnings(workflow_report(narrow, performance = evaluate_model_performance(narrow, kidiq[1:100, ])))
  expect_equal(rep1$items$status[rep1$items$item == "Held-out predictive performance"], "check")
  expect_match(rep1$items$detail[rep1$items$item == "Priors"], "user-specified for aux")

  qr <- suppressWarnings(fit_model_with_prior(kidiq, kid_score ~ mom_iq + mom_hs, QR = TRUE,
                                              chains = 2, iter = 400, refresh = 0,
                                              on_nonconvergence = "ignore"))
  rep2 <- suppressWarnings(workflow_report(qr))
  expect_match(rep2$items$detail[rep2$items$item == "Prior sensitivity"], "sensitivity_analysis")

  md <- format(workflow_report(kidiq_fit()), type = "markdown")
  expect_true(any(grepl("^\\| 1 \\| Model \\| ok \\|", md)))
  expect_true(any(grepl("### Posterior summary", md)))
})

test_that("binary outcomes get a calibration check in the report", {
  skip_if_no_stan()
  w <- wells_data()[1:600, ]
  fit <- fit_model_with_prior(w, sw ~ dist100, family = binomial(), chains = 2, iter = 600,
                              refresh = 0, on_nonconvergence = "ignore")
  pc <- suppressWarnings(prior_predictive_check(w, sw ~ dist100, family = binomial(), refresh = 0))
  rep <- workflow_report(fit, prior_check = pc)
  expect_true("calibration" %in% rep$ppc$statistic)
  expect_match(rep$items$detail[rep$items$item == "Prior predictive check"], "success proportions")
  expect_s3_class(as.data.frame(pc), "data.frame")
})

test_that("prior_table returns the priors used after autoscaling", {
  skip_if_no_stan()
  fit <- kidiq_fit()
  pt <- prior_table(fit)
  ps <- rstanarm::prior_summary(fit)
  expect_equal(pt$variable, c("(Intercept)", "mom_iq", "mom_hs", "sigma"))
  expect_equal(pt$scale[pt$variable == "mom_iq"], ps$prior$adjusted_scale[1])
  expect_equal(pt$prior_sd[pt$variable == "sigma"], pt$scale[pt$variable == "sigma"])
  expect_type(pt$prior, "character")
})
