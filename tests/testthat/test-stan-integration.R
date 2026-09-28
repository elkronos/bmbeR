test_that("fit_model_with_prior uses autoscaled defaults and records diagnostics", {
  skip_if_no_stan()
  fit <- kidiq_fit()
  expect_s3_class(fit, "stanreg")
  conv <- attr(fit, "bmb_convergence")
  expect_s3_class(conv, "bmb_convergence")
  expect_true(conv$converged)
  expect_false(is.null(rstanarm::prior_summary(fit)$prior$adjusted_scale))
})

test_that("results do not depend on the units of the outcome (regression test)", {
  skip_if_no_stan()
  d <- mtcars
  d$mpg100 <- d$mpg * 100
  f1 <- fit_model_with_prior(d, mpg ~ wt, chains = 2, iter = 1000, refresh = 0,
                             on_nonconvergence = "ignore")
  f2 <- fit_model_with_prior(d, mpg100 ~ wt, chains = 2, iter = 1000, refresh = 0,
                             on_nonconvergence = "ignore")
  expect_equal(unname(coef(f2)["wt"] / coef(f1)["wt"]), 100, tolerance = 0.05)
  # The same holds for unit-information priors.
  f3 <- fit_model_with_prior(d, mpg100 ~ wt, prior_config = empirical_bayes_priors(d, mpg100 ~ wt),
                             chains = 2, iter = 1000, refresh = 0, on_nonconvergence = "ignore")
  expect_equal(unname(coef(f3)["wt"]), unname(coef(lm(mpg100 ~ wt, d))["wt"]), tolerance = 0.05)
})

test_that("logistic models accept integer, logical and factor outcomes and family names", {
  skip_if_no_stan()
  w <- wells_data()[1:400, ]
  # Short chains: rstanarm's own sampler warnings are expected here.
  for (fam in list(binomial(), binomial, "binomial")) {
    fit <- suppressWarnings(fit_model_with_prior(w, sw ~ dist100, family = fam, chains = 2,
                                                 iter = 400, refresh = 0,
                                                 on_nonconvergence = "ignore"))
    expect_s3_class(fit, "stanreg")
  }
  w$sw_lgl <- w$sw == 1
  expect_s3_class(suppressWarnings(fit_model_with_prior(w, sw_lgl ~ dist100, family = binomial(),
                                                        chains = 2, iter = 400, refresh = 0,
                                                        on_nonconvergence = "ignore")),
                  "stanreg")
  expect_error(fit_model_with_prior(w, dist ~ sw, family = binomial()), "\\[0, 1\\]")
  expect_error(fit_model_with_prior(w, dist ~ sw, family = poisson()), "count")
})

test_that("legacy prior configurations still work", {
  skip_if_no_stan()
  data(kidiq, package = "rstanarm", envir = environment())
  legacy <- list(intercept = list(type = "normal", mu = 80, sigma = 20),
                 slope = list(type = "student_t", nu = 3, mu = 0, sigma = 1))
  fit <- suppressWarnings(fit_model_with_prior(kidiq, kid_score ~ mom_iq, prior_config = legacy,
                                               chains = 2, iter = 500, refresh = 0,
                                               on_nonconvergence = "ignore"))
  ps <- rstanarm::prior_summary(fit)
  expect_equal(ps$prior$dist, "student_t")
  expect_equal(as.numeric(ps$prior_intercept$location), 80)
})

test_that("non-convergence warns (keeping the fit) or errors on request", {
  skip_if_no_stan()
  data(kidiq, package = "rstanarm", envir = environment())
  w <- capture_warnings(fit <- fit_model_with_prior(kidiq, kid_score ~ mom_iq, chains = 2,
                                                    iter = 60, refresh = 0))
  expect_match(w, "convergence checks failed", all = FALSE)
  expect_s3_class(fit, "stanreg")
  expect_error(suppressWarnings(fit_model_with_prior(kidiq, kid_score ~ mom_iq, chains = 2,
                                                     iter = 60, refresh = 0,
                                                     on_nonconvergence = "error")),
               "convergence checks failed")
})

test_that("the intercept prior applies at the predictor means", {
  skip_if_no_stan()
  data(kidiq, package = "rstanarm", envir = environment())
  fit <- fit_model_with_prior(kidiq, kid_score ~ mom_iq,
                              prior_config = prior_config(intercept = prior_spec("normal", 80, 0.5)),
                              chains = 2, iter = 1000, refresh = 0, on_nonconvergence = "ignore")
  theta <- bmbeR:::prior_scale_draws(fit)
  sigma <- mean(theta[, "sigma"])
  prec_data <- nrow(kidiq) / sigma^2
  expected <- (80 / 0.25 + mean(kidiq$kid_score) * prec_data) / (4 + prec_data)
  expect_equal(mean(theta[, "(Intercept)"]), expected, tolerance = 0.01)
})

test_that("priors derived for a different formula are rejected", {
  skip_if_no_stan()
  data(kidiq, package = "rstanarm", envir = environment())
  cfg <- empirical_bayes_priors(kidiq, kid_score ~ mom_iq)
  expect_error(fit_model_with_prior(kidiq, kid_score ~ mom_iq + mom_hs, prior_config = cfg),
               "derived for coefficients")
  expect_error(fit_model_with_prior(kidiq, kid_score ~ mom_iq + mom_hs,
                                    prior_config = prior_config(slope = prior_spec("normal", 0, c(1, 2, 3)))),
               "3 values")
})

test_that("prior_sensitivity flags a conflicting prior but not the defaults", {
  skip_if_no_stan()
  fit <- kidiq_fit()
  ps <- prior_sensitivity(fit)
  expect_s3_class(ps, "bmb_prior_sensitivity")
  expect_true(all(ps$summary$diagnosis == "-"))
  expect_true(all(ps$summary$contraction > 0.9))
  expect_output(print(ps), "No parameter is sensitive")
  expect_s3_class(plot(ps), "ggplot")

  data(kidiq, package = "rstanarm", envir = environment())
  bad <- fit_model_with_prior(kidiq, kid_score ~ mom_iq,
                              prior_config = prior_config(slope = prior_spec("normal", 1.5, 0.05)),
                              chains = 2, iter = 1000, refresh = 0, on_nonconvergence = "ignore")
  ps_bad <- prior_sensitivity(bad, scale_priors = "mom_iq")
  expect_equal(ps_bad$summary$diagnosis[ps_bad$summary$variable == "mom_iq"], "prior-data conflict")
  expect_error(prior_sensitivity(bad, pars = "nope"), "Unknown parameter")
})

test_that("prior_sensitivity refuses unsupported models", {
  skip_if_no_stan()
  data(kidiq, package = "rstanarm", envir = environment())
  qr <- suppressWarnings(rstanarm::stan_glm(kid_score ~ mom_iq + mom_hs, data = kidiq, QR = TRUE,
                                            chains = 1, iter = 300, refresh = 0))
  expect_error(prior_sensitivity(qr), "QR")
  expect_error(prior_sensitivity(lm(kid_score ~ mom_iq, kidiq)), "stanreg")
})

test_that("prior predictive checks detect over-wide logistic priors", {
  skip_if_no_stan()
  w <- wells_data()
  wide <- prior_config(intercept = prior_spec("normal", 0, 10), slope = prior_spec("normal", 0, 10))
  pc_wide <- prior_predictive_check(w, sw ~ dist100 + arsenic, family = binomial(),
                                    prior_config = wide, refresh = 0)
  pc_def <- prior_predictive_check(w, sw ~ dist100 + arsenic, family = binomial(), refresh = 0)
  expect_s3_class(pc_wide, "bmb_prior_check")
  expect_gt(pc_wide$summary$share_extreme, 0.5)
  expect_lt(pc_def$summary$share_extreme, pc_wide$summary$share_extreme)
  expect_output(print(pc_wide), "all-or-nothing")
  expect_s3_class(plot(pc_wide), "ggplot")

  data(kidiq, package = "rstanarm", envir = environment())
  pc <- prior_predictive_check(kidiq, kid_score ~ mom_iq, plausible_range = c(0, 200), refresh = 0)
  expect_true(pc$summary$share_outside >= 0 && pc$summary$share_outside <= 1)
  expect_s3_class(plot(pc), "ggplot")
})

test_that("classification is evaluated on the probability scale", {
  skip_if_no_stan()
  w <- wells_data()
  set.seed(5)
  test <- sample(nrow(w), 500)
  fit <- fit_model_with_prior(w[-test, ], sw ~ dist100 + arsenic, family = binomial(),
                              chains = 2, iter = 1000, refresh = 0, on_nonconvergence = "ignore")
  perf <- evaluate_model_performance(fit, w[test, ])
  p <- colMeans(rstanarm::posterior_epred(fit, newdata = w[test, ]))
  expect_equal(perf$Accuracy, mean((p > 0.5) == w$sw[test]))
  expect_equal(attr(perf, "type"), "classification")
  expect_true(perf$AUC > 0.5 && perf$AUC < 1)
  expect_true(perf$Brier > 0 && perf$Brier < 0.25)
  expect_true(is.finite(perf$ELPD))
  # Legacy call signature still works.
  perf2 <- evaluate_model_performance(fit, w[test, ], sw ~ dist100 + arsenic,
                                      threshold = 0.5, analysis_type = "classification")
  expect_equal(perf$Accuracy, perf2$Accuracy)
  expect_s3_class(as.data.frame(perf), "data.frame")

  fit0 <- fit_model_with_prior(w[-test, ], sw ~ dist100, family = binomial(),
                               chains = 2, iter = 1000, refresh = 0, on_nonconvergence = "ignore")
  cmp <- compare_performance(full = perf, distance = evaluate_model_performance(fit0, w[test, ]))
  expect_equal(nrow(cmp), 2)
  expect_equal(cmp$elpd_diff[1], 0)
  expect_true(all(cmp$se_diff >= 0))
  expect_error(compare_performance(perf, perf), "Name each model")
  expect_error(compare_performance(a = perf, b = evaluate_model_performance(fit0, w[test[1:10], ])),
               "same test observations")
})

test_that("regression evaluation handles transformed responses and reports calibration", {
  skip_if_no_stan()
  data(kidiq, package = "rstanarm", envir = environment())
  fit <- fit_model_with_prior(kidiq, log(kid_score) ~ mom_iq, chains = 2, iter = 1000,
                              refresh = 0, on_nonconvergence = "ignore")
  perf <- evaluate_model_performance(fit, kidiq)
  expect_equal(attr(perf, "type"), "regression")
  expect_true(perf$RMSE < 1)  # on the log scale
  expect_equal(perf$Coverage, 0.9, tolerance = 0.05)
  expect_output(print(perf), "CRPS")
  expect_error(evaluate_model_performance(fit, kidiq, analysis_type = "classification"), "binary")
})

test_that("plots are returned (and saved plots are not empty)", {
  skip_if_no_stan()
  fit <- kidiq_fit()
  plots <- generate_plot(fit, print = FALSE)
  expect_named(plots, c("trace", "rank", "hist", "density", "autocorrelation"))
  for (p in plots) expect_s3_class(p, "ggplot")
  expect_error(generate_plot(fit, types = "nope"), "Invalid plot type")
  expect_s3_class(plot_posterior_distributions(fit), "ggplot")
  expect_s3_class(plot_posterior_distributions(fit$stanfit), "ggplot")
  expect_s3_class(plot_prior_posterior(fit), "ggplot")
  path <- withr::local_tempfile(fileext = ".pdf")
  expect_message(check_convergence(fit, save_plots = TRUE, plot_path = path), "saved")
  expect_gt(file.size(path), 10000)
})

test_that("check_convergence works on stanfit objects and converts the legacy ESS ratio", {
  skip_if_no_stan()
  fit <- kidiq_fit()
  expect_s3_class(check_convergence(fit$stanfit), "bmb_convergence")
  expect_warning(conv <- check_convergence(fit, ess_threshold = 0.1), "deprecated")
  expect_equal(conv$thresholds$min_ess, 0.1 * 2000)
  expect_error(check_convergence(fit, bogus = 1), "Unknown argument")
})

test_that("sensitivity_analysis compares posteriors and LOO across priors", {
  skip_if_no_stan()
  data(kidiq, package = "rstanarm", envir = environment())
  sens <- suppressMessages(sensitivity_analysis(
    kidiq, kid_score ~ mom_iq,
    prior_configurations = list(default = NULL,
                                wrong = prior_config(slope = prior_spec("normal", 2, 0.01))),
    chains = 2, iter = 1000, refresh = 0, on_nonconvergence = "ignore"))
  expect_s3_class(sens, "bmb_sensitivity")
  row <- sens$summary[sens$summary$config == "wrong" & sens$summary$variable == "mom_iq", ]
  expect_true(row$flag)
  expect_named(sens$metric, c("default", "wrong"))
  expect_equal(rownames(sens$comparison)[1], "default")
  expect_output(print(sens), "Predictive comparison")
  expect_s3_class(plot(sens), "ggplot")

  legacy <- list(list(label = "a", prior_config = NULL),
                 list(label = "b", prior_config = list(slope = list(type = "normal", mu = 0, sigma = 1))))
  sens2 <- suppressWarnings(suppressMessages(
    sensitivity_analysis(kidiq, kid_score ~ mom_iq, legacy, chains = 2, iter = 500, refresh = 0,
                         keep_fits = FALSE, on_nonconvergence = "ignore")))
  expect_named(sens2$metric, c("a", "b"))
  expect_null(sens2$fits)
  expect_error(sensitivity_analysis(kidiq, kid_score ~ mom_iq, list(NULL, NULL)), "named list")
})

test_that("workflow_report builds a BARG checklist", {
  skip_if_no_stan()
  fit <- kidiq_fit()
  rep <- workflow_report(fit)
  expect_s3_class(rep, "bmb_report")
  items <- as.data.frame(rep)
  expect_true(all(c("Model", "Priors", "Prior predictive check", "MCMC computation",
                    "Posterior predictive check", "Prior sensitivity") %in% items$item))
  expect_equal(items$status[items$item == "Prior predictive check"], "missing")
  expect_equal(items$status[items$item == "MCMC computation"], "ok")
  expect_output(print(rep), "BARG")
  expect_equal(rep$posterior$variable, c("(Intercept)", "mom_iq", "mom_hs", "sigma"))
})
