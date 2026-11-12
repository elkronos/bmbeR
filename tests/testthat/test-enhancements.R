# Multilevel support in the prior tools, the priorsense export and parallel
# refits in sensitivity_analysis().

glmer_fit <- function(prior_PD = FALSE) {
  cached_fit(paste0("glmer", prior_PD), {
    data(kidiq, package = "rstanarm", envir = environment())
    set.seed(1)
    kidiq$g <- factor(sample(1:8, nrow(kidiq), TRUE))
    suppressWarnings(rstanarm::stan_glmer(kid_score ~ mom_iq + mom_hs + (1 | g), data = kidiq,
                                          chains = 2, iter = 1000, refresh = 0, seed = 3,
                                          prior_PD = prior_PD))
  })
}

test_that("population-level priors of stan_glmer fits are rebuilt correctly", {
  skip_if_no_stan()
  fit <- glmer_fit(prior_PD = TRUE)
  pt <- prior_table(fit)
  expect_equal(pt$variable, c("(Intercept)", "mom_iq", "mom_hs", "sigma"))
  theta <- bmbeR:::prior_scale_draws(fit)
  # Sampling from the prior only: draws follow the rebuilt priors.
  for (v in c("(Intercept)", "mom_iq", "mom_hs")) {
    pr <- pt[pt$variable == v, ]
    expect_lt(stats::ks.test(theta[, v], "pnorm", pr$location, pr$scale)$statistic, 0.08)
  }
  expect_lt(stats::ks.test(theta[, "sigma"], "pexp", 1 / pt$scale[4])$statistic, 0.08)
})

test_that("prior tools accept stan_glmer fits", {
  skip_if_no_stan()
  fit <- glmer_fit()
  ps <- prior_sensitivity(fit)
  expect_true(ps$group_level)
  expect_equal(ps$summary$variable, c("(Intercept)", "mom_iq", "mom_hs", "sigma"))
  expect_output(print(ps), "Group-level \\(decov\\) priors are held fixed")
  expect_s3_class(plot_prior_posterior(fit), "ggplot")
  rep <- suppressWarnings(workflow_report(fit))
  expect_match(rep$items$detail[rep$items$item == "Priors"], "group-level covariance ~ decov")
  expect_equal(rep$items$status[rep$items$item == "Prior sensitivity"], "ok")
})

test_that("as_priorsense_draws returns parameters, log prior and log-likelihood", {
  skip_if_no_stan()
  fit <- kidiq_fit()
  draws <- as_priorsense_draws(fit)
  expect_s3_class(draws, "draws_array")
  vars <- posterior::variables(draws)
  n <- nrow(rstanarm::get_x(fit))
  expect_equal(vars[1:4], c("(Intercept)", "mom_iq", "mom_hs", "sigma"))
  expect_equal(sum(grepl("^lprior\\[", vars)), 4)
  expect_equal(sum(grepl("^log_lik\\[", vars)), n)
  expect_equal(posterior::nchains(draws), 4)
  m <- posterior::as_draws_matrix(draws)
  pri <- bmbeR:::resolve_priors(fit)
  theta <- bmbeR:::prior_scale_draws(fit, pri)
  expect_equal(as.numeric(m[, "(Intercept)"]), unname(theta[, "(Intercept)"]))
  expect_equal(unname(rowSums(unclass(m)[, sprintf("lprior[%d]", 1:4)])),
               unname(rowSums(bmbeR:::prior_log_density(theta, pri))))
  expect_equal(as.numeric(m[, "log_lik[1]"]), unname(rstanarm::log_lik(fit)[, 1]))
  # Group-level parameters are kept for multilevel fits.
  gdraws <- as_priorsense_draws(glmer_fit())
  expect_true(any(grepl("^b\\[", posterior::variables(gdraws))))
  expect_equal(sum(grepl("^lprior\\[", posterior::variables(gdraws))), 4)
})

test_that("priorsense agrees with prior_sensitivity on a conflicting prior", {
  skip_if_no_stan()
  skip_if_not_installed("priorsense")
  data(kidiq, package = "rstanarm", envir = environment())
  bad <- fit_model_with_prior(kidiq, kid_score ~ mom_iq,
                              prior_config = prior_config(slope = prior_spec("normal", 1.5, 0.05)),
                              chains = 2, iter = 1000, refresh = 0, on_nonconvergence = "ignore")
  ps <- priorsense::powerscale_sensitivity(as_priorsense_draws(bad), variable = "mom_iq")
  expect_match(as.character(ps$diagnosis), "conflict")
})

test_that("sensitivity_analysis can fit configurations in parallel", {
  skip_if_no_stan()
  skip_if_not_installed("future.apply")
  data(kidiq, package = "rstanarm", envir = environment())
  old <- future::plan("sequential")
  on.exit(future::plan(old), add = TRUE)
  cfgs <- list(a = NULL, b = prior_config(slope = prior_spec("normal", 0, 0.1)))
  par <- suppressWarnings(suppressMessages(sensitivity_analysis(
    kidiq, kid_score ~ mom_iq, cfgs, chains = 2, iter = 600, refresh = 0,
    on_nonconvergence = "ignore", parallel = TRUE)))
  ser <- suppressWarnings(suppressMessages(sensitivity_analysis(
    kidiq, kid_score ~ mom_iq, cfgs, chains = 2, iter = 600, refresh = 0,
    on_nonconvergence = "ignore")))
  expect_equal(par$summary, ser$summary)
  expect_identical(par$fits$a$call$data, quote(kidiq))
  expect_error(sensitivity_analysis(kidiq, kid_score ~ mom_iq, cfgs, parallel = NA), "TRUE or FALSE")
})

test_that("weighted models are compared on their posteriors only", {
  skip_if_no_stan()
  data(kidiq, package = "rstanarm", envir = environment())
  kidiq$w <- rep(c(0.5, 2), length.out = nrow(kidiq))
  cfgs <- list(a = NULL, b = prior_config(slope = prior_spec("normal", 0, 0.1)))
  expect_error(sensitivity_analysis(kidiq, kid_score ~ mom_iq, cfgs, weights = w), "cv = \"none\"")
  sens <- suppressWarnings(suppressMessages(sensitivity_analysis(
    kidiq, kid_score ~ mom_iq, cfgs, weights = w, cv = "none", chains = 2, iter = 600,
    refresh = 0, on_nonconvergence = "ignore")))
  expect_null(sens$comparison)
  expect_true(all(is.na(sens$metric)))
  expect_equal(unique(sens$summary$config), c("a", "b"))
  expect_output(print(sens), "No predictive comparison")
})
