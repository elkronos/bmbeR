# Reproduces the evidence in REVIEW.md by running the original (v1.01) code.
#
# Run from the repository root:  Rscript review/reproduce-review.R
# Requires git, rstanarm, rstan, bayesplot, loo and memoise. Takes a few minutes.

original_commit <- "3f55fb6"
files <- c("utilities.R", "distributions.R", "empirical_bayes.R", "model_convergence.R",
           "model_fitting.R", "model_visualization.R", "model_sensitivity.R",
           "model_evaluation.R")
old <- file.path(tempdir(), "bmbeR-1.01")
dir.create(old, showWarnings = FALSE)
for (f in files) {
  src <- system2("git", c("show", sprintf("%s:R/%s", original_commit, f)), stdout = TRUE)
  writeLines(src, file.path(old, f))
}
env <- new.env()
for (f in files) suppressMessages(sys.source(file.path(old, f), envir = env))
attach(env, name = "bmbeR_1.01", warn.conflicts = FALSE)
options(mc.cores = 2)
quiet_fit <- function(expr) suppressWarnings(suppressMessages(expr))
section <- function(x) cat("\n==", x, "==\n")
data(kidiq, package = "rstanarm")
data(wells, package = "rstanarm")
wells$dist100 <- wells$dist / 100

section("C1: fit_model_with_prior() fails on every call")
print(tryCatch(fit_model_with_prior(kidiq, kid_score ~ mom_iq, chains = 2, iter = 500, refresh = 0),
               error = function(e) conditionMessage(e)))

# Patch the non-exported rstan calls so that downstream behaviour can be examined.
patched <- sub("rstan::rhat(", "bayesplot::rhat(",
               sub("rstan::neff_ratio(", "bayesplot::neff_ratio(",
                   deparse(env$check_convergence), fixed = TRUE), fixed = TRUE)
env$check_convergence <- eval(parse(text = patched), envir = env)
environment(env$check_convergence) <- env
detach("bmbeR_1.01")
attach(env, name = "bmbeR_1.01", warn.conflicts = FALSE)
fit <- quiet_fit(fit_model_with_prior(kidiq, kid_score ~ mom_iq + mom_hs, chains = 2,
                                      iter = 1000, refresh = 0))

section("C2: plot_posterior_distributions() errors")
grDevices::pdf(NULL)
print(tryCatch(class(plot_posterior_distributions(fit)), error = function(e) conditionMessage(e)))
grDevices::dev.off()

section("C3: generate_plot() and save_plots produce nothing")
pdf_path <- tempfile(fileext = ".pdf")
grDevices::pdf(pdf_path)
res <- generate_plot(fit, types = "trace")
grDevices::dev.off()
cat("generate_plot() returned:", deparse(res), "; PDF size:", file.size(pdf_path), "bytes\n")
pp <- tempfile(fileext = ".pdf")
suppressWarnings(check_convergence(fit, rhat_threshold = 0.5, save_plots = TRUE, plot_path = pp))
pages <- sum(grepl("/Type /Page$", readLines(pp, warn = FALSE)))
cat("save_plots PDF pages:", pages, "\n")

section("H1: student_t_prior(mu = 5, sigma = 2, nu = 30)")
set.seed(1)
x <- student_t_prior(1e5, nu = 30, mu = 5, sigma = 2)
cat(sprintf("mean = %.2f (expected 5), sd = %.2f (expected %.2f)\n", mean(x), sd(x), 2 * sqrt(30 / 28)))

section("H2: unscaled default prior and the units of the outcome")
d <- mtcars
d$mpg100 <- d$mpg * 100
f_a <- quiet_fit(fit_model_with_prior(d, mpg ~ wt, chains = 2, iter = 1000, refresh = 0))
f_b <- quiet_fit(fit_model_with_prior(d, mpg100 ~ wt, chains = 2, iter = 1000, refresh = 0))
print(round(rbind("OLS mpg" = coef(lm(mpg ~ wt, d)), "bmbeR 1.01 mpg" = coef(f_a),
                  "OLS mpg*100" = coef(lm(mpg100 ~ wt, d)), "bmbeR 1.01 mpg*100" = coef(f_b)), 2))

section("H3: same-data 'empirical Bayes' prior N(bhat, se^2): coverage of 95% intervals")
set.seed(42)
cover <- replicate(2000, {
  x <- rnorm(50)
  y <- 1 + 0.5 * x + rnorm(50)
  est <- summary(lm(y ~ x))$coefficients["x", ]
  c(flat = abs(est[[1]] - 0.5) < 1.96 * est[[2]],
    same_data = abs(est[[1]] - 0.5) < 1.96 * est[[2]] / sqrt(2))
})
print(rowMeans(cover))

section("H4: empirical_bayes_priors() on a logistic model uses lm()")
eb <- empirical_bayes_priors(wells[, c("switch", "dist100", "arsenic")], switch ~ dist100 + arsenic,
                             dist_types = list(dist100 = "normal"))
cat(sprintf("EB prior for dist100: N(%.3f, %.3f^2); logit-scale estimate: %.3f\n",
            eb$dist100$mu, eb$dist100$sigma,
            coef(glm(switch ~ dist100 + arsenic, binomial, wells))[["dist100"]]))

section("H6: classification on the log-odds scale")
fitb <- quiet_fit(rstanarm::stan_glm(switch ~ dist100 + arsenic, family = binomial(), data = wells,
                                     chains = 2, iter = 1000, refresh = 0, seed = 1))
pr <- as.vector(predict(fitb, newdata = wells))
cat(sprintf("share of predict() values outside [0, 1]: %.3f\n", mean(pr < 0 | pr > 1)))
acc_old <- evaluate_model_performance(fitb, wells, switch ~ dist100 + arsenic,
                                      analysis_type = "classification")$Accuracy
p <- colMeans(rstanarm::posterior_epred(fitb, newdata = wells))
cat(sprintf("accuracy (1.01): %.3f; accuracy using probabilities: %.3f\n",
            acc_old, mean((p > 0.5) == wells$switch)))

section("M1: empirical_bayes_priors() output cannot be used for fitting")
print(tryCatch(build_stanarm_priors(eb$`(Intercept)`, eb$dist100), error = function(e) conditionMessage(e)))

section("M3: integer 0/1 outcome and family given as a function")
wells$sw <- as.integer(wells$switch)
print(tryCatch(fit_model_with_prior(wells, sw ~ dist100, family = binomial(), chains = 1, iter = 200,
                                    refresh = 0), error = function(e) conditionMessage(e)))
print(tryCatch(fit_model_with_prior(kidiq, kid_score ~ mom_iq, family = gaussian, chains = 1,
                                    iter = 200, refresh = 0), error = function(e) conditionMessage(e)))

section("M4: add_distribution() + get_prior_distribution()")
d2 <- add_distribution(distributions, "exp", function(sample_size, a = 1) rexp(sample_size, a))
print(tryCatch(get_prior_distribution("exp", list(sample_size = 3)), error = function(e) conditionMessage(e)))

section("M9: invalid scales accepted")
print(tryCatch(normal_prior(5, sigma = -1), warning = function(w) conditionMessage(w)))
