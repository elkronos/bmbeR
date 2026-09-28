# Evaluate predictive performance on test data

Scores a fitted model's predictions for new data using the full
posterior predictive distribution. Proper scoring rules (Gneiting &
Raftery, 2007) reward both accuracy and honest uncertainty, and are
reported alongside the familiar point-prediction metrics.

## Usage

``` r
evaluate_model_performance(
  fit,
  data_test,
  formula = NULL,
  threshold = 0.5,
  analysis_type = c("auto", "regression", "classification"),
  prob = 0.9,
  ndraws = NULL,
  seed = 1234
)
```

## Arguments

- fit:

  A `stanreg` object.

- data_test:

  A data frame of test data containing the model's variables. Rows with
  missing values in those variables are dropped.

- formula:

  Optional. Defaults to the model's formula; retained for compatibility
  with bmbeR 1.x.

- threshold:

  Probability threshold for classification.

- analysis_type:

  `"auto"` (classification for binary outcomes of a binomial model,
  regression otherwise), `"regression"` or `"classification"`.

- prob:

  Probability mass of the predictive intervals used for `Coverage`.

- ndraws:

  Number of posterior draws to use (default: all).

- seed:

  Seed for posterior predictive simulation.

## Value

An object of class `bmb_performance`: a named list of metrics (see
Details) with attributes `type` and `n`. Access metrics with `$`, e.g.
`perf$RMSE`, or convert with
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

## Details

Always reported:

- `ELPD`: expected log predictive density on the test data, the sum over
  observations of \\\log \frac{1}{S}\sum_s p(y_i \mid \theta_s)\\
  (higher is better), with its standard error `ELPD_SE` and
  per-observation mean `ELPD_mean`. This is the out-of-sample analogue
  of `elpd_loo` (Vehtari et al., 2017).

Regression (continuous or count outcomes):

- `RMSE`, `MAE` of the posterior mean prediction (on the scale of the
  model's response, e.g. `log(y)` for `log(y) ~ x`).

- `CRPS`: continuous ranked probability score of the posterior
  predictive distribution (lower is better), estimated from draws.

- `Coverage` and `Interval_Width`: share of test outcomes inside the
  central `prob` posterior predictive interval, and its mean width. For
  a well-calibrated model coverage should be close to `prob`.

Classification (binary outcomes of a binomial model):

- `Accuracy`, `Precision`, `Recall`, `F1_Score` after classifying the
  posterior mean *probability* at `threshold`.

- `Brier`: mean squared error of the predicted probabilities (lower is
  better; Brier, 1950).

- `AUC`: area under the ROC curve.

In bmbeR 1.x, classification thresholded
[`predict()`](https://rdrr.io/r/stats/predict.html) output, which for
rstanarm is on the link (log-odds) scale; probabilities are now used.

## References

Gneiting, T., & Raftery, A. E. (2007). Strictly proper scoring rules,
prediction, and estimation. *JASA*, 102(477), 359–378.
[doi:10.1198/016214506000001437](https://doi.org/10.1198/016214506000001437)

Brier, G. W. (1950). Verification of forecasts expressed in terms of
probability. *Monthly Weather Review*, 78(1), 1–3.

Vehtari, A., Gelman, A., & Gabry, J. (2017). Practical Bayesian model
evaluation using leave-one-out cross-validation and WAIC. *Statistics
and Computing*, 27(5), 1413–1432.
[doi:10.1007/s11222-016-9696-4](https://doi.org/10.1007/s11222-016-9696-4)

## Examples

``` r
# \donttest{
data(wells, package = "rstanarm")
wells$dist100 <- wells$dist / 100
set.seed(1)
test <- sample(nrow(wells), 500)
fit <- rstanarm::stan_glm(switch ~ dist100 + arsenic, family = binomial(),
                          data = wells[-test, ], chains = 2, iter = 1000,
                          refresh = 0)
evaluate_model_performance(fit, wells[test, ])
#> <bmb_performance> classification, 500 test observations
#>   ELPD (log score, higher = better)        -325
#>     SE of ELPD                             6.34
#>     ELPD per observation                   -0.651
#>   Accuracy (threshold 0.5)                 0.616
#>   Precision                                0.628
#>   Recall                                   0.846
#>   F1 score                                 0.721
#>   Brier score (lower = better)             0.229
#>   AUC                                      0.634
# }
```
