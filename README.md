# bmbeR: Bayesian Model Building and Evaluation Repository

This repository contains a set of R scripts designed to build, evaluate, visualize, and perform sensitivity analysis on Bayesian models. These scripts use a mixture of `rstan`, `rstanarm`, and other Bayesian analysis libraries to facilitate the modeling process.

## Scripts and Their Functions:

### libraries_and_setup.R
- **Purpose:** Loads the necessary libraries and global settings for the entire modeling workflow.
- **Libraries Used:**
  - `rstanarm`
  - `bayesplot`
  - `ggplot2`
  - `cowplot`
  - `purrr`
  - `rstan`
  - `loo`

### distributions.R
- **Purpose:** Defines a comprehensive set of prior distribution generators.
- **Functions:**
  - `student_t_prior`
  - `normal_prior`
  - `cauchy_prior`
  - `uniform_prior`
  - `beta_prior`
  - `gamma_prior`
  - `binomial_prior`
  - `poisson_prior`
  - `lognormal_prior`
  - `bernoulli_prior`
- **Global Objects and Utilities:**
  - `original_distributions`, `distributions`
  - `add_distribution`, `reset_distributions`, `get_prior_distribution`

### utilities.R
- **Purpose:** Provides utility functions used across the repository.
- **Functions:**
  - `validate_positive_integer`
  - `validate_numeric`
  - `%||%` (null-coalescing operator)

### empirical_bayes.R
- **Purpose:** Computes empirical Bayesian hyperparameter specifications from data.
- **Functions:**
  - `empirical_bayes_priors`: Fits a linear model and maps coefficient estimates and standard errors to hyperparameters for various prior distributions.

### model_convergence.R
- **Purpose:** Checks convergence diagnostics of a fitted Bayesian model.
- **Functions:**
  - `check_convergence`: Assesses if a model has converged based on Rhat and effective sample size (ESS) metrics, and optionally generates trace plots.

### model_fitting.R
- **Purpose:** Fits Bayesian models using specified prior configurations and data.
- **Functions:**
  - `build_stanarm_priors`: Converts hyperparameter lists into rstanarm prior objects.
  - `fit_model_with_prior`: Fits a Bayesian model using `rstanarm::stan_glm` with given priors, and checks for convergence.

### model_visualization.R
- **Purpose:** Generates diagnostic and posterior visualization plots for fitted models.
- **Functions:**
  - `generate_plot`: Generates diagnostic plots (trace, histogram, density, and autocorrelation) for a model.
  - `plot_posterior_distributions`: Visualizes posterior distributions with 95% credible intervals using bayesplot.

### model_sensitivity.R
- **Purpose:** Performs sensitivity analysis across different prior configurations.
- **Functions:**
  - `sensitivity_analysis`: Iterates through a list of prior configurations, fits models, and computes performance metrics (using LOO) to assess how changes in priors affect model outcomes.

### model_evaluation.R
- **Purpose:** Evaluates the predictive performance of fitted models.
- **Functions:**
  - `evaluate_model_performance`: Computes performance metrics such as RMSE and MAE for regression tasks, or accuracy, precision, recall, and F1 Score for classification tasks.
