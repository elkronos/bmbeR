# bmbeR: Bayesian Model Building and Evaluation Repository

This repository contains a set of R scripts designed to build, evaluate, visualize, and perform sensitivity analysis on Bayesian models. These scripts use a mixture of `rstan`, `rstanarm`, and other Bayesian analysis libraries to facilitate the modeling process.

## Scripts and Their Functions:

### 01_libraries_and_setup.R
- **Purpose:** Loads the necessary libraries for the entire modeling workflow.
- **Libraries Used:**
  - `rstanarm`
  - `bayesplot`
  - `ggplot2`
  - `cowplot`
  - `purrr`
  - `rstan`
  - `loo`

### 02_distributions.R
- **Purpose:** Defines various prior distributions.
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

### 03_utilities.R
- **Purpose:** Provides utility functions to manage distributions.
- **Functions:**
  - `add_distribution`: Add a new distribution to the working list.
  - `reset_distributions`: Resets custom distributions to their original forms.
  - `get_prior_distribution`: Retrieve a prior distribution based on type.

### 04_empirical_bayes.R
- **Purpose:** Generate empirical Bayesian priors from data.
- **Functions:**
  - `empirical_bayes_priors`: Computes priors based on data and given formula.

### 05_model_convergence.R
- **Purpose:** Checks the convergence of a given model.
- **Functions:**
  - `check_convergence`: Assesses if a model has converged.

### 06_model_fitting.R
- **Purpose:** Fits a Bayesian model using given priors and data.
- **Functions:**
  - `fit_model_with_prior`: Fits a Bayesian model using `stan_glm`.

### 07_model_visualization.R
- **Purpose:** Visualizes model fit and posterior distributions.
- **Functions:**
  - `generate_plot`: Generates trace or histogram plots.
  - `plot_posterior_distributions`: Plots 95% intervals for posterior distributions.

### 08_model_sensitivity.R
- **Purpose:** Performs sensitivity analysis for various prior configurations.
- **Functions:**
  - `define_prior`: Defines priors for the sensitivity analysis.
  - `sensitivity_analysis`: Conducts sensitivity analysis using different prior combinations.

### 09_model_evaluation.R
- **Purpose:** Evaluates the performance of a model on test data.
- **Functions:**
  - `evaluate_model_performance`: Computes RMSE for regression or accuracy for classification.

# Contact
- email: napoleonic_bores@proton.me
- discord: elkronos
