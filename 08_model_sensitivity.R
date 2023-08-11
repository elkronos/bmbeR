#' Define Prior Distribution for Bayesian Modeling
#'
#' This function allows the user to specify a prior distribution for Bayesian modeling, 
#' either manually or using empirical Bayes estimation.
#'
#' @param dist_type A character string indicating the type of the distribution (e.g., "normal", "student_t").
#' @param intercept_params A list of parameters for the intercept prior distribution (default is NULL).
#' @param slope_params A list of parameters for the slope prior distribution (default is NULL).
#' @param use_empirical_bayes A logical indicating whether to use empirical Bayes estimation (default is FALSE).
#'
#' @return A list containing the type of the distribution, parameters (if specified), and a flag indicating 
#'         whether empirical Bayes was used.
#'
#' @examples
#' \dontrun{
#' # Define a student-t prior for both intercept and slope without empirical Bayes:
#' define_prior("student_t", 
#'              intercept_params = list(sample_size = 100, nu=5, mu=0, sigma=1), 
#'              slope_params = list(sample_size = 100, nu=3, mu=0, sigma=1), 
#'              use_empirical_bayes = FALSE)
#'
#' # Define a student-t prior using empirical Bayes:
#' define_prior("student_t", use_empirical_bayes = TRUE)
#' }
#'
#' @seealso \code{\link{get_prior_distribution}}, \code{\link{empirical_bayes_priors}}
#'
#' @importFrom stats setNames
#'
#' @export
define_prior <- function(dist_type, intercept_params = NULL, slope_params = NULL, use_empirical_bayes = FALSE) {
  # Check if distribution is valid
  if (!(dist_type %in% names(distributions))) {
    stop(paste("Distribution type", dist_type, "not supported."))
  }
  
  if (use_empirical_bayes) {
    # If empirical Bayes is used, ignore other parameters
    return(list(dist_type = dist_type, use_empirical = TRUE))
  } else {
    # If empirical Bayes is not used, rely on provided or default parameters
    params_list <- list(
      intercept = if (!is.null(intercept_params)) intercept_params else list(sample_size = 100),
      slope = if (!is.null(slope_params)) slope_params else list(sample_size = 100)
    )
    return(list(dist_type = dist_type, params = params_list, use_empirical = FALSE))
  }
}

#' Conduct Sensitivity Analysis for Different Prior Combinations
#'
#' This function performs a sensitivity analysis by fitting a Bayesian model 
#' with different prior combinations. It then calculates the Widely Applicable
#' Information Criterion (WAIC) for each model to assess model fit.
#'
#' @param data A dataframe containing the data to be used in the model.
#' @param formula An object of class "formula" indicating the model specification.
#' @param prior_combinations A list of prior combinations to be used in the analysis.
#' @param elpd_column A character string indicating the column name in the loo output for expected log pointwise predictive density. Default is 'elpd_loo'.
#' @param estimate_column A character string indicating the column name in the loo output for the estimate. Default is 'Estimate'.
#'
#' @return A list containing the WAIC for each prior combination.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(y = rnorm(100), x = rnorm(100))
#' formula <- y ~ x
#' prior_combinations <- list(
#'   list(label = "normal_1", dist_type = "normal", params = list(intercept = list(mean=0, sd=1), slope = list(mean=0, sd=2))),
#'   list(label = "normal_2", dist_type = "normal", params = list(intercept = list(mean=1, sd=1), slope = list(mean=1, sd=2)))
#' )
#' sensitivity_results <- sensitivity_analysis(data, formula, prior_combinations)
#' }
#'
#' @seealso \code{\link{get_prior_distribution}}, \code{\link{fit_model_with_prior}}
#'
#' @export
sensitivity_analysis <- function(data, formula, prior_combinations,
                                 elpd_column = 'elpd_loo', estimate_column = 'Estimate') {
  results <- list()
  
  # Loop through each combination of priors
  for(i in seq_along(prior_combinations)) {
    prior_config <- prior_combinations[[i]]
    dist_type <- prior_config$dist_type
    
    current_prior <- get_prior_distribution(dist_type, prior_config$params)
    name <- paste0(prior_config$label, "_intercept_", names(prior_config$params$intercept)[1], "_slope_", names(prior_config$params$slope)[1])
    
    fit <- fit_model_with_prior(current_prior, data, formula)
    waic <- loo(fit, k_threshold = 0.7)$estimates[elpd_column, estimate_column]
    
    results[[name]] <- waic
  }
  
  return(results)
}