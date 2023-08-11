#' Generate Empirical Bayes Priors
#'
#' Constructs empirical Bayes prior distributions based on a linear model fit to the provided data. 
#' By default, it assumes student_t distribution for both intercept and slope unless specified otherwise.
#'
#' @param data Data frame containing the data to fit the linear model.
#' @param formula Formula specifying the linear model.
#' @param dist_types List specifying the type of distribution to use for each predictor. 
#'   Default is student_t for both intercept and slope.
#' @param df Degrees of freedom for the distribution. Default is 3.
#'
#' @return A list of prior distributions corresponding to each predictor in the formula.
#'
#' @examples
#' \dontrun{
#' data_sample <- data.frame(y = rnorm(100), x = rnorm(100))
#' priors <- empirical_bayes_priors(data_sample, y ~ x)
#' print(priors)
#' }
#'
#' @seealso \code{\link{get_prior_distribution}}, the function used to fetch prior distributions.
#' 
#' @export
empirical_bayes_priors <- function(data, formula, dist_types = list(intercept = "student_t", slope = "student_t"), df = 3) {
  required_columns <- all.vars(formula)
  if (!all(required_columns %in% colnames(data))) {
    stop(paste("Data does not contain necessary columns for formula:", formula))
  }
  
  prior_model <- lm(formula, data = data)
  prior_coefs <- coef(prior_model)
  prior_ses <- sqrt(diag(vcov(prior_model)))
  
  predictor_names <- names(prior_coefs)
  
  prior_list <- list()
  for(predictor in predictor_names) {
    dist_type <- dist_types[[predictor]]
    params <- list(df=df, mean=prior_coefs[predictor], sd=prior_ses[predictor])
    prior_list[[predictor]] <- get_prior_distribution(dist_type, params)
  }
  return(prior_list)
}
