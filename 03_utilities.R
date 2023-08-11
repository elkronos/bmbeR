#' Add a New Distribution Function to the Distributions List
#'
#' This function allows the addition of a new distribution function to the global
#' `distributions` list. This can be useful for extending the available set of 
#' prior distributions with custom or additional distributions.
#'
#' @param name Character string specifying the name of the new distribution. This 
#'   will be used as the key in the `distributions` list.
#' @param func A function that generates random samples from the desired 
#'   probability distribution. The function should take `sample_size` as its first
#'   argument, followed by any distribution-specific parameters.
#'
#' @return Invisible NULL. The function works by side effect, modifying the global
#'   `distributions` list.
#'
#' @examples 
#' \dontrun{
#' student_t_prior <- function(sample_size, nu=1, mu=0, sigma=1) {
#'   rt(sample_size, df=nu, ncp=mu) * sigma
#' }
#'
#' # Remove the student_t distribution from the global list (for the sake of example)
#' distributions$student_t <- NULL
#'
#' # Add the student_t distribution back to the list using add_distribution
#' add_distribution("student_t", student_t_prior)
#'
#' # Sample using the newly added distribution
#' samples <- distributions$student_t(100, nu=2, mu=5, sigma=2)
#' }
#'
#' @seealso \code{\link{distributions}}, the global list of available distribution functions.
#'
#' @export
add_distribution <- function(name, func) {
  if (!is.function(func)) stop("Provided function is not valid.")
  distributions[[name]] <- func
}

#' Reset the Distributions List to Original State
#'
#' Resets the global `distributions` list to its original state, 
#' reverting any changes or additions made with the `add_distribution` function.
#'
#' @return Invisible NULL. The function works by side effect, modifying the global
#'   `distributions` list to its original state.
#'
#' @examples 
#' \dontrun{
#' student_t_prior <- function(sample_size, nu=1, mu=0, sigma=1) {
#'   rt(sample_size, df=nu, ncp=mu) * sigma
#' }
#'
#' # Remove the student_t distribution from the global list
#' distributions$student_t <- NULL
#'
#' # Verify that the student_t distribution has been removed
#' print("student_t" %in% names(distributions))  # Should return FALSE
#'
#' # Reset distributions to their original state
#' reset_distributions()
#'
#' # Verify that the student_t distribution is back in the list
#' print("student_t" %in% names(distributions))  # Should return TRUE
#' }
#'
#' @seealso \code{\link{distributions}}, the global list of available distribution functions.
#' @seealso \code{\link{original_distributions}}, the original list of available distribution functions.
#'
#' @export
reset_distributions <- function() {
  distributions <<- original_distributions
}

#' Retrieve a Prior Distribution
#'
#' Fetches a prior distribution of the specified type using provided parameters for both intercept and slope.
#'
#' @param dist_type Character, the name of the desired prior distribution type. 
#'   Must be one of the names in the global `distributions` list.
#' @param params List containing two named sublists, 'intercept' and 'slope', 
#'   each of which holds parameters specific to the chosen distribution type.
#'
#' @return A list containing two elements, the prior distributions for intercept and slope.
#'
#' @examples 
#' \dontrun{
#' # Example using student_t_prior as the base distribution
#' student_t_prior <- function(sample_size, nu=1, mu=0, sigma=1) {
#'   rt(sample_size, df=nu, ncp=mu) * sigma
#' }
#'
#' # Parameters for student_t_prior for both intercept and slope
#' params <- list(
#'   intercept = list(sample_size=1000, nu=2, mu=0, sigma=2),
#'   slope = list(sample_size=1000, nu=3, mu=0, sigma=1)
#' )
#'
#' priors <- get_prior_distribution("student_t", params)
#' 
#' # View the first few elements of the generated prior for intercept
#' print(head(priors[[1]]))
#'}
#'
#' @seealso \code{\link{distributions}}, the global list of available distribution functions.
#' @seealso \code{\link{add_distribution}}, the function to add custom distributions.
#'
#' @export
get_prior_distribution <- function(dist_type, params) {
  if (!(dist_type %in% names(distributions))) {
    stop(paste("Distribution type", dist_type, "not supported."))
  }
  
  priors_list <- list(
    do.call(distributions[[dist_type]], params$intercept),
    do.call(distributions[[dist_type]], params$slope)
  )
  return(priors_list)
}