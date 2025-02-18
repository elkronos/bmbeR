# utilities.R
# =============================================================================
# This script defines utility functions used across the Bayesian modeling workflow.
# It includes basic input validation functions and helper operators.
#
# =============================================================================

# -----------------------------------------------------------------------------
# Validate Positive Integer
# -----------------------------------------------------------------------------
#' Validate a Positive Integer
#'
#' Ensures that the provided value is a positive integer. This function is used
#' to check input parameters that must be positive integers.
#'
#' @param n A numeric value to validate.
#' @param param_name A character string representing the name of the parameter.
#'
#' @return Invisibly returns TRUE if the input is a valid positive integer; 
#'   otherwise, execution is halted with an error message.
#' @export
validate_positive_integer <- function(n, param_name) {
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || n != as.integer(n)) {
    stop(sprintf("%s must be a positive integer.", param_name))
  }
  invisible(TRUE)
}

# -----------------------------------------------------------------------------
# Validate Numeric Parameter
# -----------------------------------------------------------------------------
#' Validate a Numeric Parameter
#'
#' Ensures that the provided parameter is a single numeric value. This function 
#' is used to check that inputs expected to be numeric are valid.
#'
#' @param x The value to be validated.
#' @param param_name A character string representing the parameter's name.
#'
#' @return Invisibly returns TRUE if the input is a valid numeric value; otherwise,
#'   execution is halted with an error message.
#' @export
validate_numeric <- function(x, param_name) {
  if (!is.numeric(x) || length(x) != 1) {
    stop(sprintf("%s must be a numeric value.", param_name))
  }
  invisible(TRUE)
}

# -----------------------------------------------------------------------------
# Null-Coalescing Operator
# -----------------------------------------------------------------------------
#' Null-Coalescing Operator
#'
#' Returns the left-hand side value if it is not NULL; otherwise, returns the 
#' right-hand side value. This operator can simplify default value assignments.
#'
#' @param x The value to test.
#' @param y The default value to return if \code{x} is NULL.
#'
#' @return \code{x} if it is not NULL; otherwise, \code{y}.
#' @export
`%||%` <- function(x, y) {
  if (!is.null(x)) x else y
}
