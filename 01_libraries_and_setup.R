#' Ensure Package Installation
#'
#' This function checks if a specified R package is installed and, if not, installs it.
#' It can also check for a specific version of the package and warn if the installed version
#' is older than the required one.
#'
#' @param package_name The name of the package to check and install. This parameter is required.
#' @param version An optional version number to check against the installed package.
#'   If the installed version is older, a warning is issued. Defaults to `NULL`, meaning no version check is performed.
#'   This parameter is optional.
#'
#' @importFrom utils packageVersion
#' @importFrom utils install.packages
#' @importFrom utils requireNamespace
#'
#' @examples
#' \dontrun{
#'   # To check and install the 'ggplot2' package without version check
#'   ensure_package("ggplot2")
#'
#'   # To check and install the 'dplyr' package with a version check
#'   ensure_package("dplyr", version = "1.0.0")
#' }
#'
#' @return No return value, called for side effects.
#' @note This function is particularly useful in scripts that require specific packages,
#'   ensuring that all necessary packages are installed before script execution.
#' @export
ensure_package <- function(package_name, version = NULL) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name)
  }
  
  if (!is.null(version)) {
    package_version <- packageVersion(package_name)
    if (package_version < version) {
      warning(paste("The installed version of", package_name, "is older than the required version:", version))
    }
  }
}


#' Load a Specific Version of an R Package
#'
#' This function loads a specified version of an R package into the R environment. 
#' If the specified version is not installed, it attempts to install it first. 
#' This is useful for ensuring compatibility in projects that rely on specific package versions.
#'
#' @param package_name A string specifying the name of the package to be loaded. 
#' This parameter is required.
#' @param version An optional string specifying the version of the package to be loaded. 
#' If `NULL` (the default), the latest available version is loaded.
#'
#' @importFrom utils install.packages
#' @importFrom utils library
#'
#' @examples
#' \dontrun{
#'   # Load the latest version of the ggplot2 package
#'   load_package("ggplot2")
#'
#'   # Load a specific version of the dplyr package
#'   load_package("dplyr", "1.0.0")
#' }
#'
#' @export
#' @keywords internal
#' @note The function requires the 'utils' package for installing and loading packages.
#' It assumes that the required package version is available in the repositories.
#' @seealso \code{\link[utils]{install.packages}}, \code{\link[utils]{library}}
load_package <- function(package_name, version = NULL) {
  ensure_package(package_name, version)
  library(package_name, character.only = TRUE)
}


#' List of required packages. Versions are for illustration purposes. Adjust as needed.
required_packages <- list(
  "rstanarm" = "2.21.1",
  "bayesplot" = "1.8.0",
  "ggplot2" = "3.3.3",
  "cowplot" = "1.1.1",
  "purrr" = "0.3.4",
  "rstan" = "2.21.2",
  "loo" = "2.4.1"
)

#' Loads required packages listed above.
for (package in names(required_packages)) {
  load_package(package, required_packages[[package]])
}
