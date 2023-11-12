# Add a New Distribution Function to the Distributions List
add_distribution <- function(distributions, name, func) {
  if (!is.function(func)) {
    stop("The provided argument 'func' is not a valid function. Please provide a function that generates random samples from a probability distribution.")
  }
  if (name %in% names(distributions)) {
    stop(paste("The distribution name", name, "already exists. Use a different name or remove the existing distribution first."))
  }
  distributions[[name]] <- func
  return(distributions)
}

# Reset the Distributions List to Original State
reset_distributions <- function(original_distributions) {
  return(original_distributions)
}

# Retrieve a Prior Distribution
get_prior_distribution <- function(distributions, dist_type, params_list) {
  if (!(dist_type %in% names(distributions))) {
    stop(paste("Distribution type", dist_type, "not supported. Check the list for available distribution types."))
  }
  
  priors_list <- lapply(params_list, function(params) {
    do.call(distributions[[dist_type]], params)
  })
  
  return(priors_list)
}
