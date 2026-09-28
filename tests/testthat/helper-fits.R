# Small, cached Stan fits shared by the integration tests.
.fits <- new.env(parent = emptyenv())

skip_if_no_stan <- function() {
  skip_on_cran()
  skip_if_not_installed("rstanarm")
}

cached_fit <- function(name, expr) {
  if (is.null(.fits[[name]])) .fits[[name]] <- force(expr)
  .fits[[name]]
}

kidiq_fit <- function() {
  cached_fit("kidiq", {
    data(kidiq, package = "rstanarm", envir = environment())
    fit_model_with_prior(kidiq, kid_score ~ mom_iq + mom_hs, chains = 4, iter = 1000,
                         refresh = 0, seed = 11)
  })
}

wells_data <- function() {
  data(wells, package = "rstanarm", envir = environment())
  wells$dist100 <- wells$dist / 100
  wells$sw <- as.integer(wells$switch)
  wells
}
