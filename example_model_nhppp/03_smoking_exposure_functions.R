
#' Generate standardized smoking exposure
#'
#' This function generates smoking exposure as a function of age.
#'
#' @param t Age
#' @param start_age Age at which smoking exposure starts
#' @param stop_age Age at which smoking exposure stops
#' @param max_exposure Maximum smoking exposure 
#' @return Smoking exposure in [0, max_exposure]
generate_smoking_exposure <- function(t, max_exposure = 1, start_age = 20, stop_age = 110) {
  (t>=start_age) * (t<=stop_age) * max_exposure * (1 + (cos(t/2) + cos(0.9*t/2))/2)
}



