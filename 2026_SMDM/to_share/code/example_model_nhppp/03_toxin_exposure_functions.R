
#' Generate standardized toxin exposure
#'
#' This function generates toxin exposure as a function of age.
#'
#' @param t Age
#' @param start_age Age at which toxin exposure starts
#' @param stop_age Age at which toxin exposure stops
#' @param max_exposure Maximum toxin exposure 
#' @return Toxin exposure in [0, max_exposure]
generate_toxin_exposure <- function(t, max_exposure = 1, start_age = 20, stop_age = 110) {
  (t>=start_age) * (t<=stop_age) * max_exposure * (0.5 + (cos( (t+50) /2) + cos(0.9* (t+50) /2))/4)
}



