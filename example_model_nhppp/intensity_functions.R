#' Vectorized constant intensity function 
#' @param t time scalar, vector, or matrix
#' @param lambda constant intensity parameter
#' @return scalar, vector, or matrix -- intensity function
lambda_constant <- function(t, lambda) {
  t*0+lambda
}


#' Vectorized Weibull intensity function 
lambda_weibull <- function(t, scale, shape) {

}
