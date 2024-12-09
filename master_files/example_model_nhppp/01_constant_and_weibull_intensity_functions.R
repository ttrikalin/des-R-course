#' Vectorized constant intensity function 
#' @param t time scalar, vector, or matrix
#' @param lambda constant intensity parameter
#' @return scalar, vector, or matrix -- intensity function
lambda_constant <- function(t, lambda) {
  t*0+lambda
}

#' Vectorized Weibull intensity function 
#' Parameterization 1 for the Weibull distribution -- as used by R
#'   f(x) = shape / scale * (x / scale)^(shape - 1) * exp(-(x / scale)^shape)
#' Then then intensity function is 
#'   lambda(t) = 
#' and the cumulative intensity function is 
#'   Lambda(t) = (t/scale)^shape
#' and the inverse cumulative intensity function is 
#'   Lambda_inv(z) = scale * z^(1/shape)
#' @param t time scalar, vector, or matrix
#' @param shape shape parameter (parameterization 1)
#' @param scale scale parameter 
#' @return scalar, vector, or matrix -- intensity function
lambda_weibull <- function(t, shape, scale) {
  shape / scale * (t/scale)^(shape - 1)
}

#' Vectorized Weibull cumulative intensity function 
#' Parameterization 1 for the Weibull distribution -- as used by R
#'   f(x) = shape / scale * (x / scale)^(shape - 1) * exp(-(x / scale)^shape)
#' Then then intensity function is 
#'   lambda(t) = 
#' and the cumulative intensity function is 
#'   Lambda(t) = (t/scale)^shape
#' and the inverse cumulative intensity function is 
#'   Lambda_inv(z) = scale * z^(1/shape)
#' @param t time scalar, vector, or matrix
#' @param shape shape parameter (parameterization 1)
#' @param scale scale parameter 
#' @return scalar, vector, or matrix -- intensity function
Lambda_weibull <- function(t, shape, scale) {
  # Lambda(t)=−log(1−F(t)), with F the CDF, so you can get the same with
  #-pweibull(t, shape, scale, lower = FALSE, log = TRUE)
  (t/scale)^shape
}

#' Vectorized Weibull inverse cumulative intensity function 
#' Parameterization 1 for the Weibull distribution -- as used by R
#'   f(x) = shape / scale * (x / scale)^(shape - 1) * exp(-(x / scale)^shape)
#' Then then intensity function is 
#'   lambda(t) = 
#' and the cumulative intensity function is 
#'   Lambda(t) = (t/scale)^shape
#' and the inverse cumulative intensity function is 
#'   Lambda_inv(z) = scale * z^(1/shape)
#' @param t time scalar, vector, or matrix
#' @param shape shape parameter (parameterization 1)
#' @param scale scale parameter 
#' @return scalar, vector, or matrix -- intensity function
Lambda_inv_weibull <- function(z, shape, scale) {
  scale * z^(1/shape)
}
