library(data.table)
library(nhppp)


set.seed(123)
shape <- 7 
scale <- 150  
N <- 10^5

dat <- data.table(id  = 1:N)
dat[, start := 40][, stop := 100][, shape := shape][, scale := scale]



#### do it from R built-in 
tictoc::tic()
dat[, 
    t_R := rweibull(n = .N, shape = shape, scale= scale)
]
dat[t_R < start | t_R > stop, t_R := NA]
dat[!is.na(t_R)]
tictoc::toc()

#### do it from NHPPP
lambda_weibull <- function(t, shape = dat$shape, scale = dat$scale, ...) {
  shape / scale * (t/scale)^(shape - 1)
}

Lambda_weibull <- function(t, shape = dat$shape, scale = dat$scale, ...) {
  (t/scale)^shape
}

Lambda_inv_weibull <- function(z, shape = dat$shape, scale = dat$scale,...) {
  scale * z^(1/shape)
}


tictoc::tic()
dat[, 
  t_nhppp := nhppp::vdraw(
      Lambda = Lambda_weibull, 
      Lambda_inv = Lambda_inv_weibull, 
      t_min = dat$start, 
      t_max = dat$stop, 
      atmost1 = TRUE
  )
]
tictoc::toc()
