## ----include = FALSE----------------------------------------------------------
set.seed(20241017)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
K <- 10^4

## ----setup--------------------------------------------------------------------
library(data.table)
library(nhppp)

## ----pop----------------------------------------------------------------------
pop <- data.table(
  id = 1:K,
  birth_cohort = 2015,
  spawn_age = 40,
  max_simulation_age = 110,
  sex = sample(c("male", "female"), K, replace = TRUE)
)

## It would make sense to execute the commented-out code now.
## It generates model parameters used in later stages.
## For expository clarity, we generate each parameter when it is introduced
# pop[, `:=`(
#    param_cancer_emergence_shape = runif(.N, 7, 9),
#    param_cancer_emergence_scale = rnorm(.N, 150, 20),
#    param_toxin_exposure_diff = pmax(0.005, rnorm(.N, 0.01, 0.005)),
#    param_cancer_death_intercept := rnorm(.N, -2, 0.5),
#    param_cancer_death_slope := runif(n= .N, min = 0, max = 0.003),
#    param_clinical_cancer_dx_rate := runif(.N, 0.20, 0.27)
#  )]

## ----rho----------------------------------------------------------------------
annual_mortality_rates_2015[
  sex %in% c("male", "female"),
  c(1:5, 111:113)
]

## ----death_other_causes_intensity_matrix--------------------------------------
rhos <- annual_mortality_rates_2015[
  pop,
  on = c("birth_cohort", "sex")
]
setindex(rhos, "id")
rho_matrix <- as.matrix(rhos[, c(paste0("age_", 0:109), "age_110+"),
  with = FALSE
])

rm(list = "rhos") # cleanup

## ----age_dead_other_causes----------------------------------------------------
pop[
  ,
  age_dead_from_other_causes :=
    nhppp::vdraw_sc_step_regular(
      lambda_matrix = rho_matrix,
      rate_matrix_t_min = 0,
      rate_matrix_t_max = 110,
      t_min = pop$spawn_age, # 40
      t_max = pop$max_simulation_age, # 110
      atmost1 = TRUE,
      atleast1 = TRUE
    )
]

## ----xi_params----------------------------------------------------------------
pop[, `:=`(
  exposure_start_age = max_simulation_age,
  exposure_stop_age = max_simulation_age,
  maximum_exposure = 0
)][
  ,
  will_start_exposure := runif(.N) < 0.20
][
  will_start_exposure == TRUE,
  will_stop_exposure := runif(.N) < 0.60
][
  will_start_exposure == TRUE,
  exposure_start_age := pmin(runif(.N, 12, 35), age_dead_from_other_causes)
][
  will_stop_exposure == TRUE,
  exposure_stop_age := pmin(
    exposure_start_age + runif(.N, 1, 35),
    age_dead_from_other_causes
  )
][
  will_start_exposure == TRUE,
  maximum_exposure := runif(.N, 1 / 5, 1)
]

# cleanup
pop[, will_start_exposure := NULL][, will_stop_exposure := NULL]

## ----xi-----------------------------------------------------------------------
xi <- toxin_exposure <- function(t, max_exposure, start_age, stop_age) {
  (start_age <= t) * (stop_age >= 1) * max_exposure * (1 / 2 + (cos(t / 2) + cos(0.9 * t / 2)) / 4)
}

## ----params_cancer_generation-------------------------------------------------
pop[, `:=`(
  param_cancer_emergence_shape = runif(.N, 7, 9),
  param_cancer_emergence_scale = rnorm(.N, 150, 20)
)]

## ----delta_k------------------------------------------------------------------
pop[, param_toxin_exposure_diff := pmax(0, rnorm(.N, 0.01, 0.005))]

## ----cancer_generation--------------------------------------------------------
lambda <- function(t, P = pop, ...) {
  # non-risk factor part: shape / scale * (t/scale)^(shape - 1)
  (P$param_cancer_emergence_shape / P$param_cancer_emergence_scale) *
    (t / P$param_cancer_emergence_scale)^(P$param_cancer_emergence_shape - 1) +
    # risk factor (toxin exposure) part: delta_k * xi(t)
    P$param_toxin_exposure_diff *
      xi(
        t = t,
        max_exposure = P$maximum_exposure,
        start_age = P$exposure_start_age,
        stop_age = P$exposure_stop_age
      )
}

## ----time breaks--------------------------------------------------------------
# define interval bounds for the step function, one row per person
M <- 10
time_breaks <- matrix(
  data = rep(x = seq(from = 40, to = 110, length.out = M + 1), each = K),
  byrow = FALSE,
  nrow = K
)

time_breaks[1:3, ]

## ----lambda star--------------------------------------------------------------
lambda_star <- nhppp::get_step_majorizer(
  fun = lambda,
  breaks = time_breaks,
  is_monotone = FALSE,
  K = 1.9 / 4 # This is the maximum slope of xi() -- which you get with some calculus
)

lambda_star[1:3, ]

## ----lambda-------------------------------------------------------------------
pop[
  ,
  age_cancer_emergence := nhppp::vdraw_intensity(
    lambda = lambda,
    lambda_maj_matrix = lambda_star,
    rate_matrix_t_min = 40,
    rate_matrix_t_max = 110,
    t_min = pop$spawn_age,
    t_max = pmin(pop$age_dead_from_other_causes, 110, na.rm = TRUE),
    atmost1 = TRUE
  )
][
  ,
  with_cancer := !is.na(age_cancer_emergence),
]

## ----params_cancer_death------------------------------------------------------
pop[, param_cancer_death_intercept := rnorm(.N, -3, 0.2)]
pop[, param_cancer_death_slope := runif(.N, 0, 0.003)]

## ----Nu-----------------------------------------------------------------------
Nu <- function(t, Lambda_args = list(population), ...) {
  P <- Lambda_args$population
  (
    exp(P$param_cancer_death_intercept + P$param_cancer_death_slope * t) -
      exp(P$param_cancer_death_intercept)
  ) / P$param_cancer_death_slope
}

## ----Nu_inv-------------------------------------------------------------------
Nu_inv <- function(z, Lambda_inv_args = list(population), ...) {
  P <- Lambda_inv_args$population
  (
    log(P$param_cancer_death_slope * z +
      exp(P$param_cancer_death_intercept)) -
      P$param_cancer_death_intercept
  ) / P$param_cancer_death_slope
}

## ----age_dead_cancer_causes---------------------------------------------------
args_list <- list(population = pop[!is.na(age_cancer_emergence), ])
pop[
  !is.na(age_cancer_emergence),
  age_dead_from_cancer_causes := nhppp::vdraw_cumulative_intensity(
    Lambda = Nu,
    Lambda_args = args_list,
    Lambda_inv = Nu_inv,
    Lambda_inv_args = args_list,
    t_min = pop[!is.na(age_cancer_emergence), age_cancer_emergence],
    t_max = pop[!is.na(age_cancer_emergence), age_dead_from_other_causes],
    atmost1 = TRUE
  )
]

rm(list = "args_list") # cleanup

## ----age_dead-----------------------------------------------------------------
pop[
  ,
  age_dead := pmin(age_dead_from_other_causes,
    age_dead_from_cancer_causes,
    na.rm = TRUE
  )
]

## ----param_cancer_dx_rates----------------------------------------------------
pop[
  !is.na(age_cancer_emergence),
  param_clinical_cancer_dx_rate := runif(.N, 0.20, 0.27)
]

## ----rexp---------------------------------------------------------------------
### Using rexp()
tictoc::tic()
pop[
  !is.na(age_cancer_emergence),
  age_clinical_cancer_dx :=
    age_cancer_emergence +
    rexp(.N, rate = param_clinical_cancer_dx_rate)
]
pop[
  age_clinical_cancer_dx >= age_dead,
  age_clinical_cancer_dx := NA
]
tictoc::toc()

## ----cancer_dx_rates----------------------------------------------------------
tictoc::tic()
mu_mat <- as.matrix(pop[
  !is.na(age_cancer_emergence),
  param_clinical_cancer_dx_rate
])

pop[
  !is.na(age_cancer_emergence),
  age_clinical_cancer_dx := nhppp::vdraw_sc_step_regular(
    lambda_matrix = mu_mat,
    rate_matrix_t_min = pop[!is.na(age_cancer_emergence), age_cancer_emergence],
    rate_matrix_t_max = pop[!is.na(age_cancer_emergence), age_dead],
    atmost1 = TRUE
  )
]
tictoc::toc()

## ----descriptives-------------------------------------------------------------
# pop$age_cancer_emergence |> summary()
summary(pop)

