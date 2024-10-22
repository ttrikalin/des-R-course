library(data.table)
source("01_constant_and_weibull_intensity_functions.R")
source("02_all_cause_mortality_function.R")
source("03_toxin_exposure_functions.R")
source("10_model_class.R")
set.seed(2024)
N <- 10^5
pop <- data.table(id  = 1:N, 
                  birth_cohort = 2015,
                  spawn_age = 40, 
                  max_simulation_age = 110, 
                  sex = sample(c("male", "female"), N, replace = TRUE),
                  param_cancer_emergence_shape = runif(N, 7, 9), 
                  param_cancer_emergence_scale = rnorm(N, 150, 20), 
                  param_cancer_death_rate = runif(N, 0.08, 1.5),
                  param_cancer_dx_rate = runif(N, .20, .27), 
                  param_toxin_exposure_irr = pmax(0.005, rnorm(N, 0.01, 0.005))
                  )
P <- ToyModel$new(population = pop, 
              annual_mortality_rates = readRDS("../data/annual_mortality_rates.rds"))

set.seed(20241027)
tictoc::tic()
P$update(with_risk_factors = FALSE, with_nhppp_package = FALSE)
tictoc::toc()
res_no_toxin_no_nhppp <- P$getPopulation(copy = TRUE)

P$reset()
set.seed(20241027)
tictoc::tic()
P$update(with_risk_factors = FALSE, with_nhppp_package = TRUE)
tictoc::toc()
res_no_toxin_nhppp <- P$getPopulation(copy = TRUE)

P$reset()
set.seed(20241027)
tictoc::tic()
P$update(with_risk_factors = TRUE, with_nhppp_package = TRUE)
tictoc::toc()
res_toxin <- P$getPopulation(copy = TRUE)

