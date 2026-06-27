#' `ToyModel class`
#'
#' @description
ToyModel <- R6::R6Class(
  classname = "ToyModel",
  public = list(
    #' @description
    #' @param population data.table
    initialize = function(population, annual_mortality_rates) {
      private$.setPopulation(population = population, copy = TRUE)
      private$.setAnnualMortalityRates(annual_mortality_rates = annual_mortality_rates)
      private$.simulation_erased <- TRUE
      invisible(self)
    }, 

    update = function(with_risk_factors, with_nhppp_package) {
      stopifnot(private$.simulation_erased)
      private$.generate_death_from_other_causes()
      private$.generate_toxin_Hx()
      private$.generate_cancers(with_risk_factors = with_risk_factors, with_nhppp_package = with_nhppp_package)
      private$.generate_death_from_cancer_causes()
      private$.simulation_erased <- FALSE 
      invisible(self)
    }, 

    reset = function() {
      for (v in c("age_dead_from_other_causes", "age_cancer_emergence", 
                  "with_cancer", "age_dead_from_cancer_causes", "age_dead", 
                  "toxin_start_age", "toxin_stop_age", "maximum_toxin_exposure")) {
        private$.population[, (v) := NULL]
      }  
      private$.simulation_erased <- TRUE
      invisible(self)
    },
    
    getPopulation = function(copy = TRUE) {
      if (copy) {
        return(copy(private$.population))
      } else {
        return(private$.population)
      }
    }, 
    
    getToxinExposureAtAge = function(t) {
      generate_toxin_exposure(
        t=t, 
        max_exposure = private$.population$maximum_toxin_exposure, 
        start_age = private$.population$toxin_start_age, 
        stop_age = private$.population$toxin_stop_age
        )
    }
   
  ), 
  private = list(
    .population = NULL, 
    .annual_mortality_rates = NULL,
    .simulation_erased = NULL,

    .setPopulation = function(population, copy) {
      stopifnot(is.data.table(population))
      stopifnot(all(c("id", "sex", "birth_cohort", "spawn_age", "max_simulation_age") %in% names(population)))
      if(copy) {
        private$.population <- copy(population)
      } else {
        private$.population <- population 
      }
      setindex(private$.population, id)
      setindex(private$.population, sex, birth_cohort, id)
      invisible(self)
    },

    .setAnnualMortalityRates  = function(annual_mortality_rates) {
      stopifnot(is.data.table(annual_mortality_rates))
      stopifnot(all(c("sex", "birth_cohort") %in% names(annual_mortality_rates)))
      private$.annual_mortality_rates <- annual_mortality_rates
      setindex(private$.annual_mortality_rates, sex, birth_cohort)
      invisible(self)
    },
    
    .generate_toxin_Hx = function(){
      private$.population[, `:=`(
          toxin_start_age = max_simulation_age, 
          toxin_stop_age = max_simulation_age, 
          maximum_toxin_exposure = 0)   
      ][, 
        ever_smoker := runif(.N) < 0.20  
      ][ever_smoker == TRUE, 
        toxin_quitter := runif(.N) < 0.60  
      ][ever_smoker == TRUE, 
        toxin_start_age := pmin(runif(.N, 12, 35), age_dead_from_other_causes)
      ][toxin_quitter == TRUE, 
        toxin_stop_age := pmin(toxin_start_age + runif(.N, 1, 35), age_dead_from_other_causes)
      ][ever_smoker == TRUE, 
        maximum_toxin_exposure := runif(.N, 1/5, 1) 
      ][, ever_smoker := NULL][, toxin_quitter := NULL ]
      
      invisible(self)
    },

    .generate_death_from_other_causes = function() {
      private$.population[order(id)]
      private$.population[, 
        age_dead_from_other_causes := generate_death_from_other_causes(
          population = private$.population, 
          annual_mortality_rates = private$.annual_mortality_rates, 
          rate_matrix_t_min = 0, 
          rate_matrix_t_max = 110,
          t_min = private$.population[, spawn_age],
          t_max = private$.population[, max_simulation_age]
        )
      ]
      invisible(self)
    }, 
    
    .generate_death_from_cancer_causes = function() {
      private$.population[
        !is.na(age_cancer_emergence), 
        age_dead_from_cancer_causes := 
          age_cancer_emergence + 
          rexp(.N, param_cancer_death_rate)
        ][
          !is.na(age_cancer_emergence), 
          age_dead_from_cancer_causes := 
            fifelse(age_dead_from_cancer_causes < age_dead_from_other_causes, 
                    age_dead_from_cancer_causes, 
                    NA)
        ][, 
          age_dead := pmin(age_dead_from_other_causes, age_dead_from_cancer_causes, na.rm =TRUE)
        ]
      invisible(self)
    },
    
    .generate_cancer_diagnosis = function() {
      private$.population[
        !is.na(age_cancer_emergence), 
        age_cancer_dx := 
          age_cancer_emergence + 
          rexp(.N, param_cancer_dx_rate)
      ][
        !is.na(age_cancer_emergence), 
        age_cancer_dx := fifelse(age_cancer_dx<age_dead, age_cancer_dx, NA)
      ]
      invisible(self)
    },

    .generate_cancers = function(with_risk_factors, with_nhppp_package) {
      if(!with_risk_factors && !with_nhppp_package) {
        private$.generate_cancers_no_risk_factors_built_in_weibull()
      }
      if(!with_risk_factors && with_nhppp_package) {
        private$.generate_cancers_no_risk_factors_nhppp()
      }
      if(with_risk_factors && with_nhppp_package) {
        private$.generate_cancers_with_toxin_nhppp()
      }
      if(with_risk_factors && !with_nhppp_package) {
        stop("Cancer generation with risk factors does not use the built-in Weibull implementation.")
      }
      private$.population[, with_cancer := !is.na(age_cancer_emergence)]
      setindex(private$.population, with_cancer)
      invisible(self)
    },
    
    .generate_cancers_no_risk_factors_built_in_weibull = function() {
      private$.population[, 
        dt:= rweibull(n=nrow(private$.population),
                      scale = param_cancer_emergence_scale, 
                      shape = param_cancer_emergence_shape) 
      ][, 
        age_cancer_emergence := fifelse(
          dt < age_dead_from_other_causes & dt > spawn_age, 
          dt, 
          NA
        )
      ][, 
        dt := NULL
      ]
      invisible(self)
    }, 
    
    .generate_cancers_no_risk_factors_nhppp = function() {
       params <- private$.population[, .(param_cancer_emergence_shape, param_cancer_emergence_scale)]
      
       l_ <- function(t, lambda_args = params){
         lambda_weibull(t, 
                        scale = lambda_args[,param_cancer_emergence_scale], 
                        shape = lambda_args[,param_cancer_emergence_shape])
       }
      
       range_t <- c(min(private$.population[,spawn_age]), max(private$.population[,max_simulation_age]))
       N <- nrow(private$.population)
       time_breaks <- matrix(
        data = rep(x = seq(from = range_t[1], to = range_t[2], length.out = 11), each = N), 
        byrow = FALSE, 
        nrow = N)
      
       lambda_maj_matrix <- nhppp::get_step_majorizer(
         fun = l_, 
         breaks = time_breaks, 
         is_monotone = TRUE
       )
      
       private$.population[, 
         age_cancer_emergence := 
           nhppp::vdraw_intensity(
             lambda = l_, 
             lambda_args = params,
             lambda_maj_matrix = lambda_maj_matrix,
             rate_matrix_t_min = range_t[1],
             rate_matrix_t_max = range_t[2],
             t_min = private$.population[, spawn_age],
             t_max = private$.population[, age_dead_from_other_causes],
             atmost1 = TRUE
          )]
       invisible(self)
    }, 
    
    .generate_cancers_with_toxin_nhppp = function() {
      params <- private$.population[, 
        .(param_cancer_emergence_shape, param_cancer_emergence_scale, 
          param_toxin_exposure_irr )]
      
      l_ <- function(t, lambda_args = params){
        return(
          lambda_weibull(t, 
                       scale = lambda_args[,param_cancer_emergence_scale], 
                       shape = lambda_args[,param_cancer_emergence_shape]) + 
          lambda_args[,param_toxin_exposure_irr] * self$getToxinExposureAtAge(t)
        )
      }
      
      range_t <- c(min(private$.population[,spawn_age]), max(private$.population[,max_simulation_age]))
      N <- nrow(private$.population)
      time_breaks <- matrix(
        data = rep(x = seq(from = range_t[1], to = range_t[2], length.out = 51), each = N), 
        byrow = FALSE, 
        nrow = N)
      
      lambda_maj_matrix <- nhppp::get_step_majorizer(
        fun = l_, 
        breaks = time_breaks, 
        is_monotone = FALSE, 
        # based on the toxin_exposure functions for max_exposure = 1
        K = max(private$.population$param_toxin_exposure_irr, na.rm = TRUE) * 1.9/4 
      )
      
      private$.population[, 
                          age_cancer_emergence := 
                            nhppp::vdraw_intensity(
                              lambda = l_, 
                              lambda_args = params,
                              lambda_maj_matrix = lambda_maj_matrix,
                              rate_matrix_t_min = range_t[1],
                              rate_matrix_t_max = range_t[2],
                              t_min = private$.population[,spawn_age],
                              t_max = private$.population[,age_dead_from_other_causes],
                              atmost1 = TRUE
                            )]
      invisible(self)
    }
  )
)


