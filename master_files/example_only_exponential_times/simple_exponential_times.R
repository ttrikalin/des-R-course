########
#######

library(nhppp)

draw_from_constant_intensity <- function(lambda, t_min, t_max, atmost1 = FALSE){
  times <- c()
  current_time  <- t_min
  while(TRUE) {
    new_time <- current_time + stats::rexp(n = 1, rate = lambda)
    if(new_time <= t_max) {
      current_time <- new_time
      times <- c(times, new_time)
    } else {
      break
    }
  }
  if(atmost1 && length(times)!=0) {
    return(times[1])
  }
  return(times)
}


###
###  draw death from other causes 
### 
set.seed(20241026)
age_dead_from_other_causes <- 
  draw_from_constant_intensity(lambda = 1/70, 
                               t_min = 40,
                               t_max = 110, 
                               atmost1 = TRUE)

##  draw tumor emergence  

age_tumor_emergence <- draw_from_constant_intensity(lambda = 1/40, 
                                                    t_min = 40, 
                                                    t_max = age_dead_from_other_causes, 
                                                    atmost1 = TRUE)


age_clinical_dx <- draw_from_constant_intensity(lambda = 1/30, 
                                                t_min = age_tumor_emergence, 
                                                t_max = age_dead_from_other_causes, 
                                                atmost1 = TRUE)


