#### The nhppp package syntax for a single person
library(nhppp)
library(ggplot2)


# single person
# constant intensity = 1/70 
# in the interval 20 to 110
baseplot <- ggplot() + xlim(0, 120) + xlab("time") 

########################################################
# Constant intensity 
########################################################

# note the vectorized lambda
l_constant <- function(t, lambda = 1/70, ... ) t*0 + lambda


baseplot + ylim(0, 0.02) + ylab("intensity") + ggtitle("Constant") +
  stat_function(fun = l_constant, col = "red", xlim = c(20, 110))


# red process: 0, or 1 events 
draw(lambda = l_constant, 
     line_majorizer_intercept = 1/70, 
     line_majorizer_slope = 0,
     t_min = 20, 
     t_max = 110, 
     atmost1 = TRUE, 
     atleast1 = FALSE)   

# black process: exactly 1 event
draw(lambda = l_constant, 
     line_majorizer_intercept = 1/70, 
     line_majorizer_slope = 0,
     t_min = 20, 
     t_max = 110, 
     atmost1 = TRUE, 
     atleast1 = TRUE)   

# blue process: 0, 1, or more events 
draw(lambda = l_constant, 
     line_majorizer_intercept = 1/70, 
     line_majorizer_slope = 0,
     t_min = 20, 
     t_max = 110, 
     atmost1 = FALSE, 
     atleast1 = FALSE)   

########################################################
# Weibull intensity 
########################################################
l_weibull <- function(t, shape = 7, scale = 150, ...) {
  shape / scale * (t/scale)^(shape - 1)
}

baseplot + ylim(0, 0.01) + ylab("intensity") +  ggtitle("Weibull") + 
  stat_function(fun = l_weibull, col = "red", xlim = c(20, 110))

L_weibull <- function(t, shape = 7, scale = 150, ...) {
   (t/scale)^shape
}

baseplot + ylim(0, 0.15) + ylab("cumulative intensity") +  ggtitle("Weibull") + 
  stat_function(fun = L_weibull, col = "red", xlim = c(20, 110))

Li_weibull <- function(z, shape = 7, scale = 150, ... ) {
  scale * z^(1/shape)
}



# as above, using the intensity (l_*) and a constant linear majorizer
# black process: exactly 1 event
draw(lambda = l_weibull, 
     line_majorizer_intercept = l_weibull(t = 110), 
     line_majorizer_slope = 0,
     t_min = 20, 
     t_max = 110, 
     atmost1 = TRUE, 
     atleast1 = TRUE)   

# as above, using the cumulative intensity (L_*) and its inverse (Li_*)
# black process: exactly 1 event
draw(Lambda = L_weibull, 
     Lambda_inv = Li_weibull, 
     t_min = 20, 
     t_max = 110, 
     atmost1 = TRUE, 
     atleast1 = TRUE)   

########################################################
# wildly varying intensity -- a beat-like variation 
########################################################
l_beat <- function(t, max_exposure = 1, start_age = 20, stop_age = 110, ...) {
  (t>=start_age) * 
  (t<=stop_age) * 
  max_exposure * 
  (0.5 + (cos( (t+50) /2) + cos(0.9* (t+50) /2))/4)
}

# We can get a cumulative intensity (L_*), but not a cheap inverse. 
# So lets focus on intensity sampling. 
# We need to do some work to find a majorizer
baseplot + ylim(0, 1) + ylab("intensity") +  ggtitle("Beat") + 
  stat_function(fun = l_beat, col = "red", xlim = c(20, 110), n=300)


# black process: exactly 1 event 
draw(lambda = l_beat, 
     line_majorizer_intercept = 1, 
     line_majorizer_slope = 0,
     t_min = 20, 
     t_max = 110, 
     atmost1 = TRUE, 
     atleast1 = TRUE)   

# blue process: 0, 1, or more events 
draw(lambda = l_beat, 
     line_majorizer_intercept = 1, 
     line_majorizer_slope = 0,
     t_min = 20, 
     t_max = 110, 
     atmost1 = FALSE, 
     atleast1 = FALSE)   


