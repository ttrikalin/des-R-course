# Weibull intensity 
l_weibull <- function(t, shape = 7, scale = 150, ...) {
  shape / scale * (t/scale)^(shape - 1)
}

# for monotone functions, the maximum is at a bound
# -- lower bound for decreasing 
# -- upper bound for increasing 

lower_bounds <- l_weibull(t = 20)
upper_bounds <- l_weibull(t = 110)
majorizer_intercept <- 
  pmax(lower_bounds, upper_bounds)  # `max()` would do too.

# for the interval from 20 to 110
nhppp::draw(
  lambda = l_weibull, 
  line_majorizer_intercept = majorizer_intercept, 
  line_majorizer_slope = 0,
  t_min = 20, 
  t_max = 110, 
  atmost1 = TRUE, 
  atleast1 = TRUE)   

# let's find a piecewise constant majorizer over M=5 intervals
time_breaks <- seq(from = 20, to = 110, length.out = 5+1)
lower_bounds <- l_weibull(time_breaks[1:5])
upper_bounds <- l_weibull(time_breaks[2:6])

l_star_vector <- 
  pmax(lower_bounds, upper_bounds) # `max()` won't do

nhppp::draw(
  lambda = l_weibull, 
  step_majorizer_vector = l_star_vector,
  t_min = 20, 
  t_max = 110, 
  atmost1 = TRUE, 
  atleast1 = TRUE)   

# you can use a helper function instead
nhppp::get_step_majorizer(
  fun = l_weibull,
  breaks = time_breaks, 
  is_monotone = TRUE
)

######################################################
# non-monotone functions are a bit more difficult 
# the maximum of an interval is not always at the bounds 
l_beat <- function(t, max_exposure = 1, start_age = 20, stop_age = 110, ...) {
  (t>=start_age) * 
    (t<=stop_age) * 
    max_exposure * 
    (0.5 + (cos( (t+50) /2) + cos(0.9* (t+50) /2))/4)
}

# This is a Lipschitz function. 
# If you can find a bound for how fast if grows (cone coefficient)
# you can always find an upper bound over the interval
# max(upper_bound, lower_bound) + K * (t_max - t_min) / 2
# The amplitude of the derivative of l_beat over time is 
# a bound for K -- the rest is sin() functions (always <=1)


