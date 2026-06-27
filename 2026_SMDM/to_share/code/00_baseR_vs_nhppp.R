### Let's compare key tasks when simulating event times in R
### We will use a Weibull so that the intensity function changes over time
### and some complexities will appear.

### The base `R` code is indicative; the `nhppp` code uses nonvectorized functions

### 1. Sample the time of a first event (if any) [RED PROCESS]
###    -- in interval (20, 110]

### 2. Sample the time of exactly one event [BLACK PROCESS]
###    -- in interval (20, 40]

### 3. Sample the time of all events (if any) [BLUE PROCESS]
###    -- in interval (20, 200]

### 4. Sample the times of all events (>=1) [BLUE PROCESS variant]
###    -- in interval (20, 30]

### Setup
library(data.table)
library(nhppp)
library(ggplot2)
set.seed(123)

baseplot <- ggplot() + xlim(0, 120) + xlab("time")


########################################################
# Weibull intensity
########################################################
l_weibull <- function(t, shape = 7, scale = 150, ...) {
  shape / scale * (t / scale)^(shape - 1)
}

baseplot +
  ylim(0, 0.01) +
  ylab("intensity") +
  ggtitle("Weibull") +
  stat_function(fun = l_weibull, col = "red", xlim = c(20, 110))

L_weibull <- function(t, shape = 7, scale = 150, ...) {
  (t / scale)^shape
}

baseplot +
  ylim(0, 0.15) +
  ylab("cumulative intensity") +
  ggtitle("Weibull") +
  stat_function(fun = L_weibull, col = "red", xlim = c(20, 110))

Li_weibull <- function(z, shape = 7, scale = 150, ...) {
  scale * z^(1 / shape)
}

################################################################
### 1. Sample the time of a first event (if any)
###    -- in interval (20, 110]
###    [RED PROCESS]
################################################################
# base R
tR <- rweibull(n = 1, shape = 7, scale = 150)
tR
tR <- if (tR > 20 & tR <= 110) tR else NA
tR

# nhppp, using the
# intensity (l_*) and
# a constant linear majorizer
draw(
  lambda = l_weibull,
  line_majorizer_intercept = l_weibull(t = 110),
  line_majorizer_slope = 0,
  t_min = 20,
  t_max = 110,
  atmost1 = TRUE,
  atleast1 = FALSE
)

# nhppp, using
# the cumulative intensity (L_*)
# and its inverse (Li_*)
#  -- same uncerlying code as base R
draw(
  Lambda = L_weibull,
  Lambda_inv = Li_weibull,
  t_min = 20,
  t_max = 110,
  atmost1 = TRUE,
  atleast1 = FALSE
)

################################################################
### 2. Sample the time of exactly one event
###    -- in interval (20, 40]
###    [BLACK PROCESS]
################################################################
# base R
tR <- NA
counter <- 0
while (is.na(tR)) {
  tR <- rweibull(n = 1, shape = 7, scale = 150)
  tR <- if (tR > 20 & tR <= 40) tR else NA
  counter <- counter + 1
}
tR
counter

# nhppp, using the
# intensity (l_*) and
# a constant linear majorizer
draw(
  lambda = l_weibull,
  line_majorizer_intercept = l_weibull(t = 40),
  line_majorizer_slope = 0,
  t_min = 20,
  t_max = 40,
  atmost1 = TRUE,
  atleast1 = TRUE
)

############################################################
### 3. Sample the time of all events (if any)
###    -- in interval (20, 200]
###    [BLUE PROCESS]
############################################################
# base R code
# out of luck -- you have to implement one of the sampling
# algorithms!
# e.g., the ordered-statistics algorithm

# nhppp, using the
# the cumulative intensity (L_*)
# and its inverse (Li_*) -- to switch it up a bit
draw(
  Lambda = L_weibull,
  Lambda_inv = Li_weibull,
  t_min = 20,
  t_max = 200,
  atmost1 = FALSE,
  atleast1 = FALSE
)


############################################################
### 4. Sample the time of all events (>=1)
###    -- in interval (20, 40]
###    [BLUE PROCESS variant]
############################################################
# base R code
# again, out of luck -- you have to implement one of the sampling
# algorithms!
# e.g., the ordered-statistics algorithm

# nhppp, using the
# the cumulative intensity (L_*)
# and its inverse (Li_*) -- to switch it up a bit
draw(
  Lambda = L_weibull,
  Lambda_inv = Li_weibull,
  t_min = 20,
  t_max = 40,
  atmost1 = FALSE,
  atleast1 = TRUE
)
