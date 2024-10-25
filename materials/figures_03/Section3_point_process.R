# 
library(data.table)
library(nhppp)
library(ggplot2)

set.seed(20241011)


##################  lambda   #######################

l <- function(t) {
  0.5*(cos(t) + cos(0.9*t)) + 1
}



