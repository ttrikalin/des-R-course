
# Same EXAMPLE as "DES_xmpl_1_SC.R" but here the simulation funciton 
#  also returns a SEED for creating examples.

# Simple DES examples with constant rates # 

# Start at age = 40 yrs (cancer-free)

#Simulate:
#   Time to death from other causes                   (risk l1)
#   Time to death from cancer given lesion emergence  (risk l2)
#   Time to lesions                                   (risk l3)

#   Each event has a different (but constant over time) risk;
#   HPPP, Exponential distribution for times.



sim.Ts.seed <- function(l1, l2, l3){
  # l1 : risk lambda(t) for death from other causes
  # l2 : risk lambda(t) for death from cancer
  # l3 : risk lambda(t) for lesions
  seed <- round(100000*runif(1)) 
  set.seed(seed)
  ### black process: exactly 1 event
  # Death from other causes ''t.dO
  l_constant.dO <- function(t, lambda = l1, ... ) t*0 + lambda
  t.dO <- draw(lambda = l_constant.dO, 
               line_majorizer_intercept = l1, 
               line_majorizer_slope = 0,
               t_min = 40, 
               t_max = 110, 
               atmost1 = TRUE, 
               atleast1 = TRUE)   
  
  ### blue process: 0, 1, or more events 
  # Time to lesions
  l_constant.lesions <- function(t, lambda = l3, ... ) t*0 + lambda
  t.lesions <- draw(lambda = l_constant.lesions, 
                    line_majorizer_intercept = l3, 
                    line_majorizer_slope = 0,
                    t_min = 40, 
                    t_max = 110, 
                    atmost1 = FALSE, 
                    atleast1 = FALSE)   
  
  
  ### red process: 0, or 1 events 
  # Death from cancer
  if(length(t.lesions)!=0){
    l_constant.dcancer <- function(t, lambda = l2, ... ) t*0 + lambda
    t.dcancer <- draw(lambda = l_constant.dcancer, 
                      line_majorizer_intercept = l2, 
                      line_majorizer_slope = 0,
                      t_min = t.lesions[length(t.lesions)], 
                      t_max = 110, 
                      atmost1 = TRUE, 
                      atleast1 = FALSE)   
  } else t.dcancer = NULL
  
  
  t.res <- list("T.death.other"=t.dO, "T.death.cancer"=t.dcancer, "T.lesions"=t.lesions, "seed"=seed)
  return(t.res)
}







# --------------------------------------------------- #
# FUNCTION plot.times:  Plot Simulated Times to event #
# --------------------------------------------------- #
plot.times <- function(T){
  xx <- seq(0,110, by=5)  # xaxis
  par(oma=c(0,5.7,0,0))
  plot(x=xx, y=seq(0, length(TTs)+1, length.out=length(xx)), pch=19, col="white",
       ylab="", xlab="Time", yaxt="n")
  # T death from other causes:
  points(TTs$T.death.other, rep(3,1) , xlab = "Time t", pch=124, cex=2)
  lines(c(40,TTs$T.death.other), rep(3,length(TTs$T.death.other)+1), lwd=2)
  abline(v=TTs$T.death.other, lty=2)
  # T death from cancer:
  points(TTs$T.death.cancer, rep(2,length(TTs$T.death.cancer)) , xlab = "Time t", pch=124, cex=2, col="red")
  lines(c(40,TTs$T.death.cancer), rep(2,length(TTs$T.death.cancer)+1), lwd=2, col="red")
  if(length(TTs$T.death.cancer) !=0 ) abline(v=TTs$T.death.cancer, lty=3, col="red")
  # T tumors:
  points(TTs$T.lesions, rep(1,length(TTs$T.lesions)) , xlab = "Time t", pch=124, cex=2, col="blue")
  lines(c(40,TTs$T.lesions), rep(1,length(TTs$T.lesions)+1), lwd=2, col="blue")
  axis(side=2, at=seq(1,length(TTs)-1,1), labels = c("Lesions", "Death from Cancer", "Death from other causes"), las=2, outer = T, line=-0.5, tick=F)
  title(main = "Simulating a single person")
}






#######################
# Examples

TTs <- sim.Ts.seed(l1=1/55, l2=1/15, l3=1/50)
TTs
plot.times(TTs)

# 9437
# 2012
# 26180
# 736
# 42512




