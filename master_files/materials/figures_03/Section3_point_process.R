# 
library(data.table)
library(nhppp)
library(ggplot2)

set.seed(20241011)


p <- ggplot() +
  theme_bw() + 
  xlab("time")+
  ylab("intensity") + 
  theme(axis.text.x = element_text(size = 14),       
        axis.title.x = element_text(size = 18))  +  
   theme(axis.title.y = element_text(size = 18), 
         axis.text.y = element_text(size = 14)) +
  theme(plot.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank() )+
  theme(panel.border= element_blank())+
  theme(axis.line.x = element_line(color="black")) 


l <- function(t, alpha = -1, beta = 0.5, ...) {
  exp(alpha + beta * t)
}

L <- function(t, alpha = -1, beta = 0.5, ...) {
  (exp(alpha + beta * t) - exp(alpha)) / beta
}

Li <- function(z, alpha = -1, beta = 0.5, ...) {
  (log(beta * z + exp(alpha)) - alpha) / beta
}

l2 <- function(t, ...) {
  0.5*(sin(20*t) + sin(18*t)) + 1
}



##################  lambda   #######################


p + xlim(0, 5) + ylim(0, 5) + 
  stat_function(fun = l, color = "red", linewidth = 1, n=300)
if(T) ggsave("01_sampling.pdf", width = 5, height = 3)


p + xlim(0, 5) + ylim(0, 5) + 
  stat_function(fun = l, color = "red", linewidth = 1, n=300) + 
  geom_segment(aes(x = 0, xend = 5, y =5, yend = 5), color = "blue", linewidth = 1)
if(T) ggsave("02_sampling.pdf", width = 5, height = 3)

set.seed(123)
Z <- ppp(rate = 5, t_min =0, t_max =5)
accept <- l(Z)/5 > runif(n= length(Z))
dat <- data.table(Z=Z, accept = accept, y = rep(0, length(Z)), l_star = rep(5, length(Z)))


p + xlim(0, 5) + ylim(0, 5) + 
  stat_function(fun = l, color = "red", linewidth = 1, n=300) + 
  geom_segment(aes(x = 0, xend = 5, y =5, yend = 5), color = "blue", linewidth = 1) + 
  geom_point(data= dat[,], aes(x = Z, y= y+0.1), shape = 1) 
if(T) ggsave("03_sampling.pdf", width = 5, height = 3)


p + xlim(0, 5) + ylim(0, 5) + 
  stat_function(fun = l, color = "red", linewidth = 1, n=300) + 
  geom_segment(aes(x = 0, xend = 5, y =5, yend = 5), color = "blue", linewidth = 1) + 
  geom_point(data= dat, aes(x = Z, y= y), shape = 1) +
  geom_point(data= tail(dat, 1), aes(x = Z, y= y), shape = 0, size=4.5) + 
  geom_point(data= tail(dat,1), aes(x = Z, y= l(Z))) + 
  geom_point(data= tail(dat,1), aes(x = Z, y= l_star))  
if(T) ggsave("04_sampling.pdf", width = 5, height = 3)



p + xlim(0, 5) + ylim(0, 5) + 
  stat_function(fun = l, color = "red", linewidth = 1, n=300) + 
  geom_segment(aes(x = 0, xend = 5, y =5, yend = 5), color = "blue", linewidth = 1) + 
  geom_point(data= dat[accept,], aes(x = Z, y= y)) +
  geom_point(data= dat[!accept,], aes(x = Z, y= y+0.1), shape = 4)  
if(T) ggsave("05_sampling.pdf", width = 5, height = 3)



dat2 <- data.table(x = seq(0, 5, length.out = 1000))
dat2[, upper:= 5][, lower:= l(x)]

p + xlim(0, 5) + ylim(0, 5) +
  geom_ribbon(data = dat2, aes(x= x, ymin = lower, ymax = upper), fill = "blue", alpha = 0.2)+
  stat_function(fun = l, color = "red", linewidth = 1, n=300) + 
  geom_segment(aes(x = 0, xend = 5, y =5, yend = 5), color = "blue", linewidth = 1)
if(T) ggsave("06_sampling.pdf", width = 5, height = 3)
L(5)/25

dat2[, upper2:= l(5)]
p + xlim(0, 5) + ylim(0, 5) +
  geom_ribbon(data = dat2, aes(x= x, ymin = lower, ymax = upper2), fill = "blue", alpha = 0.2)+
  stat_function(fun = l, color = "red", linewidth = 1, n=300) + 
  geom_segment(aes(x = 0, xend = 5, y = l(5), yend = l(5)), color = "blue", linewidth = 1)
if(T) ggsave("07_sampling.pdf", width = 5, height = 3)
L(5)/(l(5)*5)


dat2[, upper3 := l(ceiling(x))]
p + xlim(0, 5) + ylim(0, 5) +
  geom_ribbon(data = dat2, aes(x= x, ymin = lower, ymax = upper3), fill = "blue", alpha = 0.2)+
  stat_function(fun = l, color = "red", linewidth = 1, n=300) + 
  geom_segment(aes(x = 0, xend = 1, y = l(1), yend = l(1)), color = "blue", linewidth = 1) + 
  geom_segment(aes(x = 1, xend = 2, y = l(2), yend = l(2)), color = "blue", linewidth = 1) + 
  geom_segment(aes(x = 2, xend = 3, y = l(3), yend = l(3)), color = "blue", linewidth = 1) + 
  geom_segment(aes(x = 3, xend = 4, y = l(4), yend = l(4)), color = "blue", linewidth = 1) +
  geom_segment(aes(x = 4, xend = 5, y = l(5), yend = l(5)), color = "blue", linewidth = 1)
if(T) ggsave("08_sampling.pdf", width = 5, height = 3)
L(5)/sum(l(1:5))


#####################################
### Time warping 

dat3 <- data.table(x = seq(0, 7, length.out = 1000))
dat3[x<=3, upper1:= ceiling(x)]
dat3[x>3 &x<=3.5, upper1:= 0.5]
dat3[x<=6.25, upper2:= 1][, lower:= 0]
dat3[, y1 := l(x)][, y2 := L(x)][,one := 1]


p + xlim(0, 6.5) + ylim(0, 3) +
  geom_ribbon(data = dat3[x<=1], aes(x=x, ymin=lower, ymax=upper1), fill = "grey", alpha = 0.3)+
  geom_ribbon(data = dat3[x>1&x<=2], aes(x=x, ymin=lower, ymax=upper1), fill = "blue", alpha = 0.3)+
  geom_ribbon(data = dat3[x>2&x<=3], aes(x=x, ymin=lower, ymax=upper1), fill = "red", alpha = 0.3)+
  geom_ribbon(data = dat3[x>3&x<=3.5], aes(x=x, ymin=lower, ymax=upper1), fill = "darkgreen", alpha = 0.3)+
  geom_segment(aes(x = 0, xend = 1, y = 1, yend = 1), color = "red", linewidth = 1) +
  geom_segment(aes(x = 1, xend = 2, y = 2, yend = 2), color = "red", linewidth = 1) + 
  geom_segment(aes(x = 2, xend = 3, y = 3, yend = 3), color = "red", linewidth = 1) + 
  geom_segment(aes(x = 3, xend = 3.5, y = 0.5, yend = 0.5), color = "red", linewidth = 1) 
  
if(T) ggsave("09_sampling.pdf", width = 5, height = 3)

p + xlim(0, 6.5) + ylim(0, 3) + xlab("transformed time") +
  geom_ribbon(data = dat3[x<=1], aes(x=x, ymin=lower, ymax=upper2), fill = "grey", alpha = 0.3)+
  geom_ribbon(data = dat3[x>1&x<=3], aes(x=x, ymin=lower, ymax=upper2), fill = "blue", alpha = 0.3)+
  geom_ribbon(data = dat3[x>3&x<=6], aes(x=x, ymin=lower, ymax=upper2), fill = "red", alpha = 0.3)+
  geom_ribbon(data = dat3[x>6&x<=6.25], aes(x=x, ymin=lower, ymax=upper2), fill = "darkgreen", alpha = 0.3)+
  geom_segment(aes(x = 0, xend = 6.25, y = 1, yend = 1), color = "black", linewidth = 1) 
  
if(T) ggsave("10_sampling.pdf", width = 5, height = 3)






p + xlim(0, 5) + ylim(0, 3) +
  geom_ribbon(data = dat3[x<=1], aes(x=x, ymin=lower, ymax=y1), fill = "grey", alpha = 0.3)+
  geom_ribbon(data = dat3[x>1&x<=2], aes(x=x, ymin=lower, ymax=y1), fill = "blue", alpha = 0.3)+
  geom_ribbon(data = dat3[x>2&x<=3], aes(x=x, ymin=lower, ymax=y1), fill = "red", alpha = 0.3)+
  geom_ribbon(data = dat3[x>3&x<=4], aes(x=x, ymin=lower, ymax=y1), fill = "darkgreen", alpha = 0.3)+
  stat_function(fun  = l, xlim= c(0,4), color = "red", linewidth = 1) + 
  geom_point(aes(x=0, y= l(0))) + 
  geom_point(aes(x=1, y= l(1))) +
  geom_point(aes(x=2, y= l(2))) + 
  geom_point(aes(x=3, y= l(3))) + 
  geom_point(aes(x=4, y= l(4))) 

if(T) ggsave("11_sampling.pdf", width = 5, height = 3)


p + xlim(0, 5) + ylim(0, 3) + xlab("transformed time") +
  geom_ribbon(data = dat3[x<=L(1)], aes(x=x, ymin=lower, ymax=one), fill = "grey", alpha = 0.3)+
  geom_ribbon(data = dat3[x>L(1)&x<=L(2)], aes(x=x, ymin=lower, ymax=one), fill = "blue", alpha = 0.3)+
  geom_ribbon(data = dat3[x>L(2)&x<=L(3)], aes(x=x, ymin=lower, ymax=one), fill = "red", alpha = 0.3)+
  geom_ribbon(data = dat3[x>L(3)&x<=L(4)], aes(x=x, ymin=lower, ymax=one), fill = "darkgreen", alpha = 0.3)+
  stat_function(fun  = function(x) 0*x +1 , xlim= c(0,L(4)), color = "red", linewidth = 1) + 
  geom_point(aes(y=1, x= L(0))) + 
  geom_point(aes(y=1, x= L(1))) +
  geom_point(aes(y=1, x= L(2))) + 
  geom_point(aes(y=1, x= L(3))) + 
  geom_point(aes(y=1, x= L(4))) 

if(T) ggsave("12_sampling.pdf", width = 5, height = 3)




#############################################################
### Composability 


dat2[, zero:= 0]
p + xlim(0, 5) + ylim(0, 5) +
  geom_ribbon(data = dat2, aes(x= x, ymin = lower, ymax = upper), fill = "blue", alpha = 0.2)+
  stat_function(fun = l, color = "red", linewidth = 1, n=300) + 
  geom_ribbon(data = dat2, aes(x= x, ymin = zero, ymax = lower), fill = "red", alpha = 0.2)+
  geom_segment(aes(x = 0, xend = 5, y = 5, yend = 5), color = "blue", linewidth = 1)
if(T) ggsave("13_sampling.pdf", width = 5, height = 3)


p + xlim(0, 5) + ylim(0, 5) +
  stat_function(fun = l, color = "red", linewidth = 1, n=300) + 
  geom_ribbon(data = dat2, aes(x= x, ymin = zero, ymax = lower), fill = "red", alpha = 0.2)
if(T) ggsave("14_sampling.pdf", width = 5, height = 3)

dat2[,ycomp := 5 - l(x)]
p + xlim(0, 5) + ylim(0, 5) +
  stat_function(fun = function(x) 5-l(x), color = "blue", linewidth = 1, n=300) + 
  geom_ribbon(data = dat2, aes(x= x, ymin = zero, ymax = ycomp), fill = "blue", alpha = 0.2)
if(T) ggsave("15_sampling.pdf", width = 5, height = 3)



