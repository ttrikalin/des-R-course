# 
library(data.table)
library(nhppp)
library(ggplot2)

set.seed(20241011)

breaks <- c(0, 40, 110)
base <- ggplot() +
  scale_x_continuous(limits = c(0, 110), breaks =breaks) +
  theme_bw() + 
  xlab("time")+
  theme(axis.text.x = element_text(size = 14),       
        axis.title.x = element_text(size = 18), 
        panel.grid.minor.x = element_blank() )  +  
   theme(axis.title.y = element_blank(), 
         axis.text.y = element_blank(), 
         axis.ticks.y = element_blank())  +
  theme(plot.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank() )+
  theme(panel.border= element_blank())+
  theme(axis.line.x = element_line(color="black")) 
base 
if(FALSE) ggsave("00_des.pdf", width = 5, height = 3)

dat <- data.table(x = 40:110)
dat[,upper := 9][, lower := 0]
p0 <- base + 
  geom_ribbon(data = dat, aes(x = x,  ymin = lower, ymax = upper), fill = "orange", alpha  = .2) 
p0 + ylim(0, 9)
if(FALSE) ggsave("01_des.pdf", width = 5, height = 3)

###############################################################
###############################################################
###############################################################

add_trajectory <- function(p, 
                           y = 1, 
                           start = 40, 
                           stop =110, 
                           point_at_stop = FALSE,
                           col="black", 
                           points = NULL, 
                           text =NULL, 
                           text_dx = 2, 
                           shape = 21) {
 # browser()
  if(is.null(stop)) {
    stop <- 110
    point_at_stop <- FALSE
  }
  if(point_at_stop) {
    points <- sort(unique(c(points, stop)))
    points <- points[points<110]
  }
  res <- p+
    geom_segment(aes(x = start, xend = stop, y = y, yend =y), color = col, linewidth = 2) 
  
  if(!is.null(points)) {
    tmp<- data.frame(x = points, y= rep(y, length(points)))  
    res <- res + 
        geom_point(data = tmp, aes(x = x, y= y), size = 4, color = col, shape = shape, fill = "white")
  }
  if(!is.null(text)){
    res <- res + 
      annotate("text", x = start - text_dx, y =y, label = text, hjust = 1, size =5)  
  }
  return(res)
}
add_doc <- function(p, y = 1, start = 40, stop =110) {
    add_trajectory(p=p, 
                   y = y, 
                   start = start, 
                   stop =stop, 
                   point_at_stop = TRUE,
                   col="black", 
                   points = stop, 
                   text = "death (other)") 
}


add_cancer_gen <- function(p, y = 1, start = 40, stop = 110,  point_at_stop = TRUE, points = NULL) {
  #browser()
  #n <- length(points)
  if(stop <110) {
    col <- "red"
  } else {
    col <- "blue"
  }
  
  add_trajectory(p=p, 
                 y = y, 
                 start = start, 
                 stop = stop,
                 col=col, 
                 points = points, 
                 text = "cancer emergence", 
                 shape =21) 
}

add_clinical_dx <- function(p, y = 1, start = 40, stop =110) {
    add_trajectory(p=p, 
                   y = y, 
                   start = start, 
                   stop =stop, 
                   col="red", 
                   points = stop, 
                   text = "clinical dx", 
                   shape =21) 
}

add_cancer_death <- function(p, y = 1, start = 40, stop =110) {
  add_trajectory(p=p, 
                 y = y, 
                 start = start, 
                 stop =stop, 
                 col="red", 
                 points = stop, 
                 text = "death (cancer)", 
                 shape =21) 
}


##########################################################################
##########################################################################
##########################################################################
# Exactly 1 event 

p <- add_trajectory(p0 + ylim(0, 9), 
                    stop = 88, 
                    point_at_stop = TRUE, 
                    text = "death",
                    y=4.5)
p
if(FALSE) ggsave("02_des.pdf", width = 5, height = 3)

##########################################################################
##########################################################################
##########################################################################
# atmost 1 event -- occurs vs not 

p <- add_trajectory(p0 + ylim(0, 9), 
                    stop = 78, 
                    point_at_stop = TRUE,
                    text = "death (CVD)",
                    col = "red",
                    y=4.5)
p
if(FALSE) ggsave("03_des.pdf", width = 5, height = 3)


p <- add_trajectory(p0 + ylim(0, 9), 
                    stop = 110, 
                    point_at_stop = TRUE, 
                    text = "death (CVD)",
                    col = "red",
                    y=4.5)
p
if(FALSE) ggsave("04_des.pdf", width = 5, height = 3)
##########################################################################
##########################################################################
##########################################################################

# 0, 1, or more events 
p <- add_trajectory(p0 + ylim(0, 9), 
                    stop = 110, 
                    points = c(55, 68),
                    text = "lesion emergence",
                    col = "blue",
                    y=4.5)
p
if(FALSE) ggsave("05_des.pdf", width = 5, height = 3)

#########################################################################
############Chained ##############################################################

p <- add_trajectory(p0 + ylim(0, 9), 
                    stop = 110, 
                    point_at_stop = TRUE, 
                    text = "death (CVD)",
                    col = "red",
                    y=5)
p
if(FALSE) ggsave("04_des.pdf", width = 5, height = 3)



age_doc <- 88
age_cancer <- 62
age_dx <- 80
age_cancer_death <- 105


##########################################################################
##########################################################################
##########################################################################

 p <- add_doc(p0 + ylim(0, 7), stop =age_doc, y=5)
 p <- add_cancer_gen(p, points = age_cancer, stop = age_cancer, y=4)
 p <- p + geom_segment(aes(x = age_cancer, xend = age_cancer, y= 4, yend = 2), linetype =2)
 p <- add_clinical_dx(p, start = age_cancer, stop = age_dx,  y=3)
 p <- add_cancer_death(p, start = age_cancer, stop = age_cancer_death,  y=2)
p
