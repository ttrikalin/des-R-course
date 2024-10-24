# 
library(data.table)
library(nhppp)
library(ggplot2)

set.seed(20241011)
times <- list()

lambda <- matrix(rep(1, 10^4), ncol = 1)
Z <- vdraw_sc_step_regular(lambda_matrix = lambda, rate_matrix_t_min = 1, rate_matrix_t_max = 5)

times <- data.table(t(Z))
colnames(times) <- paste0("t",  1:10^4)
times[, y:= 1]
first_times <- Z[!is.na(Z[,1]),1, drop = FALSE]
# dat <- data.table(
#   id = 1:length(times0), 
#   times0 = times0, 
#   y = rep(1, length(times0))
# )

# Plot the point process

p <- ggplot() +
  xlim(0, 5) + 
  ylim(0, 2) +
  geom_segment(aes(x = 1, xend = 5, y = 0, yend = 0), color = "black", linewidth = 2) +
  theme_bw() + 
  xlab("time")+
  theme(axis.text.x = element_text(size = 14),       
        axis.title.x = element_text(size = 18))  +  
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank()) + 
  theme(plot.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank() )+
  theme(panel.border= element_blank())+
  theme(axis.line.x = element_line(color="black")) 
  
p
if(FALSE) ggsave("00_point_process.pdf", width = 5, height = 3)


p + geom_point( aes( x= times$t1, y=times$y))
if(F) ggsave("01_point_process.pdf", width = 5, height = 3)

p + geom_point(aes( x= times$t2, y=times$y))
if(F) ggsave("02_point_process.pdf", width = 5, height = 3)

p + geom_point(aes( x= times$t5, y=times$y))
if(F) ggsave("03_point_process.pdf", width = 5, height = 3)


#### event times for five instantiations 
dy <- 0.2
p + 
  geom_segment(aes(x = 1, xend = 5, y =1+2*dy, yend = 1+2*dy), color = "grey", linewidth = 0.5) +
  geom_point(aes( x= times$t1, y=times$y+2*dy)) + 
  geom_segment(aes(x = 1, xend = 5, y =1+dy, yend = 1+dy), color = "grey", linewidth = 0.5) +
  geom_point(aes( x= times$t2, y=times$y+dy)) + 
  geom_segment(aes(x = 1, xend = 5, y =1, yend = 1), color = "grey", linewidth = 0.5) +
  geom_segment(aes(x = 1, xend = 5, y =1-dy, yend = 1-dy), color = "grey", linewidth = 0.5) +
  geom_point(aes( x= times$t5, y=times$y-dy)) + 
  geom_segment(aes(x = 1, xend = 5, y =1-2*dy, yend = 1-2*dy), color = "grey", linewidth = 0.5) +
  geom_point(aes( x= times$t3, y=times$y-2*dy))
if(F) ggsave("04_point_process.pdf", width = 5, height = 3)


dat <- times
dat <- unlist(dat[, y:= NULL])
dat <- dat[!is.na(dat)]
dat <- data.frame(times = dat)

#############################
p2 <- ggplot() +
  xlim(0, 5) + 
  ylim(0, 0.5) +
  geom_segment(aes(x = 1, xend = 5, y = 0, yend = 0), color = "black", linewidth = 2) +
  theme_bw() + 
  xlab("time") +
  ylab("density") +
  theme(axis.text.x = element_text(size = 14),       
        axis.title.x = element_text(size = 18))  +  
  theme(axis.title.y = element_text(size = 18), 
        axis.text.y = element_text(size = 14)#, 
        #axis.ticks.y = element_blank()
        ) + 
  theme(plot.background = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank() )+
  theme(panel.border= element_blank())+
  theme(axis.line.x = element_line(color="black")) 

p2 + 
  geom_histogram(aes(x = dat$times[1:100], y=..density..), 
                 color = "black", 
                 fill = "gray", 
                 binwidth = 0.05, 
                 center=0.025)
if(T) ggsave("05_point_process.pdf", width = 5, height = 3)


p2 + 
  geom_histogram(aes(x = dat$times[1:1000], y=..density..), 
                 color = "black", 
                 fill = "gray", 
                 binwidth = 0.05, 
                 center=0.025)
if(T) ggsave("06_point_process.pdf", width = 5, height = 3)

p2 + 
  geom_histogram(aes(x = dat$times[1:10000], y=..density..), 
                 color = "black", 
                 fill = "gray", 
                 binwidth = 0.05, 
                 center=0.025)
if(T) ggsave("07_point_process.pdf", width = 5, height = 3)

p2 + 
  geom_histogram(aes(x = dat$times[1:10000], y=..density..), 
                 color = "black", 
                 fill = "gray", 
                 binwidth = 0.05, 
                 center=0.025) + 
  geom_segment(aes(x = 1, xend = 5, y = 1/4, yend = 1/4), color = "red", linewidth = 1) + 
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 0), color = "red", linewidth = 1)

if(T) ggsave("08_point_process.pdf", width = 5, height = 3)



p2 + 
  geom_histogram(aes(x = first_times[,1], y=..density..), 
                 color = "black", 
                 fill = "gray", 
                 binwidth = 0.05, 
                 center=0.025) 

if(T) ggsave("08_point_process.pdf", width = 5, height = 3)
