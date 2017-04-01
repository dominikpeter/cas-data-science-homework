library(ggplot2)
library(dplyr)
library(tidyr)

randomTime <- function(n, init,
                       t, s,
                       period,
                       returnDF=TRUE, ...){
  time <- vector(mode="double")
  time[1] <- init
  error <- rnorm(n, ...)
  
  for (i in 2:n) time[i] <- (time[i-1] * (1+t)) + error[i]
  intv <- n%/%period*period
  seasonal <- seq(from = 0,
                  to = intv,
                  by = period)[-1]
  
  time[seasonal] <- time[seasonal] * (1+s)
  if (returnDF){
    df <- data.frame(i = 1:n, t = time)
    return(df)
  }
  time
}

t1 <- randomTime(50, 10, 0.02, 0.1, 4)

t1 %>% 
  ggplot(aes(x=i,y=t)) +
  geom_point() +
  geom_line()

exp_smoothing <- function(t, a = 0.2){
  smoothed <- vector(mode = "double", length = length(t))
  smoothed[1] <- t[1]
  for(i in 2:length(t)){
    error <- t[i-1] - smoothed[i-1]
    smoothed[i] <- smoothed[i-1] + a * error
  }
  smoothed
}

a <- 1
t1 %>% 
  mutate(smoothed = t %>% exp_smoothing(a)) %>% 
  gather(id, value, -i) %>% 
  ggplot(aes(x=i,y=value, color=id)) +
  geom_point(size=0.8) +
  geom_line() +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  xlab("Time") +
  ylab("Value") +
  ggtitle("Exponentielles Glätten") +
  scale_color_manual(values = c("#3498db", "#e67e22"))
