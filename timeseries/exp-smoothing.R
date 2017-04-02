
# ------------------------------------------------------------------------------------------------
# Title:  Exponentielle GLättung
# Autor:  Dominik Peter
# Date:   2017-04-01
# ------------------------------------------------------------------------------------------------


library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

# set.seed(23)

randomTime <- function(n, init,
                       t, s,
                       period,
                       returnDF=TRUE, ...){
  time <- vector(mode="double")
  time[1] <- init
  error <- rnorm(n, ...)
  for (i in 2:n){
    time[i] <- (time[i-1] * (1+t)) + error[i]
  }
  intv <- n %/% period * period
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

exp_smoothing <- function(t, a = 0.2){
  smoothed <- vector(mode = "double", length = length(t))
  smoothed[1] <- t[1]
  for(i in 2:length(t)){
    error <- t[i-1] - smoothed[i-1]
    smoothed[i] <- smoothed[i-1] + a * error
  }
  smoothed
}

# --------------------
N <- 24
Start <- 50
trend <- +0.02
ses <- 0.1
period <- 4
# --------------------

rt <- randomTime(N, Start, trend, ses, period)
rt_list <- lapply(1:4, function(x) rt)

alphas <- c(0.05, 0.5, 0.7, 1)

smoothing <- function(x, y){
  x$smoothed <- exp_smoothing(x$t, y)
  x$a <- y
  x
}

purrr::map2(rt_list, alphas, .f=smoothing) %>% 
  bind_rows(.id = "serie") %>% 
  mutate(serie = paste("Zeitreihenglättung mit a =", a)) %>% 
  select(-a, serie, i, observiert=t, geglättet=smoothed) %>% 
  gather(id, value, -i,-serie) %>% 
  ggplot(aes(x=i,y=value, color=id)) +
  geom_point(size=1.1) +
  geom_line() +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  xlab("Time") +
  ylab("Value") +
  ggtitle("Exponentielle Glättung", subtitle = "Beispiel an verschiedenen Alpha Levels") +
  scale_color_manual(values = c("#3498db", "#e67e22"), name="") +
  scale_x_continuous(breaks = seq(0, N, 2)) +
  facet_wrap(~serie)
  

