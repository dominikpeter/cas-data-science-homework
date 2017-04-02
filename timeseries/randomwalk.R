

# ------------------------------------------------------------------------------------------------
# Title:  Random Walk
# Autor:  Dominik Peter
# Date:   2017-04-01
# ------------------------------------------------------------------------------------------------

rm(list=ls())

library(ggplot2)
library(dplyr)

randomWalk <- function(n, start = 10, mu = 0, sd = 1){
  white <- rnorm(n, mean = mu, sd = sd)
  t <- 1:n
  y <- vector(mode = "double", length = n)
  y[1] <- start #start
  for (i in 2:n) {
    y[i] <- y[i-1] + white[i]
  }
  data.frame(noise = white, y = y, t = 1:n)
}


# ----------------------------------------------
plotWalk <- function(N=100, pointSize = 1) {
  walks <- list()
  for(i in 1:4) walks[[i]] <- randomWalk(N)
  
  lapply(walks, function(x) c(var(x$y[1:length(x$y)/2]),
                              var(x$y[1:length(x$y)])))
  
  list(noise = 
  bind_rows(walks, .id = "id") %>% 
    mutate(walk = paste("Noise", id)) %>% 
    ggplot(aes(x=t, y=noise, group=1)) +
    geom_smooth(se = FALSE, color = "grey") +
    geom_point(size=pointSize) + 
    geom_line() +
    facet_wrap(~walk) +
    theme_minimal() +
    theme(plot.background = element_blank(),
          panel.grid = element_blank()) +
    ylab("y") +
    xlab("time") +
    ggtitle("Noise Variable in Random Walk",
            subtitle = "Normal Distribution with Mean=0 and Stdw=1"),
  
  walk = 
  bind_rows(walks, .id = "id") %>% 
    mutate(walk = paste("Random Walk", id)) %>% 
    ggplot(aes(x=t, y=y, group=1)) +
    geom_smooth(se = FALSE, color = "grey") +
    geom_point(size=pointSize) + 
    geom_line() +
    facet_wrap(~walk) +
    theme_minimal() +
    theme(plot.background = element_blank(),
          panel.grid = element_blank()) +
    ylab("y") +
    xlab("time") +
    ggtitle("Illustrating Random Walk")
  )
}


plots <- plotWalk(50)
plots[['noise']]
plots[['walk']]


plots <- plotWalk(8000, pointSize = 0.3)
plots[['noise']]
plots[['walk']]




