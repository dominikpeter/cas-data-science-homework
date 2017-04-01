
rm(list=ls())

library(ggplot2)
library(dplyr)

randomWalk <- function(n, start = 10, mu = 0, sd = 1){
  white <- rnorm(n, mean = mu, sd = sd)
  t <- 1:n
  y <- vector(mode = "double")
  y[1] <- start #start
  for (i in 2:n) {
    y[i] <- y[i-1] + white[i]
    }
  data.frame(noise = white, y = y, t = 1:n)
}

walks <- list()
for(i in 1:4) walks[[i]] <- randomWalk(30)


lapply(walks, function(x) c(var(x$y[1:length(x$y)/2]),
                            var(x$y[1:length(x$y)])))


bind_rows(walks, .id = "id") %>% 
  mutate(walk = paste("Random Walk", id)) %>% 
  ggplot(aes(x=t, y=y, group=1)) +
  geom_smooth(se = FALSE, color = "grey") +
  geom_point() + 
  geom_line() +
  facet_wrap(~walk) +
  theme_minimal() +
  theme(plot.background = element_blank(),
        panel.grid = element_blank()) +
  ylab("y") +
  xlab("time") +
  ggtitle("Illustrating Random Walk")


