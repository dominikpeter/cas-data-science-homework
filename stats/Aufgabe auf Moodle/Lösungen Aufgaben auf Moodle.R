
# ------------------------------------------------------------------------------------------------
# Title:  Lösung zu Aufgaben auf Moodle Homepage
# Autor:  Dominik Peter
# Date:   2017-01-26
# ------------------------------------------------------------------------------------------------

rm(list=ls())
library(tidyverse)

# ------------------------------------------------------------------------------------------------



load("stats/Aufgabe auf Moodle/Cholesterinwerte.RData")

df <- Cholesterinwerte

df <- df %>% mutate(Intervall = Cholesterinwerte %>% 
                      findInterval(c(0, 220, 250, Inf))
                    )

df <- df %>%
  left_join(tribble(~Intervall, ~Range,
                    1,"normal",
                    2, "unter Beobachtung",
                    3, "erhöht"))

hist(df$Cholesterinwerte,breaks=c(min(df$Cholesterinwerte), 200,250,max(df$Cholesterinwerte)))



df <- c(rep(1,1), rep(2, 9), rep(4, 29), rep(8, 33), rep(16, 23), rep(32, 5))

median(df)

quantile(df, 0.2)

mean(df)

x <- rep(100, 6)

for (i in length(x)){
  x[i+1] <- 
  
  
}


5^26

5^52
6^26

length(letters)








