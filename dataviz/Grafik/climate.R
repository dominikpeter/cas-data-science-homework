rm(list = ls())

library(readr)
library(ggplot2)
library(dplyr)
library(reshape2)
library(zoo)

climate <- read_delim("~/Desktop/data/climate.txt", 
                      "\t", escape_double = FALSE, trim_ws = TRUE, 
                      skip = 13)


climate2 <- climate[, 1:13] %>% 
  melt(measure.vars = 2:13, variable.name = "month")

lookup <- data_frame(monat = unique(climate2$month), numeric = 1:12)

climate2 <- climate2 %>% 
  left_join(lookup, by = c("month" = "monat")) %>% 
  mutate(yearmon = as.yearmon(paste(as.character(time),
                               as.character(numeric), sep = "-")),
         year = time)


climate2 %>% 
  filter(year > 1987) %>% 
  ggplot(aes(x = as.factor(year), y = value)) +
  geom_boxplot()








