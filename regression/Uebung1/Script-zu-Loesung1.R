
library(readxl)
df <- read_excel("~/Google/R/homework/homework/regression/Uebung1/Übung 1_Gebrauchtautos.xlsx")

library(ggplot2)
library(dplyr)
library(GGally)

df %>% ggplot(aes(x=Alter, y=Preis)) + 
  geom_point() + geom_smooth()

df %>% ggplot(aes(x=KM, y=Preis)) + 
  geom_point() + geom_smooth()

summary(df[,2:4])

hist(df$KM)

psych::describe(df)

df %>% ggplot(aes(x = Preis)) + geom_histogram(bins = 8, color = "white") 





