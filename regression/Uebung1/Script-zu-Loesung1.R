https://docs.google.com/document/d/1XHQstGJcD2bSe15HDHvojUMnHnBlU5llc06xGqup-ek/edit?usp=sharing


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


GGally::ggcorr(df[,2:4], digits = TRUE, label = TRUE)

x <- c(2, 2, 2)
t(x)%*%x







