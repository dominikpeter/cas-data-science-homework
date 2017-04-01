
# mwd test
library(readr)
library(dplyr)

df <- read.csv("regression/Daten/Übung 3_Rosennachfrage.csv")

model1 <- lm(Y~PR+PN, data=df)
model2 <- lm(log(Y)~log(PR)+log(PN), data=df)
summary(model1)
summary(model2)

yhat1 <- predict(model1, df)
yhat2 <- predict(model2, df)
z = log(yhat1) - yhat2
model3 <- lm(Y~PR+PN+z, data=df)
summary(model3)

# z ist nicht signifikant

z2 <- exp(yhat2) - yhat1

model4 <- lm(log(Y)~log(PR)+log(PN)+z, data=df)
summary(model4)

# Ist keine Funktion von LN







  