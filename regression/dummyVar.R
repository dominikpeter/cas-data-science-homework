library(readr)
library(dplyr)

df <- read.csv("regression/Daten/Übung 3_Rosennachfrage.csv")

model1 <- lm(Y~PR+PN, data=df)
model2 <- lm(log(Y)~log(PR)+log(PN), data=df)

yhat1 <- predict(model1, df)
yhat2 <- predict(model2, df)

cor(yhat1, df$Y)
summary(model1)
cor(yhat2, df$Y)^2


df <- read.csv("regression/Daten/Übung 5_Rauchen und Schwangerschaft.csv")

df$sex <- ifelse(df$male==0,'female',"male")
df$sex <- as.factor(df$sex)

model <- lm(bwght~sex, data = df)

summary(model)

summary(model)
tapply(df$bwght, df$sex, mean)

tapply(df$bwght, as.factor(df$male), mean)
df$female <- (df$male-1)*-1

summary(lm(bwght~male, data=df))

tapply(df$bwght, factor(df$male), mean)

summary(lm(bwght~male+female, data = df))


(X ́X )−1 X ́y


y <- df$bwght
X <- as.matrix(cbind(1, df$male, df$female))
Xf <- as.matrix(cbind(1, df$male))

solve(t(X)%*%X) %*% t(X) %*% y
solve(t(Xf)%*%Xf) %*% t(Xf) %*% y





