
# --understanding regression with r

library(ggplot2)

norm <- rnorm(10000, mean = 5, sd = 2)
norm2 <- rnorm(10000, mean = 10, sd = 2)
qplot(norm, geom = "histogram", color = norm)

normX2 <- norm2^2
normX <- norm^2

qplot(normX, geom = "histogram", color = norm)

qplot(normX/normX2, geom = "histogram", color = norm)


model <- lm(hp~disp, mtcars)


pred <- predict(model, data.frame(disp = mtcars$disp))

SSE <- sum(model$residuals^2)
SST <- sum((mtcars$hp - mean(mtcars$hp))^2)

1 - SSE / SST

summary(model)



