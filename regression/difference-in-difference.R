
# difference in difference

library(dplyr)

df <- data_frame(y=c(40,80,20,100,30,0,60,40,60,90),
                 Dt = c(0,1,0,1,0,1,0,1,0,0), #treated group = 1
                 Dp = c(0,1,0,1,0,0,1,0,1,1)
                 )

df
model <- lm(y~Dt+Dp+Dt*Dp, data=df)
summary(model)

predict(model, data_frame(Dt=c(0,1), Dp=c(1,1)))
predict(model, data_frame(Dt=c(0,0), Dp=c(0,1)))
predict(model, data_frame(Dt=c(0,1), Dp=c(1,1)))
predict(model, data_frame(Dt=c(0,1), Dp=c(0,0)))








