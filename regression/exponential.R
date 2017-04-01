

df <- read.csv("regression/Daten/Übung 4_CEOs Gehälter.csv")

model <- lm(SALARY~COMTEN+I(COMTEN^2), data=df)
