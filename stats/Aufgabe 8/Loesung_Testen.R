
# ------------------------------------------------------------------------------------------------
# Title:  Testen
# Autor:  Dominik Peter
# Date:   2017-01-05
# ------------------------------------------------------------------------------------------------

rm(list=ls())
set.seed(2323)
# library(magrittr)


# ------------------------------------------------------------------------------------------------
# Aufgabe: Linksseitiger Test bei μ, σ bekannt
# ------------------------------------------------------------------------------------------------
# Problem: Die Datei „lightbulbs.txt“ enthält eine neue Stichprobe des Glühbirnenherstellers.
# Laden Sie die Datei mit dem Befehl scan. Lässt sich aufgrund dieser Stichprobe die Behauptung des Herstellers,
# dass die Glühbirnen eine Mindestlebensdauer von 10′000 Stunden besitzen, bei einem Signifikanzniveau von 1% verwerfen?
# Die Standardabweichung beträgt 120 Stunden.

lightbulb <- scan("stats/Aufgabe 8/lightbulbs.txt")

lightbulb

xbar <- mean(lightbulb)   # sample mean 
mu0 <- 10000              # hypothesized value 
sigma <- 120              # population standard deviation 
n <- length(lightbulb)    # sample size 
SE <- sigma/sqrt(n)     # Standard Error
z <- (xbar-mu0)/SE

alpha <- .01
p <- pnorm(z)
p > alpha

confint <- qnorm(1-alpha, mean = xbar, sd = SE)
c(Inf, xbar, confint)
# Das obere 99% Quantile ist mit 9963.201 kleiner als die vom Hersteller angegebenen 10'000
# Die NULL Hypothese, dass μ ≥ 10000 kann verworfen werden. 
# TeachingDemos::z.test(lightbulb, mu0, stdev = 120, alternative = "less", conf.level = 0.99)


# ------------------------------------------------------------------------------------------------
# Aufgabe: Rechtsseitiger Test bei μ, σ bekannt
# ------------------------------------------------------------------------------------------------
# Problem: Die Datei „cookies.txt“ enthält eine neue Stichprobe des Keksherstellers.
# Laden Sie die Datei mit dem Befehl scan.
# Lässt sich aufgrund dieser Stichprobe die Behauptung des Herstellers,
# dass die Kekse einen maximalen Anteil von 2 g enthalten, bei einem Signifikanzniveau von 10% verwerfen?
# Die Standardabweichung beträgt 0.25 g.


cookies <- scan("stats/Aufgabe 8/cookies.txt")
cookies

xbar <- mean(cookies)
mu0 <- 2
sigma <- 0.25
n <- length(cookies)
SE <- sigma/sqrt(n)
z <- (xbar-mu0) / SE

alpha <- 0.1

p <- pnorm(z, lower.tail = FALSE)
p > alpha
xbar

confint <- qnorm(alpha, mean = xbar, sd = SE)
c(confint, xbar, Inf)

# Mit der vorligenden Stichprobe kann die NULL Hypothese, dass μ ≤ 2 *nicht* verworfen werden
# TeachingDemos::z.test(cookies, mu0, stdev = 0.25, alternative = "greater", conf.level = 0.9)


# ------------------------------------------------------------------------------------------------
# Aufgabe: Zweiseitiger Test bei μ, σ bekannt
# ------------------------------------------------------------------------------------------------
# Problem: Die Datei „penguins.txt“ enthält eine neue Zufallsstichprobe einer Pinguinkolonie.
# Laden Sie die Datei mit dem Befehl scan.
# Lässt sich aufgrund dieser Stichprobe die Behauptung, dass sich das Durchschnittsgewicht der Pinguine
# nicht verändert hat, bei einem Signifikanzniveau von 5% verwerfen? Die Standardabweichung beträgt 2.5 kg.

penguins <- scan("stats/Aufgabe 8/penguins.txt")
penguins

xbar <- mean(penguins)
# mu0 <- Mean der NULL Hypothese fehlt???
mu0 <- 15.4 #aus Übungsfolien
mu0
n <- length(penguins)
sigma <- 2.5
SE <- sigma/sqrt(n)

z <- (xbar - mu0) / SE
z
p <- 2 * pnorm(abs(z), lower.tail =  FALSE)
p
confint <- qnorm(c(0.025, 0.975), mean = xbar, sd = SE)
confint
# TeachingDemos::z.test(penguins, mu = mu0,stdev = 2.5, alternative = "two.sided", conf.level = 0.95)
# mit einem p-value = 0.1389 kann die Null Hypothese bei einem Signifikanzniveau von 5% *nicht* verworfen werden.
 

# ------------------------------------------------------------------------------------------------
# Aufgabe: Linksseitiger Test bei μ, σ unbekannt
# ------------------------------------------------------------------------------------------------
# Problem: Die Datei „lightbulbs.txt“ enthält eine neue Stichprobe des Glühbirnenherstellers.
# Laden Sie die Datei mit dem Befehl scan. Lässt sich aufgrund dieser Stichprobe die Behauptung des Herstellers,
# dass die Glühbirnen eine Mindestlebensdauer von 10′000 Stunden besitzen,
# bei einem Signifikanzniveau von 1% verwerfen?

lightbulb # noch im Speicher
  
xbar <- mean(lightbulb)
mu0 <- 10000
n <- length(lightbulb)
s <- sd(lightbulb)
SE <- s/sqrt(n)
t <- (xbar - mu0) / SE

alpha <- 0.01
p <- pt(t, df = n-1)
p > alpha

t_test <- t.test(lightbulb, mu = mu0, alternative = "less", conf.level = 0.99)
t_test
t_test$conf.int[1:2]
# Die NULL Hypothese, dass μ ≥ 10000 kann verworfen werden. p-value = 1.592e-05

?t.test

# ------------------------------------------------------------------------------------------------
# Aufgabe: Rechtsseitiger Test bei μ, σ unbekannt
# ------------------------------------------------------------------------------------------------
# Problem: Die Datei „cookies.txt“ enthält eine neue Stichprobe des Keksherstellers.
# Laden Sie die Datei mit dem Befehl scan.
# Lässt sich aufgrund dieser Stichprobe die Behauptung des Herstellers,
# dass die Kekse einen maximalen Anteil von 2 g enthalten, bei einem Signifikanzniveau von 10% verwerfen?

cookies # noch im Speicher

xbar <- mean(cookies)
mu0 <- 2
n <- length(cookies)
s <- sd(cookies)
SE <- s/sqrt(n)
t <- (xbar - mu0) / SE
t

alpha <- 0.1
p <- pt(t, df = n-1, lower.tail = FALSE)
p > alpha

t_test <- t.test(cookies, mu = mu0, alternative = "greater", conf.level = 0.9)
t_test
t_test$conf.int[1:2]

# Der Mittelwert der Nullhypothese liegt innerhlab des Konfidenzintervalls
# Die NULL Hypothese μ ≥ 10000 kann nicht verworfen werden



# ------------------------------------------------------------------------------------------------
# Aufgabe: Zweiseitiger Test bei μ, σ unbekannt
# ------------------------------------------------------------------------------------------------
# Problem: Die Datei „penguins.txt“ ent hält eine neue Zufallsstichprobe einer Pinguinkolonie.
# Laden Sie die Datei mit dem Befehl scan.
# Lässt sich aufgrund dieser Stichprobe die Behauptung,
# dass sich das Durchschnittsgewicht der Pinguine nicht verändert hat,
# bei einem Signifikanzniveau von 5% verwerfen?

penguins    # noch in Speicher
# mu0 fehlt wieder

xbar <- mean(penguins)

mu0 <- 
s <- sd(penguins)
n <- length(penguins)
SE <- s / sqrt(n)

t <- (xbar - mu0) / SE
p <- 2 * pt(abs(t), df = n-1, lower.tail = FALSE)
p
t_test <- t.test(penguins, mu = mu0, alternative = "two.sided", conf.level = 0.95)
t_test

# mit einem p-value = 9.478e-08 kann die Null Hypothese bei einem Signifikanzniveau von 5% verworfen werden


# ------------------------------------------------------------------------------------------------
# Aufgabe: Linksseitiger Test des Populationsanteils p
# ------------------------------------------------------------------------------------------------
# Problem: Die Datei „grocerystore.csv“ enthält eine Zufallsstichprobe von Kunden einer Metzgerei.
# Neben dem Geschlecht der Kunden wurde auch deren Verweilzeit im Laden notiert.
# Importieren Sie die Datei mit dem Befehl read.csv.
# Lässt sich aufgrund dieser Stichprobe die Behauptung,
# dass die Metzgerei mehrheitlich von Frauen besucht wird, bei einem Signifikanzniveau von 5% verwerfen?

store <- read.csv("stats/Aufgabe 8/grocerystore.csv", sep = ";")

frauen <- sum(store$gender == "F")
n <- nrow(store)

prop.test(frauen, n, alternative="less", correct=FALSE, conf.level = .95)

# Die NULL Hypothese  






