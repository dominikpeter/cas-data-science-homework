
# ------------------------------------------------------------------------------------------------
# Title:  Lösung zu Aufgaben Testen
# Autor:  Dominik Peter
# Date:   2017-01-05
# ------------------------------------------------------------------------------------------------

rm(list=ls())


# ------------------------------------------------------------------------------------------------
# Aufgabe: Linksseitiger Test bei μ, σ bekannt
# ------------------------------------------------------------------------------------------------
# Problem: Die Datei „lightbulbs.txt“ enthält eine neue Stichprobe des Glühbirnenherstellers.
# Laden Sie die Datei mit dem Befehl scan. Lässt sich aufgrund dieser Stichprobe die Behauptung des Herstellers,
# dass die Glühbirnen eine Mindestlebensdauer von 10′000 Stunden besitzen, bei einem Signifikanzniveau von 1% verwerfen?
# Die Standardabweichung beträgt 120 Stunden.

# H0: µ ≥ 10'000 Stunden, Ha: µ < 10'000 Stunden

lightbulb <- scan("stats/Aufgabe 8/lightbulbs.txt")

lightbulb

xbar <- mean(lightbulb)   # sample mean 
mu0 <- 10000              # hypothesized value 
sigma <- 120              # population standard deviation 
n <- length(lightbulb)    # sample size 
SE <- sigma/sqrt(n)       # Standard Error
z <- (xbar-mu0)/SE

alpha <- .01
p <- pnorm(z); p

confint <- qnorm(1-alpha, mean = xbar, sd = SE)
c(Inf, confint)
# Das obere 99% Quantile der Stichprobendaten ist mit 9963.201 kleiner als die vom Hersteller angegebenen 10'000
# Die NULL Hypothese, dass μ ≥ 10'000 wird verworfen. Es wird die Alternativhypothese akzeptiert.

# TeachingDemos::z.test(lightbulb, mu0, stdev = 120, alternative = "less", conf.level = 0.99)


# ------------------------------------------------------------------------------------------------
# Aufgabe: Rechtsseitiger Test bei μ, σ bekannt
# ------------------------------------------------------------------------------------------------
# Problem: Die Datei „cookies.txt“ enthält eine neue Stichprobe des Keksherstellers.
# Laden Sie die Datei mit dem Befehl scan.
# Lässt sich aufgrund dieser Stichprobe die Behauptung des Herstellers,
# dass die Kekse einen maximalen Anteil von 2 g enthalten, bei einem Signifikanzniveau von 10% verwerfen?
# Die Standardabweichung beträgt 0.25 g.

# H0: µ ≤ 2 g, Ha: µ > 2 g

cookies <- scan("stats/Aufgabe 8/cookies.txt")
cookies

xbar <- mean(cookies)
mu0 <- 2
sigma <- 0.25
n <- length(cookies)
SE <- sigma/sqrt(n)
z <- (xbar-mu0) / SE

alpha <- 0.1

p <- pnorm(z, lower.tail = FALSE); p


confint <- qnorm(alpha, mean = xbar, sd = SE)
c(confint, Inf)

# TeachingDemos::z.test(cookies, mu0, stdev = 0.25, alternative = "greater", conf.level = 0.9)

# Hinsichtlich der vorliegenden Stichprobe kann die NULL Hypothese, dass μ ≤ 2 g *nicht* verworfen werden



# ------------------------------------------------------------------------------------------------
# Aufgabe: Zweiseitiger Test bei μ, σ bekannt
# ------------------------------------------------------------------------------------------------
# Problem: Die Datei „penguins.txt“ enthält eine neue Zufallsstichprobe einer Pinguinkolonie.
# Laden Sie die Datei mit dem Befehl scan.
# Lässt sich aufgrund dieser Stichprobe die Behauptung, dass sich das Durchschnittsgewicht der Pinguine
# nicht verändert hat, bei einem Signifikanzniveau von 5% verwerfen? Die Standardabweichung beträgt 2.5 kg.

# H0: µ = 15.4 kg, Ha: µ ≠ 15.4 g

penguins <- scan("stats/Aufgabe 8/penguins.txt")
penguins

xbar <- mean(penguins)
# mu0 <- Mean der NULL Hypothese fehlt???
mu0 <- 15.4 # entnommen aus Übungsfolien
mu0
n <- length(penguins)
sigma <- 2.5
SE <- sigma/sqrt(n)

z <- (xbar - mu0) / SE; z
p <- 2 * pnorm(abs(z), lower.tail =  FALSE); p
confint <- qnorm(c(0.025, 0.975), mean = xbar, sd = SE)
confint
# TeachingDemos::z.test(penguins, mu = mu0, stdev = 2.5, alternative = "two.sided", conf.level = 0.95)

# mit einem P-Value = 0.1389 kann die Null Hypothese bei einem Signifikanzniveau von 5% *nicht* verworfen werden.
 

# ------------------------------------------------------------------------------------------------
# Aufgabe: Linksseitiger Test bei μ, σ unbekannt
# ------------------------------------------------------------------------------------------------
# Problem: Die Datei „lightbulbs.txt“ enthält eine neue Stichprobe des Glühbirnenherstellers.
# Laden Sie die Datei mit dem Befehl scan. Lässt sich aufgrund dieser Stichprobe die Behauptung des Herstellers,
# dass die Glühbirnen eine Mindestlebensdauer von 10′000 Stunden besitzen,
# bei einem Signifikanzniveau von 1% verwerfen?

# H0: µ ≥ 10'000 Stunden, Ha: µ < 10'000 Stunden

lightbulb # noch im Speicher
  
xbar <- mean(lightbulb)
mu0 <- 10000
n <- length(lightbulb)
s <- sd(lightbulb)
SE <- s/sqrt(n)
t <- (xbar - mu0) / SE

alpha <- 0.01
p <- pt(t, df = n-1); p

t_test <- t.test(lightbulb, mu = mu0, alternative = "less", conf.level = 0.99)
t_test
t_test$conf.int[1:2]
# Die NULL Hypothese, dass μ ≥ 10000 kann verworfen werden. P-Value = 1.592e-05


# ------------------------------------------------------------------------------------------------
# Aufgabe: Rechtsseitiger Test bei μ, σ unbekannt
# ------------------------------------------------------------------------------------------------
# Problem: Die Datei „cookies.txt“ enthält eine neue Stichprobe des Keksherstellers.
# Laden Sie die Datei mit dem Befehl scan.
# Lässt sich aufgrund dieser Stichprobe die Behauptung des Herstellers,
# dass die Kekse einen maximalen Anteil von 2 g enthalten, bei einem Signifikanzniveau von 10% verwerfen?

# H0: µ ≤ 2 g, Ha: µ > 2 g

cookies # noch im Speicher

xbar <- mean(cookies)
mu0 <- 2
n <- length(cookies)
s <- sd(cookies)
SE <- s/sqrt(n)
t <- (xbar - mu0) / SE

alpha <- 0.1
p <- pt(t, df = n-1, lower.tail = FALSE); p


t_test <- t.test(cookies, mu = mu0, alternative = "greater", conf.level = 0.9)
t_test
t_test$conf.int[1:2]

# Der Mittelwert der Nullhypothese liegt innerhalb des Konfidenzintervalls
# Die NULL Hypothese μ ≥ 2 kann nicht verworfen werden



# ------------------------------------------------------------------------------------------------
# Aufgabe: Zweiseitiger Test bei μ, σ unbekannt
# ------------------------------------------------------------------------------------------------
# Problem: Die Datei „penguins.txt“ ent hält eine neue Zufallsstichprobe einer Pinguinkolonie.
# Laden Sie die Datei mit dem Befehl scan.
# Lässt sich aufgrund dieser Stichprobe die Behauptung,
# dass sich das Durchschnittsgewicht der Pinguine nicht verändert hat,
# bei einem Signifikanzniveau von 5% verwerfen?

# H0: µ = 15.4 kg, Ha: µ ≠ 15.4 g

penguins    # noch in Speicher
# mu0 fehlt wieder

xbar <- mean(penguins)

mu0 <- 15.4 # entnommen aus Übungsfolien
s <- sd(penguins)
n <- length(penguins)
SE <- s / sqrt(n)

t <- (xbar - mu0) / SE
p <- 2 * pt(abs(t), df = n-1, lower.tail = FALSE)
p
t_test <- t.test(penguins, mu = mu0, alternative = "two.sided", conf.level = 0.95)
t_test

# mit einem p-value = 0.1723 kann die Null Hypothese bei einem Signifikanzniveau von 5% **nicht** verworfen werden


# ------------------------------------------------------------------------------------------------
# Aufgabe: Linksseitiger Test des Populationsanteils p
# ------------------------------------------------------------------------------------------------
# Problem: Die Datei „grocerystore.csv“ enthält eine Zufallsstichprobe von Kunden einer Metzgerei.
# Neben dem Geschlecht der Kunden wurde auch deren Verweilzeit im Laden notiert.
# Importieren Sie die Datei mit dem Befehl read.csv.
# Lässt sich aufgrund dieser Stichprobe die Behauptung,
# dass die Metzgerei mehrheitlich von Frauen besucht wird, bei einem Signifikanzniveau von 5% verwerfen?

# H0: p ≥ 60%, Ha: p < 60%

store <- read.csv("stats/Aufgabe 8/grocerystore.csv", sep = ";")
frauen <- sum(store$gender == "F")
n <- nrow(store)

prop.test(frauen, n, alternative="less", conf.level = .95, correct = FALSE)

# Die NULL Hypothese dass die Metzgerei mehrheitlich von Frauen besucht wird, kann **nicht** verworfen werden.


# ------------------------------------------------------------------------------------------------
# Aufgabe: Rechtsseitiger Test des Populationsanteils p
# ------------------------------------------------------------------------------------------------
# Problem: Um nicht in Schwierigkeiten zu geraten,
# darf der Anteil geplatzter Kredite einer Bank den Anteil von 12% nicht überschreiten.
# Die Datei „creditcards.csv“ enthält die Ergebnisse einer Untersuchung unter 1000 Kunden der Bank.
# Importieren Sie die Datei mit dem Befehl read.csv.
# Kann die Bank, bei einem Signifikanzniveau von 5%, aufgrund dieser Stichprobe sicher sein,
# dass die geplatzten Kredite den Anteil von 12% nicht übersteigen?

# H0: p ≤ 12%, Ha: p > 12%

credit <- read.csv("stats/Aufgabe 8/creditcards.csv", sep = ";")
bounced <- sum(credit$bounced == "Yes")
n <- nrow(credit)
prop.test(bounced, n, p = .12, alternative = "greater", conf.level = .95, correct = FALSE)

# Die Null Hypothese, dass die Anteil nicht über 12 % ist, wird verworfen und
# es wird die Alternativhypothese akzeptiert, dass der Anteil über 12 % ist


# ------------------------------------------------------------------------------------------------
# Lösung: Zweiseitiger Test des Populationsanteils p
# ------------------------------------------------------------------------------------------------
# Problem: Der Anteil der Rechtshänder unter den Studierenden von survey wird auf 90% geschätzt.
# Lässt sich diese Behauptung bei einem Signifikanzniveau von 1% verwerfen?

# H0: p = 90%, Ha: p ≠ 90%

library(MASS)
rechts <- sum(survey$W.Hnd == "Right", na.rm = TRUE)
n <- sum(!is.na(survey$W.Hnd))

prop.test(rechts, n, p = .9, conf.level = 0.99, correct = FALSE, alternative = "two.sided")

# Die Null Hypothese, dass der Anteil = 0.9 **kann** nicht verworfen werden.


