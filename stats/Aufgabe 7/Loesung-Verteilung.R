# ------------------------------------------------------------------------------------------------
# Title:  Verteilung
# Autor:  Dominik Peter
# Date:   2016-12-11
# ------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------------------
# Binominalverteilung
# ------------------------------------------------------------------------------------------------
# Problem: Die Wahrscheinlichkeit, dass man im Roulette bei einmaligem Setzen auf „rot“ gewinnt, ist p = 18/37 = 0.486.
# Definieren wir mit x jene Anzahl der Spiele, bei denen man bei fünfmaligem Setzen auf „rot“ gewinnt.
# Wie gross ist bei fünfmaligem Setzen auf „rot“ die Wahrscheinlichkeit, dass man öfter gewinnt als verliert?

# Wie gross ist bei fünfmaligem Setzen auf „rot“ die
# Wahrscheinlichkeit, dass man öfter gewinnt als verliert?
round(pbinom(q = 2, size = 5, prob = 18/37, lower.tail = FALSE), 3)


# Welche Anzahl der Gewinne wird in 90% der Fälle höchstens
# erreicht?
qbinom(0.9, size = 5, prob = 18/37, lower.tail = TRUE)



# ------------------------------------------------------------------------------------------------
# Poissonverteilung
# ------------------------------------------------------------------------------------------------
# Problem: Das Restaurant Fat’s Pizza führt Buch über die Anzahl an Gästen,
# die das Restaurant betreten. Laut der Aufzeichnungen ist der Erwartungswert μ = 12,1
# zwischen 20:00 und 22:00 Uhr. Bestimmen Sie mit der Poisson-Verteilung die Wahrscheinlichkeit,
# dass zwischen 20 Uhr und 22 Uhr folgende Szenarien auftauchen:

lambda <- 12.1

# Es sind genau 8 Gäste im Restaurant.
round(dpois(8, lambda = lambda), 3)

# Es sind höchstens 10 Gäste im Restaurant.
round(ppois(10, lambda = lambda), 3)


# Es sind zwischen 9 und 15 Gäste im Restaurant.
lower_tail <- ppois(8, lambda = lambda) 
upper_tail <- ppois(15, lambda = lambda, lower.tail = FALSE)
tails <- lower_tail + upper_tail

round(total - tails, 3)

# Es sind mindestens 11 Gäste anwesend.
round(ppois(10, lambda = lambda, lower.tail = FALSE), 3)


# ------------------------------------------------------------------------------------------------
# Stetige Gleichverteilung
# ------------------------------------------------------------------------------------------------
# Problem: Sie haben heute um 9 Uhr ein wichtige Meeting,
# aber Sie verschlafen und wachen erst um 8:30 Uhr auf.
# Um 8:40 rennen Sie aus der Tür, auf dem Weg ins Büro.
# Sie brauchen 6 Minuten zur Bushaltestelle. Dann warten Sie auf den Bus,
# der morgens alle fünf Minuten kommt, die Wartezeit in Minuten ist gleichverteilt
# zwischen 0 und 5. Je nach Verkehrslage braucht der Bus nun noch einmal (gleichverteilt)
# 10 bis 15 Minuten bis ins Büro.

# Welche Verteilung hat die Zufallsvariable X, welche die gesamte Pendelzeit von Haustür bis ins Büro beschreibt?

min_zeit <- 6+0+10 #6 Minuten bis zum Bus, 0 Minuten Warten, 10 Minuten Reisezeit
max_zeit <- 6+5+15 #6 Minuten bis zum Bus, 5 Minutan Warten, 15 Minuten Reisezeit

# Die Reisezeit ist Gleichverteilt mit a = 16 und b = 26
cat(paste0("X∼U(", min_zeit, "," , max_zeit,")"))

# Mit welcher Wahrscheinlichkeit schaffen Sie es noch rechtzeitig ins Büro?

# ℙ(X≤20)
round(punif(20, min = min_zeit, max = max_zeit), 3)



# ------------------------------------------------------------------------------------------------
# Exponentialverteilung
# ------------------------------------------------------------------------------------------------
# Problem: In einer vierwöchigen Datenerhebung missen Sie die Länge
# der Telefongespräche, die Sie auf Ihrem Handy führen. Sie finden
# heraus, dass die Dauer der Gespräche (in Minuten) einer
# Exponentialverteilung folgt, und Ihre Gespräche im Erwartungswert
# 3 Minuten lang sind.

# Welche Verteilung hat die Zufallsvariable X, welche die Dauer der
# Telefongespräche in Minuten beschreibt?

# Sie ist Exponetialverteilt mit dem Erwartungswert:
# 𝔼(X)=1/λ wobei λ = 3,  𝔼(X)=1/3


# Das Telefon klingelt. Wie gross ist die Wahrscheinlichkeit, dass
# dieses Gespräch höchstens eine Minute dauert?
rate <- 1/3
p <- pexp(1, rate = rate)
round(p, 3)

# Wie gross ist die Wahrscheinlichkeit, dass das Gespräch länger
# als eine Minute dauert?
round(1-p, 3)
# pexp(1, rate = rate, lower.tail = FALSE)

# Mit welcher Wahrscheinlichkeit dauert das Gespräch zwischen
# einer und drei Minuten?

lower_tail <- pexp(1, rate = rate)
upper_tail <- pexp(3, rate = rate, lower.tail = FALSE)
tails <- sum(lower_tail, upper_tail)
total <- 1

round(total - tails, 3)

# Berechnen und interpretieren Sie das 25%-Quantil dieser Verteilung.
round(qexp(0.25, rate = rate), 3)
# Das 25%-Quantil ist die Dauer in Minuten, 
# die von den kürzesten 25% der Telefonate nicht überschritten wird.

# Es dauern also 25% der Telefonate weniger als 0.863 Minuten.



# ------------------------------------------------------------------------------------------------
# Normalverteilung
# ------------------------------------------------------------------------------------------------
# Problem: In einer Fabrik werden Tüten mit Kartoffelchips befüllt. Das durchschnittliche Gewicht der
# Tüten soll nach den Angaben des Werkes 200 g betragen. Da die Tüten maschinell befüllt werden,
# wird dieser Wert nur mit einer Standardabweichung von 4 g eingehalten. Mit welcher Wahrscheinlichkeit
# werden Tüten abgefüllt, deren Gewicht...

# um weniger als 2 g vom Mittelwert abweicht?
m <- 200
toleranz <- c(m -2, m + 2)
sd <- 4

tails <- pnorm(toleranz, mean = m, sd = sd)
tails

round(diff(tails), 3)

# über 205 g liegt?
round(pnorm(205, mean = m, sd = sd, lower.tail = FALSE), 3)

# Welches Gewicht wird von 95% der Tüten überschritten?
round(qnorm(.95, mean = m, sd = sd, lower.tail = FALSE),3)


# ------------------------------------------------------------------------------------------------
# Chi-Quadrat-Verteilung
# ------------------------------------------------------------------------------------------------
# Problem: Mit welcher Wahrscheinlichkeit liegt der Wert einer χ2-Verteilung 
# mit df = 11 über 15?

round(pchisq(15, df = 11, lower.tail = FALSE), 3)


# ------------------------------------------------------------------------------------------------
# Studentsche t-Verteilung
# ------------------------------------------------------------------------------------------------
# Problem: Mit welcher Wahrscheinlichkeit liegt der Wert der Studentschen t-Verteilung
# unter −0.5, respektive unter 1? Der Freiheitsgrad sei 7.

#unter -05
round(pt(-0.5, df = 7), 3)

#unter 1
round(pt(1, df = 7), 3)






