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
# Problem: Sie haben heute um 9 Uhr einwichtige Meeting,
# aber Sie verschlafen und wachen erst um 8:30 Uhr auf.
# Um 8:40 rennen Sie aus der Tür, auf dem Weg ins Büro.
# Sie brauchen 6 Minuten zur Bushaltestelle. Dann warten Sie auf den Bus,
# der morgens alle fünf Minuten kommt, die Wartezeit in Minuten ist gleichverteilt
# zwischen 0 und 5. Je nach Verkehrslage braucht der Bus nun noch einmal (gleichverteilt)
# 10 bis 15 Minuten bis ins Büro.

# Welche Verteilung hat die Zufallsvariable X, welche die gesamte Pendelzeit von Haustür bis ins Büro beschreibt?




