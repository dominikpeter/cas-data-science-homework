# Ü38
# -----------------------------------------------------------------------------------------------------------------------
# In einem Vorrat von neun Glühbirnen sind vier defekt. Sie wählen zufällig drei Stück aus.
# Berechnen Sie mit dem Taschenrechner, wie wahrscheinlich es ist, dass sich darunter
# a) keine,
# b) eine,
# c) mehr als eine defekte Glühbirne befindet.
#   
# a
dhyper(0, 4, 5, 3)
# b
dhyper(1, 4, 5, 3)
# c
phyper(1, 4, 5, 3, lower.tail = FALSE)

# Ü39 
# -----------------------------------------------------------------------------------------------------------------------
# Alle vier Jahre werden „seit Generationen“ Abziehbilder für das Fußballer-WM-Sammelalbumder Firma Panini gesammelt.
# Nehmen Sie an, dass jedes Paket mit fünf Abziehbilder bei der Produktion im Jahr 2010 völlig zufällig mit
# fünf verschiedenen aus den insgesamt 640 Fotos gefülltwurde. Wie wahrscheinlich war es dann,
# dass man durch Kauf eines Paketes sein Album vervollständigt hat, wenn nur noch 5 Abziehbilder gefehlt haben?.
dhyper(x = 5, m = 5, n = 640-5 ,k = 5)
1 / choose(640, 5)
# kein mehrmaliges Ziehen, daher geht Binominalkoeffizient

# Ü40
# -----------------------------------------------------------------------------------------------------------------------
# Aus einer Gruppe von 20 Personen mit acht Frauen und zwölf Männern werden fünf Personenzufällig ausgewählt.
# Wie wahrscheinlich ist es, dass in einer so ausgewählten Gruppe mehrFrauen als Männer sind?

# mehr Frauen >= 3
1-phyper(2, m = 8, n = 12, k = 5)
# oder
phyper(2, m = 8, n = 12, k = 5, lower.tail = FALSE)


# Ü41
# -----------------------------------------------------------------------------------------------------------------------
# Beim statischen Testen von Hypothesen (siehe Kapitel III) wird in den Sozial- und Wirtschaftswissenschaften die
# Wahrscheinlichkeit für einen α-Fehler, den man genau dann macht, wenn mansich irrtümlich für die Einshypothese statt
# für die Nullhypothese entscheidet, üblicherweise mitα = 0,05 festgelegt. Wie wahrscheinlich ist es demnach, dass bei
# 10 unabhängigen Durchführungen solcher Tests
# a) kein einziges Mal
# b) einmal,
# c) höchstens zweimal
# ein solcher Fehler passiert,
# wenn die Nullhypothese gültig ist?

p <- 0.05

# a)
dbinom(0, 10, p)

# b)
dbinom(1, 10, p)

# c)
pbinom(2, 10, p)


# Ü42
# -----------------------------------------------------------------------------------------------------------------------
# Erfahrungsgemäß bedarf ein neuer „Tablet-PC“ einer bestimmten Marke innerhalb der ersten beiden
# Jahre mit einer Wahrscheinlichkeit von 0,2 einer Reparatur. Wie wahrscheinlich ist es,dass von 5 lagernden Geräten
# a) keines
# b) eines
# c) mindestens eines
# in den ersten beiden Jahren nach dem Verkauf repariert werden muss?  

# a)
dbinom(0, 5, 0.2)
# b)
dbinom(1, 5, 0.2)
# c)
1-pbinom(0, 5, 0.2)


# Ü43
# -----------------------------------------------------------------------------------------------------------------------
# In einer Bevölkerung vom Umfang N = 2 Millionen besitzen 40 % eine bestimmte Eigenschaft.Es werden 40 Erhebungseinheiten
# zufällig (und natürlich ohne Zurücklegen) gezogen. Wie wahrscheinlich ist es, dass 16 davon diese Eigenschaft besitzen,
# dass also in der Stichprobe derselbe Anteil dieser Eigenschaft vorkommt, wie in der Grundgesamtheit? Sollten Sie Probleme
# mit den großen Zahlen bei der Hypergeometrischen Verteilung bekommen, versuchen Sie es mit der Binomialverteilung.
# Warum ist dies hier erlaubt?

white <- 2000000*0.4
black <- 2000000*0.6
dhyper(16, white, black, k = 40)


dbinom(16, 40, 0.4)

# Dies ist erlaubt - obwohl ohne Zurücklegen gezogen wird - da bei großen Grundgesamtheiten und
# vergleichsweise kleinen Stichproben der Unterschied zwischen dem Ziehen ohne und dem Ziehen mit
# Zurücklegen praktisch verschwindet. Daher sind die Lösungen der (falschen) Binomialver- teilung jenen der
# (eigentlich richtigen) Hypergeometrischen Verteilung sicherlich sehr ähnlich.


# Ü44
# -----------------------------------------------------------------------------------------------------------------------
# Ein Merkmal sei standardnormalverteilt.
# Wie wahrscheinlich ist es, dass ein Messwert von
# a) höchstens 1,65,
# b) höchstens 1,96,
# c) mindestens 1,65,
# d) größer als – 1,65,
# e) kleiner als – 1,96 auftritt?

# a)
pnorm(1.65)
# b)
pnorm(1.96)
# c)
1-pnorm(1.65)
# d)
1-pnorm(-1.65)
# e)
pnorm(-1.96)


# Ü44
# -----------------------------------------------------------------------------------------------------------------------
# Die Länge von Schrauben einer Produktion ist normalverteilt mit Erwartungswert μ= 6 cm und
# theoretische Varianzσ 2= 0,01 (cm2). Wie wahrscheinlich ist es, dass eine Schraube
# a) höchstens 6,15 cm,
# b) höchstens 6,196 cm lang ist?
# c) mindestens 5,85 cm,
# d) höchstens 5,83 cm lang ist.

# standardisierung = 
(6.15 - 6) / sqrt(0.01)
pnorm(1.5)

# a)
pnorm(6.15, 6, sqrt(0.01))
# b)
pnorm(6.196, 6, sqrt(0.01))
# c)
1-pnorm(5.85, 6, sqrt(0.01))
# d)
pnorm(5.83, 6, sqrt(0.01))

# Ü46
# -----------------------------------------------------------------------------------------------------------------------
# Fortsetzung von Ü45:
# Berechnen Sie nun jene Länge der Schrauben, die mit einer Wahrscheinlichkeit von 0,95 unterschritten wird.
qnorm(p = 0.95, mean = 6, sd = sqrt(0.01))



# Ü47
# -----------------------------------------------------------------------------------------------------------------------
# Fortsetzung von Ü45 bis Ü46: Berechnen Sie das symmetrische Intervall,
# in welchem die Längeder Schrauben mit einer Wahrscheinlichkeit von 0,95 liegt.
qnorm(c(0.025, 0.975), 6, sqrt(0.01))


# Ü48
# -----------------------------------------------------------------------------------------------------------------------
# Die Länge von Drehbleistiftminen (in cm) verteilt sich annähernd normal mit
# Erwartungswert μ= 5 und theoretische Varianz σ2= 0,25.
# Wie wahrscheinlich ist es dann, dass eine solche Mine
# a) kürzer als 5,2 cm,
# b) länger als 5,98 cm ist?

# a)
pnorm(5.2, 5, sqrt(0.25))
# b)
1-pnorm(5.98, 5, sqrt(0.25))

# Ü49
# -----------------------------------------------------------------------------------------------------------------------
# Fortsetzung von Ü48:
# Wie wahrscheinlich ist es, dass eine solche Mine zwischen 4,8 und 5,2 cm lang ist?

pnorm(5.2, 5, sqrt(0.25))-pnorm(4.8, 5, sqrt(0.25))

# Ü50
# -----------------------------------------------------------------------------------------------------------------------
# Die Psychologen Stanford, Binet und Wechsler haben festgestellt, dass die Intelligenz –
# definiertals geistige Leistungsfähigkeit bei der Lösung von Testaufgaben – unter der Bevölkerung normalverteilt ist.
# Für Mittelwert und Standardabweichung des Intelligenzquotienten (= IQ) gilt:μ= 100 und σ= 15.
# In welchem symmetrischen Intervall liegt der IQ einer zufällig ausgewählten Testperson mit einer Wahrscheinlichkeit von
# a) von 0,95,
# b) von 0,5
# ?

# a)
qnorm(c(0.025, 0.975), 100, 15)
# b)
qnorm(c(0.25, 0.75), 100, 15)










