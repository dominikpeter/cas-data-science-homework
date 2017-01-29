
# -----------------------------------------------------------------------------------------------------------------------
# Title:  Lösung zu Aufgaben aus dem Buch
# Autor:  Dominik Peter
# Date:   2017-01-26
# -----------------------------------------------------------------------------------------------------------------------

rm(list=ls())
library(magrittr)

# Ü36
# -----------------------------------------------------------------------------------------------------------------------
# Bei einem Rouletteversuch wird zufällig eine der ganzen Zahlen von 0 bis 36 realisiert.
# Wie groß sind die Wahrscheinlichkeiten dafür, dass die Kugel
# a) auf die Zahl 22
# b) auf eine gerade Zahl
# c) eine Zahl zwischen 1 und 12 fällt,
# d) eine Zahl zwischen 1 und 12 gefallen ist, wenn man weiß, dass eine Zahl zwischen 1 und 18 gekommen ist?

# a)
1/length(0:36)
# b)
18/length(0:36)
# c)
12/length(0:36)
# d)
12/length(1:18)


# Ü37
# Wie viele gleich wahrscheinliche Gruppen zu 5 Personen lassen sich aus 20 Personen (A – T)
# ziehen und wie wahrscheinlich ist es, dass
# a) die Gruppe {A, B, C, D, E}
# b) die Gruppe {A, C, F, H, M} gezogen wird,
# c) die Person A in der 5-Personen-Gruppe vorkommt?

# Anzahl Kombinationen
choose(20,5)

# a)
1/choose(20,5)
# b)
1/choose(20,5)
# c)
5/20
# oder
(choose(20,5)-choose(19,5)) / choose(20,5)


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


# Ü54
# -----------------------------------------------------------------------------------------------------------------------
# Bei der Abstimmung über den EU-Beitritt Österreichs stimmten
# im Jahr 1994 66,6 Prozent der Bevölkerung dafür.
# In welchem Bereich wäre an diesem Tag mit einer Wahrscheinlichkeit von
# 0,95 ein diesbezüglicher Anteil in einer zufällig aus dieser Grundgesamtheit
# gezogenen uneingeschränkten Zufallsauswahl gelegen, wenn man dazu
# a) 100,
# b) 400,
# c) 800
# Personen ausgewählt hätte?

# a)
prop.test(x = 100*2/3, n=100, p=2/3)

# b)
prop.test(x = 400*2/3, n=400, p=2/3)

# c)
prop.test(x = 800*2/3, n=800, p=2/3)

# Leicht andere Resultate im Buch
# a)
0.666 + qnorm(0.975) * sqrt(0.666*(1-0.666) / 100)
0.666 - qnorm(0.975) * sqrt(0.666*(1-0.666) / 100)


# Ü56
# ----------------------------------------------------------------------------------------------------------------------
# Ein Meinungsforschungsinstitut veröffentlichte das
# Umfrageergebnis einer Zufallsstichprobe vonn = 500 Personen
# aus der Grundgesamtheit aller Wahlberechtigten.
# Es gaben 80 Prozent der Befragten an, gegen den Einkauf
# von neuen Flugzeugen für die Landesverteidigung zu sein.Berechnen Sie
# a) das Konfidenzintervall zur Sicherheit 1−α= 0,95,
# b) das Konfidenzintervall zur Sicherheit 1−α= 0,9,
# c) das Konfidenzintervall zur Sicherheit 1−α= 0,99 
# für diese relative Häufigkeit in der Gesamtheit aller Wahlberechtigten.

# a)
prop.test(x = 500*0.8, n = 500, p = 0.8)

# oder
0.8 + qnorm(0.975) * sqrt(0.8*(1-0.8) / 500)
0.8 - qnorm(0.975) * sqrt(0.8*(1-0.8) / 500)

# b)
prop.test(x = 500*0.8, n = 500, p = 0.8, conf.level = 0.9)

0.8 + qnorm(0.95) * sqrt(0.8*(1-0.8) / 500)
0.8 - qnorm(0.95) * sqrt(0.8*(1-0.8) / 500)

# c)
prop.test(x = 500*0.8, n = 500, p = 0.8, conf.level = 0.99)

0.8 + qnorm(0.995) * sqrt(0.8*(1-0.8) / 500)
0.8 - qnorm(0.995) * sqrt(0.8*(1-0.8) / 500)


# Unterschied prop.test und chisq.test
# ----------------------------------------------------------------------------------------------------------------------

prop.test(table(survey$Smoke, survey$Sex))
chisq.test(table(survey$Smoke, survey$Sex))
prop.test(table(survey$Sex))

prop.test(table(quine$Eth, quine$Sex), correct=FALSE) 
chisq.test(table(quine$Eth, quine$Sex), correct = FALSE)
# 
# Eigentlich der gleiche Test. Wenn es nur eine eindimensionaler Test ist, liefert 
# prop.test übersichtlechere Resultate inkl. Confintervall
# ----------------------------------------------------------------------------------------------------------------------


# Ü57
# ----------------------------------------------------------------------------------------------------------------------
# Eine Zufallsstichprobe von n = 300 Werkstücken aus einer Produktion
# ergab einen Ausschussanteil p = 0,14.
# Berechnen Sie daraus das Konfidenzintervall zur Sicherheit 1−α= 0,95
# für den Ausschussanteil π in der gesamten Produktion.

0.14 + qnorm(0.975) * sqrt(0.14*(1-0.14) / 300)
0.14 - qnorm(0.975) * sqrt(0.14*(1-0.14) / 300)

prop.test(300*0.14, n = 300, p= 0.14)


# Ü58
# ----------------------------------------------------------------------------------------------------------------------
# In einer Meinungsumfrage soll festgestellt werden, wie hoch der derzeitige Stimmenanteil einerbestimmten
# Partei wäre. Wie viele Wahlberechtigte sind in einer uneingeschränkten Zufallsauswahl aus der Grundgesamtheit
# mindestens zu befragen, wenn das Konfidenzintervall zur Sicherheit 1−α= 0,95 eine Schwankungsbreite ε von 0,03
# besitzen soll und man davon ausgehenkann, dass diese relative Häufigkeit
# a) bei etwa 42 Prozent,
# b) zwischen 5 und 15 Prozent,
# c) bei mindestens 30 Prozent liegen wird?

# a)
ceiling( qnorm(0.975)^2/0.03^2 * 0.42 * (1-0.42) )

# b)
ceiling( qnorm(0.975)^2/0.03^2 * 0.15 * (1-0.15) )
# Wert nehmen, der 0.5 am nächsten liegt

# c)
# Achtung bei mindestens 30 %
ceiling( qnorm(0.975)^2/0.03^2 * 0.5 * (1-0.5) )
# Für π: jenen Wert zwischen 0,3 und 1, der 0,5 am Nächsten liegt. Das ist 0,5 selbst!


# Ü59
# ----------------------------------------------------------------------------------------------------------------------
# Die relative Häufigkeit des Auftretens einer bestimmten Eigenschaft in der Gesamtbevölkerung soll
# in einer Stichprobe geschätzt werden. Welcher Stichprobenumfang ist zu wählen, wenn keinerlei
# Abschätzung des tatsächlichen Anteils existiert und das Konfidenzintervall zur
# Sicherheit1−α= 0,95 eine von Ihnen festzulegende Schwankungsbreite aufweisen soll?

# Annahme von E 0.02
ceiling( qnorm(0.975)^2/0.02^2 * 0.5 * (1-0.5) )

# Ü60
# ----------------------------------------------------------------------------------------------------------------------
# In einer Mitarbeiterbefragung soll mittels einer Zufallsstichprobe erhoben werden,
# wie hoch diederzeitige Ablehnung von Umstrukturierungen unter den 1.200 Beschäftigten ist.
# Wie viele Personen sind mindestens zu befragen, wenn das Konfidenzintervall zur Sicherheit 1−α= 0,95 für
# diesen Anteil eine Schwankungsbreite von ε= 0,025 besitzen soll und man vermutet, dass dieser bei 0,8 liegen wird?

# Achtung kleine Grundgesamtheit

ceiling( ( 1200 * qnorm(0.975)^2 *0.8 * (1-0.8) ) / ( (1200-1) * 0.025^2 + qnorm(0.975)^2 * 0.8 * (1-0.8) ) )



# Ü61
# ----------------------------------------------------------------------------------------------------------------------
# Eine politische Partei will feststellen, ob sich die relative Häufigkeit an österreichischen EU-Beitritts-Befürwortern
# gegenüber dem Ergebnis bei der Volksabstimmung im Jahr 1994 inzwischen verändert hat. Bei der Volksabstimmung hatten 66,6 Prozent zugestimmt.
# a) Formulieren Sie für dieses Problem geeignete statistische Hypothesen.In einer aktuellen Umfrage unter n = 800 zufällig ausgewählten
# Wahlberechtigten erhält man die Antworten, die sich in der zu diesem Beispiel im Internet bereitstehenden Excel-Datei befinden.
# b) Entscheiden Sie sich auf Basis dieses Stichprobenergebnisses auf einem Signifikanzniveau α= 0,05 für eine der beiden Hypothesen.
# c) Wie würde Ihre Entscheidung bei p = 0,536 lauten?
# d) Wie würde Ihre Entscheidung bei p = 0,636 lauten?


library(readxl)
u61 <- read_excel("~/Google/R/homework/homework/testprep/Lerndatei (Kapitel 3) ab EXCEL 2007.xlsx", 
                  sheet = "Ü61") %>% .[,1:2]

u61$Einstellung <- factor(u61$Einstellung, levels = c(1,2), labels = c("dafür", "dagegen"))

table(u61$Einstellung)

# a)
# H0 = relative Häufigkeit = 66.6%
# H1 = relative Häufigkeit <> 66.6%

# b)
prop.test(table(u61$Einstellung), n = 800, p = 2/3)
# H0 verwerfen

# manuelle Berechnung des Konfintervalls um das Ergebnis aus der Grundgesamtheit

0.666 + qnorm(0.975) * sqrt((0.666*(1-0.666))/800)
0.666 - qnorm(0.975) * sqrt((0.666*(1-0.666))/800)

# H0 verwerfen

# c)
prop.test(800*0.536, n = 800, p = 2/3)

# d)

prop.test(800*0.636, n = 800, p = 2/3)
# Das Testergebnis ist nicht signifikant.



# Ü62
# ----------------------------------------------------------------------------------------------------------------------
# Ein TV-Sender möchte feststellen, ob die Einschaltquote (= relative Häufigkeit der zugeschalteten Haushalte
# von allen Haushalten mit TV-Anschluss) einer TV-Show unter 10 Prozent gefallen ist. 
# a) Formulieren Sie für dieses Problem geeignete statistische Hypothesen. In einer Stichprobe unter n = 1.200 Haushalten hatten sich 102 Haushalte zugeschaltet.
# b) Entscheiden Sie sich auf Basis dieses Stichprobenergebnisses auf einem Signifikanzniveau α= 0,05 für eine der beiden Hypothesen.
# c) Wie würde Ihre Entscheidung bei p = 0,09 lauten?d) Wie würde Ihre Entscheidung bei p = 0,08 lauten?

# a)
# H0: relative Häufkeit => 10%
# H1: relative Häufigkeit < 10%

# b)
prop.test(102, n = 1200, p = 0.1, alternative = "less")
# Test signifikant. H1 wir akzeptiert

# c)
# p = 0.09
prop.test(1200*0.09, n = 1200, p = 0.1, alternative = "less")
# Test nicht signifikant. H0 wir beibehalten

prop.test(1200*0.08, n = 1200, p = 0.1, alternative = "less")
# Test signifikant. H1 wir akzeptiert

# Ü63
# ----------------------------------------------------------------------------------------------------------------------
# In einer Zufallsstichprobe vom Umfang n = 350 aus der wahlberechtigten Bevölkerung stellteman Ende des Jahres 2010 fest,
# dass 192 der Befragten einem EU-Beitritt der Türkei skeptischgegenüberstanden.Konnte man aus dem Stichprobenergebnis auf dem 
# Signifikanzniveau α= 0,05 folgern, dasseine Mehrheit der Gesamtbevölkerung in dieser Frage skeptisch eingestellt war?






