
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
# für die Nullhypothese entscheidet, üblicherweise mit α = 0,05 festgelegt. Wie wahrscheinlich ist es demnach, dass bei
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
# theoretische Varianz σ 2= 0,01 (cm2). Wie wahrscheinlich ist es, dass eine Schraube
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
prop.test(x = 100*2/3, n=100, p=0.6666667, correct = FALSE)

prop.test

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

prop.table(table(u61$Einstellung))

# a)
# H0 = relative Häufigkeit = 66.6%
# H1 = relative Häufigkeit <> 66.6%

# b)
prop.test(table(u61$Einstellung), n = 800, p = 2/3,correct = FALSE)
# H0 verwerfen

# manuelle Berechnung des Konfintervalls um das Ergebnis aus der Grundgesamtheit
0.666 + qnorm(0.975) * sqrt((0.666*(1-0.666))/800)
0.666 - qnorm(0.975) * sqrt((0.666*(1-0.666))/800)

# oder

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
# c) Wie würde Ihre Entscheidung bei p = 0,09 lauten?
# d) Wie würde Ihre Entscheidung bei p = 0,08 lauten?

# a)
# H0: relative Häufkeit => 10%
# H1: relative Häufigkeit < 10%

# b)
prop.test(102, n = 1200, p = 0.1, alternative = "less", correct = FALSE)
# Test signifikant. H1 wir akzeptiert



# Ü63
# ----------------------------------------------------------------------------------------------------------------------
# In einer Zufallsstichprobe vom Umfang n = 350 aus der wahlberechtigten Bevölkerung stellte man Ende des Jahres 2010 fest,
# dass 192 der Befragten einem EU-Beitritt der Türkei skeptisch gegenüberstanden.
# Konnte man aus dem Stichprobenergebnis auf dem 
# Signifikanzniveau α= 0,05 folgern, dass eine Mehrheit der Gesamtbevölkerung in dieser Frage skeptisch eingestellt war?
# 

# p = Anteil an skeptischer Bevölkerung
# H0: Relative Häufigkeit <= 0.5
# H1: Relative Häufigkeit > 0.5

prop.test(192, n = 350, p = 0.5, alternative = "greater")
# Test ist signifikanz. H0 wird verworfen

# Ü64
# ----------------------------------------------------------------------------------------------------------------------
# Von Werkstücken, die ein Jahr lang gelagert wurden, sind 40 Prozent unbrauchbar.
# Nach einer Änderung der Lagerbedingungen wird überprüft, ob sich die relative Häufigkeit an unbrauchbaren Werkstücken verringert hat.
# a) Formulieren Sie für dieses Problem geeignete statistische Hypothesen.
# b) Entscheiden Sie sich auf dem Signifikanzniveau α= 0,05 für eine der beiden Hypothesen, wenn unter 100 zufällig ausgewählten Stücken nunmehr 36 Prozent unbrauchbar waren.

# p = unbrauchbare Werkstücke
# H0: p => 40%
# H1: p < 40%


prop.test(100*0.36, 100, p = 0.4, correct = FALSE, alternative = "less")
# Test nicht signifikant. H0 wird beibehalten


# Abstecher Vergleich chisq.test zu prop.test
# ----------------------------------------------------------------------------------------------------------------------

# H0: p = 40%
# H1: p <> 40%

prop_test <- prop.test(100*0.36, 100, p = 0.4, correct = FALSE); prop_test
# Test ist nicht signifikanz, H0 wir beibehalten


test <- c(1:100)
test[1:36] <- 1
test[37:100] <- 2
test <- factor(test, levels = c(1,2), labels = c("brauchbar", "unbrauchbar"))
prop.table(table(test))

# geht wohl auch oder?
chisq_test <- chisq.test(table(test), p = c(0.4, 0.6)); chisq_test



prop_test$p.value
chisq_test$p.value


prop_test$statistic
chisq_test$statistic

# das gleiche wenn less oder greater, dann P-Value / 2
# ----------------------------------------------------------------------------------------------------------------------


# Ü66
# ----------------------------------------------------------------------------------------------------------------------
# Zur Erprobung der Wirksamkeit eines Schlafmittels wurde zuerst an 264 zufällig ausgewählten
# Testpersonen das Merkmal Schlafdauer gemessen. Die Messergebnisse finden sich in einer
# Excel-Datei im Internet.Berechnen Sie das Konfidenzintervall zur Sicherheit 1−α= 0,95 für die mittlere Schlafdauer in der Grundgesamtheit.

library(readxl)
u66 <- read_excel("~/Google/R/homework/homework/testprep/Lerndatei (Kapitel 3) ab EXCEL 2007.xlsx", 
                                                sheet = "Ü66")
schlafdauer <- u66$Schlafdauer %>% na.omit

mu <- mean(schlafdauer)
error <- sd(schlafdauer)/sqrt(length(schlafdauer))
qnorm(0.975, mean = mu, sd = error)
qnorm(0.025, mean = mu, sd = error)
  
# Ü67
# ----------------------------------------------------------------------------------------------------------------------
# Bei der Abfüllung von Mineralwasser in Literflaschen wird der Magnesiumgehalt je Liter gemessen. n = 116
# Kontrollmessungen ergaben folgende Werte für den Gehalt an Magnesium (in mg/l):x = 25,452, s2= 0,850.
# Berechnen Sie das Konfidenzintervall zur Sicherheit 1−α= 0,95 für den mittleren Magnesiumgehalt in der Gesamtproduktion.

qnorm(0.975, mean = 25.452, sd = sqrt(0.850/116))
qnorm(0.025, mean = 25.452, sd = sqrt(0.850/116))



# Ü68
# ----------------------------------------------------------------------------------------------------------------------
# Eine Zufallsstichprobe vom Umfang n = 836 ergab hinsichtlich eines Merkmals x einen Mittelwert x = 22,5 bei einer Standardabweichung
# von s = 3,2. Bestimmen Sie das Konfidenzintervall zur Sicherheit 1−α= 0,95 für den wahren Mittelwert von x.

mu <- 22.5
error = 3.2/sqrt(836)

qnorm(0.975, mean = mu, sd = error)
qnorm(0.025, mean = mu, sd = error)


# Ü69
# ----------------------------------------------------------------------------------------------------------------------
# Eine Zufallsstichprobe vom Umfang n = 86 ergab hinsichtlich eines normalverteilten Merkmals x Messergebnisse,
# die einer im Internet bereitgestellten Excel-Datei zu finden sind. Testen Sie zum Signifikanzniveau α= 0,05 folgende Hypothesen:
# a) H0: μ = 4500 gegen H1: μ ≠4500.
# b) H0: μ ≥ 4500 gegen H1: μ< 4500.

u69 <- read_excel("~/Google/R/homework/homework/testprep/Lerndatei (Kapitel 3) ab EXCEL 2007.xlsx", 
                                                sheet = "Ü69")

x <- u69$`Merkmal x` %>% na.omit()

# a)
# Eigentlich muss ich die Standardaweichung aus der Stichprobe nehmen, daher t.test. Aber das Buch macht wohl kein T.test?
t.test(x, alternative = "two.sided", paired = FALSE, mu = 4500)
# H0 wird verworfen, Test ist signifikant

# b)
t.test(x, alternative = "less", paired = FALSE, mu = 4500)
# H0 wird verworfen. Test ist signifikant

# nein wie gedacht Buch nimmt das 0.975 Quartil von der Normalverteilung <- also Z-Test



# Ü70
# ----------------------------------------------------------------------------------------------------------------------
# Die (stetige) Punktezahl bei einem Aufnahmetest sei annähernd normalverteilt mit μ= 75.
# Nach Einführung verpflichtender vorbereitender Kurse soll überprüft werden, ob sich der Mittelwert der Punktezahlen erhöht hat.
# a) Formulieren Sie für dieses Problem geeignete statistische Hypothesen.
# b) Testen Sie diese Hypothesen auf einem Signifikanzniveau von α= 0,05,
# wenn in einer Zufallsstichprobe vom Umfang n = 120 nach Einführung der Kurse ein Mittelwert von 78,4 Punkten
# erzielt wird und in dieser Stichprobe eine Standardabweichung von s = 6 gemessen wird.

H0: u <= 75
H1: u > 75

# rein theoretisch wieder t.test
TeachingDemos::z.test(78.4, mu = 75, alternative = "greater", sd = 6, n = 120)
# Test signifikanz H0 wird verworfen


# Ü71
# ----------------------------------------------------------------------------------------------------------------------
# Die Psychologen Stanford, Binet und Wechsler haben festgestellt, dass der Intelligenzquotient inder Bevölkerung normalverteilt ist.
# In der Bevölkerung besitzt der IQ einen Mittelwert von 100. Überprüfen Sie auf einem Signifikanzniveau von α= 0,05, ob dieser Mittelwert
# unter Studierenden höher ist. Dazu steht eine Zufallsstichprobe vom Umfang n = 100 zur Verfügung, in der ein durchschnittlicher IQ von 108
# bei einer Standardabweichung von s = 19 gemessen wurde.

H0: IQ <= 100
H1: IQ > 100

TeachingDemos::z.test(108, mu = 100, sd = 19, n=100)
# Test ist signifikanz. H0 wird verworfen


# Ü73
# ----------------------------------------------------------------------------------------------------------------------
# Zwei im Abstand zweier Monate gezogene unabhängige Stichproben von je 700 Wahlberechtigten erhoben für den Spitzenkandidaten
# einer Partei seine Sympathisanten. Sie finden die Datenin einer im Internet bereitgestellten Excel-Datei. Die Schlagzeile in
# einer Zeitung lautete: „Kandidat legt zu!“ Überprüfen Sie diese Behauptung auf einem Signifikanzniveau von α= 0,05,
# nachdem Sie geeignete Hypothesen für diesen Test aufgestellt haben.

# H0: Differenz >= 0
# H1: Differenz < 0
library(readxl)
u73 <- read_excel("~/Google/R/homework/homework/testprep/Lerndatei (Kapitel 3) ab EXCEL 2007.xlsx", 
                                                sheet = "Ü73")

u73 <- u73[,1:2]
u73[] <- lapply(u73, as.factor)

u73 <- tidyr::gather(u73)
u73$value <- as.factor(u73$value)
u73$key <- as.factor(u73$key)

t(table(u73$key, u73$value))

prop.test(table(u73$key, u73$value), alternative = "less", correct = FALSE)
# Test ist signifikant. H0 wird verworfen


# geht irgendwie nicht so

# Ü74
# ----------------------------------------------------------------------------------------------------------------------
# Verwenden Sie die Daten aus Ü73:
# Die Partei will nun überprüfen, ob sich die relative Häufigkeitan Sympathisanten für
# ihren Spitzenkandidaten innerhalb der zwei Monate auf einem Signifikanzniveau von α= 0,05 verändert hat.

prop.test(table(u73$key, u73$value), alternative = "two.sided", correct = FALSE)

# nicht im bereich von 0


# Ü75
# ----------------------------------------------------------------------------------------------------------------------
# Es soll die positive Wirkung eines konzentrationssteigernden Mittels auf die Lernleistung getestet werden.
# Dazu werden unabhängig voneinander eine Gruppe von 200 Versuchspersonenohne Einnahme des Mittels und eine gleich große nach Einnahme des Mittels geprüft.
# Es bestehen 41,5 Prozent der ersten und 44,0 Prozent der zweiten Gruppe.
# Formulieren Sie geeignete Hypothesen und testen Sie diese auf einem Signifikanzniveau von α= 0,05.

# H0: 
# H1: 
  
prop.test(x = 200*0.44, p = 0.415, n = 200, alternative = "greater", correct = FALSE)  

# test ist nicht signifikanz, h0 verworfen



# Ü76
# ----------------------------------------------------------------------------------------------------------------------
# Eine Stichprobe vom Umfang n = 350 lieferte vor einem Jahr eine relative Häufigkeit von
# 65,1 Prozent an Zustimmung zur Regierungsarbeit. Es soll nun auf einem Signifikanzniveau vonα= 0,05 überprüft werden,
# ob sich dieser Anteil im Jahresabstand verändert hat. Dazu wirdeine neue Stichprobe vom Umfang n = 420 gezogen.
# In dieser ergibt sich ein diesbezüglicherAnteil von 62,1 Prozent. Formulieren Sie die diesbezüglichen Hypothesen
# und führen Sie den Test auf dem Signifikanzniveau α= 0,05 durch.

# H0 =
# H1 nicht gleich

prop.test(420*0.621, n = 420, p = 0.651)

# nicht gleich
# aber wahrscheinlich müssen dieste Test alle von hand gerechnet werden, R ist nicht ausgelegt auf solche Fragen



# Ü77
# ----------------------------------------------------------------------------------------------------------------------
# In Ü66 wurde an 264 zufällig ausgewählten Personen das Merkmal Schlafdauer gemessen.
# Nu nwird eine zweite, von der ersten unabhängige Stichprobe vom Umfang 200 gezogen und abermals das Merkmal Schlafdauer gemessen,
# nachdem diesen neuen 200 Personen jedoch Schlafmittel verabreicht wurden. Als Kennzahlen dieser neuen Stichprobe ergeben sich: x=7.1,
# s = 1,5. 
# Überprüfen Sie auf einem Signifikanzniveau von α= 0,05, ob sich die Einnahme des Schlafmittels positiv auf die Schlafdauer auswirkt.

library(readxl)
u77 <- read_excel("~/Google/R/homework/homework/testprep/Lerndatei (Kapitel 3) ab EXCEL 2007.xlsx", 
                                                sheet = "Ü66")
schlaf <- u77$Schlafdauer %>% na.omit()

TeachingDemos::z.test(7.1, mu = mean(schlaf), sd = 1.5, alternative = "greater", n = 200)
# Testergebnis = signifikant


# Ü78
# ----------------------------------------------------------------------------------------------------------------------
# Für die Lebensdauer von Taschenrechnerbatterien soll überprüft werden,
# ob sich die diesbezüglichen Mittelwerte zweier Hersteller in den Grundgesamtheiten unterscheiden.
# Eine Stichprobevom Umfang 125 der einen Marke liefert einen Mittelwert von 5.996,5 Stunden und eine Standardabweichung von s = 65,3 h.
# Eine Stichprobe von 122 Batterien der anderen Marke ergibt diesbezüglich die Werte:x = 6.125,6 und s = 57,0. 
# Formulieren Sie geeignete Hypothesen und entscheiden Sie sich auf einem Signifikanzniveauα= 0,05 für eine der beiden.

# H0: Diff = 0
# H1: Diff <> 0

d0 <- qnorm(0.975) * sqrt((65.3^2/125) + (57^2 / 122))
d1 <- d0*-1

5996.5 - 6125.6

# nicht in d0 <-> d1 daher H0 verwerfen


# Ü79
# ----------------------------------------------------------------------------------------------------------------------
# In Ü11 wurde das Ergebnis einer Befragung unter 794 Personen zum Thema Geschlecht und Einstellung zur Euro-Währung präsentiert.
# In Ü28 wurde mit Hilfe einer Excel-Lerndatei der statistische Zusammenhang dieser beiden Merkmale gemessen.
# Verwenden Sie nun diese Ergebnisse,um zu überprüfen, ob die beiden Merkmale auch in der Grundgesamtheit aller Wahlberechtigtenzusammenhängen.
# a) Formulieren Sie für dieses Problem geeignete statistische Hypothesen.
# b) Entscheiden Sie sich anhand der Ergebnisse von Ü28 auf einem Signifikanzniveau von α= 0,05 für eine der Hypothesen.
# c) Verwenden Sie die Excel-Funktion CHIVERT: Wie groß ist der zur Teststatistik gehörende p-Wert?


library(readxl)
library(magrittr)
u79 <- read_excel("~/Google/R/homework/homework/testprep/Lerndatei (Kapitel 1) für EXCEL ab 2007.xlsx", 
                                                    sheet = "Ü28", skip = 2) %>% as.data.frame()


u79 <- u79[,-1]
u79 <- u79[1:2, 1:4]
rownames(u79) <- u79[,1]
u79 <- u79[,-1]
u79[] <- lapply(u79, as.numeric)
m <- as.matrix(u79)

# h0: x2 = 0
# h1: x2 > 0

chisq.test(m)

# test signifikant h0 wird verworfen


# Ü80
# ----------------------------------------------------------------------------------------------------------------------
# In einer Zufallsstichprobe unter 350 Abonnenten einer Zeitung wurde erhoben,
# ob diese dieZeitung jeden Tag lesen und wie ihre Einstellung zu einer bestimmten Frage ist:

m <- matrix(c(10, 20, 120, 100, 60, 40), ncol = 3, byrow = TRUE)
m
#                 positiv neutral negativ
# täglich            10      20     120
# nicht täglich     100      60      40

colnames(m) <- c("positiv", "neutral", "negativ")
rownames(m) <- c("täglich", "nicht täglich")

# a) Formulieren Sie für die Überprüfung des Zusammenhangs von Leseverhalten und Einstellung unter 
# den gesamten Abonnenten der Zeitung geeignete statistische Hypothesen und testen Sie diese anhand der Daten auf einem Signifikanzniveau α= 0,05.
# b) Wie groß ist der zur errechneten Teststatistik gehörende p-Wert (Excel)?

# a)
# H0: X2 = 0
# H1: X2 > 0

chisq.test(m)

# H0 wird verworfen. Test ist signifikant


# Ü81
# In einer Studie wird der Einfluss von Strategietraining bei n = 235 zufällig
# ausgewählten Managern auf den Unternehmenserfolg untersucht:Prüfen Sie auf einem Signifikanzniveau α= 0,05,
# ob ein statistischer Zusammenhang besteht.Wie groß ist der zur Teststatistik gehörende p-Wert (Excel)?

m <- matrix(c(40, 75, 30, 90), ncol = 2, byrow = TRUE)
rownames(m) <- c("nein","ja")
colnames(m) <- c("nein", "ja")
m
#       nein ja
# nein   40 75
# ja     30 90

# H0: X2 = 0
# H1: X2 > 0
chisq.test(m, correct = FALSE)

# nicht signifikant. H0 wird beibehalten


# Ü82
# ----------------------------------------------------------------------------------------------------------------------
# Zur Untersuchung der Wirksamkeit von Vorsorgeimpfungen gegen Grippe wurde eine uneingeschränkte Zufallsstichprobe aus der Bevölkerung
# vom Umfang n = 1.000 gezogen. Die Überprü-fung des Zusammenhangs der beiden Merkmale Impfschutz (ja/nein)
# und Erkrankung (ja/nein) in einem Chiquadrattest ergab einen p-Wert von 0,0943.
# Schließen Sie mit dieser Informationen über den Ausgang des statistischen Tests auf dem Signifikanzniveau α= 0,05,
# ob die Merkmale Impfschutz und Erkrankung in der Bevölkerung eine nstatistischen Zusammenhang aufweisen.


# Nein P-Vert grösser als Signifikanzniveau




# Ü83
# ----------------------------------------------------------------------------------------------------------------------
# Bei n = 400 zufällig ausgewählten Automobilen eines 1986 erstmals zugelassenen Typs wurde 
# für ein elektronisches Bauteil das Merkmal Lebensdauer (in 1.000 km) erhoben:
# Lebensdauer    pi 
# unter 40       0,140
# 40 – 60        0,350
# 60 – 80        0,370
# über 80        0,140
# Überprüfen Sie auf einem Signifikanzniveau α= 0,05, ob die Lebensdauer dieser Bauteile nichtnormalverteilt ist.
# In der Stichprobe errechnete sich ein Mittelwert von 60.000 km und eine Stichprobenstandardabweichung von 20.000 km.
# Wie groß ist außerdem der zum errechneten Chiquadrat gehörende p-Wert (Excel)?

norm <- c(pnorm(40, mean = 60, sd = 20),
          pnorm(60, 60, 20) - pnorm(40, 60, 20),
          pnorm(80, 60, 20) - pnorm(60, 60, 20),
          pnorm(80, 60, 20, lower.tail = FALSE))

# norm <- c(0.159, 0.341, 0.341, 0.159) # ich habe genauere Werte
obser <- c(0.14, 0.35, 0.37, 0.14)
x2 <- chisq.test(400*obser, p = norm, correct = FALSE)$statistic

pchisq(x2, df = 1, lower.tail = FALSE)

# df = classes - 3
# classes = observationen in dem Beispiel sind es 4

# Nicht signifikant, H0 beibehalten


# Ü84
# ----------------------------------------------------------------------------------------------------------------------
# Verwenden Sie die Daten aus Ü83 und überprüfen Sie abermals, 
# ob die Lebensdauer dieser Bauteile nicht normalverteilt ist.
# In der Stichprobe ergab sich diesmal ein Mittelwert von 80.000 km und eine Standardabweichung von 20.000 km bei
# gleicher Häufigkeitsverteilung auf die Intervalle. Wie groß ist der zum errechneten Chiquadrat gehörende p-Wert (Excel)?
# 
# h0: x2 = 0
# h1: x2 > 0

norm <- c(pnorm(40, mean = 80, sd = 20),
          pnorm(60, 80, 20) - pnorm(40, 80, 20),
          pnorm(80, 80, 20) - pnorm(60, 80, 20),
          pnorm(80, 80, 20, lower.tail = FALSE))

x2 <- chisq.test(obser*400, correct = FALSE, p = norm)$statistic %>% unname()

pchisq(x2, df = 1, lower.tail = FALSE)
# extrem klein. p-value wird verworen






