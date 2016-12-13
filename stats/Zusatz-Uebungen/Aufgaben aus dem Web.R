library(magrittr)
library(ggplot2)

# Eine Urne enthält 4 schwarze, 3 rote und 3 weisse Kugeln. Es wird 10-mal mit
# Zurücklegen gezogen. Wie wahrscheinlich ist es, genau 5 schwarze Kugeln zu ziehen? 
p <- 4 / (3+3+4)

data.frame(x = factor(0:10), y = dbinom(0:10, 10, p)) %>% 
  ggplot(aes(x = x, y = y)) +
  stat_sum(geom = "bar")

p

dbinom(5, 10, p)

# 2. Ein fairer Würfel wird 36 mal geworfen. Berechne die Wahrscheinlichkeit dafür, dass die
# Augenzahl 6 in der erwarteten Anzahl, also 6-mal, eintritt.

p <- 1/6

dbinom(6, 36, p)

# Der Anteil der Nichtschwimmer an einer Schule beträgt 10%. In einer Klasse werden vier
# Schüler zufällig ausgewählt.
# Wie gross ist die Wahrscheinlichkeit dafür, dass genau einer der Schüler Nichtschwimmer
# ist?

dbinom(1, 4, 0.1)

# 
# In einem Keller sind alte Weine gelagert; man weiss, dass im Durchschnitt 20% davon
# nicht mehr geniessbar sind. Wie gross ist die Wahrscheinlichkeit, dass

# a) von zehn Flaschen acht noch geniessbar sind,
dbinom(8, 10, .8)


# b) von 20 Flaschen 16 noch geniessbar sind. 
dbinom(16, 20, 0.8)


 
# Der Computer eines Heiratsvermittlungsbüros kombiniert aus den Interessenten
# "Traumpaare". Längere Beobachtungen haben ergeben, dass 30% der "Produktion"
# unbrauchbar ist. Man lässt nun den Computer 10 Paare zusammenstellen. Wie gross ist
# die Wahrscheinlichkeit, dass genau 7 gute Paare entstanden sind? 

dbinom(7, 10, .7)




# Angenommen, wir werfen einen Würfel zehn mal, und notieren die Anzahl der Sechsen,
# die wir werfen (eine Sechs ist also ein "Treffer").

p <- 1/6

# b) Wie hoch ist die Wahrscheinlichkeit für zwei Sechser?

dbinom(2, 10, p)

# c) Wie hoch ist die Wahrscheinlichkeit für höchstens einen Sechser?

pbinom(1, 10, p)


# d) Wie hoch ist die Wahrscheinlichkeit für mindestens zwei Sechser?

1 - pbinom(1, 10, p)



# Bei einer Versicherung gehen pro Tag durchschnittlich 3 Schadensmeldungen wegen Autounfällen ein.
#
# b) Wie hoch ist die Wahrscheinlichkeit, dass genau die 3 erwarteten Schadensmeldungen eintreten?

dpois(3, lambda = 3)

# c) Mit welcher Wahrscheinlichkeit gehen zwei bis vier Schadensmeldungen ein?

lower <- ppois(1, 3)
upper <- ppois(4, 3, lower.tail = FALSE)
1 - (lower+upper)

ppois(1, lambda = 3, lower.tail = FALSE) - ppois(4, lambda = 3, lower.tail = FALSE)

# d) Wie hoch ist die Wahrscheinlichkeit, dass mindestens eine Schadensmeldung hereinkommt?

1 - ppois(0, lambda = 3)

# e) Welche Verteiung hat die Anzahl der Schadensmeldung pro Arbeitswoche (5 Tage)?





