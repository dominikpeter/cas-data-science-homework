

# ------------------------------------------------------------------------------------------------
# Title:  Lösung zu Aufgaben X2
# Autor:  Dominik Peter
# Date:   2017-01-18
# ------------------------------------------------------------------------------------------------

rm(list=ls())
library(MASS)


# ------------------------------------------------------------------------------------------------
# Aufgabe: Anpassungstests
# ------------------------------------------------------------------------------------------------
# Problem: Die Unileitung vermutet folgendes Rauchverhalten ihrer Studierenden.
# Heavy Never Occassionaly Regular 
# 4.5% 79.5% 8.5% 7.5%
# Prüfen Sie, ob die Stichprobe aus survey sich mit dieser Behauptung verträgt.
# Bestimmen Sie den p-Wert, ohne auf die Funktion chisq.test zurückzugreifen.

# H0: die beobachteten und die erwarteten Häufigkeiten sind gleich
# Ha: die beobachteten und die erwarteten Häufigkeiten sind verschieden.


o <- table(survey$Smoke)
n <- sum(o)
e <- c(0.045, 0.795, 0.085, 0.075) * n

chi <- sum((o-e)^2/e)
dof <- length(table(survey$Smoke))-1
pchisq(chi, df = dof, lower.tail = FALSE)

# chisq.test(table(survey$Smoke), p=c(0.045, 0.795, 0.085, 0.075))

# P-Value = 0.9909295; Die Null-Hypothese kann nicht verworfen werden

# ------------------------------------------------------------------------------------------------
# Aufgabe: Unabhängigkeitstests
# ------------------------------------------------------------------------------------------------
# Problem: Die Datei RauchenGeschlecht.xlsx zeigt die Geschlecht des Neugeborenen
# mit dem Rauchverhalten der Eltern in den ersten Monaten der Schwangerschaft.
# Sind diese beiden Variablen unabhängig?
# Arbeiten Sie mit α = 0.05.

library(readxl)

df <- read_excel("stats/Aufgabe 9/RauchenGeschlecht.xlsx")
tbl <- table(df)

chisq.test(tbl)

# P-Value 0.2287 > Signifikanzniveau. Nullypothese wird beibehalten





