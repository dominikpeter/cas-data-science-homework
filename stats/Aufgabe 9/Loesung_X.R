

# ------------------------------------------------------------------------------------------------
# Title:  Lösung zu Aufgaben Testen
# Autor:  Dominik Peter
# Date:   2017-01-05
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

o <- as.numeric(table(survey$Smoke))
n <- length(na.omit(survey$Smoke))
e <- c(0.045, 0.795, 0.085, 0.075) * n

chi <- sum((o-e)^2/e)
dof <- length(table(survey$Smoke))-1
pchisq(chi, df = dof, lower.tail = FALSE)


