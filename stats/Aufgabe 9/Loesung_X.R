

# ------------------------------------------------------------------------------------------------
# Title:  Lösung zu Aufgaben Testen
# Autor:  Dominik Peter
# Date:   2017-01-05
# ------------------------------------------------------------------------------------------------

rm(list=ls())
library(MASS)


# ------------------------------------------------------------------------------------------------
# Aufgabe: Linksseitiger Test bei μ, σ bekannt
# ------------------------------------------------------------------------------------------------
# Problem: Die Datei „lightbulbs.txt“ enthält eine neue Stichprobe des Glühbirnenherstellers.
# Laden Sie die Datei mit dem Befehl scan. Lässt sich aufgrund dieser Stichprobe die Behauptung des Herstellers,
# dass die Glühbirnen eine Mindestlebensdauer von 10′000 Stunden besitzen, bei einem Signifikanzniveau von 1% verwerfen?
# Die Standardabweichung beträgt 120 Stunden.


o <- as.numeric(table(survey$Smoke))
n <- length(survey$Smoke)
e <- c(0.045, 0.795, 0.085, 0.075) * n

chi <- sum(((o-e)^2)/e)
dof <- n - 1
pchisq(chi, df = dof, lower.tail = FALSE)



