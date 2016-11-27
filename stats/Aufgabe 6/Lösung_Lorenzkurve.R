rm(list=ls())

# --------------------------------------------------------------------------------------------------
# Aufgabe: Lorenzkurve und Ginikoeffizient
# --------------------------------------------------------------------------------------------------

if(!require("ineq")) install.packages("ineq")


# Problem: In der folgenden Tabelle sind die Marktanteile der 5 marktstärksten
# TV-Sender des deutschen Fernsehmarktes aus dem Jahr 2007 in Prozent angegeben.

markt.daten <- c("ARD-Dritte" = 13.5,
                 "ARD" = 13.4,
                 "ZDF" = 12.9,
                 "RTL" = 12.4,
                 "Sat.1" = 9.6)

Lcx <- Lc(markt.daten)

# Zeichnen Sie die Lorenzkurve
# --------------------------------------------------------------------------------------------------

plot(Lcx, main = "Lorenzkurve",
     xlab=expression(u[i]),
     ylab = expression(v[i]))


# Berechnen Sie den normalen Ginikoeffizienten
# --------------------------------------------------------------------------------------------------

round(Gini(markt.daten), 3)


# Berechnen Sie den normierten 
# --------------------------------------------------------------------------------------------------

round(Gini(markt.daten, corr = TRUE), 3)




