rm(list=ls())

# --------------------------------------------------------------------------------------------------
# Aufgabe: Lorenzkurve und Ginikoeffizient
# --------------------------------------------------------------------------------------------------

if(!require("ineq")) install.packages("ineq")


# Problem: In der folgenden Tabelle sind die Marktanteile der 5 marktstärksten
# TV-Sender des deutschen Fernsehmarktes aus dem Jahr 2007 in Prozent angegeben.

marktdaten <- c("ARD-Dritte" = 13.5,
                 "ARD" = 13.4,
                 "ZDF" = 12.9,
                 "RTL" = 12.4,
                 "Sat.1" = 9.6)

Lcx <- Lc(marktdaten)

# Zeichnen Sie die Lorenzkurve
# --------------------------------------------------------------------------------------------------

plot(Lcx,
     main = "Lorenzkurve",
     col = "red",
     lty = 5,
     lwd = 1,
     xlab = expression(u[i]),
     ylab = expression(v[i]),
     xaxt = "n",
     yaxt = "n")

seq <- seq(0,1, by = 0.2)
labels <- paste0(seq * 100, "%")

axis(side = 1, at = seq, labels = labels)
axis(side = 2, at = seq, labels = labels)
grid()



# Berechnen Sie den normalen Ginikoeffizienten
# --------------------------------------------------------------------------------------------------

round(Gini(marktdaten), 3)


# Berechnen Sie den normierten 
# --------------------------------------------------------------------------------------------------

round(Gini(marktdaten, corr = TRUE), 3)




