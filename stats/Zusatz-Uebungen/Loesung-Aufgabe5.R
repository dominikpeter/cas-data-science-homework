# Aufgabe 5
# Die Aktie eines Unternehmens wuchs innerhalb von fünf Jahren im 1. Jahr um 1,5 Prozent, im 2. Jahr um 28,2 Prozent,
# im 3. Jahr um 80,7 Prozent, im 4. Jahr um 14,5 Prozent und fiel im 5. Jahr um 0,7 Prozent.
# . Berechnen Sie die fünf Wachstumsfaktoren und damit ausgehend von einem Basiskurs von 100 Euro
#   vor Beginn dieser Zeitspanne den Aktienkurs am Ende der fünf Jahre unter Berücksichtigung des jährlichen Wachstums,
# . mit den fünf Wachstumsfaktoren das durchschnittliche jährliche prozentuelle Kurswachstum mit dem Taschenrechner
#   und überprüfen Sie Ihr Ergebnis in Excel mit Hilfe der im Internet bereitstehenden Excel-Lerndatei.
# . Wie hoch hätte demnach der Prozentsatz eines festverzinslichen Wertpapiers sein müssen, um einen höheren
#   Gewinn als die Aktie zu erzielen?

aktien <- c("1 Jahr" = 1.5,
            "2 Jahr" = 28.2,
            "3 Jahr" = 80.7,
            "4 Jahr" = 14.5,
            "5 Jahr" = -0.7)

idx <- (100 + aktien)/100
idx

geom <- exp(mean(log(idx)))


100*e^5


# >21.73442



