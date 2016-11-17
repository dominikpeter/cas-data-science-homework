rm(list=ls()) #bereinige Workspace

# ===============================================================
# Aufgaben zu QuantDaten
# ===============================================================

# Load Packages und Daten
if (!require("MASS")) install.packages("MASS")
if (!require("RColorBrewer")) install.packages("RColorBrewer") # Farbpaletten

df <- faithful

# ===============================================================
# Aufgabe: Häufigkeitsverteilung quantitativer Daten
# ===============================================================

# Bestimmen Sie die Häufigkeitsverteilung der Wartezeiten aus faithful


nbr.intr.waiting <- nclass.Sturges(df$waiting)
# selbe Formel zur Definition der Anzahl Klassen wie in Base hist()
# nclass.FD() hat sich auch häufig als zuverlässige Formel herausgestellt

breaks.waiting <- pretty(df$waiting, nbr.intr.waiting) #Anzahl in Breaks umwandeln
cut.waiting <- cut(df$waiting, breaks.waiting, right=FALSE) # cuts erstellen

tbl.waiting <- table(cut.waiting)
cbind(tbl.waiting)

# Welches Intervall der Eruptionsdauer enthält die meisten Eruptionen?

nbr.intr.erup <- nclass.Sturges(df$eruptions) # selber intervall range formel wie default in base hist
breaks.erup <- pretty(df$eruptions, nbr.intr.erup) # anzahl intervall in breaks umwandeln
cut.erup <- cut(df$eruptions, breaks.erup, right=FALSE) # cuts erstellen

tbl.erup <-table(cut.erup)
cbind(tbl.erup)

# max =
tbl.erup[which.max(tbl.erup)]
# Eruptionen von [4,4.5) kommen mit 73 Vorkommnissen am meisten vor

# Bestimmen Sie die Häufigkeitsverteilung der Eruptionszeiten aus faithful mit der Funktion hist

color.erup <- brewer.pal(name = "Set3", n = length(tbl.erup))

hist(df$eruptions, col = color.erup,
     main = "Histogram of Eruptions", xlab = "eruptions")


# ===============================================================
# Aufgabe: Histogramm
# ===============================================================

# Problem: Zeichnen Sie das Histogramm der Wartezeiten aus faithful

color.waiting <- brewer.pal(name = "Paired", n = length(tbl.waiting))
hist(df$waiting, col = color.waiting, main = "Histogram of Eruptions", xlab = "eruptions")


# ===============================================================
# Aufgabe: Relative Häufigkeitsverteilung stetiger Daten
# ===============================================================

# Problem: Bestimmen Sie die relative Häufigkeitsverteilung der Wartezeiten aus faithful

rel.freq.waiting <- prop.table(tbl.waiting)
cbind(round(rel.freq.waiting * 100, 2))

# ===============================================================
# Aufgabe: Kumulierte Häufigkeitsverteilung
# ===============================================================

# Problem: Bestimmen Sie die kumulierte Häufigkeitsverteilung der Wartezeiten aus faithful.

kum.freq.waiting <- cumsum(tbl.waiting)
cbind(kum.freq.waiting)

# ===============================================================
# Aufgabe: Kumulierte Häufigkeitsverteilungskurve
# ===============================================================

# Problem: Bestimmen Sie die kumulierte Häufigkeitsverteilungskurve der Wartezeiten aus faithful.

plot(kum.freq.waiting, xlab = "Waiting minutes",
     ylab = "cumulative waitings", main = "Old Faithful Waitings")
lines(kum.freq.waiting)

# ===============================================================
# Aufgabe: Kumulierte relative Häufigkeitsverteilung
# ===============================================================

# Problem: Bestimmen Sie die kumulierte relative Häufigkeitsverteilung der Wartezeiten aus faithful

kum.rel.freq.waiting <- cumsum(prop.table(tbl.waiting))
cbind(round(kum.rel.freq.waiting * 100, 2))

# ===============================================================
# Aufgabe: Kumulierte relative Häufigkeitskurve
# ===============================================================

plot(kum.rel.freq.waiting, xlab = "Waiting minutes",
     ylab = "cumulative waitings proportions", main = "Old Faithful Waitings")
lines(kum.rel.freq.waiting)


