rm(list=ls()) #bereinige Workspace

# ===============================================================
# Aufgaben zu 2dim Verteilung 
# ===============================================================

# Load Packages und Daten
# if (!require("data.table")) install.packages("data.table")
if (!require("MASS")) install.packages("MASS")

# Pfad ändern für das Laden der Daten
load("/Users/dominikpeter/Google/datenanalyse/moodle/R-Unterrichtsdateien-20161027/Daten_WachstumX.RData")

df <- Daten_Wachstum

# ===============================================================
# Aufgabe: zweidimensionale Häufigkeitsverteilung
# ===============================================================

# Problem: Bestimmen Sie die zweidimensionale Häufigkeitsverteilung
# der Merkmale Geschlecht und Motiv.

freq <- table(df$Geschlecht, df$Motiv)
freq

# ===============================================================
# Aufgabe: Randverteilungen
# ===============================================================

# Problem: Fügen Sie die Randverteilungen zur zweidimensionalen
# Häufigkeitsverteilung der Merkmale Geschlecht und Motiv hinzu.

randv <- addmargins(freq)
randv

# ===============================================================
# Aufgabe: Relative Zweidimensionale Verteilung
# ===============================================================

# Problem: Bestimmen Sie die relative zweidimensionale
# Häufigkeitsverteilung der Merkmale Geschlecht und Motiv.

rel.freq <- prop.table(freq)
rel.freq

# ===============================================================
# Aufgabe: Bedingte Verteilung 1
# ===============================================================

# Problem: Wie verteilen sich die verschiedenen Motive innerhalb
# der Geschlechtergruppen?

bedingte.rel.freq <-prop.table(freq, margin = 1)
addmargins(bedingte.rel.freq)

# ===============================================================
# Aufgabe: Bedingte Verteilung 2
# ===============================================================

# Problem: Wie verteilen sich die beiden Geschlechter auf die verschiedenen Motive?

bedingte.rel.freq2 <- prop.table(freq, margin = 2)
addmargins(bedingte.rel.freq2)










