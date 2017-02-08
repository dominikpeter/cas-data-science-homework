rm(list=ls()) #bereinige Workspace

# ===============================================================
# Aufgaben zu QualiDaten
# ===============================================================

# Load Packages
if (!require("data.table")) install.packages("data.table")
if (!require("MASS")) install.packages("MASS")

# Load Data
df <- data.table(Painter = rownames(painters), painters)
head(df, 5)


# ===============================================================
# Aufgabe: Häufigkeitsverteilung
# ===============================================================

# Bestimmen Sie die Häufigkeitsverteilung der Variablen
N <- table(df$Composition)
cbind(N)

# Bestimmen Sie mit R diejenige Schule mit den meisten Malern
count.school <- df[, .N, by=School]
count.school[N == max(N)]

# Die Schulen **A** und **D** haben die meisten Maler mit jeweils **10 Malern**


# ===============================================================
# Aufgabe: Relative Häufigkeitsverteilung
# ===============================================================

# Bestimmen Sie die relative Häufigkeitsverteilung der Variablen Composition
RH <- prop.table(table(df$Composition))
cbind(RH)  


# ===============================================================
# Aufgabe: Balkendiagramm
# ===============================================================

# Stellen Sie die Häufigkeitsverteilung der Variablen Composition mit einem Balkendiagramm dar
tbl <- table(df$Composition) 
colors <- c("#6E9BD5", "#F04903", "#ECE653", "#3E4651",
            "#00B5B5", "#92F22A", "#D8335B", "#59A9C2",
            "#E6567A", "#27AE60", "#BBA900", "#81E2E6",
            "#FF4C43", "#FFDE49", "#22202B", "#39B4FF",
            "#B6FF75", "#B6FF75")

barplot(tbl, col = colors, main = "Häufigkeitsverteilung der Variablen Composition")


# ===============================================================
# Aufgabe: Kuchendiagramm
# ===============================================================

# Stellen Sie die Häufigkeitsverteilung der Variablen Composition mit einem Kuchendiagramm dar
tbl <- table(df$Composition)
colors <- c("#6E9BD5", "#F04903", "#ECE653", "#3E4651",
            "#00B5B5", "#92F22A", "#D8335B", "#59A9C2",
            "#E6567A", "#27AE60", "#BBA900", "#81E2E6",
            "#FF4C43", "#FFDE49", "#22202B", "#39B4FF",
            "#B6FF75", "#B6FF75")

pie(tbl, col = colors, radius = 1, main = "Häufigkeitsverteilung der Variablen Composition")


# ===============================================================
# Aufgabe 1: Gruppenstatistik
# ===============================================================

# Finden Sie die Schule mit dem höchsten Wert der Variable Composition
df[Composition == max(Composition), .(School, Composition)]

# Schule **E** und **G** haben Werte mit **Composition = 18**


# ===============================================================
# Aufgabe 2: Gruppenstatistik
# ===============================================================

# Bestimmen Sie den Anteil aller Maler, deren Color-Wert mindestens 14 beträgt
check <- df[, ColorCheck := Colour >= 14]
head(check, 5)

# Anzahl Maler mit Color >= 14
sum.ColorCheck <- sum(check$ColorCheck)
sum.ColorCheck

# Relativer Anteil Maler mit Color >= 14
mean.ColorCheck <- mean(check$ColorCheck)
mean.ColorCheck

paste0("Anzahl Maler >= 14: ", sum.ColorCheck)
paste0("Relative Anzahl Maler >= 14: ", round(mean.ColorCheck * 100, 2), "%")

# **20 Maler** haben Colour >= 14. Das sind **37.04 %** aller Maler in den Beispieldaten


mean(painters$Colour >= 14)


