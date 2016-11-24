
rm(list=ls())

# ==================================================
# Aufgabe: Zusammenhang nominaler Merkmale
# ==================================================

# Problem:
# Um festzustellen, ob ein Zusammenhang zwischen Werbekampagne und Absatzwachstum besteht,
# wurden 100 Unternehmen nach der angewandten Werbekampagne sortiert und ihr Absatzwachstum
# nach der Kampagne erfasst.

m <- matrix(c(19, 7, 1, 27, 8, 13, 4, 5, 16), ncol = 3)
colnames(m) <- c("stark", "mittel", "schwach")
rownames(m) <- c("A", "B", "C")
m

# ==================================================
# Aufgabe: Zusammenhang nominaler Merkmale
# ==================================================
# Problem: Berechnen Sie ein geeignetes Mass für den Zusammenhang zwischen
# diesen beiden Merkmalen und interpretieren Sie Ihr Ergebnis.

# test <- chisq.test(m)
# chi2 <- unname(test$statistic)

# Cramer's V
# https://en.wikipedia.org/wiki/Cram%C3%A9r's_V

cramersV <- function(x){
  test <- chisq.test(x)
  chi2 <- unname(test$statistic)
  
  sqrt(chi2/(sum(x)*(min(dim(x))-1)))
}

round(cramersV(m), 3)
# round(lsr::cramersV(m), 3)
# gleiches Resultat

# Rule of Thumb
# --------------------------------
# 0.0         = kein
# 0.0 - 0.2   = schwacher
# 0.2 - 0.6   = mittlerer
# 0.6 - 1     = starker
# 1.0         = vollständiger
# --------------------------------

# Nach der Faustregel von Cramer's V herrscht mit einem Wert von **0.354**
# eine **mittlerer** Zusammenhang zwischen den Variablen


# ==================================================
# Statistischer Zusammenhang: Metrische Merkmale
# ==================================================
# Problem: Laden Sie den Data Frame StorchBabies aus der Datei StorchBabies.RData.
# Die Tabelle zeigt neben der Anzahl der Storchenpaare auch die Geburtenrate (in 1000 Geburten pro Jahr)
# in 17 europäischen Ländern.

load("~/Google/datenanalyse/homework/stats/Aufgabe 5/StorchBabies.RData")

# Bestimmen Sie mit einer passenden Kennzahl den Zusammenhang
# zwischen den Merkmalen Storchenpaare und Geburtenrate.

# Zusammenhang zweier metrischer Variablen werden mit dem Korrelationskoeffizient bestimmt
round(cor(StorchBabies$Storchenpaare, StorchBabies$Geburtenrate, method = "pearson"), 3)

# Rule of Thumb 
# --------------------------------------
# +           = positiver Zusammenhang
# -           = negativer Zusammenhang

# 0.0         = kein
# 0.0 - 0.2   = schwacher
# 0.2 - 0.6   = mittlerer
# 0.6 - 1     = starker
# 1.0         = vollständiger
# ---------------------------------------

# Mit einem Korrelationskoeffizient von **0.609** besteht ein **starker** Zusammenhang
# zwischen den Variablen. Ein kausaler Zusammenhang kann nicht bestimmt werden.


# ==================================================
# Statistischer Zusammenhang: Ordinale Merkmale
# ==================================================
# Problem: Auf einer Whiskydegustation wurden verschiedene Whiskysorten sowohl
# von einem professionellen Tasting-Master als auch von einem privaten Whiskyfreund begutachtet.
# Beide konnten pro Whisky Punkte zwischen 0 („furchtbar schlecht“) und 12 („fantastisch“) vergeben.
# Es ergab sich folgende Bewertung:

whiskey <- matrix(c(9,7,1,5,10,12,6,10,5,8,8,3), ncol=6)
rownames(whiskey) <- c("Punkte des Tasting-Masters",
                       "Punkte des Whiskyfreunds")
whiskey

# Spearman's rank correlation coefficient
round(cor(whiskey[1,], whiskey[2,], method = "spearman"), 3)

# Spearmanscher Korrelationskoeffizient kann wie der Korrelatoionskoffizient nach Pearson
# intepretiert werden.
# Mit einem Korrelationskoeffizient von **0.371** herrscht eine **mittlerer**
# Zusammenhang zwischen den Variablen











