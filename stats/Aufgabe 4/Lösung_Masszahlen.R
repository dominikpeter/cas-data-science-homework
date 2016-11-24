rm(list=ls())


# if(!require(data.table)) install.packages("data.table")

df <- faithful

# ==================================================
# Aufgabe: arithmetischer Mittelwert
# ==================================================

# Problem: Bestimmen Sie die durchschnittliche Wartezeit zwischen den Eruptionen aus faithful.
mean(df$waiting, rm.na = TRUE)

# ==================================================
# Aufgabe: Median
# ==================================================

# Problem: Bestimmen Sie den Median der Wartezeiten zwischen den Eruptionen aus faithful.
median(df$waiting, na.rm = TRUE)

# ==================================================
# Aufgabe: Quartile
# ==================================================
# Problem: Bestimmen Sie die Quartile der Wartezeiten aus faithful
quantile(df$waiting)

  # ==================================================
# Aufgabe: Quartile
# ==================================================
# Problem: Bestimmen Sie das 0.17-Quantil, das 43%-Quantil,
# das 67%-Quantil und das 0.85-Quantil der Wartezeiten aus faithful.
quantile(df$waiting, c(0.17, 0.43, 0.67, 0.85))


# ==================================================
# Aufgabe: Spannweite
# ==================================================
# Problem: Bestimmen Sie die Spannweite der Wartezeiten aus faithful.


# max(df$waiting) - min(df$waiting)

diff(range(df$waiting))


  
# ==================================================
# Aufgabe: Interquartilsabstand
# ==================================================
# Problem: Bestimmen Sie den Interquartilsabstand der Wartezeiten aus faithful.

IQR(df$waiting)

# ==================================================
# Aufgabe: Boxplot
# ==================================================
# Problem: Bestimmen Sie den Boxplot der Wartezeiten aus faithful.

boxplot(df$waiting, col = "#ecf0f1", notch = TRUE, main = "Boxplot der Wartezeiten")
grid(nx=NA, ny=NULL)

# ==================================================
# Aufgabe: Varianz
# ==================================================
# Problem: Bestimmen Sie die beiden Varianzen der Wartezeiten aus faithful.

var2 <- function(x, sample = TRUE){
  if (sample)
    return(var(x))
  
  var(x)*(length(x)-1)/length(x)
}
  

# Stichprobenvarianz
round(var2(df$waiting, sample = TRUE), 3)

# Populationsvarianz
round(var2(df$waiting, sample = FALSE), 3)

# ==================================================
# Aufgabe: Standardabweichung
# ==================================================
# Problem: Bestimmen Sie die beiden Varianzen der Wartezeiten aus faithful.

sd2 <- function(x, sample = TRUE){
  if (sample)
    return(sd(x))
  
  sqrt(var(x)*(length(x)-1)/length(x))
}

# Stichprobenstandardabweichung
round(sd2(df$waiting, sample = TRUE), 3)

# Populationsstandardabweichung
round(sd2(df$waiting, sample = FALSE), 3)


# ==================================================
# Aufgabe: Korrelationskoeffizient
# ==================================================
# Problem: Öffnen Sie den Datensatz swiss. Bestimmen Sie die Korrelation zwischen 
# der Fruchtbarkeitsrate und dem Anteil derjenigen, deren Ausbildung über den
# Primarschulabschluss hinausgeht.

ch <- swiss
# ?swiss

# [,1]	Fertility	Ig, ‘common standardized fertility measure’
# [,4]	Education	% education beyond primary school for draftees.

round(cor(ch$Fertility, ch$Education), 3)

# Es herrscht ein starker negativer Zusammenhang zwischen den Variablen (Faustregel)

