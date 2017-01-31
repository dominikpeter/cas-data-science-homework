  
# ------------------------------------------------------------------------------------------------
# Title:  Lösung zu Aufgaben Regression
# Autor:  Dominik Peter
# Date:   2017-01-26
# ------------------------------------------------------------------------------------------------

rm(list=ls())


# ------------------------------------------------------------------------------------------------
# Aufgabe: Schätzen eines y-Wertes
# ------------------------------------------------------------------------------------------------
# Betrachten Sie den Datensatz mtcars.
# Modellieren Sie das Gewicht wt der Autos als Funktion der Motorleistung hp.
# Welches durchschnittliche Gewicht wird für ein Auto geschätzt,
# dessen Motor eine Leistung von 200 PS aufweist?

model <- lm(wt ~ hp, data = mtcars)
predict(model, data.frame(hp = 200))

# Gewicht von 3.718439 

# ------------------------------------------------------------------------------------------------
# Aufgabe: Bestimmtheitsmass
# ------------------------------------------------------------------------------------------------
# Bestimmen Sie das Bestimmtheitsmass r2 des linearen Modells zu mtcars.

r2 <- summary(model)$r.squared
r2

# ------------------------------------------------------------------------------------------------
# Aufgabe: Bestimmtheitsmass
# ------------------------------------------------------------------------------------------------
# Untersuchen Sie, ob zwischen den Grössen wt und hp aus
# mtcars ein signifikanter Zusammenhang besteht.

# H0 :β1 = 0
# H1 :β1 ≠ 0

model_summary <- summary(model)
coefficients(model_summary)

# Mit P-Value = 4.145827e-05 liegt ein signifikanter Zusammenhang vor.
# Es wird die H1 Hypothese akzeptiert

# ------------------------------------------------------------------------------------------------
# Aufgabe: Konfidenzintervalle für y
# ------------------------------------------------------------------------------------------------
# Bestimmen Sie ein 95%-Konfidenzintervall für das durchschnittliche
# Gewicht bei einer Motorenleistung von 200 PS.

predict(model, data.frame(hp = 200), interval = "confidence", level = 0.95)


# ------------------------------------------------------------------------------------------------
# Aufgabe: Prognoseintervalle für y
# ------------------------------------------------------------------------------------------------
# Bestimmen Sie ein 95%-Prognoseintervall für das durchschnittliche
# Gewicht bei einer Motorenleistung von 200 PS.

predict(model, data.frame(hp = 200), interval = "predict", level = 0.95)


# ------------------------------------------------------------------------------------------------
# Aufgabe: Residuen-Plot
# ------------------------------------------------------------------------------------------------
# Stellen Sie die Residuen des linearen Modells zwischen dem
# Gewicht und der Leistung aus mtcars grafisch dar.

plot(mtcars$hp, residuals(model), ylab="Residuen", xlab="HP", main="Residuen-Plot")
abline(0,0)

# oder

plot(model, which = 1)


# ------------------------------------------------------------------------------------------------
# Aufgabe: QQ-Plot
# ------------------------------------------------------------------------------------------------
# Erstellen Sie das Normal-Wahrscheinlichkeits-Diagramm der Residuen aus dem Datensatz mtcars.

plot(model, which = 2)




