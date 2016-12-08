# ------------------------------------------------------------------------------------------------
# Title:  Analyse Ebay Date
# Autor:  Dominik Peter
# Date:   2016-12-04
# ------------------------------------------------------------------------------------------------

rm(list=ls())

library(data.table)
library(magrittr)
library(foreign)
library(ggplot2)
library(stringr)
library(broom)

raw_df <- read.dta("http://www.farys.org/daten/ebay.dta") %>% as.data.table()
# sold: Ob das Mobiltelefon verkauft wurde
# price: Der erzielte Verkauftspreis
# sprice: Der Startpreis der Auktion
# sepos: Anzahl positiver Bewertungen des Verkäufers
# seneg: Anzahl negativer Bewertungen des Verkäufers
# subcat: Das Modell des Mobiltelefons
# listpic: Kategorialer Indikator, ob die Auktion ein Thumbnail, ein “has-picture-icon” oder kein Thumbnail hat.
# listbold: Dummy, ob die Auktion fettgedruckt gelistet ist
# sehasme: Dummy, ob der Verkäufer eine “Me-page” hat oder nicht

# Data Wrangling
raw_df[, rating := sepos/rowSums(.SD), .SDcols = c("sepos", "seneg")]
raw_df[, `:=` (makellos = factor(rating > .98, levels = c(TRUE, FALSE), labels = c("Ja", "Nein")),
               cat = str_replace(subcat, "\\ \\(\\d+\\)", ""))]

df <- raw_df[sepos > 11, !"subcat", with = FALSE]

rbindlist(list(head(df), tail(df)))

# Plotting
# ------------------------------------------------------------------------------------------------

# Es gibt Preise mit NA (nicht verkauft), daher funktioniert der Reorder nicht ohne anonyme Funktion mit na.rm = TRUE
# Absteigende Anordung mit -1
df %>%
  ggplot(aes(x=reorder(factor(cat), price, function(x) mean(x, na.rm = TRUE)*-1), y = price)) +
  geom_boxplot(aes(fill = makellos), notch = TRUE, position = position_dodge(.85)) +
  scale_fill_manual(values = c("#66CC99", "#FC575E"), name = "Makellos") +
  xlab("\nKategorie") +
  ylab("Preis") +
  ggtitle("Ebay Verkäufe nach Kategorie") +
  theme(panel.background = element_rect(fill = "#F0F1F5"),
        panel.grid.major = element_line(color = "white", size = 1.1),
        panel.grid.minor = element_blank())

# Bewertet anhand "Rule of Thumb", dass bei signifikanter Differenz die Notches nicht überlappen sollten.
# Daher die Feststellung, dass kein signifikanter Unterschied zwischen den makellosen und
# nicht makellosen Ratings besteht
# https://en.wikipedia.org/wiki/Box_plot#Variations


# Regression
# ------------------------------------------------------------------------------------------------

# Rechnen Sie zwei kleine Regressionsmodelle für den Preis von verkauften Geräten.
# Modell 1 soll als Prädiktoren den Modelltyp und das Rating beinhalten.
# Modell 2 soll zusätzlich die Variable listpic beinhalten.
# Haben das Rating und die Thumbnails einen Einfluss auf den Verkaufspreis?
# Exportieren Sie eine Regressionstabelle, die beide Modelle beinhaltet.

# df[rating > 0.95] %>%
#   ggplot(aes(rating, price)) +
#   geom_point(alpha = 2/4) +
#   geom_smooth(method = "lm") +
#   facet_grid(.~cat)

# Linear Model 1
model_1 <- lm(price ~ cat + rating, data = df)
summary(model_1)
glance(mode_1)
tidy(model_1)


# Linear Model 2
model_2 <- lm(price ~ cat + rating + listpic, data = df)
summary(model_2)
glance(model_2)
tidy(model_2)

# compare model
list(Model_1 = c(BIC = round(BIC(model_1),2),
                AIC = round(AIC(model_1),2)),
     Model_2 = c(BIC = round(BIC(model_2),2),
                AIC = round(AIC(model_2),2)))
# https://en.wikipedia.org/wiki/Akaike_information_criterion
# https://en.wikipedia.org/wiki/Bayesian_information_criterion


# Die Thumbnails haben mit einem P-Value ~ 7.7e-06 einen signifikanten Einfluss auf den Preis.
# Mit einem Koeffizienten von 6.72 steigt der Preis durchschnittlich um diesen Wert, gegenüber dem Factor "none"
# Mit niedrigeren BIC sowie AIC Werten verbessert sich zudem das Modell beim Hinzufügen des Predictors "listpic"


# analyse listpic
df %>%
  ggplot(aes(x=reorder(factor(cat), price, function(x) mean(x, na.rm = TRUE)*-1), y = price)) +
  geom_boxplot(aes(fill = listpic),notch = TRUE, position=position_dodge(.85)) +
  scale_fill_manual(values = c("#e67e22", "#2ecc71", "#2980b9"), name = "Makellos") +
  xlab("\nKategorie") +
  ylab("Preis") +
  ggtitle("Ebay Verkäufe nach Kategorie") +
  theme(panel.background = element_rect(fill = "#F0F1F5"),
        panel.grid.major = element_line(color = "white", size = 1.1),
        panel.grid.minor = element_blank())




