rm(list=ls())

library(ggplot2)
library(data.table)
library(magrittr)
library(foreign)


df <- read.dta("http://www.farys.org/daten/ebay.dta") %>% as.data.table

# sold: Ob das Mobiltelefon verkauft wurde
# price: Der erzielte Verkauftspreis
# sprice: Der Startpreis der Auktion
# sepos: Anzahl positiver Bewertungen des Verkäufers
# seneg: Anzahl negativer Bewertungen des Verkäufers
# subcat: Das Modell des Mobiltelefons
# listpic: Kategorialer Indikator, ob die Auktion ein Thumbnail, ein “has-picture-icon” oder kein Thumbnail hat.
# listbold: Dummy, ob die Auktion fettgedruckt gelistet ist
# sehasme: Dummy, ob der Verkäufer eine “Me-page” hat oder nicht

# data wrangling
df[, rating := sepos/rowSums(.SD), .SDcols = c("sepos", "seneg")]
df[, makellos := factor(rating > 0.98, labels = c("Nein", "Ja"))]
df[, cat := str_replace(subcat, "\\(\\d+\\)", "")] # clean categorie name

new_df <- df[sepos > 11]

new_df %>% ggplot(aes(x=cat, y = price)) +
  geom_boxplot(aes(fill = makellos), notch = TRUE) +
  scale_fill_brewer(palette = "Set3", name = "Makellose\nBewertung") +
  xlab("\nKategorie") +
  ylab("Preis")
  




