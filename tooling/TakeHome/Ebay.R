rm(list=ls())
library(ggplot2)
library(magrittr)
library(foreign)
library(stringr)
library(data.table)

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
new_df <- df %>% 
  .[, rating := sepos/rowSums(.SD), .SDcols = c("sepos", "seneg")] %>% 
  .[, makellos := factor(rating > 0.98, levels = c(TRUE, FALSE), labels = c("Ja", "Nein"))] %>%
  .[, cat := str_trim(str_replace(subcat, "\\ \\(\\d+\\)", ""))] %>% # clean categorie name
  .[, sold := factor(sold, levels = c(1, 0), labels = c("Ja", "Nein"))] %>%
  .[sepos > 11]  # eigentlich sollten ja nur die verkauften analyisiert betrachtet werden . sold = "Ja"

rbindlist(list(head(new_df), tail(new_df)))

# es gibt preise mit NA, daher funktioniert der reorder nicht ohne anonyme funktion mit na.rm = TRUE
# -1 als hack zum reversen
new_df[sold == "Ja"] %>%
  ggplot(aes(x=reorder(factor(cat), price, function(x) mean(x, na.rm = TRUE)*-1), y = price)) + 
  geom_boxplot(aes(fill = makellos), notch = TRUE, alpha = .8) +
  scale_fill_manual(values = c("#66CC99", "#FC575E"), name = "Makellos") +
  xlab("\nKategorie") +
  ylab("Preis")

  

