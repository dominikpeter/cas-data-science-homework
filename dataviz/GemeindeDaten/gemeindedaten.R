
rm(list=ls())

# data.table version
# -----------------------------------------------------
library(dplyr)
library(readr)

df <- read_csv("dataviz/GemeindeDaten/gemeindedaten.csv")

# Beantworten Sie folgende Fragen:
# Wie viele Gemeinden gab es in der Schweiz im Jahr 2014?
nrow(df)
# Was ist die mittlere Einwohnerzahl einer Schweizer Gemeinde?
mean(df$bev_total)
# Wie viele Einwohner leben in der grössten Gemeinde?
df %>%
  arrange(desc(bev_total)) %>%
  head(1) %>%
  select(gmdename, kantone, grossregionen, bev_total)
# Wie viele in der kleinsten?
df %>%
  arrange(bev_total) %>%
  head(1) %>%
  select(gmdename, kantone, grossregionen, bev_total)

library(ggplot2)
# 5. In welchem Kanton gibt es am meisten Gemeinden? In welchem am wenigsten?

df %>% 
  group_by(kantone) %>% 
  summarise(N = n()) %>% 
  ggplot(aes(x = reorder(kantone, N), y = N)) +
  geom_point(size = 3) +
  geom_segment(aes(xend = kantone, yend = 0), lineend = 1) +
  coord_flip()

# 6. Betrachten Sie die Einwohnerzahlen der Gemeinden gruppiert nach Sprachregionen. Wie
# heissen die jeweils grössten Gemeinden?

df %>%
  group_by(sprachregionen) %>% 
  mutate(rank = dense_rank(-bev_total)) %>% 
  filter(rank == 1) %>% 
  ggplot(aes(x = gmdename, y = bev_total, fill = sprachregionen)) +
  geom_bar(stat = "identity", position = "dodge")


# 7. Betrachten Sie die Veränderung der Einwohnerzahl von 2010 bis 2014 nach Sprachregionen. In
# welcher Sprachregionen sind die Gemeinden am stärksten gewachsen? In welcher am wenigsten oder gibt es Sprachregionen, in welcher die Einwohnerentwicklung in der Tendenz sogar eher rückläufig ist? Analysieren sie zusätzlich graphisch, ob die Unterscheidung von städtischen und ländlichen Gemeinden dabei eine Rolle spielt?
# 
# 
# 
# 
# 8. Untersuchen Sie die Zusammenhangsstruktur folgender Variablen:
#   bev_dichte, bev_ausl, alter_0_19, alter_20_64, alter_65.,bevbew_geburt, sozsich_sh, strafen_stgb
# Gibt es Korrelationen? Falls ja, lassen Sie sich erklären oder sind sie eher unerwartet? Suchen Sie sich einen Ihnen interessant erscheinenden Zusammenhang und schauen Sie sich diesen in einem eigenen Scatterplot an
# 9. Visualisieren Sie eine Kontingenztabelle mit den Variablen Stadt_Land und Sprachregionen. Welcher Gemeindetyp überwiegt bei deutschsprachigen Gemeinden, welcher bei italienischsprachigen Gemeinden. Gibt es in jeder Sprachregion isolierte Städte?
# 10. Erstellen Sie ein politisches Profil nach Sprachregionen mit der Hilfe der Variablen zu den Wähleranteilen.
# 



