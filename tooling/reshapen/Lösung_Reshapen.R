rm(list=ls())

##################################################
## Reshapen
##################################################

# ================================================
# Load
# ================================================

library(data.table)
library(magrittr)
library(stringr)

load("~/Google/datenanalyse/homework/tooling/reshapen/Satisfaction.RData")

df <- as.data.table(data)

# ================================================
# Tidy
# ================================================

col <- colnames(df)
check <- grepl("age", col)
id <- col[!check]

# two step melting
melted.df <- df %>%
  melt(id = id, variable.name = "id.alter", value.name = "alter") %>%
  melt(id = c("idpers", "alter", "id.alter"), variable.name = "jahr") 

# tidy up the data
tidy.df <- melted.df[, jahr := paste0('20',str_sub(jahr, 2,3)) %>% as.integer] %>%
  .[, .(idpers, jahr, alter, zufriedenheit = value)] %>%
  .[order(idpers)]


head(tidy.df, 6)



# just for fun...
# --------------------------------------------------------------------------------

library(ggplot2)

tidy.df[, cuts := cut(alter,breaks = pretty(alter, nclass.Sturges(alter)))] %>%
  .[alter > quantile(alter, 0.05) & alter < quantile(alter, 0.95)] %>%
  na.omit() %>%
  ggplot(aes(x=cuts, y=zufriedenheit)) +
  geom_boxplot(notch = TRUE, fill = "#42729B", alpha = 3/5) +
  ylab("Zufriedenheit") +
  xlab("\nAlterskategorie")



