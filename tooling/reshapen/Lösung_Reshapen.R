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


# Musterlösung
library(dplyr)
library(tidyr)
library(reshape2)
#Das sind Alters und Zufriedenheitsdaten (Generelle Lebenszufriedenheit) von
#SHP Teilnehmern, die von 2000 bis 2014 im Panel waren

#Man muss die Daten ins sehr lange Längsformat überführen
#damit man auf die Jahresinformation (in den Variablennamen enthalten)
#zugreifen kannID=c("idpers"),
data<-melt(data,
           id.vars = "idpers",
           variable.name = "variable",
           value.name = "wert",
           measure.vars = names(data)[names(data)!="idpers"])


#Alternativ:

# data<-gather(data,
#              key= variable,
#              value= wert,
#              p00c44:age14)

#Aus den Variablennamen die Jahresinformation hinausziehen und 
#eine Jahresvariable, sowie eine "Variablenvariable" (Welche Art
#von Information?) bilden
data<-data%>%
  mutate(jahr=ifelse(grepl("c44",variable),
                     2000+as.numeric(substr(variable,2,3)),
                     2000+as.numeric(substr(variable,4,5))),
         variable=ifelse(grepl("c44",variable),"zufriedenheit",
                         "alter"))

#Aus der "Variablenvariable" jeweils ein Beobachtung pro Personenjahr und Dimension
#erstellen
data<-dcast(data, idpers+jahr~variable,value.var="wert")

data

