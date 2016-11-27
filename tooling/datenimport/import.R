
rm(list=ls())

library(readxl)
library(magrittr)
library(data.table)
library(stringr)

# Load Excels
# ------------------------------------------------------------------------------------------------
bev <- "~/Google/datenanalyse/homework/tooling/datenimport/bevoelkerung.xls" %>%
  read_excel(sheet = "2014", skip = 6) %>%
  as.data.table %>%
  .[, c(1, 8), with = FALSE] %>%
  na.omit

names(bev) <- c("Kanton", "Anzahl_Einwohner")

wald <- "~/Google/datenanalyse/homework/tooling/Datenimport/waldflaeche.xls" %>% 
  read_excel(sheet = "2014", skip = 10) %>%
  as.data.table %>% 
  .[, c(1, 3), with = FALSE] %>%
  na.omit

names(wald) <- c("Kanton", "Waldfläche")

# Clean Kanton string
# ------------------------------------------------------------------------------------------------
wald[, Kanton := str_trim(Kanton, side = "both")] %>% 
  .[, Kanton := str_replace(Kanton, "\\. ", "\\.")] #St.Gallen und Appenzell machen Probleme

bev[, Kanton := str_trim(Kanton, side = "both")]



# Merge and Calculate
# ------------------------------------------------------------------------------------------------
wald %>% setkey(Kanton)
bev %>% setkey(Kanton)

merged <- merge(bev, wald, all.x = TRUE)

merged[, Anzahl_Bäume := Waldfläche * 400] %>% 
  .[, Baum_Pro_Pers := Anzahl_Bäume / Anzahl_Einwohner]


rank <- merged[ , `:=` (Rang_Baum_Pro_Pers = rank(-Baum_Pro_Pers),
                       Rang_Einwohner = rank(-Anzahl_Einwohner),
                       Rang_Waldfläche = rank(-Waldfläche))] %>%
  .[order(Rang_Waldfläche)] %>% 
  .[, Kum_Waldfläche := cumsum(Waldfläche)] %>% 
  .[, Rel_Kum_Waldfläche := Kum_Waldfläche / sum(Waldfläche)]
  


rank



# Write to CSV
# ------------------------------------------------------------------------------------------------
rank %>% 
  write.table("~/Google/datenanalyse/homework/tooling/datenimport/rank.csv",
              row.names = FALSE, sep = ",")




