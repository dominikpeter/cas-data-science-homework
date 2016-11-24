
rm(list=ls())

library(readxl)
library(magrittr)
library(data.table)
library(stringr)


bev <- "~/Google/datenanalyse/homework/tooling/datenimport/bevoelkerung.xls" %>%
  read_excel(sheet = "2014", skip = 6) %>%
  as.data.table %>%
  .[, c(1, 8), with = FALSE] %>%
  na.omit

names(bev) <- c("Kanton", "AnzahlEinwohner")

wald <- "~/Google/datenanalyse/homework/tooling/Datenimport/waldflaeche.xls" %>% 
  read_excel(sheet = "2014", skip = 10) %>%
  as.data.table %>% 
  .[, c(1, 3), with = FALSE] %>%
  na.omit

names(wald) <- c("Kanton", "Waldfläche")

wald[, Kanton := str_trim(Kanton, side = "both")]
bev[, Kanton := str_trim(Kanton, side = "both")]

merged <- merge(bev, wald, by = "Kanton")

merged[, AnzahlBäume := Waldfläche * 400] %>% 
  .[, BaumProPers := AnzahlEinwohner / AnzahlBäume]


rank <- merged[order(-BaumProPers)]
head(rank)

rank %>% 
  write.table("~/Google/datenanalyse/homework/tooling/datenimport/rank.csv",
              row.names = FALSE, sep = ";")


