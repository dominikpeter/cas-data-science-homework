library(openxlsx)
library(readxl)
library(magrittr)
library(data.table)


x <- readxl::read_excel("/Users/dominikpeter/Google/datenanalyse/homework/tooling/Datenimport/su-d-01.02.04.04.xls",
                        sheet = "2014", skip = 4)

bev <- x[, 1:2] %>% na.omit
names(bev) <- c("kanton", "Anzahl_Bevölkerung")
bev$Anzahl_Bevölkerung <- as.numeric(bev$Anzahl_Bevölkerung)
bev <- bev[-1, ]


x2 <- readxl::read_excel("/Users/dominikpeter/Google/datenanalyse/homework/tooling/Datenimport/je-d-07.03.02.01.xls", sheet = "2014", skip = 9)

wald <- x2[, 1:2] %>% na.omit
names(wald) <- c("kanton", "waldfläche")

wald$waldfläche <- as.numeric(wald$waldfläche)
wald <- wald[-1, ]

bev$kanton <- stringr::str_trim(bev$kanton)
wald$kanton <- stringr::str_trim(wald$kanton)

bev <- bev %>% as.data.table
wald <- wald %>% as.data.table

merged <- merge(bev, wald, by = "kanton")

hektar <- 400

merged[, AnzahlBäume := waldfläche * 400][, BaumProPers := Anzahl_Bevölkerung / AnzahlBäume]

rank <- merged[order(-BaumProPers)]


write.csv(x = rank, file =  "/Users/dominikpeter/Google/datenanalyse/homework/tooling/Datenimport/rank.csv", row.names = FALSE)



