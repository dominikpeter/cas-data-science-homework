


rm(list=ls())

library(dplyr)
library(readr)
library(lubridate)

fit <- read_csv("dataviz/Grafik/fit.csv")

fit <- fit %>% mutate(Datum = ymd(Datum),
                      Monat = month(Datum) %>% as.factor,
                      Jahr = year(Datum) %>% as.factor,
                      Wday = wday(Datum) %>% as.factor)


fit %>%
  ggplot(aes(y = Schrittzahl, x = Wday)) + 
  geom_boxplot()


fit %>%
  mutate(Schlaf_Total = `Leichter Schlaf – Dauer (ms)` + `Tiefer Schlaf – Dauer (ms)`,
         Qualität_Schlaf = `Tiefer Schlaf – Dauer (ms)` / Schlaf_Total) %>% 
  filter(!is.na(Schlaf_Total)) %>% 
  ggplot(aes(y = Schlaf_Total, x = Wday)) +
  geom_boxplot()


fit %>%
  mutate(Schlaf_Total = `Leichter Schlaf – Dauer (ms)` + `Tiefer Schlaf – Dauer (ms)`,
         Qualität_Schlaf = `Tiefer Schlaf – Dauer (ms)` / Schlaf_Total) %>% 
  filter(!is.na(Schlaf_Total)) %>%
  ggplot(aes(y = Qualität_Schlaf, x = Wday)) +
  geom_boxplot()
  

lubridate::as_datetime("")


?lubridate::as_datetime


zeit <- "2016-02-02T04:03:03 00"

ymd(zeit)
as_datetime(zeit)

strsplit(zeit, "T") %>% unlist



x <- stringr::str_split(zeit, "T|\\ ")

t <- ymd_hms(paste0(x[1], x[2]))
hour(t)

as.POSIXct(zeit)
as.POSIXlt(zeit, )
