
# 2016 HS CAS Datenanalyse
# Seitenpfad
# Startseite / ▶︎ TI Technik und Informatik / ▶︎ Weiterbildung / ▶︎ 2016 HS CAS DA / ▶︎ Data Mining / ▶︎ Aufgabe 1
# Aufgabe 1
# 1. Datenaufbereitung
# Ein findiger Programmierer hat den Apache HTTPD server über ein Modul so angepasst dass neben normalen Seitenzugriffen
# auch mitgeLogIDged wird um welchen Mitarbeiter es sich handelt, zu welcher Abteilung er gehört und welchen Kunden er gerade zugegriffen hat.
# Leider hat er sich keine grossen Gedanken gemacht wie ein Data Scientist die Daten verarbeitet.
# Der Link zur Datei ist hier: https://raw.githubusercontent.com/romeokienzler/developerWorks/master/LogID
# Hier wurde einfach über ein Apache HTTPD modul für jeden Request eine 2. Zeile eingefügt in der der Payload die gewünschten Informationen enthält,
# in folgerner Reihenfolge: departmentid, employeeid, clientid
# a) Lesen Sie die LogID Datei mittels R ein und bereiten Sie so auf, dass daraus ein Data Frame entsteht welcher folgendes Format hat:
# Spalte 1> employeeid, Spalte 2> departmentid, Spalte 3> clientid
# b) Erweitern Sie Ihr R Script dass nun auch die Stunde des Zugriffsdatums aus der LogID Datei in der ersten Zeile des Data Frame erscheint.
# Das Format ist nun Spalte 1 > hour, Spalte 2> employeeid, Spalte 3> departmentid, Spalte 4> clientid
# Der Link zur Datei ist hier: https://raw.githubusercontent.com/romeokienzler/developerWorks/master/testdata.csv
# 2. Die bekommen nun das aus Aufgabe 1 extrahierte CSV file von Ihrem Junior Data Scientist geliefert.
# Die Forensik Abteilung möchte wissen ob in diesem Trace anomales Verhalten auftritt. Können Sie helfen?
# Zuletzt geändert: Donnerstag, 25. Februar 2016, 11:38
rm(list = ls())

library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(reshape2)
library(ggplot2)


# Data Wrangling

raw_file <- read_log("https://raw.githubusercontent.com/romeokienzler/developerWorks/master/log") %>%
  as_data_frame()

df <- raw_file[seq(2, nrow(raw_file),by = 2 ), ]

split <- colsplit(df$X6, ",", names = c(1, 2, 3))
df <- bind_cols(df, split)
names(df) <- 1:ncol(df)

df <- df %>% select(1, 4, 8, 9, 10)
names(df) <- c("IP", "LogID", "departmentid", "employeeid", "clientid")

result <- df %>% select(employeeid, departmentid, clientid)

# Time
result_with_time <- df %>% 
  mutate(time      = str_extract(LogID, '.+\\ ') %>% dmy_hms,
         hour      = hour(time),
         minutes   = minute(time),
         seconds   = second(time),
         day       = day(time),
         month     = month(time),
         year      = year(time)) %>% 
  select(-LogID)


final_result <- result_with_time %>% select(hour, employeeid, departmentid)


# Anomalie Detection

library(mvoutlier)
library(scales)

df_anomalie <- read_csv("https://raw.githubusercontent.com/romeokienzler/developerWorks/master/testdata.csv")
df_anomalie <- df_anomalie %>% select(-X1)

m_scaled <- scale(df_anomalie)

# ch <- chisq.plot(m_scaled)
df_anomalie <- df_anomalie %>% mutate(outlier = outl$wfinal01)

k <- kmeans(m_scaled, centers = 5, algorithm = "MacQueen", iter.max = 10000)

df_anomalie <- df_anomalie %>% mutate(cluster = k$cluster)


df_anomalie %>% ggplot(aes(hour, departmentid, color = factor(cluster))) + geom_point() + theme_light()




