
# ------------------------------------------------------------------------------------------------
# Title:  Lösung zu Aufgaben 1 
# Autor:  Dominik Peter
# Date:   2017-02-18
# ------------------------------------------------------------------------------------------------


# Aufgabe 1
# ------------------------------------------------------------------------------------------------
# Aufgabe 1 - ETL
# 1. Datenaufbereitung
# 
# Ein findiger Programmierer hat den Apache HTTPD server über ein Modul so angepasst dass neben normalen Seitenzugriffen
# auch mitgelogged wird um welchen Mitarbeiter es sich handelt, zu welcher Abteilung er gehört und welchen Kunden er gerade
# zugegriffen hat. Leider hat er sich keine grossen Gedanken gemacht wie ein Data Scientist die Daten verarbeitet.
# 
# Der Link zur Datei ist hier: https://raw.githubusercontent.com/romeokienzler/developerWorks/master/log
# Hier wurde einfach über ein Apache HTTPD modul für jeden Request eine 2. Zeile eingefügt in der der Payload die gewünschten
# Informationen enthält, in folgerner Reihenfolge: departmentid, employeeid, clientid
# a) Lesen Sie die LOG Datei mittels R ein und bereiten Sie so auf, dass daraus ein Data Frame entsteht welcher folgendes Format hat:
# Spalte 1> employeeid, Spalte 2> departmentid, Spalte 3> clientid
# 
# b) Erweitern Sie Ihr R Script dass nun auch die Stunde des Zugriffsdatums aus der LOG Datei in der ersten Zeile des Data Frame erscheint.
# Das Format ist nun Spalte 1 > hour, Spalte 2> employeeid, Spalte 3> departmentid, Spalte 4> clientid
# 
# Der Link zur Datei ist hier: https://raw.githubusercontent.com/romeokienzler/developerWorks/master/testdata.csv
# 
# 2. Die bekommen nun das aus Aufgabe 1 extrahierte CSV file von Ihrem Junior Data Scientist geliefert. Die Forensik Abteilung möchte wissen ob in diesem Trace anomales Verhalten auftritt.
# Können Sie helfen?
# 
# Zuletzt geändert: Mittwoch, 15. März 2017, 21:14
# ------------------------------------------------------------------------------------------------

rm(list = ls())

library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(reshape2)
library(ggplot2)


# Data Wrangling
url_log <- "https://raw.githubusercontent.com/romeokienzler/developerWorks/master/log"
dest_log <- "logfile"
download.file(url_log, destfile = dest_log)

raw_file <- dest_log %>% read_log()

df <- raw_file[seq(2, nrow(raw_file),by = 2 ), ]

split <- colsplit(df$X6, ",", names = c(1, 2, 3))
df <- bind_cols(df, split)
names(df) <- 1:ncol(df)

df <- df %>% select(1, 4, 8, 9, 10)
names(df) <- c("IP", "LogID", "departmentid", "employeeid", "clientid")

# Resultat Aufgabe a)
result <- df %>%
  select(employeeid, departmentid, clientid)

head(result, 5)

# Time
result_with_time <- df %>% 
  mutate(time = str_extract(LogID, '.+\\ ') %>% dmy_hms,
         hour = hour(time)) %>% 
  select(-LogID)

# Resultat Aufgabe b)
final_result <- result_with_time %>%
  select(hour, employeeid, departmentid, clientid)

head(final_result, 5)

# final_result %>%
#   reshape2::melt(value.name = "value", variable.name = "id") %>%
#   ggplot(aes(x = factor(value))) +
#   geom_bar(fill = "#1DABB8") +
#   theme_minimal() +
#   theme(panel.background = element_blank(),
#         panel.grid = element_blank(),
#         panel.border = element_blank()) +
#   theme(panel.background = element_blank(),
#         panel.grid = element_blank()) +
#   facet_wrap(~id, scales = "free") +
#   ylab("Anzahl") +
#   xlab("Wert")


# final_result %>% 
#   reshape2::melt(value.name = "value", variable.name = "id") %>% 
#   ggplot(aes(x = factor(value))) +
#   geom_bar(fill = "#1DABB8") +
#   theme_minimal() +
#   theme(panel.background = element_blank(),
#         panel.grid = element_blank(),
#         panel.border = element_blank()) +
#   theme(panel.background = element_blank(),
#         panel.grid = element_blank()) +
#   facet_wrap(~id, scales = "free") +
#   ylab("Anzahl") +
#   xlab("Wert")


# Anomalie Detection (Lösung zu Aufgabe 2.)
# --------------------------------------------------------------------------------------------------
url_csv <- "https://raw.githubusercontent.com/romeokienzler/developerWorks/master/testdata.csv"
dest_csv <- "testdata.csv"
download.file(url_csv, destfile = dest_csv)

df_anomalie <- dest_csv %>%
  read_csv() %>%
  select(-X1)

df_anomalie %>% 
  reshape2::melt(value.name = "value", variable.name = "id") %>% 
  ggplot(aes(x = factor(value))) +
  geom_bar(fill = "#1DABB8") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank()) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank()) +
  facet_wrap(~id, scales = "free") +
  ylab("Anzahl") +
  xlab("Wert")
# employeeid und departmentid vertauscht
# departmentid und clientid sehen eher gleichverteilt aus. hour sieht sehr künstlich aus mit
# der perfekt symetrischen Verteilung


# korrektur emplyeeid und departmentid
names(df_anomalie) <- c("hour", "departmentid", "employeeid", "clientid")

# Grafik mit Korrektur
df_anomalie %>%
  reshape2::melt(value.name = "value", variable.name = "id") %>%
  ggplot(aes(x = factor(value))) +
  geom_bar(fill = "#1DABB8") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank()) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank()) +
  facet_wrap(~id, scales = "free") +
  ylab("Anzahl") +
  xlab("Wert")

# Fokus auf hour und employeeid

# departmentid Anzahl Zugriffe pro Stunde
# ---------------------------------------------
df_grouped <- df_anomalie %>%
  group_by(employeeid, hour) %>%
  summarise(n = n())

# cluster mit zwei Gruppen
cluster <- kmeans(df_grouped, 2, iter.max = 10000)


df_grouped <- df_grouped %>%
  ungroup() %>%
  mutate(cluster = cluster$cluster %>% as.integer)

# Visuelle Suche nach Outlier
df_grouped %>%
  ggplot() +
  geom_point(aes(factor(hour), employeeid, color = factor(cluster), size = n)) +
  geom_segment(aes(4, 26, xend = 1.5, yend = 23.5), color = "black", size = 1.05,
               arrow = arrow(length = unit(.03, "npc"))) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf")) +
  labs(color = "Cluster", size = "Anzahl") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank()) +
  xlab("hour") +
  ggtitle("Visuelle Suche nach Anomalien")
  

# suchen des Outliers anhand der visuellen Betrachtung
df_grouped %>%
  filter(hour == 0) %>%
  group_by(cluster) %>% 
  filter(n() == 1) #Cluster sollte nur ein Datenpunkt haben
#employeeid 23 hat 1000 Aufrufe in der Stunde 0


# Verteilung der employeeid 23
df_anomalie %>%
  filter(employeeid == 23) %>%
  ggplot(aes(factor(hour), fill = factor(departmentid))) +
  geom_bar() +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank()) +
  xlab("Stunde") +
  ylab("Anzahl") +
  labs(fill = "departmentid")

# Innerhalb des Departements 23 sind die Daten normalverteilt, 
# bis auf den Outlier mit der employeeid 23 in Department 7 hat 
# 1000 requests in der Stunde 0






