
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


final_result <- result_with_time %>% select(hour, employeeid, departmentid, clientid)
final_result

# Anomalie Detection

df_anomalie <- read_csv("https://raw.githubusercontent.com/romeokienzler/developerWorks/master/testdata.csv") %>%
  select(-X1)
df_anomalie


df_anomalie %>% 
  reshape2::melt(value.name = "value", variable.name = "id") %>% 
  ggplot(aes(x = factor(value))) +
  geom_bar() +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank()) +
  facet_wrap(~id, scales = "free")

# df_anomalie %>% 
#   ggplot(aes(x = hour, y = departmentid)) +
#   geom_point()
# 
# df_anomalie %>% 
#   ggplot(aes(x = hour, y = clientid)) +
#   geom_point()

#employeeid nahezu uniform verteilt, keine zusatzinfo 

df_anomalie

# m_scaled <- scale(df_anomalie %>% select(-employeeid, -clientid), center = TRUE, scale = TRUE)



k <- kmeans(df_anomalie %>% select(departmentid), centers = 3)

# k <- kmeans(m_scaled, centers = 3, algorithm = "Lloyd", iter.max = 10000)

df_anomalie <- df_anomalie %>% mutate(cluster = k$cluster)

centers <- k$centers[k$cluster, ]
distances <- sqrt(rowSums((m_scaled - centers)^2))
m <- tapply(distances, k$cluster, mean)
d <- distances/(m[k$cluster])

df_anomalie <-
  df_anomalie %>%
  mutate(distance = d)

df_anomalie %>%
  ggplot(aes(hour, departmentid, color = factor(cluster))) +
  geom_point() +
  theme_light()



# ---------------departmentid
df2 <- df_anomalie %>%
  group_by(departmentid, hour) %>%
  summarise(n = n())


k2 <- kmeans(df2 %>% select(departmentid, hour, n), 2)
df2 <- df2 %>% ungroup() %>%  mutate(cluster = k2$cluster)

df2 %>%
  ggplot(aes(hour, departmentid, color = factor(cluster), size = n)) +
  geom_point() +
  theme_light() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank())

df2 %>% filter(departmentid == 23) %>% View()



# ---------------departmentid

df2 <- df_anomalie %>%
  group_by(departmentid, hour) %>%
  summarise(n = n())


k2 <- kmeans(df2 %>% select(departmentid, hour, n), 2)
df2 <- df2 %>% ungroup() %>%  mutate(cluster = k2$cluster)

df2 %>%
  ggplot() +
  geom_point(aes(hour, departmentid, color = factor(cluster), size = n)) +
  geom_text(aes(1.5, 27), label = "outlier", size = 4, color = "black") +
  theme_light() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank()) +
  scale_color_manual(values = c("#ef8a62", "#67a9cf"))


df_anomalie %>%
  filter(departmentid == 23) %>%
  ggplot(aes(factor(hour), fill = factor(employeeid))) +
  geom_bar() +
  scale_fill_brewer(palette = "Paired") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank())



# outlier detected!!!




