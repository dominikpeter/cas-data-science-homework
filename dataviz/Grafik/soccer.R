
# soccer analytics
library(readr)
library(foreach)
library(dplyr)
library(lubridate)
library(reshape2)

files <- dir("/Users/dominikpeter/Desktop/data/soccer")
files

lst <- list()

for (i in seq_along(files)){
  file <- files[i]
  print(paste0("/Users/dominikpeter/Desktop/data/soccer/", file))
  lst[[i]] <- read_csv(paste0("/Users/dominikpeter/Desktop/data/soccer/", file),
                       col_names = FALSE)
}

file <- bind_rows(lst) %>% .[,1:11]
colnames(file) <- file[1,]
file <- file[-1,]
file <- file %>% select(-Div) %>% 
  melt(id.vars = c("Date","Referee", "FTHG", "FTAG", "FTR"),
       measure.vars = c("HomeTeam", "AwayTeam"),
       value.name = "Team") %>% 
  mutate(Date = dmy(Date),
         Year = year(Date),
         Month = month(Date),
         Day = day(Date)) %>%
  na.omit()



