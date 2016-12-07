# ------------------------------------------------------------------------------------------------
# Title:  Webscraping and Tidying
# Autor:  Dominik Peter
# Date:   2016-12-04
# ------------------------------------------------------------------------------------------------

rm(list=ls())

library(data.table)
library(magrittr)
library(stringr)
library(ggplot2)
library(rvest)

# get table from wikipedia
# ------------------------------------------------------------------------------------------------

url <- "https://de.wikipedia.org/wiki/Bern#Klima"
xpath <- '//*[@id="mw-content-text"]/table[4]' #sloppy xpath, is there a better one?

# maybe nicer but the very specifique table selection could cause problem
# with just little update on the wiki page
# -----------------------------------------------------------------------
html_table <- url %>%
  read_html() %>%
  html_node(xpath = xpath) %>%
  html_table(fill = TRUE, header = FALSE) %>%
  as.data.table()
# -----------------------------------------------------------------------

# search by table index or string pattern?
# -----------------------------------------------------------------------
# html_table <- url %>% read_html() %>%
#   html_nodes("table") %>%
#   .[str_detect(., "Monatliche Durchschnittstemperaturen und -niederschläge für Bern")] %>% 
#   .[[1]] %>% 
#   html_table(fill = TRUE, header = FALSE) %>% 
#   as.data.table()


# clean table
# ------------------------------------------------------------------------------------------------

# some indexing to get relevant data (with false because of data.table formatting)
n <- length(month.name)
df <- html_table %>% 
  .[(1:n)[-1], 1:(n+1), with = FALSE]

# make clean header
header <- df[1, -1, with = FALSE] %>% as.character() 
setnames(df, names(df), c("typ", header))
df <- df[-1, ]

# convert value to numeric and eliminate NA columns
to_numeric <- function(x) {
  x <- str_replace_all(x, ",", "\\.")
  as.numeric(x)
}
months <- colnames(df)[-1]
# apply funtion
df[, months] <- lapply(df[, months, with = FALSE], to_numeric)
# remove NA's
df <- df %>% na.omit()
df

# tidy df
# ------------------------------------------------------------------------------------------------

tidy_df <- df[1:2] %>%
  melt(id.vars = "typ", variable.name = "Monat") %>% 
  dcast(Monat ~ typ)

col_names <- colnames(tidy_df)
setnames(tidy_df, col_names, c(col_names[1], c("Max", "Min")))

tidy_df

# plotting 
# ------------------------------------------------------------------------------------------------
tidy_df[, Mittelwert := rowMeans(.SD), .SDcols = c("Max", "Min")]

mean_temp <- mean(tidy_df$Mittelwert)

tidy_df  %>% 
  ggplot(aes(x = Monat, y = Mittelwert)) +
  geom_point(size = 5/2, color = "#444B54") +
  geom_hline(yintercept = mean_temp, color = "#2980B9", size = 3/2, alpha = 2/5) +
  geom_errorbar(aes(ymin = Min, ymax = Max), width = 1/3, color = "#444B54", size = 3/4) +
  ylab("\nTemperatur (°C)") +
  xlab("\nMonat") +
  ggtitle("Monatliche Durchschnittstemperaturen", subtitle = "für Bern 1981 – 2010") +
  theme(panel.background = element_rect(fill = "#F0F1F5"),
        panel.grid.major = element_line(color = "white", size = 4/5),
        panel.grid.minor = element_blank())




