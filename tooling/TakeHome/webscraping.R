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

html_table <- url %>%
  read_html() %>%
  html_node(xpath = xpath) %>%
  html_table(fill = TRUE, header = FALSE) %>% 
  as.data.table()


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


# tidy df
# ------------------------------------------------------------------------------------------------

tidy_df <- df[1:2] %>%
  melt(id.vars = "typ", variable.name = "Monat") %>% 
  dcast(Monat ~ typ)

col_names <- colnames(tidy_df)
setnames(tidy_df, col_names, c(col_names[1], c("Max", "Min")))

tidy_df

# plot (just for fun)
# ------------------------------------------------------------------------------------------------

tidy_df[, Mittelwert := rowMeans(.SD), .SDcols = c("Max", "Min")]  %>% 
  ggplot(aes(x = Monat, y = Mittelwert)) +
  geom_point(size = 2, color = "#444B54") +
  geom_errorbar(aes(ymin = Min, ymax = Max), width = .5, color = "#444B54") +
  ylab("\nTemperatur (°C)") +
  xlab("\nMonat") +
  ggtitle("Monatliche Durchschnittstemperaturen\nfür Bern 1981 – 2010") +
  theme(panel.background = element_rect(fill = "#F0F1F5"),
        panel.grid.major = element_line(color = "white", size = .8),
        panel.grid.minor = element_blank())



