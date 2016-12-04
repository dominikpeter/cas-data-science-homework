rm(list=ls())

# ------------------------------------------------------------------------------------------------
# Webscraping and Tidying
# ------------------------------------------------------------------------------------------------

library(ggplot2)
library(magrittr)
library(stringr)
library(data.table)
library(rvest)

# get table from wikipedia
# ------------------------------------------------------------------------------------------------

url <- "https://de.wikipedia.org/wiki/Bern#Klima"

html_table <- url %>%
  read_html() %>%
  html_node(xpath='//*[@id="mw-content-text"]/table[4]') %>%
  html_table(fill = TRUE, header = FALSE) %>% 
  as.data.table()


# clean table
# ------------------------------------------------------------------------------------------------

# some indexing to get relevant data (with false because of data.table formating)
df <- html_table %>% 
  .[(1:12)[-1], 1:13, with = FALSE]

# make clean header
header <- df[1, -1, with = FALSE] %>% as.character() 
names(df) <- c("typ", header)
df <- df[-1, ]

# convert value to numeric and eliminate NA columns
months <- colnames(df)[-1]
df[, months] <- lapply(df[, months, with = FALSE], function(x) as.numeric(str_replace_all(x, ",", "\\.")))
# remove NA's
df <- df %>% na.omit()


# tidy df
# ------------------------------------------------------------------------------------------------

tidy_df <- df[1:2] %>%
  melt(id.vars = "typ", variable.name = "Monat") %>% 
  dcast(Monat ~ typ)

tidy_df


# plot
# ------------------------------------------------------------------------------------------------





