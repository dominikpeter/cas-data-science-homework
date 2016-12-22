
rm(list=ls())
library(data.table)
library(magrittr)

df <- fread("dataviz/GemeindeDaten/gemeindedaten.csv")

df %>% setkey(sprachregionen)
df[, rank := frank(-bev_total, ties.method = "dense"), by = key(df)]
df[rank == 1, .(bfsid, gmdename,
                kantone, grossregionen,
                sprachregionen, stadt_land)]

