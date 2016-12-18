Untitled
================

Take-Home: Ebay Daten
---------------------

``` r
library(data.table)
library(magrittr)
library(foreign)
library(ggplot2)
library(stringr)
library(broom)
library(knitr)
```

``` r
raw_df <- read.dta("http://www.farys.org/daten/ebay.dta") %>%
  as.data.table(.)

raw_df %>% head(10) %>% kable(.)
```

|  sold|   price| subcat             |  sprice| listpic                  |  listbold|  sepos|  seneg|  sehasme|
|-----:|-------:|:-------------------|-------:|:-------------------------|---------:|------:|------:|--------:|
|     1|  221.00| Nokia 6230 (3)     |       1| auction-has-picture icon |         0|    372|      5|        0|
|     1|  211.00| Samsung E700 (12)  |       1| thumbnail                |         0|    153|      0|        0|
|     1|  221.00| Motorola V600 (10) |     200| auction-has-picture icon |         0|     20|      0|        0|
|     1|  251.51| Samsung E800 (11)  |       1| none                     |         0|     11|      0|        0|
|     1|   76.00| Sony T610 (1)      |       1| none                     |         0|     16|      0|        0|
|     1|  243.00| Samsung E800 (11)  |       1| auction-has-picture icon |         0|     81|      1|        1|
|     1|  211.00| Nokia 6230 (3)     |       1| thumbnail                |         0|      3|      0|        0|
|     0|      NA| Samsung E800 (11)  |     290| auction-has-picture icon |         0|      0|      0|        0|
|     1|  135.00| Sony T610 (1)      |       1| auction-has-picture icon |         0|      9|      0|        0|
|     1|  250.99| Nokia 6230 (3)     |       1| thumbnail                |         1|    535|      1|        0|

Variablen
=========

sold: Ob das Mobiltelefon verkauft wurde price: Der erzielte Verkauftspreis sprice: Der Startpreis der Auktion sepos: Anzahl positiver Bewertungen des Verkäufers seneg: Anzahl negativer Bewertungen des Verkäufers subcat: Das Modell des Mobiltelefons listpic: Kategorialer Indikator, ob die Auktion ein Thumbnail, ein “has-picture-icon” oder kein Thumbnail hat. listbold: Dummy, ob die Auktion fettgedruckt gelistet ist sehasme: Dummy, ob der Verkäufer eine “Me-page” hat oder nicht

``` r
raw_df[, rating := sepos/rowSums(.SD), .SDcols = c("sepos", "seneg")]
```

    ##       sold  price             subcat sprice                  listpic
    ##    1:    1 221.00     Nokia 6230 (3)      1 auction-has-picture icon
    ##    2:    1 211.00  Samsung E700 (12)      1                thumbnail
    ##    3:    1 221.00 Motorola V600 (10)    200 auction-has-picture icon
    ##    4:    1 251.51  Samsung E800 (11)      1                     none
    ##    5:    1  76.00      Sony T610 (1)      1                     none
    ##   ---                                                               
    ## 5496:    1 106.66      Sony T610 (1)     95 auction-has-picture icon
    ## 5497:    1 220.00     Nokia 6230 (3)      1                thumbnail
    ## 5498:    1 227.00     Nokia 6230 (3)      1 auction-has-picture icon
    ## 5499:    1 235.99     Nokia 6230 (3)      1 auction-has-picture icon
    ## 5500:    1 230.99     Nokia 6230 (3)      1 auction-has-picture icon
    ##       listbold sepos seneg sehasme    rating
    ##    1:        0   372     5       0 0.9867374
    ##    2:        0   153     0       0 1.0000000
    ##    3:        0    20     0       0 1.0000000
    ##    4:        0    11     0       0 1.0000000
    ##    5:        0    16     0       0 1.0000000
    ##   ---                                       
    ## 5496:        0    28     3       0 0.9032258
    ## 5497:        1   183     0       0 1.0000000
    ## 5498:        0    16     0       0 1.0000000
    ## 5499:        0    75     2       0 0.9740260
    ## 5500:        0   129     1       0 0.9923077

``` r
raw_df[, `:=` (makellos = factor(rating > .98, levels = c(TRUE, FALSE), labels = c("Ja", "Nein")),
               cat = str_replace(subcat, "\\ \\(\\d+\\)", ""))]
```

    ##       sold  price             subcat sprice                  listpic
    ##    1:    1 221.00     Nokia 6230 (3)      1 auction-has-picture icon
    ##    2:    1 211.00  Samsung E700 (12)      1                thumbnail
    ##    3:    1 221.00 Motorola V600 (10)    200 auction-has-picture icon
    ##    4:    1 251.51  Samsung E800 (11)      1                     none
    ##    5:    1  76.00      Sony T610 (1)      1                     none
    ##   ---                                                               
    ## 5496:    1 106.66      Sony T610 (1)     95 auction-has-picture icon
    ## 5497:    1 220.00     Nokia 6230 (3)      1                thumbnail
    ## 5498:    1 227.00     Nokia 6230 (3)      1 auction-has-picture icon
    ## 5499:    1 235.99     Nokia 6230 (3)      1 auction-has-picture icon
    ## 5500:    1 230.99     Nokia 6230 (3)      1 auction-has-picture icon
    ##       listbold sepos seneg sehasme    rating makellos           cat
    ##    1:        0   372     5       0 0.9867374       Ja    Nokia 6230
    ##    2:        0   153     0       0 1.0000000       Ja  Samsung E700
    ##    3:        0    20     0       0 1.0000000       Ja Motorola V600
    ##    4:        0    11     0       0 1.0000000       Ja  Samsung E800
    ##    5:        0    16     0       0 1.0000000       Ja     Sony T610
    ##   ---                                                              
    ## 5496:        0    28     3       0 0.9032258     Nein     Sony T610
    ## 5497:        1   183     0       0 1.0000000       Ja    Nokia 6230
    ## 5498:        0    16     0       0 1.0000000       Ja    Nokia 6230
    ## 5499:        0    75     2       0 0.9740260     Nein    Nokia 6230
    ## 5500:        0   129     1       0 0.9923077       Ja    Nokia 6230

``` r
df <- raw_df[sepos > 11, !"subcat"]

list(head(df), tail(df)) %>% rbindlist(.) %>% kable(.)
```

|  sold|   price|  sprice| listpic                  |  listbold|  sepos|  seneg|  sehasme|     rating| makellos | cat           |
|-----:|-------:|-------:|:-------------------------|---------:|------:|------:|--------:|----------:|:---------|:--------------|
|     1|  221.00|       1| auction-has-picture icon |         0|    372|      5|        0|  0.9867374| Ja       | Nokia 6230    |
|     1|  211.00|       1| thumbnail                |         0|    153|      0|        0|  1.0000000| Ja       | Samsung E700  |
|     1|  221.00|     200| auction-has-picture icon |         0|     20|      0|        0|  1.0000000| Ja       | Motorola V600 |
|     1|   76.00|       1| none                     |         0|     16|      0|        0|  1.0000000| Ja       | Sony T610     |
|     1|  243.00|       1| auction-has-picture icon |         0|     81|      1|        1|  0.9878049| Ja       | Samsung E800  |
|     1|  250.99|       1| thumbnail                |         1|    535|      1|        0|  0.9981343| Ja       | Nokia 6230    |
|     1|  231.00|       1| thumbnail                |         0|     43|      1|        0|  0.9772727| Nein     | Nokia 6230    |
|     1|  106.66|      95| auction-has-picture icon |         0|     28|      3|        0|  0.9032258| Nein     | Sony T610     |
|     1|  220.00|       1| thumbnail                |         1|    183|      0|        0|  1.0000000| Ja       | Nokia 6230    |
|     1|  227.00|       1| auction-has-picture icon |         0|     16|      0|        0|  1.0000000| Ja       | Nokia 6230    |
|     1|  235.99|       1| auction-has-picture icon |         0|     75|      2|        0|  0.9740260| Nein     | Nokia 6230    |
|     1|  230.99|       1| auction-has-picture icon |         0|    129|      1|        0|  0.9923077| Ja       | Nokia 6230    |

``` r
# Es gibt Preise mit NA (nicht verkauft), daher funktioniert der Reorder nicht ohne anonyme Funktion mit na.rm = TRUE
# Absteigende Anordung mit -1
df %>%
  ggplot(aes(x=reorder(factor(cat), price, function(x) mean(x, na.rm = TRUE)*-1), y = price)) +
  geom_boxplot(aes(fill = makellos), notch = TRUE, position = position_dodge(.85)) +
  scale_fill_manual(values = c("#66CC99", "#FC575E"), name = "Makellos") +
  xlab("\nKategorie") +
  ylab("Preis") +
  ggtitle("Ebay Verkäufe nach Kategorie") +
  theme(panel.background = element_rect(fill = "#F0F1F5"),
        panel.grid.major = element_line(color = "white", size = 1.1),
        panel.grid.minor = element_blank())
```

    ## Warning: Removed 155 rows containing non-finite values (stat_boxplot).

![](ebay_files/figure-markdown_github/plotting-1.png)

``` r
# + coord_flip()
```

Bewertet anhand "Rule of Thumb", dass bei signifikanter Differenz die Notches nicht überlappen sollten.Daher die Feststellung, dass kein signifikanter Unterschied zwischen den makellosen undnicht makellosen Ratings besteht <https://en.wikipedia.org/wiki/Box_plot#Variations>
