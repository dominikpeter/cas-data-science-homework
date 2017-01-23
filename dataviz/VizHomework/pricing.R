
# Horizontale Recherche <- verschiedene Modelle
# Vertikale Recherche <- Gestaltung wie?
# Gestaltungsraster ist wichtig
# Typography wichtig
# Blocksatz lang, kurz, lang kurz <- am besten


rm(list=ls())

library(tidyverse)
library(lubridate)
library(viridis)
library(gridExtra)


# dch <- data.table::fread("/Users/dominikpeter/Desktop/Export/export_dch2\ Kopie.csv",
#                          sep = "|", encoding = "Latin-1", integer64 = "character")
# fch <- data.table::fread("/Users/dominikpeter/Desktop/Export/export_fch\ Kopie.csv",
#                          sep = ";", encoding = "Latin-1", integer64 = "character")
# 
# fch <- fch %>% mutate(Latest_Update     = Latest_Update %>% as.character,
#                       idOwnBrandOrigin  = idOwnBrandOrigin %>% as.character,
#                       Quantity          = Quantity %>% as.numeric,
#                       GrossSales        = GrossSales %>% as.numeric,
#                       Sales             = Sales %>% as.numeric,
#                       Margin            = Margin %>% as.numeric)
# 
# dch <- dch %>% mutate(Latest_Update     = Latest_Update %>% as.character,
#                       idOwnBrandOrigin  = idOwnBrandOrigin %>% as.character,
#                       Quantity          = Quantity %>% as.numeric,
#                       GrossSales        = GrossSales %>% as.numeric,
#                       Sales             = Sales %>% as.numeric,
#                       Margin            = Margin %>% as.numeric)

# pricing <- bind_rows(fch, dch)
# pricing %>% write_delim("/Users/dominikpeter/Google/R/datasets/pricing.txt", delim = "|")

rm(list=ls())

raw_data <- read_delim("/Users/dominikpeter/Google/R/datasets/pricing.txt", delim = "|")
# raw_data <- read_delim("C:/Users/peterd/Downloads/pricing.txt", delim = "|")


lookup_brand <- tibble(Key = raw_data$OwnBrand %>% unique) %>% 
  mutate(PrivateLabel = grepl("casa", tolower(Key)))

cats <- tribble(~cats,
                "Sanitär-Armaturen",
                "Waschtische",
                "Garnituren-Programme")

units <- tribble(~QuantityUnit, "PCE", "STK")

df <- raw_data %>% 
  inner_join(cats, by = c("Hierarchy_Level_2" = "cats"))

# features
df <- df %>% 
  mutate(GrossPrice = GrossSales / Quantity,
         NetPrice   = Sales / Quantity,
         `Margin %` = Margin / Sales,
         day        = day(date),
         date       = ymd(date),
         year       = year(date),
         month      = month(date),
         quarter    = quarter(date),
         qy         = paste(as.character(year), as.character(quarter), sep = "-"),
         Cat_1      = Hierarchy_Level_1,
         Cat_2      = Hierarchy_Level_2,
         Cat_3      = Hierarchy_Level_3,
         ST         = ifelse(year > 2012, TRUE, FALSE)) %>% 
  filter(year %>% between(2010, 2016),
         Quantity > 0,
         Sales > 0,
         Margin > 0,
         NetPrice > 0,
         !is.infinite(NetPrice)) %>%
  inner_join(units, by = c("QuantityUnit" = "QuantityUnit")) %>% 
  left_join(tribble(~quarter, ~midy, 1,1,2,1,3,2,4,2), by = c("quarter" = "quarter")) %>%
  mutate(midyear = paste(as.character(year), as.character(midy), sep = "-")) %>% 
  left_join(lookup_brand, by = c("OwnBrand" = "Key")) %>% 
  na.omit()
    
raw_data <- NULL

waschtisch <- df %>% 
  filter(Cat_2 == "Waschtische") %>% 
  filter(NetPrice < NetPrice %>% quantile(0.99),
         NetPrice > NetPrice %>% quantile(0.01)) %>% 
  inner_join(tribble(~Cat_3,
                     "Waschtische Keramik",
                     "Doppelwaschtische Keramik")) %>% 
  na.omit()

# Waschtisch Normal
# -----------------------------------------------
w1 <- waschtisch %>%
  filter(Cat_3 == "Waschtische Keramik") %>% 
  filter(NetPrice > NetPrice %>% quantile(0.01),
         NetPrice < NetPrice %>% quantile(0.85))

w1_breaks <- w1$NetPrice %>%
  pretty(nclass.Sturges(w1$NetPrice))

w1_analysis <- w1 %>% 
  mutate(Range = NetPrice %>% cut(w1_breaks)) %>% 
  group_by(midyear, Range) %>% 
  summarise(Sales = sum(Sales),
            N = n())

w1_analysis %>% 
  ggplot(aes(x = factor(midyear), y = Range)) +
  geom_tile(aes(fill = Sales), color = "white") +
  # scale_fill_gradient(low = "#ece7f2", high = "#2b8cbe") +
  scale_x_discrete(position = "top") +
  scale_fill_viridis(alpha = 0.2, option = "A", begin = 0.1, end = 1, direction = -1) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank()) +
  ggtitle("Waschtische Keramik") +
  xlab("") +
  ylab("") +
  geom_vline(xintercept = 7, size = 2, color = "white") -> w1_plot; w1_plot
                                                                
w1 %>%
  # filter(year > 2011) %>% 
  ggplot(aes(x = NetPrice, fill = factor(year))) +
  geom_density(alpha = 0.9) +
  scale_fill_viridis(alpha = 0.2, option = "A", begin = 0.1, end = 1, direction = -1) +
  # scale_fill_gradient2(low = "#edf8b1",mid = "#7fcdbb", high = "#2c7fb8", midpoint = 100000) +
  scale_x_discrete(position = "top") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank()) +
  ggtitle("Doppelwaschtische Keramik") +
  xlab("") +
  ylab("") +
  geom_vline(xintercept = 7, size = 2, color = "white") -> w1_density


# Doppel - Waschtisch
# -----------------------------------------------

w2 <- waschtisch %>%
  filter(Cat_3 == "Doppelwaschtische Keramik") %>% 
  filter(NetPrice > NetPrice %>% quantile(0.01),
         NetPrice < NetPrice %>% quantile(0.99))

w2_breaks <- w2$NetPrice %>%
  pretty(nclass.Sturges(w2$NetPrice))

w2_analysis <- w2 %>% 
  mutate(Range = NetPrice %>% cut(w2_breaks)) %>% 
  group_by(midyear, Range, ST) %>% 
  summarise(Sales = sum(Sales),
            N = n())

w2_analysis %>% 
  ggplot(aes(x = factor(midyear), y = Range)) +
  geom_tile(aes(fill = Sales), color = "white") +
  # scale_color_gradient() +
  # scale_fill_brewer()+
  scale_fill_viridis(alpha = 0.2, option = "A", begin = 0.1, end = 1, direction = -1) +
  # scale_fill_gradient2(low = "#edf8b1",mid = "#7fcdbb", high = "#2c7fb8", midpoint = 100000) +
  scale_x_discrete()+
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank()) +
  ggtitle("Doppelwaschtische Keramik") +
  xlab("") +
  ylab("") +
  geom_vline(xintercept = 6.5, size = 2, color = "white", ) -> w2_plot;w2_plot


w2 %>%
  # filter(year > 2011) %>% 
  ggplot(aes(x = NetPrice, fill = factor(ST))) +
  geom_density(alpha = 0.8) +
  # scale_fill_brewer(palette = "Set1") +
  # scale_fill_manual(values = c("#33of66", "#bf3875")) +
  scale_fill_viridis(discrete = TRUE, option = "A",
                     guide = guide_legend(title = "Preiskampf"),
                     begin = 0.6) +
  scale_y_continuous(labels = scales::percent) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank()) +
  ylab("") +
  xlab("") +
  ggtitle("Doppelwaschtische Keramik") -> w2_density;w2_density


  
grid.arrange(w1_plot, w2_plot)


grid.arrange(w2_plot, w2_density)


# Barcharts
# ------------------------------------------------------

w1_analysis %>%
  # filter(year > 2011) %>% 
  ggplot(aes(x = factor(year), y = Sales, fill = Range)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis(discrete = TRUE) +
  scale_y_continuous(labels = scales::comma) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank())



w2_analysis %>%
  # filter(year > 2011) %>% 
  ggplot(aes(x = factor(year), y = Sales, fill = Range)) +
  scale_fill_viridis(discrete = TRUE) +
  geom_bar(stat = "identity") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank())








  
  