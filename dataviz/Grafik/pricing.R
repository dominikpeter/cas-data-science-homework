
# Horizontale Recherche <- verschiedene Modelle
# Vertikale Recherche <- Gestaltung wie?
# Gestaltungsraster ist wichtig
# Typography wichtig
# Blocksatz lang, kurz, lang kurz <- am besten


rm(list=ls())

library(tidyverse)
library(lubridate)

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

lookup_brand <- tibble(Key = raw_data$OwnBrand %>% unique) %>% 
  mutate(PrivateLabel = grepl("casa", tolower(Key)))

cats <- tribble(~cats,
                "Sanitär-Armaturen",
                "Waschtische",
                "Garnituren-Programme")

units <- tribble(~QuantityUnit, "PCE", "STK")

# features
df <- raw_data %>% 
  mutate(GrossPrice = GrossSales / Quantity,
         NetPrice   = Sales / Quantity,
         `Margin %` = Margin / Sales,
         day = day(date),
         date = ymd(date),
         year = year(date),
         month = month(date),
         Cat_1 = Hierarchy_Level_1,
         Cat_2 = Hierarchy_Level_2,
         Cat_3 = Hierarchy_Level_3) %>% 
  filter(year %>% between(2010, 2016),
         Quantity > 0,
         Sales > 0,
         Margin > 0,
         NetPrice > 0,
         !is.infinite(NetPrice),
         `Margin %` %>% between(.01, .90)) %>% 
  inner_join(cats, by = c("Cat_2" = "cats")) %>%
  inner_join(units) %>% 
  left_join(lookup_brand, by = c("OwnBrand" = "Key")) %>% 
  na.omit()
  

waschtisch <- df %>% 
  filter(Cat_2 == "Waschtische") %>% 
  filter(NetPrice < NetPrice %>% quantile(0.99),
         NetPrice > NetPrice %>% quantile(0.01)) %>% 
  inner_join(tribble(~Cat_3,
                     "Waschtische Keramik",
                     "Doppelwaschtische Keramik")) %>% 
  na.omit()

# Waschtisch
# -----------------------------------------------
w1 <- waschtisch %>%
  filter(Cat_3 == "Waschtische Keramik") %>% 
  filter(NetPrice > NetPrice %>% quantile(0.01),
         NetPrice < NetPrice %>% quantile(0.75))

w1_breaks <- w1$NetPrice %>% pretty(nclass.Sturges(w1$NetPrice))

w1_analysis <- w1 %>% 
  mutate(Range = NetPrice %>% cut(w1_breaks)) %>% 
  group_by(year, Range) %>% 
  summarise(Sales = sum(Sales),
            N = n())

w1_analysis %>% 
  ggplot(aes(x = factor(year), y = Range)) +
  geom_tile(aes(fill = Sales), color = "white", size = .5) +
  scale_fill_gradient(low = "#ece7f2", high = "#2b8cbe") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank())
                                                                
w1 %>%
  filter(year > 2011) %>% 
  ggplot(aes(x = NetPrice, fill = factor(year))) +
  geom_density(alpha = 0.9) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank()) +
  ylab("Density")


# Doppel <- Waschtisch
# -----------------------------------------------

w2 <- waschtisch %>%
  filter(Cat_3 == "Doppelwaschtische Keramik") %>% 
  filter(NetPrice > NetPrice %>% quantile(0.01),
         NetPrice < NetPrice %>% quantile(0.90))

w2_breaks <- w2$NetPrice %>% pretty(nclass.Sturges(w2$NetPrice))

w2_analysis <- w2 %>% 
  mutate(Range = NetPrice %>% cut(w2_breaks)) %>% 
  group_by(year, Range) %>% 
  summarise(Sales = sum(Sales),
            N = n())


w2_analysis %>% 
  ggplot(aes(x = factor(year), y = Range)) +
  geom_tile(aes(fill = Sales), color = "white", size = .5) +
  scale_fill_gradient(low = "#ece7f2", high = "#2b8cbe") +
  scale_x_discrete(position = "top") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank())


w2 %>%
  filter(year > 2011) %>% 
  ggplot(aes(x = NetPrice, fill = factor(year))) +
  geom_density(alpha = 0.9) +
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank()) +
  ylab("Density")
  
  







  
  