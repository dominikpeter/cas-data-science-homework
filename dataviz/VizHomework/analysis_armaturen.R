
# Horizontale Recherche <- verschiedene Modelle
# Vertikale Recherche <- Gestaltung wie?
# Gestaltungsraster ist wichtig
# Typography wichtig
# Blocksatz lang, kurz, lang kurz <- am besten


rm(list=ls())

library(tidyverse)
library(lubridate)
library(viridis)


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

# raw_data <- read_delim("/Users/dominikpeter/Google/R/datasets/pricing.txt", delim = "|")
raw_data <- read_delim("C:/Users/peterd/Downloads/pricing.txt", delim = "|")

lookup_brand <- tibble(Key = raw_data$OwnBrand %>% unique) %>% 
  mutate(PrivateLabel = grepl("casa", tolower(Key)))

cats <- tribble(~cats,
                "Sanitär-Armaturen",
                "Waschtische",
                "Garnituren-Programme")

cats

units <- tribble(~QuantityUnit, "PCE", "STK")

raw_data %>% select(QuantityUnit, Hierarchy_Level_2) %>%
  filter(Hierarchy_Level_2 == 'Sanitär-Armaturen') %>% head()

# features
df <- raw_data %>% 
  filter(Hierarchy_Level_2 == 'Sanitär-Armaturen') %>%
  inner_join(units) %>% 
  left_join(lookup_brand, by = c("OwnBrand" = "Key")) %>% 
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
         !is.infinite(NetPrice)) %>% 
  na.omit()


raw_data <- NULL



armaturen <- df %>% 
  filter(Cat_2 == "Sanitär-Armaturen") %>% 
  filter(NetPrice < NetPrice %>% quantile(0.90),
         NetPrice > NetPrice %>% quantile(0.01)) %>% 
  na.omit()



# Armaturen
# -----------------------------------------------


armaturen %>% 
  ggplot(aes(x = NetPrice, fill = factor(year))) +
  geom_density(alpha = 0.7) +
  scale_fill_viridis(discrete = TRUE,
                     option = "C",
                     guide = guide_legend(title = "Year")) +
  # scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank()) +
  ylab("Density") +
  xlab("Price") +
  facet_grid(year~.)
        


armaturen_breaks <- armaturen$NetPrice %>%
  pretty(nclass.Sturges(armaturen$NetPrice))

armaturen_analysis <- armaturen %>% 
  filter(NetPrice < NetPrice %>% quantile(0.99)) %>% 
  filter(NetPrice > NetPrice %>% quantile(0.1)) %>% 
  mutate(Range = NetPrice %>% cut(armaturen_breaks)) %>% 
  group_by(year, Range) %>% 
  summarise(Sales = sum(Sales),
            N = n())

armaturen_analysis %>% 
  ggplot(aes(x = factor(year), y = Range)) +
  geom_tile(aes(fill = Sales), color = "white") +
  # scale_color_gradient() +
  scale_fill_viridis(alpha = 0.2, option = "D") +
  # scale_fill_gradient(low = "#ece7f2", high = "#2b8cbe") +
  scale_x_discrete(position = "top") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank()) +
  xlab("") +
  ylab("")



armaturen %>% 
  ggplot(aes(x = NetPrice, fill = factor(year))) +
  geom_density(alpha = 0.6) +
  scale_fill_viridis(discrete = TRUE,
                     option = "D",
                     guide = guide_legend(title = "Year")) +
  # scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(labels = scales::percent) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank()) +
  ylab("Density") +
  xlab("Price") +
  ggtitle("Sanitär-Armaturen")
        