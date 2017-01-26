
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
library(zoo)
library(extrafont)

font_import("Helvetica")

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

rm(list = "raw_data")

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
         qy         = paste(as.character(year), paste0("Q", as.character(quarter)),
                            sep = "\n") %>% as.factor,
         yearqtr    = as.yearqtr(date),
         yearmon    = as.yearmon(date),
         qtr_int    = year*10 + quarter,
         Cat_1      = Hierarchy_Level_1,
         Cat_2      = Hierarchy_Level_2,
         Cat_3      = Hierarchy_Level_3,
         ST         = ifelse(year > 2012, TRUE, FALSE)) %>% 
  filter(year %>% between(2008, 2016),
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


waschtisch <- df %>% 
  filter(Cat_2 == "Waschtische") %>% 
  filter(NetPrice < NetPrice %>% quantile(0.999),
         NetPrice > NetPrice %>% quantile(0.001)) %>%
  inner_join(tribble(~Cat_3,
                     "Waschtische Keramik",
                     "Doppelwaschtische Keramik")) %>% 
  na.omit()


# Doppel - Waschtisch
# -----------------------------------------------

w2 <- waschtisch %>%
  filter(year > 2009) %>% 
  filter(Cat_3 == "Doppelwaschtische Keramik") %>% 
  filter(NetPrice > NetPrice %>% quantile(0.01))
#          NetPrice < NetPrice %>% quantile(0.99))

w2_breaks <- w2$NetPrice %>%
  pretty(nclass.Sturges(w2$NetPrice))

w2_analysis <- w2 %>% 
  # filter(NetPrice > NetPrice %>% quantile(0.005)) %>% 
  mutate(Range = NetPrice %>% cut(w2_breaks)) %>% 
  group_by(qy, Range, ST) %>% 
  summarise(Sales = sum(Sales),
            N = n())


w2_analysis %>%
  ggplot(aes(x = factor(qy), y = Range)) +
  geom_tile(aes(fill = Sales), color = "white", width=.99, height=.99) +
  # geom_segment(x=12.5, xend=12.5, y=0, yend=14.5, size=.9, lineend = "round") +
  # scale_color_gradient() +
  # scale_fill_brewer() +
  scale_fill_viridis(alpha = 0.2, option = "A", begin = 0, end = 1, direction = -1,
                     # guide=guide_colourbar(ticks=5,
                     #                       barheight=.3,
                     #                       barwidth=20)
                     breaks = seq(0,200000, by= 20000)
                     )+
  # ggtitle("Doppelwaschtische Keramik") + 
  labs(x="", y="", fill="") +
  theme(
    # legend.position="left",
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        title=element_text(hjust=-1.2, face="bold", vjust=2, family="Helvetica")) 
# +
  # coord_polar() -> w2_plot; w2_plot
require(svglite)
ggsave("newplot.svg")


w2 %>%
  # filter(year > 2011) %>% 
  ggplot(aes(x = NetPrice, fill = factor(year))) +
  geom_density(alpha = 0.9, color = "white") +
  # scale_fill_brewer(palette = "Set1") +
  # scale_fill_manual(values = c("#33of66", "#bf3875")) +
  scale_fill_viridis(discrete = TRUE, option = "A") +
  scale_y_continuous(labels = scales::percent) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank()) +
  # ggtitle("Doppelwaschtische Keramik") +
  ylab("") +
  xlab("") -> w2_density;w2_density


combn(1:5)
combn(1:5, 2)
