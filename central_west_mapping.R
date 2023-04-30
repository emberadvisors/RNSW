library(strayr)
library(tidyverse)
library(sf)
library(scales)
library(viridis)

central_west <- c("bathurst|blayney|cabonne|cowra|forbes|lachlan|lithgow|mid-western|oberon|orange|parkes|weddin")

data <- read.csv("./Data/mapping_data.csv") %>%
  mutate(lga = if_else(lga == "Mid-Western", "Mid-Western Regional", lga))

lga <- strayr::read_absmap("lga2021") %>%
  filter(str_detect(tolower(lga_name_2021), central_west) & !str_detect(tolower(lga_name_2021), "shire")) %>%
  mutate(lga_name_2021 = if_else(lga_name_2021 == "Bathurst Regional", "Bathurst", lga_name_2021))

lga <- lga %>%
  left_join(data, by = c("lga_name_2021" = "lga"))

lga$labels <- scales::label_percent(accuracy = 1)(lga$data)

lga %>%
  filter(metric == "Early education") %>%
  ggplot() +
  geom_sf(aes(geometry = geometry,
              fill = data),
          colour = "white",
          show.legend = FALSE) +
  geom_sf_text(aes(label = labels, geometry = geometry), fun.geometry = st_centroid, colour = "white", size = 6) +
  theme_void() +
  coord_sf() +
  scale_fill_viridis(direction = 1, option = "G", end = 0.7)

