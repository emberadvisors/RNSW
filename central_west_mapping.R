library(strayr)
library(tidyverse)
library(sf)
library(scales)

central_west <- c("bathurst|blayney|cabonne|cowra|forbes|lachlan|lithgow|oberon|orange|parkes|weddin")

vacancies <- rnorm(11, 0.15, 0.01)

lga <- strayr::read_absmap("lga2021") %>%
  filter(str_detect(tolower(lga_name_2021), central_west) & !str_detect(tolower(lga_name_2021), "shire")) %>%
  mutate(lga_name_2021 = if_else(lga_name_2021 == "Bathurst Regional", "Bathurst", lga_name_2021))

lga$Vacancies <- vacancies
lga$labels <- paste0(scales::label_percent(accuracy = 1)(lga$Vacancies), "\n", lga$lga_name_2021)

lga %>%
  ggplot() +
  geom_sf(aes(geometry = geometry,
              fill = Vacancies),
          colour = "white",
          show.legend = FALSE) +
  # geom_sf_text(aes(label = labels, geometry = geometry), fun.geometry = st_centroid, colour = "white", size = 4, alpha = 10) +
  geom_label_repel(data = lga,
                   aes(x = cent_long,
                       y = cen_lat,
                       label = labels)) +
  theme_void() +
  coord_sf()
