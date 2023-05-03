##########
## Script to visualise indicators in a heatmap
## Author: Ember Advisors
## Date: 20230504
##########

### Setup
## Packages
required_packages <- c("tidyverse", "sf", "scales", "viridis")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

library(tidyverse)
library(sf)
library(scales)
library(viridis)
library(strayr) # to install run remotes::install_github("runapp-aus/strayr")

## Set working directory
setwd(here::here())

### Make heatmaps
## CW LGA's
central_west <- c("bathurst|blayney|cabonne|cowra|forbes|lachlan|lithgow|mid-western|oberon|orange|parkes|weddin")

## Read data
data <- read.csv("./Data/mapping_data.csv") %>%
  mutate(lga = if_else(lga == "Mid-Western", "Mid-Western Regional", lga))

## Create tibble of CW LGA's with shapefiles
lga <- strayr::read_absmap("lga2021") %>%
  filter(str_detect(tolower(lga_name_2021), central_west) & !str_detect(tolower(lga_name_2021), "shire")) %>%
  mutate(lga_name_2021 = if_else(lga_name_2021 == "Bathurst Regional", "Bathurst", lga_name_2021))

## Some mutations
lga <- lga %>%
  left_join(data, by = c("lga_name_2021" = "lga")) %>%
  mutate(labels = if_else(metric == "Unemployment", 
                          label_percent(accuracy = 0.1)(data),
                          label_percent(accuracy = 1)(data)))

## Make the maps
childcare_vacancies_map <- lga %>%
  filter(metric == "Early education") %>%
  ggplot() +
  geom_sf(aes(geometry = geometry,
              fill = data),
          colour = "white",
          show.legend = FALSE) +
  geom_sf_text(aes(label = labels, geometry = geometry), fun.geometry = st_centroid, colour = "white", size = 6) +
  theme_void() +
  theme(panel.background = element_rect(fill = "white")) +
  coord_sf() +
  scale_fill_viridis(direction = 1, option = "G", end = 0.7) +
  ggtitle("Child care vacancies")

unemployment_map <- lga %>%
  filter(metric == "Unemployment") %>%
  ggplot() +
  geom_sf(aes(geometry = geometry,
              fill = data),
          colour = "white",
          show.legend = FALSE) +
  geom_sf_text(aes(label = labels, geometry = geometry), fun.geometry = st_centroid, colour = "white", size = 6) +
  theme_void() +
  theme(panel.background = element_rect(fill = "white")) +
  coord_sf() +
  scale_fill_viridis(direction = -1, option = "G", end = 0.7) +
  ggtitle("Unemployment rate")

## Save the maps to png files
ggsave("./Heatmaps/childcare_vacancies.png", childcare_vacancies_map)
ggsave("./Heatmaps/unemployment.png", unemployment_map)
