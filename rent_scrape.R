##########
## Script to scrape median asking data for RNSW indicators
## Author: Faza Bijaksana
## Date: 20221209
##########

### Setup
## Packages
library(tidyverse)
library(glue)
library(rvest)
library(polite)
library(absmapsdata) #to install run remotes::install_github("wfmackey/absmapsdata")

## Read functions
source("./R/get_data.R")

## Get NSW postcodes
# ABS correspondence for postcode to state so we can get postcodes in NSW
corro <- absmapsdata::get_correspondence_absmaps("postcode", 2019, "state", 2016) %>%
  filter(STATE_NAME_2016 == "New South Wales",
         ratio > 0.90) %>%
  select(postcode = POSTCODE_2019) %>%
  unique()
postcodes <- corro$postcode %>%
  unique()

## Get the urls for the relevant websites we are scraping
asking_rents_url <- "https://sqmresearch.com.au/weekly-rents.php?postcode="

## Make tibble with url to scrape (i.e. static website url + dynamic postcode landing page)
urls <- tibble(postcode = postcodes) %>%
  mutate(rents_url = glue::glue("{asking_rents_url}{postcode}&t=1"))

### Results
## We purrr::safely for the scraping functions to handle errors
get_rent_safely <- purrr::safely(get_rent)

## Iterate the relevant scraping functions over each postcode
rent_data <- purrr::map2(.x = urls$rents_url,
                         .y = urls$postcode,
                         ~get_rent_safely(link = .x, postcode = .y))

## Get only the scrape yielding results (purrr::safely splits out valid results and errors)
rent_data_df <- rent_data %>%
  map("result") %>%
  bind_rows() %>%
  mutate(state = "nsw",
         X12_month_change = str_remove_all(X12_month_change, "%") %>%
           as.numeric(X12_month_change)/100,
         median_rent = str_remove_all(median_rent, ",") %>%
           as.numeric(median_rent))

## ABS correspondence for postcode to LGA so we can get postcodes for the relevant LGAs in Central West 
# Get poa to lga corro
corro <- absmapsdata::get_correspondence_absmaps("postcode", 2019, "lga", 2019) %>%
  # only keep the correspondence with >50% ratio for simplicity
  filter(ratio > 0.5) %>%
  select(postcode = POSTCODE_2019,
         lga = LGA_NAME_2019) %>%
  mutate(postcode = as.numeric(postcode))

## Make list of relevant LGAs in Central West directorate
central_west_lga <- c("bathurst|blayney|cabonne|cowra|forbes|lachlan|lithgow|oberon|orange|parkes|weddin|mid-west")
central_west_lga <- corro %>%
  filter(str_detect(tolower(lga), central_west_lga) & !str_detect(tolower(lga), "shire"))

## map data to CW lga
rent <- rent_data_df %>%
  left_join(corro, by = "postcode") %>%
  mutate(cw_lga = if_else(postcode %in% central_west_lga$postcode, 1, 0))

## Calculate results
# State avg
rent_state <- rent %>%
  filter(!is.na(median_rent)) %>%
  group_by(state, week_ending) %>%
  summarise(median_rent = median(median_rent),
            median_change = median(X12_month_change)) %>%
  rename(geography = state)

# CW avg
rent_cw <- rent %>%
  filter(!is.na(median_rent),
         cw_lga == 1) %>%
  group_by(week_ending) %>%
  summarise(median_rent = median(median_rent),
            median_change = median(X12_month_change)) %>%
  mutate(geography = "central_west")

# LGA avgs
rent_lga <- rent %>%
  filter(!is.na(median_rent),
         cw_lga == 1) %>%
  group_by(lga, week_ending) %>%
  summarise(median_rent = median(median_rent),
            median_change = median(X12_month_change)) %>%
  rename(geography = lga)

rent_lga %>%
  bind_rows(rent_cw) %>%
  bind_rows(rent_state) %>%
  mutate(geography = str_remove_all(geography, " .*")) %>%
  write.csv("./rent_results.csv")