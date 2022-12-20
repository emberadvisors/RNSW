##########
## Script to transform scraped data to input into LGA spreadsheet
## Author: Faza Bijaksana
## Date: 20221219
##########

### Setup
## Packages
library(tidyverse)
library(absmapsdata) #to install run remotes::install_github("wfmackey/absmapsdata")

## Read scraped data
childcare <- read.csv("./childcare_data_poa.csv") %>%
  select(-1)
rent <- read.csv("./rent_data_poa.csv") %>%
  select(-1) %>%
  mutate(X12_month_change = str_remove_all(X12_month_change, "%") %>%
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

# Make list of relevant LGAs in Central West directorate
central_west_lga <- c("bathurst|blayney|cabonne|cowra|forbes|lachlan|lithgow|oberon|orange|parkes|weddin|mid-west")
central_west_lga <- corro %>%
  filter(str_detect(tolower(lga), central_west_lga) & !str_detect(tolower(lga), "shire"))

# map data to CW lga
childcare <- childcare %>%
  left_join(corro, by = "postcode") %>%
  mutate(cw_lga = if_else(postcode %in% central_west_lga$postcode, 1, 0))

rent <- rent %>%
  left_join(corro, by = "postcode") %>%
  mutate(cw_lga = if_else(postcode %in% central_west_lga$postcode, 1, 0))

### Calculate results
## NSW State average
childcare_state <- childcare %>%
  group_by(state) %>%
  summarise(total_centres = sum(total_centres),
            vacancies = sum(vacancies),
            vacancy_rate = vacancies/total_centres,
            avg_cost = median(avg_cost))

# get quality 
childcare_state <- childcare %>%
  count(avg_quality) %>%
  slice_max(n) %>%
  select(avg_quality) %>%
  bind_cols(childcare_state) %>%
  tibble() %>%
  rename(geography = state)

rent_state <- rent %>%
  filter(!is.na(median_rent)) %>%
  group_by(state, week_ending) %>%
  summarise(median_rent = median(median_rent),
            median_change = median(X12_month_change)) %>%
  rename(geography = state)

## Central West average
childcare_cw <- childcare %>% 
  filter(cw_lga == 1) %>%
  summarise(total_centres = sum(total_centres),
            vacancies = sum(vacancies),
            vacancy_rate = vacancies/total_centres,
            avg_cost = median(avg_cost))

# get quality 
childcare_cw <- childcare %>%
  filter(cw_lga == 1) %>%
  count(avg_quality) %>%
  slice_max(n) %>%
  select(avg_quality) %>%
  bind_cols(childcare_cw) %>%
  tibble() %>%
  mutate(geography = "central_west")

rent_cw <- rent %>%
  filter(!is.na(median_rent),
         cw_lga == 1) %>%
  group_by(week_ending) %>%
  summarise(median_rent = median(median_rent),
            median_change = median(X12_month_change)) %>%
  mutate(geography = "central_west")

## LGA averages
childcare_lga <- childcare %>%
  filter(cw_lga == 1) %>%
  group_by(lga) %>%
  summarise(total_centres = sum(total_centres),
            vacancies = sum(vacancies),
            vacancy_rate = vacancies/total_centres,
            avg_cost = median(avg_cost))

# get quality 
childcare_lga <- childcare %>%
  filter(cw_lga == 1) %>%
  count(lga, avg_quality) %>%
  group_by(lga) %>%
  slice_max(n) %>%
  select(lga, avg_quality) %>%
  left_join(childcare_lga, by = "lga") %>%
  tibble() %>%
  rename(geography = lga)

rent_lga <- rent %>%
  filter(!is.na(median_rent),
         cw_lga == 1) %>%
  group_by(lga, week_ending) %>%
  summarise(median_rent = median(median_rent),
            median_change = median(X12_month_change)) %>%
  rename(geography = lga)

### Combine state, cw and lga dataframes and export
childcare_lga %>%
  bind_rows(childcare_cw) %>%
  bind_rows(childcare_state) %>%
  mutate(geography = str_remove_all(geography, " .*")) %>%
  write.csv("./childcare_results.csv")

rent_lga %>%
  bind_rows(rent_cw) %>%
  bind_rows(rent_state) %>%
  mutate(geography = str_remove_all(geography, " .*")) %>%
  write.csv("./rent_results.csv")
