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
library(RSelenium)
library(lubridate)

t_date <- today() %>%
  str_replace_all("-", "_")

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

#### Median asking rent scrape ####
## Get the url for the relevant website
asking_rents_url <- "https://sqmresearch.com.au/weekly-rents.php?postcode="
p_prices_url <- "https://sqmresearch.com.au/asking-property-prices.php?postcode="

## Make tibble with url to scrape (i.e. static website url + dynamic postcode landing page)
urls <- tibble(postcode = postcodes) %>%
  mutate(rents_url = glue::glue("{asking_rents_url}{postcode}&t=1"),
         p_prices_url = glue::glue("{p_prices_url}{postcode}&t=1"))

### Results
## We purrr::safely for the scraping functions to handle errors
get_rent_safely <- purrr::safely(get_rent)
get_property_price_safely <- purrr::safely(get_property_price)

## Iterate the relevant scraping functions over each postcode
rent_data_df <- purrr::map2(.x = urls$rents_url,
                         .y = urls$postcode,
                         ~get_rent_safely(link = .x, postcode = .y))
p_prices_df <- purrr::map2(.x = urls$p_prices_url,
                           .y = urls$postcode,
                           ~get_property_price_safely(link = .x, postcode = .y))

## Get only the scrape yielding results (purrr::safely splits out valid results and errors)
rent_data_df <- rent_data_df %>%
  map("result") %>%
  bind_rows() %>%
  mutate(state = "nsw",
         X12_month_change = str_remove_all(X12_month_change, "%") %>%
           as.numeric(X12_month_change)/100,
         median_rent = str_remove_all(median_rent, ",") %>%
           as.numeric(median_rent))
p_prices_df <- p_prices_df %>%
  map("result") %>%
  bind_rows() %>%
  mutate(state = "nsw",
         `12_month_change` = str_remove_all(`12_month_change`, "%") %>%
           as.numeric(`12_month_change`)/100,
         median_asking_price = str_remove_all(median_asking_price, ",") %>%
           as.numeric(median_asking_price))

#### Vacancy rate scrape ####
vacancy_rates <- "/html/body/div/div/div/div/div/div[1]/div/svg/g[7]/g[3]"

## Set up selenium Webdriver
scrape_dt <- lubridate::now(tzone = "Australia/Sydney")
  
driver <- rsDriver(browser = c("firefox"),
                   port = 4570L, chromever = "108.0.5359.71")

remote_driver <- driver[["client"]]

remote_driver$open()

driver$server$output()

## start date of data
start_date_of_data <- "2005-01-01"

## Create empty tibble for results to be put into
vacancy_data <- tibble()

## If the scraper stops midway through, get remaining postcodes
postcodes <- postcodes[!postcodes %in% vacancy_data$postcode]

for (i in 1:length(postcodes)) {
  
  postcode = postcodes[i]
  
  random_wait_time <- runif(n = 1, min = 6, max = 20) %>% round()
  
  Sys.sleep(random_wait_time)
    
  url <- glue::glue("https://sqmresearch.com.au/graph_vacancy.php?postcode={postcode}&t=1") %>% as.character()
  
  remote_driver$navigate(url)
  
  safely_extract <- safely(extract_chart_data_sqm, otherwise = NULL)
  
  xpath_y_trans <- "/html/body/div/div/div/div/div/div[1]/div/svg/g[11]"
  
  dat <-
    safely_extract(
      page_source = remote_driver$getPageSource(),
      xpath_chart = "/html/body/div/div/div/div/div/div[1]/div/svg/g[7]/g[3]",
      start_date = start_date_of_data,
      xpath_y = xpath_y_trans
    ) 
  
  if (!is.null(dat$result)) {
    
    dat <- dat %>%
      pluck(1) %>%
      rename("vacancy_rate" = "translated_y",
             "approximate_date" = "translated_x") %>%
      select(-x, -y) %>%
      mutate(postcode = postcode,
             month = format(approximate_date, "%Y-%m")) %>%
      group_by(postcode, month) %>%
      slice_max(approximate_date) %>%
      filter(month > "2021-01")
    
    vacancy_data <- bind_rows(vacancy_data, dat)
    
  }
  
  random_wait_time <- runif(n = 1, min = 3, max = 20) %>% round()
  
  Sys.sleep(random_wait_time)
  
}

try(remote_driver$quit())

write.csv(vacancy_data, glue::glue("./Data/vacancies_raw_{t_date}.csv"))

#### Clean results ####
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
vacancies <- vacancy_data %>%
  mutate(postcode = as.numeric(postcode)) %>%
  left_join(corro, by = "postcode") %>%
  mutate(cw_lga = if_else(postcode %in% central_west_lga$postcode, 1, 0))
prices <- p_prices_df %>%
  mutate(postcode = as.numeric(postcode)) %>%
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
vacancies_state <- vacancies %>%
  filter(vacancy_rate != 0) %>%
  mutate(state = "nsw") %>%
  group_by(state, month) %>%
  summarise(vacancy_rate = mean(vacancy_rate)) %>%
  rename(geography = state)
prices_state <- prices %>%
  filter(!is.na(median_asking_price)) %>%
  mutate(state = "nsw") %>%
  group_by(state, week_ending) %>%
  summarise(median_asking_price = median(median_asking_price),
            median_change = median(`12_month_change`)) %>%
  rename(geography = state)

# CW avg
rent_cw <- rent %>%
  filter(!is.na(median_rent),
         cw_lga == 1) %>%
  group_by(week_ending) %>%
  summarise(median_rent = median(median_rent),
            median_change = median(X12_month_change)) %>%
  mutate(geography = "central_west")
vacancies_cw <- vacancies %>%
  filter(vacancy_rate != 0,
         cw_lga == 1) %>%
  group_by(month) %>%
  summarise(vacancy_rate = mean(vacancy_rate)) %>%
  mutate(geography = "central_west")
prices_cw <- prices %>%
  filter(!is.na(median_asking_price),
         cw_lga == 1) %>%
  group_by(week_ending) %>%
  summarise(median_asking_price = median(median_asking_price),
            median_change = median(`12_month_change`)) %>%
  mutate(geography = "central_west")

# LGA avgs
rent_lga <- rent %>%
  filter(!is.na(median_rent),
         cw_lga == 1) %>%
  group_by(lga, week_ending) %>%
  summarise(median_rent = median(median_rent),
            median_change = median(X12_month_change)) %>%
  rename(geography = lga)
vacancies_lga <- vacancies %>%
  filter(vacancy_rate != 0,
         cw_lga == 1) %>%
  group_by(lga, month) %>%
  summarise(vacancy_rate = mean(vacancy_rate)) %>%
  rename(geography = lga)
prices_lga <- prices %>%
  filter(!is.na(median_asking_price),
         cw_lga == 1) %>%
  group_by(lga, week_ending) %>%
  summarise(median_asking_price = median(median_asking_price),
            median_change = median(`12_month_change`)) %>%
  rename(geography = lga)

rent_lga %>%
  bind_rows(rent_cw) %>%
  bind_rows(rent_state) %>%
  mutate(geography = str_remove_all(geography, " .*")) %>%
  write.csv(glue::glue("./Data/rent_results_{t_date}.csv"))

vacancies_lga %>%
  bind_rows(vacancies_cw) %>%
  bind_rows(vacancies_state) %>%
  mutate(geography = str_remove_all(geography, " .*")) %>%
  write.csv(glue::glue("./Data/vacancies_results_{t_date}.csv"))

prices_lga %>%
  bind_rows(prices_cw) %>%
  bind_rows(prices_state) %>%
  mutate(geography = str_remove_all(geography, " .*")) %>%
  write.csv(glue::glue("./Data/price_results_{t_date}.csv"))
