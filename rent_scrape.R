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
vacancies <- vacancy_data %>%
  mutate(postcode = as.numeric(postcode)) %>%
  left_join(corro, by = "postcode") %>%
  mutate(cw_lga = if_else(postcode %in% central_west_lga$postcode, 1, 0))

## Calculate results
# State avg
vacancies_state <- vacancies %>%
  filter(vacancy_rate != 0) %>%
  mutate(state = "nsw") %>%
  group_by(state, month) %>%
  summarise(vacancy_rate = mean(vacancy_rate)) %>%
  rename(geography = state)

# CW avg
vacancies_cw <- vacancies %>%
  filter(vacancy_rate != 0,
         cw_lga == 1) %>%
  group_by(month) %>%
  summarise(vacancy_rate = mean(vacancy_rate)) %>%
  mutate(geography = "central_west")

# LGA avgs
vacancies_lga <- vacancies %>%
  filter(vacancy_rate != 0,
         cw_lga == 1) %>%
  group_by(lga, month) %>%
  summarise(vacancy_rate = mean(vacancy_rate)) %>%
  rename(geography = lga)

vacancies_lga %>%
  bind_rows(vacancies_cw) %>%
  bind_rows(vacancies_state) %>%
  mutate(geography = str_remove_all(geography, " .*"),
         month = as.Date(glue("{month}-01"), format = "%Y-%m-%d"),
         month = format(month, "%d-%m-%Y")) %>%
  write.csv(glue::glue("./Data/rent_vacancies_results_{t_date}.csv"))

