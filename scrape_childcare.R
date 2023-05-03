##########
## Script to scrape childcare vacancies data for RNSW indicators
## Author: Ember Advisors
## Date: 20230504
##########

### Setup
## Packages
required_packages <- c("tidyverse", "glue", "rvest", "polite", "RSelenium", "lubridate", "here")
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

library(tidyverse)
library(glue)
library(rvest)
library(polite)
library(absmapsdata) #to install run remotes::install_github("wfmackey/absmapsdata")
library(RSelenium)
library(lubridate)

## Set working directory
setwd(here::here())

## Today's date
t_date <- today() %>%
  str_replace_all("-", "_")

## Read functions
source("./R/get_data.R")

## Get NSW locations
# ABS correspondence for lga to state so we can get lgas in NSW
corro <- read.csv("./ABS correspondences/CG_LGA_2018_GCCSA_2021.csv") %>%
  mutate(state = case_when(str_detect(GCCSA_NAME_2021, "NSW|Syd") ~ "NSW",
                           str_detect(GCCSA_NAME_2021, "Vic|Melb") ~ "Vic",
                           str_detect(GCCSA_NAME_2021, "Qld|Bris") ~ "Qld",
                           str_detect(GCCSA_NAME_2021, "NT|Dar") ~ "NT",
                           str_detect(GCCSA_NAME_2021, "WA|Per") ~ "WA",
                           str_detect(GCCSA_NAME_2021, "SA|Adel") ~ "SA",
                           str_detect(GCCSA_NAME_2021, "Tas|Hob") ~ "Tas",
                           str_detect(GCCSA_NAME_2021, "Cap|Can") ~ "ACT",
                           TRUE ~ GCCSA_NAME_2021)) %>%
  filter(state == "NSW") %>%
  group_by(LGA_NAME_2018) %>%
  slice_max(RATIO_FROM_TO) %>%
  select(lga = LGA_NAME_2018,
         state) %>%
  mutate(lga = str_remove(lga, " \\(.*"))

# lgas in CW
central_west_lga <- c("bathurst|blayney|cabonne|cowra|forbes|lachlan|lithgow|oberon|orange|parkes|weddin|mid-west")

## Set up selenium Webdriver
driver <- RSelenium::rsDriver(browser = "firefox",
                              port = 4444L, chromever = "108.0.5359.22")

remote_driver <- driver[["client"]]

remote_driver$open()

driver$server$output()

## Create empty tibble for results to be put into
childcare_data <- tibble()

## If scrape stops mid-way through, filter the postcodes that haven't been scraped
corro <- corro %>%
  filter(!lga %in% childcare_data$lga)

## Loop the locations into selenium scrape
for (i in 1:length(corro$lga)) {
  
  location = corro$lga[i]
  
  random_wait_time <- runif(n = 1, min = 6, max = 20) %>% round()
  
  Sys.sleep(random_wait_time)
  
  url <- glue::glue("https://www.startingblocks.gov.au/find-child-care#/distance/100km/address/{location}%2520NSW%252C%2520Australia/all/all/all/all/all/all") %>% as.character()
  
  remote_driver$navigate(url)
  
  Sys.sleep(5)
  
  results <- get_childcare_source() %>%
    get_childcare_details() %>%
    mutate(lga = location)
  
  childcare_data <- bind_rows(childcare_data, results)
  
  random_wait_time <- runif(n = 1, min = 3, max = 20) %>% round()
  
  Sys.sleep(random_wait_time)
}

remote_driver$quit()

## remove duplicates
childcare_data <- childcare_data %>%
  unique()

## use postcodes to accurate map to lgas because the scrape is not 100% accurate
corro <- readxl::read_xlsx("./ABS correspondences/CG_POA_2021_LGA_2021.xlsx") %>% 
  filter(OVERALL_QUALITY_INDICATOR == "Good") %>%
  group_by(POA_CODE_2021) %>%
  slice_max(RATIO_FROM_TO) %>%
  select(postcode = POA_CODE_2021,
         lga = LGA_NAME_2021)
childcare_data <- childcare_data %>%
  mutate(postcode = str_extract(address, "\\d{4}")) %>%
  left_join(corro, by = "postcode") %>%
  select(name, address, vacancy, postcode, lga = lga.y)

## save data
write.csv(childcare_data, glue::glue("./Data/raw/childcare_vacancies_raw_{t_date}.csv"))

## calculate vacancies for central west lgas
central_west_results <- childcare_data %>%
  filter(str_detect(tolower(lga), central_west_lga) & !str_detect(tolower(lga), "shire"),
         # filter out those without vacancy data
         !is.na(vacancy)) %>%
  group_by(lga) %>%
  count(vacancy) %>%
  pivot_wider(names_from = vacancy, values_from = n) %>%
  replace(is.na(.), 0) %>%
  mutate(Total = `No Vacancy` + Vacancy,
         vacancy_rate = Vacancy/Total)

# calculate Central West total
central_west_results <- central_west_results %>%
  ungroup() %>%
  select(-lga, -vacancy_rate) %>%
  summarise_all(sum) %>%
  mutate(lga = "central_west",
         vacancy_rate = Vacancy/Total) %>%
  bind_rows(central_west_results) %>%
  select(geography = lga, vacancy_rate, no_vacancy = `No Vacancy`, vacancy = Vacancy, total = Total)

## calculate vacancies for all of NSW
results <- childcare_data %>%
  # filter out those without vacancy data
  filter(!is.na(vacancy)) %>%
  count(vacancy) %>%
  pivot_wider(names_from = vacancy, values_from = n) %>%
  replace(is.na(.), 0) %>%
  mutate(geography = "nsw",
         Total = `No Vacancy` + Vacancy,
         vacancy_rate = Vacancy/Total) %>%
  select(geography, vacancy_rate, no_vacancy = `No Vacancy`, vacancy = Vacancy, total = Total) %>%
  bind_rows(central_west_results)
  

write.csv(results, glue::glue("./Data/childcare_vacancies_results_{t_date}.csv"))
