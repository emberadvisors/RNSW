##########
## Script to scrape data for RNSW indicators
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

## Get postcodes we are interested in
# ABS correspondence for postcode to LGA so we can get postcodes for the relevant LGAs in Central West 
corro <- absmapsdata::get_correspondence_absmaps("postcode", 2019, "lga", 2019)

# Make list of relevant LGAs in Central West directorate
central_west_lga <- c("bathurst|blayney|cabonne|cowra|forbes|lachlan|lithgow|oberon|orange|parkes|weddin")

# Filter postcodes to only those in central_west_lga
central_west_abs <- corro %>%
  filter(str_detect(tolower(LGA_NAME_2019), central_west_lga) & !str_detect(tolower(LGA_NAME_2019), "shire"))
postcodes <- corro$POSTCODE_2019 %>%
  unique()

## Get the urls for the relevant websites we are scraping
childcare_url <- "https://www.careforkids.com.au/child-care/"
asking_rents_url <- "https://sqmresearch.com.au/weekly-rents.php?postcode="

## Make tibble with url to scrape (i.e. static website url + dynamic postcode landing page)
urls <- tibble(postcode = postcodes) %>%
  mutate(childcare_url = glue::glue("{childcare_url}{postcode}"),
         rents_url = glue::glue("{asking_rents_url}{postcode}&t=1"))

### Results
## We purrr::safely for the scraping functions to handle errors
get_childcare_data_safely <- purrr::safely(get_childcare_data)
get_rent_safely <- purrr::safely(get_rent)

## Iterate the relevant scraping functions over each postcode
childcare_data <- purrr::map2(.x = urls$childcare_url, 
                              .y = urls$postcode,
                              ~ get_childcare_data_safely(link = .x, postcode = .y))

rent_data <- purrr::map2(.x = urls$rents_url,
                         .y = urls$postcode,
                         ~get_rent_safely(link = .x, postcode = .y))

## Get only the scrape yielding results (purrr::safely splits out valid results and errors)
childcare_data_df <- childcare_data %>%
  map("result") %>%
  bind_rows()

rent_data_df <- rent_data %>%
  map("result") %>%
  bind_rows()

## Write data
write.csv(childcare_data_df, "./childcare_data.csv")
write.csv(rent_data_df, "./rent_data.csv")
