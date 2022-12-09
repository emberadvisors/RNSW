##########
## Script to scrape childcare data from https://www.careforkids.com.au/child-care
## Author: Faza Bijaksana
## Date: 20221209
##########


### Setup
# Packages
library(tidyverse)
library(glue)
library(rvest)
library(absmapsdata) #to install run remotes::install_github("wfmackey/absmapsdata")

# get postcodes we are interested in
corro <- absmapsdata::get_correspondence_absmaps("postcode", 2019, "lga", 2019)
central_west_lga <- c("bathurst|blayney|cabonne|cowra|forbes|lachlan|lithgow|oberon|orange|parkes|weddin")
central_west_abs <- corro %>%
  filter(str_detect(tolower(LGA_NAME_2019), central_west_lga) & !str_detect(tolower(LGA_NAME_2019), "shire"))

# url
childcare_url <- "https://www.careforkids.com.au/child-care/"

# make tibble with url to scrape
urls <- tibble(postcode = corro$POSTCODE_2019) %>%
  unique()
urls <- urls %>%
  mutate(url = glue::glue("{childcare_url}{postcode}"))

# function to scrape relevant data
get_vacancies <- function(url){
  
  rvest::read_html(url) %>%
    rvest::html_text() %>%
    str_remove_all("\r|\t|\n") %>%
    str_match_all("Number of Centres\\s*(.*?)\\s*View") %>%
    purrr::pluck(1,2)
  
}

get_avg_cost <- function(url){
  
  rvest::read_html(url) %>%
    rvest::html_text() %>%
    str_remove_all("\r|\t|\n") %>%
    str_match("\\$([0-9,.]+)") %>%
    purrr::pluck(2)
  
}
