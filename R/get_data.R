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