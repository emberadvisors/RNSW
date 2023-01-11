
## This function scrapes the childcare website to collect vacancies data
get_childcare_source <- function(pagesource = remote_driver$getPageSource()){
  
  results <- pagesource %>%
    pluck(1) %>%
    xml2::read_html() %>%
    rvest::html_element(".results") %>%
    rvest::html_children() %>%
    map(~ rvest::html_text2(.x)) %>%
    unlist()
  
  results <- results[-c(1:2)]
  
  return(results)
  
}

get_childcare_details <- function(results) {
  
  # Name
  name <- results %>%
    str_extract(".*\n") %>%
    str_remove("\n")
  
  # Address
  address <- results %>%
    str_extract(".*\nContact") %>%
    str_remove("\nContact")
  
  # Vacancy
  vacancy <- results %>%
    str_extract(".*\nFees") %>%
    str_remove("\nFees")
  
  tibble(name = name,
         address = address,
         vacancy = vacancy)
  
}

get_childcare_data_superseded <- function(link, postcode){
  
  session <- polite::bow(link)
  
  web <- session %>%
    polite::scrape() %>%
    rvest::html_text() %>%
    str_remove_all("\r|\t|\n")
  
  vacancies <- web %>%
    str_match_all("Number of Centres\\s*(.*?)\\s*View") %>%
    purrr::pluck(1,2) %>%
    str_replace("Centres with Vacancies", "_") %>%
    str_split("_")
  
  vacancies <-tibble(total_centres = vacancies %>%
                       pluck(1,1) %>%
                       as.numeric(),
                     vacancies = vacancies %>%
                       pluck(1,2) %>%
                       as.numeric()) %>%
    mutate(vacancy_rate = vacancies/total_centres)
  
  avg_cost <- web %>%
    str_match("\\$([0-9,.]+)") %>%
    purrr::pluck(2) %>%
    as.numeric()
  
  cost <- tibble(avg_cost = avg_cost)
  
  quality <- web %>%
    str_match_all("NQS Rating is\\s*(.*?)\\s*National Quality Standard") %>%
    purrr::pluck(1,2)
  
  quality <- tibble(avg_quality = quality) %>%
    mutate(avg_quality = if_else(str_detect(tolower(avg_quality), "provisional"), "Provisional - not yet assessed", avg_quality))
  
  tibble(postcode = postcode) %>%
    bind_cols(vacancies, cost, quality)
  
}

## This function scrapes the SQM website to collect median weekly rent
get_rent <- function(link, postcode) {
  
  session <- polite::bow(link, delay = 7)
  
  # scrape rent table using the combined median rent 
  df <- session %>%
    polite::scrape() %>%
    html_table() %>%
    pluck(1) %>%
    slice_tail() %>%
    select(3, 7)
  
  names(df) <- c("median_rent", "12_month_change")
  
  # get week ending date
  date <- session %>%
    polite::scrape() %>%
    html_table() %>%
    pluck(1) %>%
    pluck(1,2) %>%
    str_extract_all("\\d{2} [a-zA-Z]{3} \\d{4}") %>%
    pluck(1) %>%
    as.Date(format = "%d %b %Y")
  
  df %>%
    mutate(week_ending = date,
           postcode = postcode) %>%
    select(postcode, week_ending, median_rent, `12_month_change`)
  
}
  






