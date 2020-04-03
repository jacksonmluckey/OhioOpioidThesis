library(tidyverse)
library(rvest)
library(glue)

# creates the URL to grab for a particular year
url_year <- function(year) {
  url <- glue("https://www.cdc.gov/drugoverdose/maps/rxcounty{year}.html")
  url
}

prescriptionYear <- function(year) {
  year <- as.character(year)
  df <- url_year(year) %>%
    read_html() %>%
    html_node("table") %>%
    html_table() %>%
    mutate(year = year)
  col_to_rename <- glue("{Year} Prescribing Rate")
  df <- df %>%
    filter(State == "OH") %>%
    rename()
}

test <- prescriptionYear(2017)

# years covered in CDC data
years <- 2006:2018

prescriptions <- map_dfr(years, prescriptionYear)