library(tidyverse)
library(rvest)
library(glue)

# creates the URL to grab for a particular year
url_year <- function(year) {
  year <- as.character(year)
  url <- glue("https://www.cdc.gov/drugoverdose/maps/rxcounty{year}.html")
  url
}

prescriptionYear <- function(year) {
  url_year(year) %>%
    read_html() %>%
    html_node("table") %>%
    html_table() %>%
    filter(State == "OH") %>%
    mutate(year = year)
}

# years covered in CDC data
years <- 2006:2018