library(tidyverse)
library(rvest)
library(glue)

# download prescription data for one particular year
prescriptionYear <- function(year) {
  year <- as.character(year)
  df <- glue("https://www.cdc.gov/drugoverdose/maps/rxcounty{year}.html") %>%
    read_html() %>%
    html_node("table") %>%
    html_table() %>%
    mutate(year = year)
  colnames(df) <- c("county", "State", "FIPS", "PrescribingRate", "year")
  df <- df %>%
    filter(State == "OH") %>%
    select(-State, -FIPS) %>%
    mutate(county = str_remove(county, ", OH"))
}

# years covered in CDC data
years <- 2006:2018

prescriptions <- map_dfr(years, prescriptionYear) %>%
  mutate(county = as_factor(county),
         PrescribingRate = as.numeric(PrescribingRate))