library(censusapi)
library(tidyverse)
library(glue)

apis <- listCensusApis()

get_url <- function(year, glue_string, valid_years) {
  year <- as.character(year)
  valid_years <- as_vector(as.character(valid_years))
  if(year %in% valid_years) {
    glue(glue_string)
  } else {
    warning(glue("Please input a year between {valid_years[1]} and {valid_years[length(valid_years)]}. {year} is out of range"))
  }
}

get_url_acs1_2005_to_2009 <- function(year) {
    get_url(year = year,
            glue_string = glue("https://api.census.gov/data/{year}/acs/acs1"),
            valid_years = 2005:2009)
}