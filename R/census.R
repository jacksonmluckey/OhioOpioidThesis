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

get_url_acs1_2010_to_2018 <- function(year) {
  get_url(year = year,
          glue_string = glue("https://api.census.gov/data/{year}/acs/acs1"),
          valid_years = 2010:2018)
}

# identify useful variables
acs1_vars <- listCensusMetadata("acs/acs1", vintage = 2018, type = "variables")
acs1_vars_income <- acs1_vars %>%
  filter(str_detect(concept, "INCOME") & (str_detect(concept, "HOUSEHOLD") | str_detect(concept, "FAMILY")))
income_concepts <- enframe(unique(acs1_vars_income$concept)) %>%
  select(value, name)


####################
# INCOME VARIABLES #
####################
# confirmed to exist for acs1 in 2005
# HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2005 INFLATION-ADJUSTED DOLLARS)
# MEDIAN FAMILY INCOME IN THE PAST 12 MONTHS (IN 2005 INFLATION-ADJUSTED DOLLARS)
# FAMILY INCOME IN THE PAST 12 MONTHS (IN 2005 INFLATION-ADJUSTED DOLLARS)
# MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2005 INFLATION-ADJUSTED DOLLARS)

# HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN {year} INFLATION-ADJUSTED DOLLARS)
# group: B19001

get_census_year <- function(api_name, year, group) {
  census_year <- getCensus("acs/acs1",
                           vintage = year,
                           region = "county:*",
                           regionin = "state:39":
                           group = "B19001")
}
get_census_multiple_years <- function(api_name, years, group) {
  
}
for(year in 2005:2018) {
  print(year)
  if(year == 2005) {
    test <- getCensus("acs/acs1",
                      vintage = year,
                      region = "county:*",
                      regionin = "state:39",
                      vars = c("B19013_001E")) %>%
              mutate(year = year)
  } else {
    test <- getCensus("acs/acs1",
                      vintage = year,
                      region = "county:*",
                      regionin = "state:39",
                      vars = c("B19013_001E")) %>%
            mutate(year = year) %>%
            bind_rows(test)
  }
}