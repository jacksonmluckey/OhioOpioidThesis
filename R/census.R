library(censusapi)
library(tidyverse)
library(glue)

apis <- listCensusApis()

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

get_census_year <- function(api_name, year, vars) {
  census_year <- getCensus("acs/acs1",
                           vintage = year,
                           vars = vars,
                           region = "county:*",
                           regionin = "state:39")
  census_year
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

# B19013_001E = median household income in last 12 months in that year's inflation adjusted dollars