library(censusapi)
library(tidyverse)
library(glue)

#############
# FUNCTIONS #
#############
get_census_year <- function(year, api_name, vars, vars_names) {
  getCensus(name = api_name,
            vintage = year,
            vars = vars,
            region = "county:*",
            regionin = "state:39") %>%
    mutate(year = year) %>%
    select(-state) %>% # always going to be 39
    rename(!!vars_names)
}

get_census_multiple_years <- function(api_name, years, vars, vars_names) {
  map_dfr(years, get_census_year, api_name, vars, vars_names)
}

apis <- listCensusApis()

# identify useful variables
acs1_vars <- listCensusMetadata("acs/acs1", vintage = 2018, type = "variables")



# Variables used in census dataset:
# B19013_001E = median household income in last 12 months in that year's inflation adjusted dollars
census <- get_census_multiple_years("acs/acs1", 2005:2018, c("B19013_001E"), c(acs1_income_median_household = "B19013_001E")) %>%
  # race
  left_join(get_census_multiple_years("acs/acs1", 2005:2018, c("B02001_002E", "B02001_003E"), c(acs1_race_white = "B02001_002E", acs1_race_black = "B02001_003E"))) %>%
  # income pc individual (only includes those with reported income)
  left_join(get_census_multiple_years("acs/acs1", 2005:2018, c("B06011_001E"), c(acs1_income_pc_individual = "B06011_001E")))
