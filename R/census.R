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

census <- get_census_multiple_years("acs/acs1", 2005:2018, c("B19013_001E"), c(acs1_income_median_household = "B19013_001E"))

# B19013_001E = median household income in last 12 months in that year's inflation adjusted dollars