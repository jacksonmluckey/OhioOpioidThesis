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


########
# ACS1 #
########
# B19013_001E = median household income in last 12 months in that year's inflation adjusted dollars
acs1_income_household <- get_census_multiple_years("acs/acs1", 2005:2018, c("B19013_001E"), c(acs1_income_median_household = "B19013_001E"))

acs1_race <- get_census_multiple_years("acs/acs1", 2005:2018, c("B02001_002E", "B02001_003E"), c(acs1_race_white = "B02001_002E", acs1_race_black = "B02001_003E"))

acs1_income_individual <- get_census_multiple_years("acs/acs1", 2005:2018, c("B06011_001E"), c(acs1_income_pc_individual = "B06011_001E"))
    
acs1_education <- get_census_multiple_years("acs/acs1", 2008:2018, c("C15003_001E", # list of variables to grab
                                                                     "C15003_002E",
                                                                     "C15003_003E",
                                                                     "C15003_004E",
                                                                     "C15003_005E",
                                                                     "C15003_006E",
                                                                     "C15003_007E",
                                                                     "C15003_008E",
                                                                     "C15003_009E",
                                                                     "C15003_010E",
                                                                     "C15003_011E",
                                                                     "C15003_012E",
                                                                     "C15003_013E",
                                                                     "C15003_014E",
                                                                     "C15003_015E",
                                                                     "C15003_016E",
                                                                     "C15003_017E",
                                                                     "C15003_018E"),
                              c(acs1_education_total = "C15003_001E", # rename variables
                                acs1_education_no_schooling = "C15003_002E",
                                acs1_education_nursery_to_4th = "C15003_003E",
                                acs1_education_5th_6th = "C15003_004E",
                                acs1_education_7th_8th = "C15003_005E",
                                acs1_education_9th = "C15003_006E",
                                acs1_education_10th = "C15003_007E",
                                acs1_education_11th = "C15003_008E",
                                acs1_education_12th_no_diploma = "C15003_009E",
                                acs1_education_12th_with_diploma = "C15003_010E",
                                acs1_education_ged = "C15003_011E",
                                acs1_education_college_less_than_1_year = "C15003_012E",
                                acs1_education_college_1_year_or_more = "C15003_013E",
                                acs1_education_associates = "C15003_014E",
                                acs1_education_bachelors = "C15003_015E",
                                acs1_education_masters = "C15003_016E",
                                acs1_education_professional = "C15003_017E",
                                acs1_education_doctorate = "C15003_018E"))