# load the necessary packages
library(tidyverse)
library(tidycensus)
library(stringr)

####################
# census functions #
####################
make_census_table_wide <- function(df) {
  
  df <- df %>%
    select(-label, -concept) %>%
    pivot_longer(cols = c(estimate, moe)) %>%
    pivot_wider(names_from = c(name, variable), values_from = value)
  
}

# removes " County, Ohio" from county column (because it's the same in all cases)
clean_county_col<- function(df) {
  
  df <- df %>%
    mutate(county = str_remove(county, " County, Ohio"))
  
}

clean_tidycensus_table <- function(df) {
  
  # rename the "NAME" column to county
  df <- df %>%
    rename(county = NAME)
  
  df <- clean_county_col(df)
  
}

get_census_table <- function(table, year) {
  df <- get_acs("county",
                year = year,
                table = table,
                survey = "acs1", # if things break for older code make a version of this function with this line removed
                state = "OH")
  df <- clean_tidycensus_table(df)
}

# wrapper for get_census_table that handles a year range
get_census_table_multiple_years <- function(table, years) {
  count <- 0
  for (year in years) {
    tmp <- get_census_table(table, year)
    tmp$year <- year
    if (count == 0) {
      df <- tmp
      count <- 1
    } else {
      df <- bind_rows(tmp, df)
    }
  }
  df
}


####################
# grab census data #
####################
# household_income
# B19037 is household income by age of householder
# not sure if this is the right table to work with
household_income_age_var_labels <- load_variables(2010, "acs1", cache = TRUE) %>%
  filter(str_detect(name, "B19037_")) %>%
  rename(variable = name)
household_income_age_tall <- get_census_table("B19037", 2018) %>%
  left_join(household_income_age_var_labels)
household_income_age_wide <- make_census_table_wide(household_income_age_tall)


# disability
# load the variable labels so I can actually figure out what the columns mean
disability_var_labels <- load_variables(2010, "acs1", cache = TRUE) %>%
  filter(stringr::str_detect(name, "B18101_")) %>%
  rename(variable = name)
# create a tall version that has the labels for doing some sanity checking
disability_tall <- get_census_table_multiple_years("B18101", 2012:2018)
disability_tall <- disability_tall %>%
  left_join(disability_var_labels)
# create a wide version
disability_wide <- make_census_table_wide(disability_tall)
# create new variables
# table/category_n/percent_agebin_sex
disability_wide <- disability_wide %>%
  mutate(disability_percent_under5_male = estimate_B18101_004 / estimate_B18101_003,
         disability_percent_5to17_male = estimate_B18101_007 / estimate_B18101_006,
         disability_percent_18to34_male = estimate_B18101_010 / estimate_B18101_009,
         disability_percent_35to64_male = estimate_B18101_013 / estimate_B18101_012,
         disability_percent_65to74_male = estimate_B18101_016 / estimate_B18101_015,
         disability_percent_75andup_male = estimate_B18101_019 / estimate_B18101_018,
         disability_percent_under5_female = estimate_B18101_023 / estimate_B18101_022,
         disability_percent_5to17_female = estimate_B18101_026 / estimate_B18101_025,
         disability_percent_18to34_female = estimate_B18101_029 / estimate_B18101_028,
         disability_percent_35to64_female = estimate_B18101_032 / estimate_B18101_031,
         disability_percent_65to74_female = estimate_B18101_035 / estimate_B18101_034,
         disability_percent_75andup_female = estimate_B18101_038 / estimate_B18101_037)
# drop the original columns
disability <- disability_wide %>%
  select(GEOID, county, year, starts_with("disability"))

# race
# load all of the data
race_tall <- get_census_table_multiple_years("B02001", 2010:2018)

# load the variable labels so I can actually figure out what the columns mean
race_var_labels <- load_variables(2010, "acs1", cache = TRUE) %>%
  filter(stringr::str_detect(name, "B02001_")) %>%
  rename(variable = name)

# make race wide
race_wide <- race_tall %>%
  left_join(race_var_labels) %>%
  make_census_table_wide()

# reduce down to only percent white and percent black
# because the census data that can differentiate latino and white is complicated
race <- race_wide %>%
  mutate(percent_white = estimate_B02001_002 / estimate_B02001_001,
         percent_black = estimate_B02001_003 / estimate_B02001_001) %>%
  select(GEOID, county, year, percent_white, percent_black)

# income
income_var_labels <- load_variables(2010, "acs1", cache = TRUE) %>%
  filter(stringr::str_detect(name, "B06011_")) %>%
  rename(variable = name)
income_tall <- get_census_table_multiple_years("B06011", 2010:2018)
income_wide <- income_tall %>%
  left_join(income_var_labels) %>%
  make_census_table_wide()
# income_individual is median income pc for individuals with reported income
income_individual <- income_wide %>%
  rename(income_pc_individual = estimate_B06011_001) %>%
  select(GEOID, county, year, income_pc_individual)
rm(income_var_labels)
rm(income_tall)
rm(income_wide)

# education
education_var_labels <- load_variables(2010, "acs1", cache = TRUE) %>%
  filter(stringr::str_detect(name, "C15003_")) %>%
  rename(variable = name)
education_tall <- get_census_table_multiple_years("C15003", 2010:2018)
education_wide <- education_tall %>%
  left_join(education_var_labels) %>%
  make_census_table_wide()
education <- education_wide %>%
  mutate(education_percent_less_than_highschool = (estimate_C15003_002 + estimate_C15003_003 + estimate_C15003_004 + estimate_C15003_005 + estimate_C15003_006 + estimate_C15003_007 + estimate_C15003_008 + estimate_C15003_009) / estimate_C15003_001,
         education_percent_highschool = estimate_C15003_010 / estimate_C15003_001,
         education_percent_ged = estimate_C15003_011 / estimate_C15003_001,
         education_percent_some_college = (estimate_C15003_012 + estimate_C15003_013 + estimate_C15003_014) / estimate_C15003_001,
         education_percent_college = estimate_C15003_015 / estimate_C15003_001,
         education_percent_graduate = (estimate_C15003_016 + estimate_C15003_017 + estimate_C15003_018) / estimate_C15003_001) %>%
  select(GEOID, county, year, starts_with("education"))


#############
# finish up #
#############
# death data
# 2018 values will always be 0
# might be able to get better data directly from CDC?
# GET DEATHS INTO RATE
# BRING IN DESCRIPTIVE/SUMMARY statistics
deaths <- read_csv(here("Data", "CSV", "OverdoseDeathsTall.csv")) %>%
  rename(county = County)

# prepare final df
df <- left_join(disability, race) %>%
  left_join(deaths) %>%
  left_join(education) %>%
  left_join(income_individual)