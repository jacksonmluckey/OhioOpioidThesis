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

var_labels <- function(table) {
  load_variables(2010, "acs1", cache = TRUE) %>%
    filter(str_detect(name, paste0(table, "_"))) %>%
    rename(variable = name)
}

####################
# grab census data #
####################

disability <- get_census_table_multiple_years("B18101", 2012:2018) %>%
  left_join(var_labels("B18101")) %>%
  make_census_table_wide %>%
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
         disability_percent_75andup_female = estimate_B18101_038 / estimate_B18101_037) %>%
  select(GEOID, county, year, starts_with("disability"))

race <- get_census_table_multiple_years("B02001", 2010:2018) %>%
  left_join(var_labels("B02001")) %>%
  make_census_table_wide() %>%
  mutate(percent_white = estimate_B02001_002 / estimate_B02001_001,
         percent_black = estimate_B02001_003 / estimate_B02001_001) %>%
  select(GEOID, county, year, percent_white, percent_black)

income <- get_census_table_multiple_years("B06011", 2010:2018) %>%
  left_join(var_labels("B06011")) %>%
  make_census_table_wide() %>%
  rename(income_pc_individual = estimate_B06011_001) %>%
  select(GEOID, county, year, income_pc_individual)

# education
education <- get_census_table_multiple_years("C15003", 2010:2018) %>%
  left_join(var_labels("C15003")) %>%
  make_census_table_wide() %>%
  rename(population = estimate_C15003_001) %>%
  # create the small education bins (_s_), which should add up to one
  mutate(education_s_percent_less_than_highschool = (estimate_C15003_002 + estimate_C15003_003 + estimate_C15003_004 + estimate_C15003_005 + estimate_C15003_006 + estimate_C15003_007 + estimate_C15003_008 + estimate_C15003_009) / population,
         education_s_percent_highschool = estimate_C15003_010 / population,
         education_s_percent_ged = estimate_C15003_011 / population,
         education_s_percent_some_college = (estimate_C15003_012 + estimate_C15003_013 + estimate_C15003_014) / population,
         education_s_percent_college = estimate_C15003_015 / population,
         education_s_percent_graduate = (estimate_C15003_016 + estimate_C15003_017 + estimate_C15003_018) / population) %>%
  # create the big education bins (_b_), which won't add up to one because some college is excluded
  mutate(education_b_percent_highschool_or_less = education_s_percent_less_than_highschool + education_s_percent_highschool + education_s_percent_ged,
         education_b_percent_college_or_more = education_s_percent_college + education_s_percent_graduate) %>%
  select(GEOID, county, year, population, starts_with("education"))

df <- left_join(income, race) %>%
  left_join(disability) %>%
  left_join(education)

#############
# finish up #
#############
# death data
# 2018 values will always be 0
# might be able to get better data directly from CDC?
# GET DEATHS INTO RATE
# BRING IN DESCRIPTIVE/SUMMARY statistics
deaths <- read_csv("Data/OverdoseDeathsTall.csv") %>%
  rename(county = County)

# prepare final df
df <- left_join(disability, race) %>%
  left_join(deaths) %>%
  left_join(education) %>%
  left_join(income_individual)

# convert OD raw to OD rate
# change OD rate to be deaths per 10,000 pop
df <- df %>%
  mutate(OD_rate = (deaths / population) * 10000)

# convert percentages to be XX.X instead of .XXX
df <- df %>%
  mutate_at(vars(contains("percent")), ~ .x * 100)

# add CDC overdose death rate
# downloaded from https://www.cdc.gov/nchs/data-visualization/drug-poisoning-mortality/#data-tables
OverdoseDeathRateCDC <- read_csv(here("data", "OverdoseDeathRate.csv")) %>%
  filter(State == "Ohio") %>%
  select(GEOID = FIPS,
         county = County,
         year = Year,
         DeathRateCDC = `Model-based Death Rate`,
         PopulationCDC = Population,
         UrbanRural = `Urban/Rural Category`) %>%
  mutate(county = str_remove(county, " County, OH")) %>%
  mutate(GEOID = as.character(GEOID)) # to enable merging with DF


df <- left_join(df, OverdoseDeathRateCDC, by = c("GEOID" = "GEOID",
                                                 "year" = "year",
                                                 "county" = "county"))
# save the final df as RDA file
save(df, file  = "data/df.Rda")