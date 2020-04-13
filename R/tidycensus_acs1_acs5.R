####################
# CENSUS FUNCTIONS #
####################

get_census_table <- function(year, table, survey = "acs1") { # defaults to "acs1" to support legacy code
  df <- get_acs("county",
                year = year,
                table = table,
                survey = survey,
                state = "OH",
                output = "wide",
                cache_table = TRUE) %>%
    rename(county = NAME) %>%
    mutate(county = str_remove(county, " County, Ohio")) %>%
    mutate(year = year,
           survey = survey)
}

# wrapper for get_census_table that handles a year range
get_census_table_multiple_years <- function(years, table, survey = "acs1") { # defaults to "acs1" to support legacy code
  map_df(years, get_census_table, table, survey)
}


###############
# CENSUS DATA #
###############

# concept: grab all data from acs1 and acs5
# then merge, favoring acs5 over acs1
# which I believe I can do using joins
# "B" tables should be available back to 2010 for both acs1 and acs5

# Pulls down data from census if it is not already stored as a .Rda object in data/
if(!file.exists(here("data", "census.Rda"))){
  
  # DISABILITY
  if(!file.exists(here("data", "disability.Rda"))) {
    
    disability <- bind_rows(get_census_table_multiple_years(2010:2018, "B18101", "acs1"),
                            get_census_table_multiple_years(2012:2018, "B18101", "acs5")) %>%
      mutate(disability_male_percent_under5 = B18101_004E / B18101_003E,
             disability_male_percent_5to17 = B18101_007E / B18101_006E,
             disability_male_percent_18to34 = B18101_010E / B18101_009E,
             disability_male_percent_35to64 = B18101_013E / B18101_012E,
             disability_male_percent_65to74 = B18101_016E / B18101_015E,
             disability_male_percent_75andup = B18101_019E / B18101_018E,
             disability_female_percent_under5 = B18101_023E / B18101_022E,
             disability_female_percent_5to17 = B18101_026E / B18101_025E,
             disability_female_percent_18to34 = B18101_029E / B18101_028E,
             disability_female_percent_35to64 = B18101_032E / B18101_031E,
             disability_female_percent_65to74 = B18101_035E / B18101_034E,
             disability_female_percent_75andup = B18101_038E / B18101_037E) %>%
      select(GEOID, survey, county, year, starts_with("disability"))
    
    save(disability, file = here("data", "disability.Rda"))
    
  } else {
    
    load(here("data", "disability.Rda"))
    
  }
  
  # RACE
  if(!file.exists(here("data", "race.Rda"))) {
    
    race <- bind_rows(get_census_table_multiple_years(2010:2018, "B02001", "acs1"),
                      get_census_table_multiple_years(2010:2018, "B02001", "acs5")) %>%
      mutate(percent_white = B02001_002E / B02001_001E,
             percent_black = B02001_003E / B02001_001E) %>%
      select(GEOID, survey, county, year, percent_white, percent_black)
    
    save(race, file = here("data", "race.Rda"))
    
  } else {
    
    load(here("data", "race.Rda"))
    
  }
  
  # MEDIAN INDIVIDUAL INCOME
  if(!file.exists(here("data", "income_individual.Rda"))) {
    
    income_individual <- bind_rows(get_census_table_multiple_years(2010:2018, "B06011", "acs1"),
                                          get_census_table_multiple_years(2010:2018, "B06011", "acs5")) %>%
      mutate(income_individual_median = B06011_001E) %>%
      select(GEOID, survey, county, year, income_individual_median)
    
    save(income_individual, file = here("data", "income_individual.Rda"))
    
  } else {
    
    load(here("data", "income_individual.Rda"))
    
  }
  
  # MEDIAN HOUSEHOLD INCOME
  if(!file.exists(here("data", "income_household.Rda"))) {
    
    income_household <- bind_rows(get_census_table_multiple_years(2010:2018, "B19013", "acs1"),
                                  get_census_table_multiple_years(2010:2018, "B19013", "acs5")) %>%
      mutate(income_household_median = B19013_001E) %>%
      select(GEOID, survey, county, year, income_household_median)
    
    save(income_household, file = here("data", "income_household.Rda"))
    
  } else {
    
    load(here("data", "income_household.Rda"))
    
  }
  
  # EDUCATION
  if(!file.exists(here("data", "education.Rda"))) {
    
    education <- bind_rows(get_census_table_multiple_years(2010:2018, "B15003", "acs1"),
                           get_census_table_multiple_years(2012:2018, "B15003", "acs5")) %>%
      mutate(education_percent_less_than_highschool = ((B15003_002E + B15003_003E + B15003_004E + B15003_005E + B15003_006E + B15003_007E + B15003_008E + B15003_009E + B15003_010E + B15003_011E + B15003_012E + B15003_013E + B15003_014E + B15003_015E + B15003_016E + B15003_017E + B15003_018E) / B15003_001E),
             education_percent_some_college = ((B15003_019E + B15003_020E + B15003_021E) / B15003_001E),
             education_percent_bachelors_or_more = ((B15003_022E + B15003_023E + B15003_024E + B15003_025E) / B15003_001E)) %>%
      select(GEOID, survey, county, year, starts_with("education"))
    
    save(education, file = here("data", "education.Rda"))
    
  } else {
  
    load(here("data", "education.Rda"))  
  }
  
  census <- left_join(disability, race) %>%
    left_join(education) %>%
    left_join(income_individual) %>%
    left_join(income_household)
    mutate(year = as.numeric(year),
           survey = as_factor(survey),
           county = as_factor(county))
  
  # save the census data as an RDA file
  save(census, file  = here("data", "census.Rda"))
  
} else {
  
  load(here("data", "census.Rda"))
  
}



#################
# OVERDOSE DATA #
#################
if(!file.exists(here("data", "overdose.Rda"))) {
  
  # table sourced from https://wonder.cdc.gov/controller/datarequest/D76;jsessionid=C5396F5BBB4351ECF2A36732347C7AFB?stage=results&action=toggle&p=O_show_suppressed&v=true
  # suppressed = 9 or fewer deaths
  # county, year with suppressed rows on
  # all drug-related causes (includes alcohol and the like)
  # fix suppression with https://doi.org/10.2105/AJPH.2014.301900
  # rates per 100,000
  # citation: Centers for Disease Control and Prevention, National Center for Health Statistics. Underlying Cause of Death 1999-2018 on CDC WONDER Online Database, released in 2020. Data are from the Multiple Cause of Death Files, 1999-2018, as compiled from data provided by the 57 vital statistics jurisdictions through the Vital Statistics Cooperative Program. Accessed at http://wonder.cdc.gov/ucd-icd10.html on Apr 5, 2020 10:34:28 AM
  overdose_real <- read_tsv(here("data", "cdc_drug_overdose_deaths.txt")) %>%
    select(-Notes, -`Year Code`) %>% # na in all cases for Notes, Year Code is duplicate of Year
    rename(GEOID = `County Code`,
           year = Year,
           county = County,
           overdose_deaths = Deaths,
           cdc_population = Population,
           fatal_overdose_crude_rate = `Crude Rate`,
           fatal_overdose_age_adjusted_rate = `Age Adjusted Rate`) %>%
    # create dummies for unreliable / suppressed
    mutate(overdose_deaths_suppressed = if_else(overdose_deaths == "Suppressed", TRUE, FALSE),
           fatal_overdose_crude_rate_unreliable = if_else(fatal_overdose_crude_rate == "Unreliable", TRUE, FALSE),
           fatal_overdose_crude_rate_suppressed = if_else(fatal_overdose_crude_rate == "Suppressed", TRUE, FALSE),
           fatal_overdose_age_adjusted_rate_unreliable = if_else(fatal_overdose_age_adjusted_rate == "Unreliable", TRUE, FALSE),
           fatal_overdose_age_adjusted_rate_suppressed = if_else(fatal_overdose_age_adjusted_rate == "Suppressed", TRUE, FALSE)) %>%
    # parse columns with suppressed/unreliable into numbers
    mutate(overdose_deaths = parse_number(overdose_deaths),
           fatal_overdose_crude_rate = parse_number(fatal_overdose_crude_rate),
           fatal_overdose_age_adjusted_rate = parse_number(fatal_overdose_age_adjusted_rate)) %>%
    mutate(county = str_remove(county, " County, OH")) %>%
    # make GEOID a character to make it easy to make it a factor after merging
    mutate(GEOID = as.character(GEOID))
  
  # https://www.cdc.gov/nchs/data-visualization/drug-poisoning-mortality/
  # citation: Rossen LM, Bastian B, Warner M, Khan D, Chong Y. Drug poisoning mortality: United States, 1999–2017. National Center for Health Statistics. 2019.
  overdose_modeled <- read_csv(here("data", "OverdoseDeathRate.csv")) %>%
    filter(State == "Ohio") %>%
    select(GEOID = FIPS,
           county = County,
           year = Year,
           fatal_overdose_modeled_rate = `Model-based Death Rate`,
           cdc_population = Population,
           urban_rural = `Urban/Rural Category`) %>%
    mutate(county = str_remove(county, " County, OH"),
           GEOID = as.character(GEOID),
           year = as.numeric(year),
           urban_rural = as_factor(urban_rural))
  
  overdose <- left_join(overdose_real, overdose_modeled)
  
  save(overdose, file = here("data", "overdose.Rda"))
  
} else {
  
  load(here("data", "overdose.Rda"))
  
}


#################
# PRESCRIPTIONS #
#################
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

if(!file.exists(here("data", "prescriptions.Rda"))) {
  
  prescriptions <- map_dfr(2006:2018, prescriptionYear) %>%
    mutate(county = as_factor(county),
           PrescribingRate = as.numeric(PrescribingRate),
           year = as.numeric(year))
  
  save(prescriptions, file = here("data", "prescriptions.Rda"))
  
} else {
  
  load(here("data", "prescriptions.Rda"))
  
}

###############
# FINALIZE DF #
###############
if(!file.exists(here("data", "raw_df.Rda"))) {
  
  raw_df <- full_join(overdose, prescriptions) %>% # full join b/c a lot of rows with data only used for making plots / not covered in sample
    left_join(census)
  
  save(raw_df, file = here("data", "raw_df.Rda"))

} else {
  
  load(here("data", "raw_df.Rda"))
  
}