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

########################
# NEW CODE BEGINS HERE #
########################

# disability

# race

# education

# income

# pair with non-census data

# save