# table sourced from https://wonder.cdc.gov/controller/datarequest/D76;jsessionid=C5396F5BBB4351ECF2A36732347C7AFB?stage=results&action=toggle&p=O_show_suppressed&v=true
# suppressed = 9 or fewer deaths
# county, year with suppressed rows on
# all drug-related causes (includes alcohol and the like)
# fix suppression with https://doi.org/10.2105/AJPH.2014.301900
# rates per 100,000
# citation: Centers for Disease Control and Prevention, National Center for Health Statistics. Underlying Cause of Death 1999-2018 on CDC WONDER Online Database, released in 2020. Data are from the Multiple Cause of Death Files, 1999-2018, as compiled from data provided by the 57 vital statistics jurisdictions through the Vital Statistics Cooperative Program. Accessed at http://wonder.cdc.gov/ucd-icd10.html on Apr 5, 2020 10:34:28 AM
library(tidyverse)
overdose <- read_tsv("data/cdc_drug_overdose_deaths.txt") %>%
  select(-Notes) # na in all cases