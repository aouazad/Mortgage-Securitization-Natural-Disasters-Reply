
#                            _                            _                   _   _             
#                           | |                          | |                 | | (_)            
#  ___  __ _ _ __ ___  _ __ | | ___    ___ ___  _ __  ___| |_ _ __ _   _  ___| |_ _  ___  _ __  
# / __|/ _` | '_ ` _ \| '_ \| |/ _ \  / __/ _ \| '_ \/ __| __| '__| | | |/ __| __| |/ _ \| '_ \ 
# \__ \ (_| | | | | | | |_) | |  __/ | (_| (_) | | | \__ \ |_| |  | |_| | (__| |_| | (_) | | | |
# |___/\__,_|_| |_| |_| .__/|_|\___|  \___\___/|_| |_|___/\__|_|   \__,_|\___|\__|_|\___/|_| |_|
#                     | |                                                                       
#                     |_|                                                                       

rm(list=ls())

source("project_constants.R")
outputdir <- "output"

library(tidyverse)
library(tidylog)
library(foreign)
library(gplots)

figuredir <- "figures/"

#### Crosswalks
#       ZIP    --> county  
#       county --> MSA 
#       county --> CBSA 

countycbsa <- read_csv(paste0("external_data/county_to_cbsa_nber_crosswalk_2015.csv")) %>%
  mutate(county.fips = as.numeric(paste0(fipsstatecode, 
                                         fipscountycode))) %>%
  dplyr::select(cbsacode, county.fips, cbsatitle)

zipcbsa <- read_rds(paste0("external_data/crosswalk_zip_to_county.rds")) %>% 
  left_join(countycbsa,
            by = "county.fips") %>%
  dplyr::select(-county.fips) 

est_sample <- read_rds(paste0(outputdir,'/hmda_event_study_zip_level.Rds')) %>%
              mutate(above_limit = as.numeric(diff_log_loan_amount >  0),
                     below_limit = as.numeric(diff_log_loan_amount <= 0),
                     treated     = as.numeric(treated),
                     log_income  = log(applicant.income),
                     approved    = action.type %in% c(1,2),
                     originated  = action.type == 1, # approved AND accepted
                     name_event  = ifelse(name_event == "", "control", name_event),
                     securitized = (action.type %in% c(1,6)) & (purchaser.type %in% 1:4),
                     pls_securitized = (action.type %in% c(1,6) & (purchaser.type %in% 5:9)))  %>% 
              
              ############# KEY FILTERS
              filter( # loan.type == 1 has already been filtered earlier in the extraction of HMDA data
                      # property.type == 1 same comment
                     loan.purpose == 1, # purchase
                     occupancy == 1, # owner occupied
                     action.type %in% c(1:3, 6)) %>% # this will be narrowed down depending on the dependent
  
              ############# ADDING THE % OF ZIP IN SFHA
              left_join(read_rds(paste0("external_data/frac_SFHA_2017_by_zip.rds")),
                        by = "ZCTA5CE10") %>%
              
              ############# ADDING THE LONG RUN HISTORY OF HURRICANES
              left_join(read_rds(paste0("external_data/history_zip.Rds")),
                        by = "ZCTA5CE10") %>%
              ungroup() %>%
              mutate(proba_total_demedianed = proba_total - mean(proba_total, na.rm=TRUE)) %>%
  
              ############# ADDING INDICATOR VARIABLES FOR EACH BANKING AGENCY
              mutate(agency_1 = as.numeric(agency == 1),
                     agency_2 = as.numeric(agency == 2),
                     agency_3 = as.numeric(agency == 3),
                     agency_4 = as.numeric(agency == 4),
                     agency_5 = as.numeric(agency == 5),
                     agency_7 = as.numeric(agency == 7),
                     agency_8 = as.numeric(agency == 8),
                     agency_9 = as.numeric(agency == 9)) %>%
  
              ############ ADDING ZILLOW HOUSE PRICES AND RENTS
              left_join(zipcbsa,
                        by = "ZCTA5CE10") %>%
              left_join(read_rds("external_data/zillow_data_long_by_zip_1996_2019.rds") %>%
                          group_by(year, ZCTA5CE10) %>%
                          summarise(mean_price = mean(price, na.rm=TRUE),
                                    mean_rent  = mean(rent, na.rm=TRUE)),
                        by = c("year", "ZCTA5CE10")) %>%
  
              ############ ADDING SAIZ HOUSING SUPPLY ELASTICITY
              left_join(read_rds(paste0("external_data/",
                                        "crosswalk_county_to_msa.rds")) %>%
                          rename(county.fips = countycode) %>%
                          mutate(county.fips = as.numeric(as.character(county.fips))),
                        by = c("county.fips")) %>%
              left_join(read.dta("external_data/HOUSING_SUPPLY.dta") %>%
                          dplyr::select(msanecma, elasticity) %>%
                          rename(msacode = msanecma),
                        by = "msacode") %>%
              mutate(elasticity_demeaned = elasticity - mean(elasticity, na.rm=TRUE))

# creating year dummies
for (y in range(est_sample$year)[1]:range(est_sample$year)[2])
  est_sample[,paste0("year_", y)] <- as.numeric(est_sample$year == y)

# creating time dummies
for (timek in setdiff(unique(est_sample$time), -1)) {
  print(timek)
  est_sample[,sprintf("time_%s%d",
                       ifelse(timek<0, "m",""),abs(timek))] = 
    as.numeric(est_sample$time == timek)
}

# checking whether the number of treated and control observations is well balanced in each time period 
for(timek in unique(est_sample$time)) {
  print(timek)
  print(table(est_sample$treated[est_sample$time == timek], 
              est_sample$above_limit[est_sample$time == timek]))
}

# time ranges for each event
for (evn in unique(est_sample$name_event)) {
  
  print(paste0(" *** for event ", evn, " the time range is "))
  print(range(est_sample$time[est_sample$name_event == evn]))
  
}

est_sample <- est_sample %>%
              filter(time >= -10 & time <= +4)

## get the price in -1 for each natural disaster and ZIP of the treatment group

initial_prices <- est_sample %>%
  filter(time == - 1) %>%
  dplyr::select(ZCTA5CE10, name_event, year_event, mean_price, mean_rent) %>%
  distinct(ZCTA5CE10, name_event, year_event, .keep_all=TRUE) %>%
  rename(mean_price_m1 = mean_price,
         mean_rent_m1  = mean_rent)

## we don't have prices and/or elasticities for all ZIPs, code a dummy for that
est_sample <- est_sample %>% 
  left_join(initial_prices,
            by = c("ZCTA5CE10", "name_event", "year_event")) %>%
  mutate(log_price_change = log(mean_price) - log(mean_price_m1),
         log_rent_change  = log(mean_rent)  - log(mean_rent_m1),
         log_price_to_rent_change = log_rent_change - log_price_change) %>%
  mutate(is_na_log_price_change = is.na(log_price_change)) %>%
  mutate(log_price_change_with_na = ifelse(is_na_log_price_change, 0, log_price_change)) %>%
  mutate(is_na_elasticity = is.na(elasticity),
         elasticity_with_na = ifelse(is_na_elasticity, 0, elasticity_demeaned)) 

depvar_array = c("approved",
                 "originated",
                 "securitized")

write_rds(est_sample, "output/est_sample_for_revision.rds")



