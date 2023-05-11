
rm(list=ls())

library(tidyverse)

for (y in 1995:2016) {

print(paste0(" *** Processing year ", y))

dta <- read_rds(paste0('output/hmda_individual_with_loan_limit_',y,'.rds')) %>%
      mutate( respondent.id = as.numeric(respondent.id) )

dta <- dta %>%
  filter(in_window == 1)
    
crosswalk <- read_csv(paste0('external_data/elena_crosswalk_formatted.csv')) %>%
      rename( respondent.id = respondentid) %>%
      subset( !duplicated(respondent.id) ) %>%
      dplyr::select( -year ) # default year is 1995
    
table(dta$respondent.id %in% crosswalk$respondent.id)
    
hmda_with_bank <- left_join(dta,
                            crosswalk,
                            by = c('agency','respondent.id'))
    
callreports <- read_csv(paste0('external_data/call_reports_panel.csv')) %>%
      subset(year == 2010)
    
table(duplicated(callreports$rssd_id))
table(hmda_with_bank$rssd_id %in% callreports$rssd_id)
    
hmda_with_bank <- left_join(hmda_with_bank,
                            callreports,
                            by = 'rssd_id')
  
print(names(hmda_with_bank))
  
hmda_with_bank <- hmda_with_bank %>%
      mutate(originated           = (action.type == 1), 
             denied = as.numeric(action.type == 3),
             approved = as.numeric(1-denied),
             log_loan_amount      = log(loan.amount),
             diff_log_loan_amount = log_loan_amount - log(effective_loanlimit/1000),
             above_limit          = diff_log_loan_amount > 0) %>%
      filter(abs(diff_log_loan_amount) < 0.20) %>%
      mutate(securitized_alt = ifelse(action.type == 1, purchaser.type %in% 1:5,
                                      NA)) %>%
      dplyr::select(log_loan_amount, diff_log_loan_amount,
             originated, denied, approved,
             denial, above_limit, white, black, hispanic, asian,
             effective_loanlimit, lti, lti_below_1, applicant.income, 
             liquidity_ratio, securitizability, 
             securitized,
             securitized_alt,
             action.type, purchaser.type,
             census.tract, county_fips, county.fips, loan.purpose,
             agency, occupancy, highcost
      ) %>%
      mutate(year = y)
    
write_rds(hmda_with_bank, 
          paste0("output/hmda_in_window_", y, ".rds"))

}

dtaold <- read_rds(paste0("output/hmda_in_window_2016.rds"))

# 2017 has a different format --------------------------------------------------

y <- 2017

print(paste0(" *** Processing year ", y))

dta <- read_rds(paste0('output/hmda_individual_with_loan_limit_',y,'.rds')) %>%
  mutate( respondent.id = as.numeric(`Respondent-ID`),
          purchaser.type = as.numeric(`Type of Purchaser`),
          census.tract = as.numeric(`Census Tract`),
          loan.purpose = as.numeric(`Loan Purpose`),
          occupancy = as.numeric(`Owner Occupancy`)) %>%
  dplyr::select(-`Respondent-ID`, -`Type of Purchaser`,
                -`Census Tract`, -`Loan Purpose`, -`Owner Occupancy`)

dta <- dta %>%
  filter(in_window == 1)

crosswalk <- read_csv(paste0('external_data/elena_crosswalk_formatted.csv')) %>%
  rename( respondent.id = respondentid) %>%
  subset( !duplicated(respondent.id) ) %>%
  dplyr::select( -year ) # default year is 1995

table(dta$respondent.id %in% crosswalk$respondent.id)

hmda_with_bank <- left_join(dta %>%
                              rename(agency = `Agency Code`),
                            crosswalk,
                            by = c('agency','respondent.id'))

callreports <- read_csv(paste0('external_data/call_reports_panel.csv')) %>%
  subset(year == 2010)

table(duplicated(callreports$rssd_id))
table(hmda_with_bank$rssd_id %in% callreports$rssd_id)

hmda_with_bank <- left_join(hmda_with_bank,
                            callreports,
                            by = 'rssd_id')

print(names(hmda_with_bank))

hmda_with_bank <- hmda_with_bank %>%
  mutate(originated           = (action.type == 1), 
         denied = as.numeric(action.type == 3),
         approved = as.numeric(1-denied),
         log_loan_amount      = log(loan.amount),
         diff_log_loan_amount = log_loan_amount - log(effective_loanlimit/1000),
         above_limit          = diff_log_loan_amount > 0) %>%
  filter(abs(diff_log_loan_amount) < 0.20) %>%
  mutate(securitized_alt = ifelse(action.type == 1, purchaser.type %in% 1:5,
                                  NA)) %>%
  dplyr::select(log_loan_amount, diff_log_loan_amount,
                originated, denied, approved,
                denial, above_limit, white, black, hispanic, asian,
                effective_loanlimit, lti, lti_below_1, applicant.income, 
                liquidity_ratio, securitizability, 
                securitized,
                securitized_alt,
                action.type, purchaser.type,
                census.tract, county_fips, county.fips, highcost
  ) %>%
  mutate(year = y)

hmda_with_bank <- as.data.frame(hmda_with_bank)

hmda_with_bank <- hmda_with_bank[, names(hmda_with_bank)[names(hmda_with_bank) %in% names(dtaold)]]

write_rds(hmda_with_bank, paste0("output/hmda_in_window_", y, ".rds"))


