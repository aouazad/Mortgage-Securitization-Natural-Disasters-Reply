
#########################################
##
## Takes the formatted HMDA file (full) 
##   and adds the conforming loan limit (time-varying and county-specific)
##
#########################################

rm(list=ls())
library(tidyverse)
library(data.table)

limitsdf <- read_rds("output/conforming_loan_limits_1980_2017.rds")

outputfolder <- "output/" # you can change this if you have more space on another drive for all HMDA files

for (selected_year in 1995:2017) {

  hmda <-   fread(paste0(outputfolder, 'output_hmda/hmda_individual_', 
                          selected_year, '.csv'))

  hmda <- hmda %>%
          mutate(county_fips = sprintf("%05.0f", county.fips)) %>%
          left_join(limitsdf %>% 
                      filter(year == selected_year) %>%
                      dplyr::select(county_fips, conforming_loan_limit, highcost) %>%
                      rename(effective_loanlimit = conforming_loan_limit),
                    by = "county_fips") %>%
          # note the discussion of Section 3.2 of the paper
          mutate(in_window = 
                     (loan.amount > 0.8 * (effective_loanlimit/1000)) &
                     (loan.amount < 1.2 * (effective_loanlimit/1000)),
                 jumbo = loan.amount > (effective_loanlimit/1000)) 

  write_rds(hmda, 
          paste0('output/hmda_individual_with_loan_limit_',
                 selected_year, '.rds'))

}


