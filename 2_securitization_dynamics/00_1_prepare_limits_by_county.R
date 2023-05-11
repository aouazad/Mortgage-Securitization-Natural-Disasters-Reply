
rm(list=ls())
library(tidyverse)

# IMPORTANT NOTES
# !!!! Check that this 2009 file is consistent with later limits
# !!!! One Unit Limit: does it apply to multi units? 
# !!!! Should we check that some areas are not single-family?
# !!!! 2008 has a special status: address this, needs digitization of the pdf
# !!!! check this:
#> warnings()
#Warning messages:
#  1: In read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet,  ... :
#                   Expecting logical in L16170 / R16170C12: got 'AL'
#                 2: In read_fun(path = enc2native(normalizePath(path)), sheet_i = sheet,  ... :
#                                  Expecting logical in M16170 / R16170C13: got '33860'



# limits post 2008 -------------------------------------------------------------
fn <- c(
  "2017" = "limits/FullCountyLoanLimitList2017_HERA-BASED_FINAL_FLAT.xlsx",
  "2016" = "limits/FullCountyLoanLimitList2016_HERA-BASED_FINAL_FLAT.xlsx",
  "2015" = "limits/FullCountyLoanLimitList2015_HERA-BASED_FINAL_FLAT.xlsx",
  "2014" = "limits/FullCountyLoanLimitList2014_HERA-BASED_FINAL_FLAT.xls",
  "2013" = "limits/FullCountyLoanLimitList2013_HERA-BASED_FINAL.xls",
  "2012" = "limits/FullCountyLoanLimitList2012_HERA-BASED_FINAL_Z.xls",
  "2011" = "limits/FullCountyLoanLimitList2011_HERA-BASED_FINAL_Z.xls",
  "2010" = "limits/FullCountyLoanLimitList2010_PL111-88_FINAL.xls",
  "2009" = "limits/FullCountyLoanLimitList2009_ARRA.xls"
)

# !!!! Check that this 2009 file is consistent with later limits
# !!!! One Unit Limit: does it apply to multi units? 
# !!!! Should we check that some areas are not single-family?

df2009 <- readxl::read_xls(fn["2009"]) %>%
  filter(Year == 2009) %>%
  mutate(county_fips = paste0(`FIPS State Code`, `FIPS County Code`)) %>% 
  dplyr::select(county_fips, `One-Unit Limit`) %>%
  rename(conforming_loan_limit = `One-Unit Limit`) %>%
  mutate(year = 2009)

df2010 <- readxl::read_xls(fn["2010"], skip = 1) %>%
  mutate(county_fips = paste0(`FIPS State Code`, `FIPS County Code`)) %>% 
  dplyr::select(county_fips, `One-Unit Limit`) %>%
  rename(conforming_loan_limit = `One-Unit Limit`) %>%
  mutate(year = 2010)

df2011 <- readxl::read_xls(fn["2011"], skip = 1) %>%
  mutate(county_fips = paste0(`FIPS State Code`, `FIPS County Code`)) %>% 
  dplyr::select(county_fips, `One-Unit Limit`) %>%
  rename(conforming_loan_limit = `One-Unit Limit`) %>%
  mutate(year = 2011)

df2012 <- readxl::read_xls(fn["2012"], skip = 1) %>%
  mutate(county_fips = paste0(`FIPS State Code`, `FIPS County Code`)) %>% 
  dplyr::select(county_fips, `One-Unit Limit`) %>%
  rename(conforming_loan_limit = `One-Unit Limit`) %>%
  mutate(year = 2012)

df2013 <- readxl::read_xls(fn["2013"], skip = 1) %>%
  mutate(county_fips = paste0(`FIPS State Code`, `FIPS County Code`)) %>% 
  dplyr::select(county_fips, `One-Unit Limit`) %>%
  rename(conforming_loan_limit = `One-Unit Limit`) %>%
  mutate(year = 2013)

df2014 <- readxl::read_xls(fn["2014"], skip = 1) %>%
  mutate(county_fips = paste0(`FIPS State Code`, `FIPS County Code`)) %>% 
  dplyr::select(county_fips, `One-Unit Limit`) %>%
  rename(conforming_loan_limit = `One-Unit Limit`) %>%
  mutate(year = 2014)

df2015 <- readxl::read_xlsx(fn["2015"], skip = 1) %>%
  mutate(county_fips = paste0(`FIPS State Code`, `FIPS County Code`)) %>% 
  dplyr::select(county_fips, `One-Unit Limit`) %>%
  rename(conforming_loan_limit = `One-Unit Limit`) %>%
  mutate(year = 2015)

df2016 <- readxl::read_xlsx(fn["2016"], skip = 1) %>%
  mutate(county_fips = paste0(`FIPS State Code`, `FIPS County Code`)) %>% 
  dplyr::select(county_fips, `One-Unit Limit`) %>%
  rename(conforming_loan_limit = `One-Unit Limit`) %>%
  mutate(year = 2016)

df2017 <- readxl::read_xlsx(fn["2017"], skip = 1) %>%
  mutate(county_fips = paste0(`FIPS State Code`, `FIPS County Code`)) %>% 
  dplyr::select(county_fips, `One-Unit Limit`) %>%
  rename(conforming_loan_limit = `One-Unit Limit`) %>%
  mutate(year = 2017)

df <- rbind(
  df2009, df2010, df2011, df2012, df2013, df2014, df2015, df2016, df2017
)

# limits 2008 ------------------------------------------------------------------
# ! these are new loan limits. the old limit of 417000 applies unless specified

df2008_extra <- read_csv("limits/AREA_LIST_5_2008.csv") %>%
  mutate(fips = sprintf("%05.0f", fips))

usual_2008_limit <- 417000

all_counties <- unique(df$county_fips)

table(df2008_extra$fips %in% all_counties)

df2008 <- data.frame(
  county_fips = all_counties,
  conforming_loan_limit = usual_2008_limit
) %>%
  left_join(df2008_extra %>%
              select(fips, one) %>%
              rename(county_fips = fips,
                     high_cost_limit = one),
            by = "county_fips") %>%
  mutate(conforming_loan_limit = ifelse(!is.na(high_cost_limit),
                                        high_cost_limit,
                                        conforming_loan_limit)) %>%
  select(-high_cost_limit) %>%
  mutate(year = 2008)

# limits pre 2008 - they do not provide these limits ---------------------------

conflimits_pre2008 <- readxl::read_xlsx(path = 
                                          paste0('limits/conforming_loan_limits.xlsx'),
                                        skip = 2)[, c(1, 2, 7)]

names(conflimits_pre2008) <- c("year", "conforming_loan_limit", 
                               "high_cost_limit")

conflimits_pre2008 <- conflimits_pre2008 %>%
  filter(year <= 2007)

all_counties <- unique(df$county_fips)

conflimits_pre2008 <- lapply(1995:2007, function(selected_year) {
data.frame(
  county_fips = all_counties,
  conforming_loan_limit = rep(conflimits_pre2008 %>% 
                                filter(year == selected_year) %>%
                                pull(conforming_loan_limit),
                              length(all_counties)),
  year = selected_year
)}) %>% { do.call(rbind, .) } 

# !!! there is a change in 2008 itself


# merge pre and post 2008 ------------------------------------------------------

highcost <- read_csv('limits/high_cost_areas_gse_formatted.csv')

df <- rbind(
  df,
  df2008,
  conflimits_pre2008
) %>%
  left_join(highcost %>%
              mutate(county_fips = sprintf("%05.0f", county_fips)),
            by = "county_fips") %>%
  mutate(highcost = ifelse(is.na(highcost), 0, 1))

# Save -------------------------------------------------------------------------

write_rds(df, "output/conforming_loan_limits_1980_2017.rds")





