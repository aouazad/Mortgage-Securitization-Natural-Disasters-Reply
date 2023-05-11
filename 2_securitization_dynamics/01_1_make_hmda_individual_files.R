
# make an aggregated HMDA file at the county level

rm(list = ls())

library(tidyverse)

# these are the raw files, unfiltered, from the National Archives,
#  the FFIEC and the CFPB
source_folder <- 
  "Fill in here the name of the folder with HMDA files"

outputfolder <- "output/"

# individual 1990 to 2003

for (year in 1990:2003) {
  
  hmda <- read_rds(paste0(source_folder, "/hmda_", year, ".Rds")) %>%
    as.data.frame() %>%
    filter(action.type %in% 1:8,
           loan.type == 1 & occupancy ==1)
  
  hmda$securitized <- hmda$action.type == 6 & !is.na(hmda$action.type)
  hmda$application <- hmda$action.type %in% 1:5
  hmda$denial <- hmda$action.type == 3
  hmda$denial[!hmda$application] <- NA
  
  hmda$white    <- hmda$applicant.race == 5 
  hmda$black    <- hmda$applicant.race == 3
  hmda$hispanic <- hmda$applicant.race == 4
  hmda$asian    <- hmda$applicant.race == 2
  hmda$race_missing <- !(hmda$applicant.race %in% 1:5) | 
    is.na(hmda$applicant.race) |
    !hmda$application
  for (s in c('white','black','hispanic','asian'))
    hmda[hmda$race_missing,s] <- NA
  
  hmda$loan.amount <- as.numeric(hmda$loan.amount)       
  hmda$loan.amount[hmda$loan.amount > 5000 |
                     hmda$loan.amount <= 0 ] <- NA
  
  hmda$applicant.income <- as.numeric(hmda$applicant.income)
  hmda$applicant.income[hmda$applicant.income > 5000 |
                          hmda$applicant.income <= 0 ] <- NA
  
  hmda$lti <- with(hmda, loan.amount / applicant.income)
  hmda$lti[hmda$lti > 4.5] <- NA
  hmda$lti_below_1 <- hmda$lti < 1
  hmda$lti[hmda$lti_below_1] <- NA
  
  hmda <- filter(hmda, county.fips > 1000)
  hmda$year <- year
  
  write_csv(hmda, paste0(outputfolder,'output_hmda/hmda_individual_', year, '.csv'))
  
}

# individual data 2004 to 2011

for (year in 2004:2011) {
  
  hmda <- read_rds(paste0(source_folder, "/hmda_", year, ".Rds"))   %>%
    as.data.frame() %>%
    filter(action.type %in% 1:8,
           loan.type == 1 & property.type ==1)
  
  hmda$securitized <- hmda$action.type == 6 & !is.na(hmda$action.type)
  hmda$application <- hmda$action.type %in% 1:5
  hmda$denial <- hmda$action.type == 3
  hmda$denial[!hmda$application] <- NA
  
  hmda$white    <- (hmda$applicant.race.1 == 5)  & (hmda$applicant.ethnicity != 1)
  hmda$black    <- hmda$applicant.race.1 == 3  & hmda$applicant.ethnicity != 1
  hmda$hispanic <- hmda$applicant.ethnicity == 1
  hmda$asian    <- hmda$applicant.race.1 == 2  & hmda$applicant.ethnicity != 1
  hmda$race_missing <- !(hmda$applicant.race.1 %in% 1:5) | 
    is.na(hmda$applicant.race.1) |
    !hmda$application
  for (s in c('white','black','hispanic','asian'))
    hmda[hmda$race_missing,s] <- NA
  
  hmda$loan.amount <- as.numeric(hmda$loan.amount)       
  hmda$loan.amount[hmda$loan.amount > 5000 |
                     hmda$loan.amount <= 0 ] <- NA
  
  hmda$applicant.income <- as.numeric(hmda$applicant.income)
  hmda$applicant.income[hmda$applicant.income > 5000 |
                          hmda$applicant.income <= 0 ] <- NA
  
  hmda$lti <- with(hmda, loan.amount / applicant.income)
  hmda$lti[hmda$lti > 4.5] <- NA
  hmda$lti_below_1 <- hmda$lti < 1
  hmda$lti[hmda$lti_below_1] <- NA
  
  hmda <- filter(hmda, county.fips > 1000)
  hmda$year <- year
  
  write_csv(hmda, paste0(outputfolder,'output_hmda/hmda_individual_', year, '.csv'))
  
}

# individual data 2012 to 2016


for (year in 2012:2016) {
  
  hmda <- read_rds(paste0(source_folder, "/hmda_", year, ".Rds")) %>%
    as.data.frame() %>%
    filter(action.type %in% 1:8,
           loan.type == 1 & property.type ==1)
  
  hmda$securitized <- hmda$action.type == 6 & !is.na(hmda$action.type)
  hmda$application <- hmda$action.type %in% 1:5
  hmda$denial      <- hmda$action.type == 3
  hmda$denial[!hmda$application] <- NA
  
  hmda$white    <- (hmda$applicant.race.1 == 5)  & (hmda$applicant.ethnicity != 1)
  hmda$black    <- hmda$applicant.race.1 == 3  & hmda$applicant.ethnicity != 1
  hmda$hispanic <- hmda$applicant.ethnicity == 1
  hmda$asian    <- hmda$applicant.race.1 == 2  & hmda$applicant.ethnicity != 1
  hmda$race_missing <- !(hmda$applicant.race.1 %in% 1:5) | 
    is.na(hmda$applicant.race.1) |
    !hmda$application
  
  for (s in c('white','black','hispanic','asian'))
    hmda[hmda$race_missing,s] <- NA
  
  hmda$loan.amount <- as.numeric(hmda$loan.amount)       
  hmda$loan.amount[hmda$loan.amount > 5000 |
                     hmda$loan.amount <= 0 ] <- NA
  
  hmda$applicant.income <- as.numeric(hmda$applicant.income)
  hmda$applicant.income[hmda$applicant.income > 5000 |
                          hmda$applicant.income <= 0 ] <- NA
  
  hmda$lti <- with(hmda, loan.amount / applicant.income)
  hmda$lti[hmda$lti > 4.5] <- NA
  hmda$lti_below_1 <- hmda$lti < 1
  hmda$lti[hmda$lti_below_1] <- NA
  
  hmda <- filter(hmda, county.fips > 1000)
  hmda$year <- year
  
  write_csv(hmda, paste0(outputfolder,'output_hmda/hmda_individual_', year, '.csv'))
  
}


# 2017

hmdaspec <- read_csv(paste0("/media/amine/GRaid/01_Master_Data/",
                     "Mortgage Market/HMDA/2017/2017_Dynamic_LAR_Spec.csv"),
                     skip = 1)

hmda <- read_delim(paste0("/media/amine/GRaid/01_Master_Data/",
                          "Mortgage Market/HMDA/2017/2017_lar.txt"),
                   delim = "|", col_names = hmdaspec$`Data Field Name`) %>%
  rename(action.type = `Type of Action Taken`,
         loan.type   = `Loan Type`,
         property.type = `Property Type`,
         applicant.race.1 = `Applicant Race: 1`,
         applicant.ethnicity = `Applicant Ethnicity`,
         applicant.income = `Applicant Income`,
         loan.amount = `Loan Amount`) %>%
  mutate(county.fips = as.numeric(paste0(`State Code`, `County Code`)))

hmda <- hmda %>%
  filter(action.type %in% 1:8,
         loan.type == 1 & property.type ==1)

hmda$securitized <- hmda$action.type == 6 & !is.na(hmda$action.type)
hmda$application <- hmda$action.type %in% 1:5
hmda$denial      <- hmda$action.type == 3
hmda$denial[!hmda$application] <- NA

hmda$white    <- (hmda$applicant.race.1 == 5)  & (hmda$applicant.ethnicity != 1)
hmda$black    <- hmda$applicant.race.1 == 3  & hmda$applicant.ethnicity != 1
hmda$hispanic <- hmda$applicant.ethnicity == 1
hmda$asian    <- hmda$applicant.race.1 == 2  & hmda$applicant.ethnicity != 1
hmda$race_missing <- !(hmda$applicant.race.1 %in% 1:5) | 
  is.na(hmda$applicant.race.1) |
  !hmda$application

for (s in c('white','black','hispanic','asian'))
  hmda[hmda$race_missing,s] <- NA

hmda$loan.amount <- as.numeric(hmda$loan.amount)       
hmda$loan.amount[hmda$loan.amount > 5000 |
                   hmda$loan.amount <= 0 ] <- NA

hmda$applicant.income <- as.numeric(hmda$applicant.income)
hmda$applicant.income[hmda$applicant.income > 5000 |
                        hmda$applicant.income <= 0 ] <- NA

hmda$lti <- with(hmda, loan.amount / applicant.income)
hmda$lti[hmda$lti > 4.5] <- NA
hmda$lti_below_1 <- hmda$lti < 1
hmda$lti[hmda$lti_below_1] <- NA

hmda <- filter(hmda, county.fips > 1000)
hmda$year <- 2017

write_csv(hmda, paste0(outputfolder,'output_hmda/hmda_individual_2017.csv'))


