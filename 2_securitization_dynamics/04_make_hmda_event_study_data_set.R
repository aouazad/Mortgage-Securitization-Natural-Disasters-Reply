
rm(list=ls())
#########################################
##
## Takes the HMDA file with conforming loan limit (full) 
##   focuses on the 80% to 120% window around the limit
##   and adds billion dollar events
##
## This uses the entire panel
##
#########################################

crosswalkdir = "external_data/" # folder to tract to ZIP crosswalks

crosswalk1995_2002_fn = paste0(crosswalkdir, "crosswalk_1995_2002.rds")
crosswalk2003_2011_fn = paste0(crosswalkdir, "crosswalk_2004_2012_onwards.rds")
crosswalk2012_fn = paste0(crosswalkdir, "crosswalk_2012_onwards.rds")

atlantic_states = c(
  'Maine'          = 23,
  'New Hampshire'  = 33,
  'Massachussetts' = 25,
  'Rhode Island'   = 44,
  'Connecticut'    = 09,
  'New York'       = 36,
  'New Jersey'     = 34,
  'Delaware'       = 10,
  'Maryland'       = 24,
  'Virginia'       = 51,
  'North Carolina' = 37,
  'South Carolina' = 45,
  'Georgia'        = 13,
  'Florida'        = 12,
  'Alabama'        = 01,
  'Mississipi'     = 28,
  'Louisiana'      = 22,
  'Texas'          = 48
)


library(tidyverse)
library(gam)
library(lfe)

mainfn <- 'output/hmda_panel_inwindow.Rds'
output_dir <- 'output/'

write_rds(do.call(plyr::rbind.fill,sapply(1995:2017, function(y) {
    
    print(paste0(' *** selected year ', y))
    
    df <- read_rds(paste0(output_dir, 
                          'hmda_in_window_', y, '.rds')) 
    
    df$state <-  floor(df$county.fips/1000)
    
    df <- df %>%
      filter(as.numeric(as.character(state)) %in% atlantic_states)

    df 
    
  }, simplify = F)), file = mainfn)


######### Now load the data base of ZIPs hit by a billion dollar event

zips_treated = read_csv(paste0("external_data/treated_zips_all_events.csv")) %>%
               filter(treated == TRUE)

# merge HMDA with the tract to ZIP crosswalk
crosswalk1995_2002 = read_rds(crosswalk1995_2002_fn)
crosswalk2003_2011 = read_rds(crosswalk2003_2011_fn)
crosswalk2012_2017 = read_rds(crosswalk2012_fn)

hmda <- read_rds(mainfn)
hmda$census.tract.num <- hmda$census.tract
hmda$census.tract <- paste0(sprintf("%04d", floor(as.numeric(hmda$census.tract))), 
      sprintf("%02.0f",(100*as.numeric(hmda$census.tract) - 
                            100*floor(as.numeric(hmda$census.tract)))))

hmda$GISJOIN = sprintf("G%02.0f0%03.0f0%s%s",
      floor(hmda$county.fips/1000), # state fips
      hmda$county.fips - 1000*floor(hmda$county.fips/1000), # county fips
      substr(hmda$census.tract,1,4),# first four digits of tract
      substr(hmda$census.tract,5,6))# last two digits of tract

# works for 2012-2017
table(hmda$GISJOIN %in% crosswalk2012_2017$GISJOIN,
      hmda$year)

# works for 2003-2011
table(hmda$GISJOIN %in% crosswalk2003_2011$GISJOIN,
      hmda$year)

# works for 1995-2002
table(hmda$GISJOIN %in% crosswalk1995_2002$GISJOIN,
      hmda$year)

hmda_2003_2011 <- left_join((hmda %>% filter(year %in% 2003:2011)),
                            crosswalk2003_2011,
                            by = 'GISJOIN')

hmda_2012_2017 <- left_join((hmda %>% filter(year %in% 2012:2017)),
                            crosswalk2012_2017,
                            by = 'GISJOIN')


hmda_1995_2002 <- left_join((hmda %>% filter(year %in% 1995:2002)),
                            crosswalk1995_2002,
                            by = 'GISJOIN')

hmda <- rbind(
          hmda_1995_2002,
          hmda_2003_2011,
          hmda_2012_2017
        )

rm(hmda_1995_2002)
rm(hmda_2003_2011)
rm(hmda_2012_2017)

# first, extract the "never treated" zips
all_zips = setdiff(unique(hmda$ZCTA5CE10),NA)
zips_never_treated = all_zips[!(all_zips %in% zips_treated$ZCTA5CE10)]

# second, build a treated sample for each disaster
hmda_event_sample = c()

#sampling = 1 # share of never treated sampled at random

for (name_event in unique(zips_treated$event_name)) {
  print(name_event)
  year_event            = as.numeric(substr(name_event,nchar(name_event)-3,nchar(name_event)))
  print(year_event)
  zips_hit          = zips_treated$ZCTA5CE10[zips_treated$event_name == name_event]
  print(sprintf(" --> %d zips hit ", length(zips_hit)))
  
  hmda_treated            = hmda[hmda$ZCTA5CE10 %in% zips_hit,]
  hmda_treated$time       = hmda_treated$year - year_event
  hmda_treated$name_event = name_event
  hmda_treated$year_event = year_event
  hmda_treated$treated    = TRUE
  
  hmda_event_sample = rbind(hmda_event_sample,
                          hmda_treated)
  
  print(sprintf(" --> number of treated rows %d",      nrow(hmda_treated)))
  #printf(" --> number of never treated rows %d", nrow(hmda_never_treated))
  print(sprintf(" provisional number of rows %d",      nrow(hmda_event_sample)))
  
  print(sprintf(" memory size %f Mb ", as.numeric(object.size(hmda_event_sample)/(1024*1024))))
  
}

# impose a distance to the coast criterion here ?

hmda_never_treated = hmda[hmda$ZCTA5CE10 %in% zips_never_treated, ]
# uncomment if you want a smaller sample
#hmda_never_treated = hmda_never_treated[
#  floor(runif(floor(sampling*nrow(hmda_never_treated)),
#  min = 1,
#  max = nrow(hmda_never_treated)+1)), ]
hmda_never_treated$treated = FALSE
hmda_never_treated$time = -1
hmda_never_treated$name_event = ""
hmda_never_treated$year_event = NA

hmda_event_sample = rbind(hmda_event_sample,
                          hmda_never_treated)

write_rds(hmda_event_sample,
          path = paste0('output/hmda_event_study_zip_level.Rds'))

