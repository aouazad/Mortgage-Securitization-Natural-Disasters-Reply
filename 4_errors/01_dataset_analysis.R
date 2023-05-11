rm(list=ls())

library(tidyverse)
library(tidylog)

originated_05 <- read_csv("originated_05_CT_treatment.csv")

# these lines from the LaCour-Little data archive
# 
# %%%%%%%%%%%%%%%
#originated_05$as_of_year.f <- factor(originated_05$as_of_year)
#originated_05$FIPS.f <- factor(originated_05$FIPS_2010)
#originated_05$belowLimit <- 0
#originated_05$belowLimit[originated_05$loan_to_limit_difference<=0] <- 1

#linearModel <- felm(Originated ~ belowLimit + belowLimit:Treatment +
#                      Treatment:T_minus_4 + Treatment:T_minus_3 + Treatment:T_minus_2 +
#                      Treatment:T_minus_0 + Treatment:T_plus_1 + Treatment:T_plus_2 +
#                      Treatment:T_plus_3 + Treatment:T_plus_4 + belowLimit:as_of_year.f +
#                      Treatment:T_minus_4:belowLimit + Treatment:T_minus_3:belowLimit +
#                      Treatment:T_minus_2:belowLimit + Treatment:T_minus_0:belowLimit +
#                      Treatment:T_plus_1:belowLimit + Treatment:T_plus_2:belowLimit +
#                      Treatment:T_plus_3:belowLimit + Treatment:T_plus_4:belowLimit| as_of_year.f + FIPS.f +
#                      FRANCES2004 + CHARLEY2004 + IVAN2004 + JEANNE2004 + DENNIS2005 + WILMA2005 +
#                      KATRINA2005 + RITA2005 + OPHELIA2005 + GUSTAV2008 + IKE2008 +
#                      DOLLY2008 + IRENE2011 + SANDY2012 + ISAAC2012 + MATTHEW2016| 
#                            0 |as_of_year.f + FIPS.f,
#                    data=originated_05)
#
# tidy_lmfit <- tidy(linearModel)
# %%%%%%%%%%%%%%%%%%



# let's analyze treatment years

possible_hurricanes <- 
  c("FRANCES2004",    "CHARLEY2004",
    "IVAN2004",        "JEANNE2004",    "DENNIS2005",    "WILMA2005",    
    "KATRINA2005",     "RITA2005",     "OPHELIA2005",    "GUSTAV2008", 
    "IKE2008",         "DOLLY2008", "IRENE2011", "SANDY2012", "ISAAC2012", 
    "MATTHEW2016")

tdf <- originated_05 %>% filter(Treatment == 1)

years <- sapply(possible_hurricanes, function(ch) substr(ch, nchar(ch)-3, 100))

require(xtable)

for (hr in possible_hurricanes) {
  
  print(hr)
  print(table(tdf$as_of_year[tdf$T_plus_1 == 1 & tdf[,hr] == 1]-1))
  
}

for (hr in possible_hurricanes) {
  
  print(hr)
  print(table(tdf$as_of_year[tdf$T_plus_1 == 1 & tdf[,hr] == 1]-1,
              tdf$belowLimit[tdf$T_plus_1 == 1 & tdf[,hr] == 1]))
  
}

# make pretty tables of these errors

tbldf <- data.frame(
  hurricane = c(),
  v1 = c(),
  v2 = c(),
  v3 = c(),
  v4 = c(),
  v5 = c()
)

for (hr in possible_hurricanes) {
  
  print(hr)
  
  onedf <- Reduce(function(x,y) left_join(x,y, by = "year"),
    lapply(1:4, function(ct) {
            df1y <- tdf %>% 
                    filter(get(paste0("T_plus_",ct)) == 1,
                           get(hr) == 1) %>%
                    group_by(as_of_year) %>%
                    summarise(nobs = sum(!is.na(Originated))) %>%
                    mutate(as_of_year = as_of_year - ct)
            names(df1y) <- c("year", paste0("t+",ct))
            df1y
    })) %>%
    mutate(hurricane = hr) %>%
    relocate(hurricane)
  
  if (nrow(onedf) == 0) {
    
    onedf <- tibble(hurricane = c(hr),
                        year = NA,
                        `t+1` = 0,
                        `t+2` = 0,
                        `t+3` = 0,
                        `t+4` = 0)
    
  }
    
  tbldf <- plyr::rbind.fill(tbldf, onedf)

}

print(xtable(tbldf, digits=0), include.rownames = FALSE)


#> table(tdf$Treatment_Year_1[tdf$KATRINA2005==1])

# 2004  2005 
# 72 12071 

#> table(originated_05$Treatment_Year_1,originated_05$SANDY2012)

#0       1
#0    1463951       0
#2004   22101       0
#2005   32307       0
#2008    1924       0
#2011    2280    1579
#2012       2   63710
#2016   11431       0

#> table(originated_05$Treatment_Year_2,originated_05$SANDY2012)

#0       1
#0    1506326   63710
#2004    9013       0
#2005   12923       0
#2008    2627       0
#2011     348       0
#2012    2723    1579
#2016      36       0
#> table(originated_05$Treatment_Year_3,originated_05$SANDY2012)

#0       1
#0    1524709   65289
#2005    6637       0
#2012    2370       0
#2016     280       0
#> table(originated_05$Treatment_Year_4,originated_05$SANDY2012)

# 0       1
# 0 1533996   65289


# > table(tdf$Treatment_Year_1[tdf$SANDY2012==1])

# 2011  2012 
# 1579 63710


# > table(tdf$Treatment_Year_1[tdf$IVAN2004==1])

# 2004 
# 4624 
# > table(tdf$Treatment_Year_2[tdf$IVAN2004==1])

# 0 2005 
# 4552   72 
# > table(tdf$Treatment_Year_3[tdf$IVAN2004==1])

# 0 
# 4624 

# > table(tdf$Treatment_Year_4[tdf$IVAN2004==1])

# 0 
# 4624 

#> table(tdf$Treatment_Year_1[tdf$WILMA2005==1])

# 2004  2005 
# 13366 22721 

# > table(tdf$yearCategorical[tdf$T_minus_0==1 & tdf$WILMA2005])
# 
# 2004 2005 
# 854 3207 
# > table(tdf$yearCategorical[tdf$T_minus_0==1 & tdf$WILMA2005==1])
# 
# 2004 2005 
# 854 3207 
# > table(tdf$yearCategorical[tdf$T_minus_0==1 & tdf$SANDY2012==1])
# 
# 2011 2012 
# 28  919 
# > table(tdf$yearCategorical[tdf$T_plus_1==1 & tdf$SANDY2012==1])
# 
# 2012 2013 
# 14 1254 
# > table(tdf$yearCategorical[tdf$T_plus_2==1 & tdf$SANDY2012==1])
# 
# 2013 2014 
# 32  980 
# > table(tdf$yearCategorical[tdf$T_plus_3==1 & tdf$SANDY2012==1])
# 
# 2014 2015 
# 31 1180 
# > table(tdf$yearCategorical[tdf$T_plus_4==1 & tdf$SANDY2012==1])
# 
# 2015 2016 
# 36 1476 


# count the aggregate number of misclassified loans

possible_hurricanes <- 
  c("FRANCES2004",    "CHARLEY2004",
    "IVAN2004",        "JEANNE2004",    "DENNIS2005",    "WILMA2005",    
    "KATRINA2005",     "RITA2005",     "OPHELIA2005",    "GUSTAV2008", 
    "IKE2008",         "DOLLY2008", "IRENE2011", "SANDY2012", "ISAAC2012", 
    "MATTHEW2016")

tdflong <- Reduce(rbind,
  lapply(possible_hurricanes,
         function (chosen_hurricane) {
           
            tdf %>%
             filter(get(chosen_hurricane)==1) %>%
             mutate(hurricane = chosen_hurricane)
           
         }))

tdflong <- tdflong %>%
  mutate(actual_year = substr(hurricane, nchar(hurricane) -3, 100))

tdflong$misclassified <- 0

for (kt in 1:4) {

  tdflong$misclassified <- tdflong$misclassified | 
    (tdflong[,paste0("T_plus_", kt)] == 1  &
     tdflong$actual_year != (tdflong$as_of_year - kt))

}

for (kt in 0:4) {
  
  tdflong$misclassified <- tdflong$misclassified | 
    (tdflong[,paste0("T_minus_", kt)] == 1  &
       tdflong$actual_year != (tdflong$as_of_year + kt))
  
}

tdflong$misclassified <- as.numeric(tdflong$misclassified)

tdflong$diff_log_loan_amount <- tdflong$log_loan_amt - tdflong$log_limit

ggplot(tdflong, aes(x=diff_log_loan_amount,y=misclassified)) +
  stat_summary_bin(fun='mean', bins=40,
                   color='black', size=2, geom='point') +
  xlab("Difference log(Loan Amount) - log(Conforming Loan Limit)") +
  ylab("Incorrect Treatment Year") +
  geom_vline(lty = 2, col = "red", xintercept = 0.00)

for (ch in possible_hurricanes) {

p <- ggplot(tdflong %>%
         filter(hurricane == ch), 
       aes(x=diff_log_loan_amount,y=misclassified)) +
  stat_summary_bin(fun='mean', bins=40,
                   color='black', size=2, geom='point') +
  xlab("Difference log(Loan Amount) - log(Conforming Loan Limit)") +
  ylab("Incorrect Treatment Year") +
  geom_vline(lty = 2, col = "red", xintercept = 0.00)

ggsave(paste0("figures/miscoding_year_for_hurricane_", ch, ".pdf"), plot = p,
       width = 6, height = 6)
  
}

# let's test it empirically with an RD with a bandwidth

require(fixest)


list(
feols(misclassified ~ belowLimit,
      data = tdflong,
      cluster = ~ ZCTA5 + as_of_year),
feols(misclassified ~ belowLimit + poly(diff_log_loan_amount,1),
      data = tdflong,
      cluster = ~ ZCTA5 + as_of_year),
feols(misclassified ~ belowLimit + poly(diff_log_loan_amount,2),
      data = tdflong,
      cluster = ~ ZCTA5 + as_of_year),
feols(misclassified ~ belowLimit + poly(diff_log_loan_amount,3),
      data = tdflong,
      cluster = ~ ZCTA5 + as_of_year)) %>%
  etable(tex = TRUE)

list(
  feols(misclassified ~ belowLimit,
        data = tdflong,
        cluster = ~ ZCTA5 + as_of_year),
  feols(misclassified ~ belowLimit + poly(diff_log_loan_amount,1)*belowLimit,
        data = tdflong,
        cluster = ~ ZCTA5 + as_of_year),
  feols(misclassified ~ belowLimit + poly(diff_log_loan_amount,2)*belowLimit,
        data = tdflong,
        cluster = ~ ZCTA5 + as_of_year),
  feols(misclassified ~ belowLimit + poly(diff_log_loan_amount,3)*belowLimit,
        data = tdflong,
        cluster = ~ ZCTA5 + as_of_year)) %>%
  etable(tex = TRUE)

#linearModel <- felm(Originated ~ belowLimit + belowLimit:Treatment +
#                      Treatment:T_minus_4 + Treatment:T_minus_3 + Treatment:T_minus_2 +
#                      Treatment:T_minus_0 + Treatment:T_plus_1 + Treatment:T_plus_2 +
#                      Treatment:T_plus_3 + Treatment:T_plus_4 + belowLimit:as_of_year.f +
#                      Treatment:T_minus_4:belowLimit + Treatment:T_minus_3:belowLimit +
#                      Treatment:T_minus_2:belowLimit + Treatment:T_minus_0:belowLimit +
#                      Treatment:T_plus_1:belowLimit + Treatment:T_plus_2:belowLimit +
#                      Treatment:T_plus_3:belowLimit + Treatment:T_plus_4:belowLimit| as_of_year.f + FIPS.f +
#                      FRANCES2004 + CHARLEY2004 + IVAN2004 + JEANNE2004 + DENNIS2005 + WILMA2005 +
#                      KATRINA2005 + RITA2005 + OPHELIA2005 + GUSTAV2008 + IKE2008 +
#                      DOLLY2008 + IRENE2011 + SANDY2012 + ISAAC2012 + MATTHEW2016| 
#                            0 |as_of_year.f + FIPS.f,
#                    data=originated_05)
#

originated_05 <- originated_05 %>%
  mutate(sum =
  T_minus_4 + T_minus_3 + T_minus_2 + T_minus_1 + T_minus_0 +
    T_plus_1 + T_plus_2 + T_plus_3 + T_plus_4)



