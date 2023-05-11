
rm(list=ls())
source("05_make_estimation_sample.R")

library(fixest)

########################################################################################################
# LENDING STANDARDS REGRESSION                                                                         #
########################################################################################################

figure_dir <- "figures"
outputdir <- "output"
depvar_array <- c("originated", "securitized", "approved")

for (depvar in depvar_array) {
        
        est_sample_filtered <- est_sample %>% 
                        filter(time %in% -4:4) %>%
                        mutate(below_limit = as.numeric(log_loan_amount <= log(effective_loanlimit/1000))) %>%
                        filter(abs(diff_log_loan_amount) <= 0.05)

        if (depvar == "securitized")
            est_sample_filtered <- est_sample_filtered %>%
                filter(action.type == 1) # can be changed to action.type %in% c(1,6) with no impact. 6 is loan sold
        if (depvar %in% c("approved", "originated"))
            est_sample_filtered <- est_sample_filtered %>%
                filter(action.type %in% c(1,2,3)) # all applications
        
        regf <- as.formula(sprintf(paste0("%s ~ %s + %s + %s + %s + %s + %s ", # covariates
                                          "+ below_limit + below_limit:treated + below_limit:as.factor(name_event) + below_limit:treated:as.factor(name_event) ",
                                          "| as.factor(year) + as.factor(ZCTA5CE10) + as.factor(name_event)" # fixed effects
                                          ), 
                                   depvar,
                                   paste(sprintf("below_limit:time_m%d:treated",
                                                 (abs(range(est_sample_filtered$time)[1]):2)),
                                         collapse = " + "),
                                   paste(sprintf("below_limit:time_%d:treated",
                                                 0:range(est_sample_filtered$time)[2]),
                                         collapse = " + "),
                                   paste(sprintf("time_m%d:treated",
                                                 (abs(range(est_sample_filtered$time)[1]):2)),
                                         collapse = " + "),
                                   paste(sprintf("time_%d:treated",
                                                 0:abs(range(est_sample_filtered$time)[2])),
                                         collapse = " + "),
                                   paste(sprintf("below_limit:year_%d",
                                                 (abs(range(est_sample_filtered$year)[1]):2012)),
                                         collapse = " + "),
                                   paste(sprintf("below_limit:year_%d",
                                                 2014:range(est_sample_filtered$year)[2]),
                                         collapse = " + ")))
        
        print(regf)
        
        results <- feols(regf, data = est_sample_filtered %>% filter(year >= 2001), # this year filter speeds up estimation, but doesn't affect the estimation
                         cluster = ~ ZCTA5CE10 + year, # double-clustering
                         ssc = ssc(adj = FALSE, cluster.adj = FALSE))   # no impact on SEs
        
        print(sprintf(' ****************** Results for %s', depvar))
        print(summary(results))
        print(results$N)
        
        jumbodf <- results %>% tidy() %>%
                filter(substr(term, 1, nchar("treated:time_")) == "treated:time_") %>%
                mutate(term = gsub("treated:time_", "", term, fixed = TRUE),
                       term = gsub("m", "-", term),
                       term = as.numeric(term))
        
        conformingdf <- results %>% tidy() %>%
                filter(grepl("below_limit:treated", term, fixed = TRUE),
                       substr(term, 1, nchar("below_limit:treated:")) == "below_limit:treated:") %>%
          mutate(term = gsub("below_limit:treated:time_", "", term, fixed = TRUE),
                 term = gsub("m", "-", term),
                 term = as.numeric(term))

        pdf(paste0("figures/impact_on_jumbo_conforming_market_", 
                  depvar, ".pdf"))   
        plotCI(y = jumbodf$estimate,
               x = jumbodf$term,
               uiw = 1.96*jumbodf$std.error,
               ylim = range(c(jumbodf$estimate + 1.96 * jumbodf$std.error, 
                              conformingdf$estimate + 1.96 * conformingdf$std.error,
                              jumbodf$estimate - 1.96 * jumbodf$std.error, 
                              conformingdf$estimate - 1.96 * conformingdf$std.error)),
               xlab = "Years Since Hurricane Exposure",
               ylab = paste0("Impact on Probability ", str_to_title(depvar)))
        lines(y = rep(0, 10),
              x = -4 + 9 * (0:9)/9,
              lty = 2,
              col = "red")
        dev.off()
        
        pdf(paste0("figures/impact_on_conforming_market_", 
                   depvar, ".pdf"))   
        plotCI(y = conformingdf$estimate,
               x = conformingdf$term,
               uiw = 1.96*conformingdf$std.error)
        lines(y = rep(0, 10),
              x = -4 + 9 * (0:9)/9,
              lty = 2,
              col = "red")
        dev.off()
    
}


