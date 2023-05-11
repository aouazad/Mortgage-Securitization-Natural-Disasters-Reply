
rm(list=ls())
source("05_make_estimation_sample.R")

library(lfe) # this package is likely not needed anymore, superseded by fixest
library(fixest)

########################################################################################################
# LENDING STANDARDS REGRESSION                                                                         #
########################################################################################################

figure_dir <- "figures"
outputdir <- "output"
depvar_array <- c("originated", "securitized", "approved")

depvar = "originated"
#depvar = "approved"
#depvar = "securitized" 
   
est_sample_filtered <- est_sample %>% 
      filter(abs(diff_log_loan_amount) <= 0.20) %>%
      filter(time %in% -4:4) 
      
if (depvar == "securitized")
      est_sample_filtered <- est_sample_filtered %>%
        filter(action.type == 1) # only among originated mortgages
      
if (depvar %in% c("approved", "originated"))
      est_sample_filtered <- est_sample_filtered %>%
        filter(action.type %in% c(1,2,3)) # we consider only applications that have not been withdrawn by the applicant

est_sample_filtered <- est_sample_filtered %>%
  ungroup() %>%
  filter(year >= 2001) 

regf <- as.formula(sprintf(paste0("%s ~ %s + %s ",
                                      "| as.factor(year) + as.factor(ZCTA5CE10) + as.factor(name_event)"), 
                               depvar,
                               paste(sprintf("time_m%d:treated",
                                             (abs(range(est_sample_filtered$time)[1]):2)),
                                     collapse = " + "),
                               paste(sprintf("time_%d:treated",
                                             0:range(est_sample_filtered$time)[2]),
                                     collapse = " + ")))
    
print(regf)

points <- -0.10 + 0.20 * (0:40)/40
    
results <- lapply( points, 
  function(chosen_point) {
  feols(regf, 
        data = est_sample_filtered,
        cluster = ~ ZCTA5CE10 + year,
        ssc = ssc(adj = FALSE, cluster.adj = FALSE),
        weights = (chosen_point <= 0) * 
          (est_sample_filtered$diff_log_loan_amount <= 0) * 
          dnorm((est_sample_filtered$diff_log_loan_amount - chosen_point)/0.01) +
      (chosen_point > 0) * 
      (est_sample_filtered$diff_log_loan_amount > 0) * 
      dnorm((est_sample_filtered$diff_log_loan_amount - chosen_point)/0.01))
  })

print(sprintf(' ****************** Results for %s', depvar))
print(summary(results))
print(results$N)

dfgraph <- data.frame(
  diff_log_loan_amount = points,
  te0 = sapply(results, function(x) x$coefficients["treated:time_0"]),
  te1 = sapply(results, function(x) x$coefficients["treated:time_1"]),
  te2 = sapply(results, function(x) x$coefficients["treated:time_2"]),
  te3 = sapply(results, function(x) x$coefficients["treated:time_3"]),
  te4 = sapply(results, function(x) x$coefficients["treated:time_4"])
) %>%
  filter(abs(diff_log_loan_amount) < 0.10)

dev.off()
pdf(paste0("figures_RD/RD_pictures_", depvar, "_time1.pdf"))
plot(dfgraph$diff_log_loan_amount[dfgraph$diff_log_loan_amount<0], 
     dfgraph$te1[dfgraph$diff_log_loan_amount<0], type = "b",
     xlim = c(-0.10,0.10),
     ylim = range(dfgraph$te1),
     ylab = "Treatment Effect",
     xlab = "log Loan Amount - log Conforming Loan Limit",
     main = "t+1")
lines(dfgraph$diff_log_loan_amount[dfgraph$diff_log_loan_amount>0], 
      dfgraph$te1[dfgraph$diff_log_loan_amount>0], type = "b")
lines(x = rep(0, 10),
      y = -0.20 + 0.40 * (0:9)/9,
      lty = 2,
      col = "red")
dev.off()

pdf(paste0("figures_RD/RD_pictures_", depvar, "_time2.pdf"))
plot(dfgraph$diff_log_loan_amount[dfgraph$diff_log_loan_amount<0], 
     dfgraph$te2[dfgraph$diff_log_loan_amount<0], type = "b",
     xlim = c(-0.10,0.10),
     ylim = range(dfgraph$te2),
     ylab = "Treatment Effect",
     xlab = "log Loan Amount - log Conforming Loan Limit",
     main = "t+2")
lines(dfgraph$diff_log_loan_amount[dfgraph$diff_log_loan_amount>0], 
     dfgraph$te2[dfgraph$diff_log_loan_amount>0], type = "b")
lines(x = rep(0, 10),
      y = -0.20 + 0.40 * (0:9)/9,
      lty = 2,
      col = "red")
dev.off()

pdf(paste0("figures_RD/RD_pictures_", depvar, "_time3.pdf"))
plot(dfgraph$diff_log_loan_amount[dfgraph$diff_log_loan_amount<0], 
     dfgraph$te3[dfgraph$diff_log_loan_amount<0], type = "b",
     xlim = c(-0.10,0.10),
     ylim = range(dfgraph$te3),
     ylab = "Treatment Effect",
     xlab = "log Loan Amount - log Conforming Loan Limit",
     main = "t+3")
lines(dfgraph$diff_log_loan_amount[dfgraph$diff_log_loan_amount>0], 
      dfgraph$te3[dfgraph$diff_log_loan_amount>0], type = "b")
lines(x = rep(0, 10),
      y = -0.20 + 0.40 * (0:9)/9,
      lty = 2,
      col = "red")
dev.off()

pdf(paste0("figures_RD/RD_pictures_", depvar, "_time4.pdf"))
plot(dfgraph$diff_log_loan_amount[dfgraph$diff_log_loan_amount<0], 
     dfgraph$te4[dfgraph$diff_log_loan_amount<0], type = "b",
     xlim = c(-0.10,0.10),
     ylim = range(dfgraph$te4),
     ylab = "Treatment Effect",
     xlab = "log Loan Amount - log Conforming Loan Limit",
     main = "t+4")
lines(dfgraph$diff_log_loan_amount[dfgraph$diff_log_loan_amount>0], 
      dfgraph$te4[dfgraph$diff_log_loan_amount>0], type = "b")
lines(x = rep(0, 10),
      y = -0.20 + 0.40 * (0:9)/9,
      lty = 2,
      col = "red")
dev.off()

# RD design with tight window
# Question: what is the optimal bandwidth

est_sample_filtered <- est_sample_filtered %>%
  mutate(year_below = paste0(year, "_", below_limit),
         ZCTA5CE10_below = paste0(ZCTA5CE10, "_", below_limit),
         name_event_below = paste0(name_event, "_", below_limit)) 

regf <- as.formula(sprintf(paste0("%s ~ %s + %s + %s + %s ",
                                  "| year_below + ZCTA5CE10_below + name_event_below "), 
                           depvar,
                           paste(sprintf("time_m%d:treated",
                                         (abs(range(est_sample_filtered$time)[1]):2)),
                                 collapse = " + "),
                           paste(sprintf("time_%d:treated",
                                         0:range(est_sample_filtered$time)[2]),
                                 collapse = " + "),
                           paste(sprintf("below_limit:time_m%d:treated",
                                         (abs(range(est_sample_filtered$time)[1]):2)),
                                 collapse = " + "),
                           paste(sprintf("below_limit:time_%d:treated",
                                         0:range(est_sample_filtered$time)[2]),
                                 collapse = " + ")
                          ))

print(regf)

possible_bandwidths <- c(0.01, 0.02, 0.03, 0.04, 0.05, 0.10, 0.15, 0.20)

regout <- lapply(possible_bandwidths,
  function(pb)
    feols(regf, 
      data = est_sample_filtered,
      cluster = ~ ZCTA5CE10 + year,
      ssc = ssc(adj = FALSE, cluster.adj = FALSE),
      weights = 
        dnorm((est_sample_filtered$diff_log_loan_amount)/pb)))

etable(regout)

print(summary(regout))

write_rds(regout, paste0("figures_RD/regression_result_", depvar, ".rds"))

etable(regout[1:4], tex = TRUE) %>%
  write_lines(paste0("figures_RD/regression_result_", depvar, "_1_to_4.tex"))

etable(regout[5:8], tex = TRUE) %>%
  write_lines(paste0("figures_RD/regression_result_", depvar, "_5_to_8.tex"))


