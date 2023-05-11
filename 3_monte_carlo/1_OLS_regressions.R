rm(list=ls())

library(tidyverse)
library(tidylog)

# load all possible counties and limits for a given year to assess the bias

window_size <- 0.5

# generate a DGP data set with a discontinuity at either
# $424,100, which is the one-unit limit for
# 12	019	CLAY	FL	27260	 $424,100 	 $543,000 	 $656,350 	 $815,650 
# and $450,800 which is the one-unit limit for
# 12	021	COLLIER	FL	34940	 $450,800 	 $577,100 	 $697,600 	 $866,950 

clay_fl_cll <- 424100
collier_fl_cll <- 450800

logit <- function(x)
  exp(x) / (1+exp(x))

round_hmda <- function(x) {
  
  ce <- ceiling(x)
  fl <- floor(x)
  
  if (abs(x - ce) <= abs(x - fl)) {
    return(ce)
  }

  return(fl)
    
}

# the true model is 

generate_data_set <- function(nobs, alpha, beta, conforming_loan_limit) {
  
  loanamounts_lb <- conforming_loan_limit * (1-window_size)
  loanamounts_ub <- conforming_loan_limit * (1+window_size)
  
  df <- data.frame(index = 1:nobs)
  
  df$loanamount <- runif(nobs, min = loanamounts_lb, max = loanamounts_ub)
  
  # this below is rounded to the next
  df$loanamount_rounded <- sapply(df$loanamount, round_hmda)
  df$classification_true <- as.numeric(df$loanamount <= conforming_loan_limit)
  df$approved <- sapply(1:nrow(df),
                        function(x) as.numeric(runif(1) < logit(alpha + beta * df$classification_true[x])))
    
  df$classification_hmda <- as.numeric(df$loanamount_rounded <= 
                                            conforming_loan_limit)
  df$classification_lacourlittle <- as.numeric(df$loanamount_rounded <= 
                                            ceiling(conforming_loan_limit))

  df  
  
}

get_misclassification <- function(nobs, alpha, beta, conforming_loan_limit) {
  
  df <- generate_data_set(nobs, alpha, beta, conforming_loan_limit)
  
  data.frame(
    share_conforming_hmda = mean(as.numeric(df$classification_hmda)),
    share_conforming_lacourlittle = mean(as.numeric(df$classification_lacourlittle)),
    share_misclassified_hmda = mean(as.numeric(df$classification_hmda != df$classification_true)),
    share_misclassified_lacourlittle = mean(as.numeric(df$classification_lacourlittle != df$classification_true))
  )
  
}

get_coefficients <- function(df) {
  
  coeff_true = lm(approved ~ classification_true, data = df)
  coeff_hmda = lm(approved ~ classification_hmda, data = df)
  coeff_lacourlittle = lm(approved ~ classification_lacourlittle, data = df)
  
  data.frame(coeff_true = coeff_true$coefficients["classification_true"],
             coeff_hmda = coeff_hmda$coefficients["classification_hmda"],
             coeff_lacourlittle = coeff_lacourlittle$coefficients["classification_lacourlittle"])
  
}

run_mc <- function(nobs, alpha, beta, conforming_loan_limit, nsim = 10000) {
  
  do.call(rbind,
          lapply(1:nsim,
                 function(x) 
                   get_coefficients(generate_data_set(nobs, alpha, beta, 
                                                      conforming_loan_limit))))
}

summarize_coeffs <- function(nobs, alpha, beta, conforming_loan_limit) {
  
  dfmc <- run_mc(nobs, alpha, beta, conforming_loan_limit)
  
  data.frame(
    alpha = alpha,
    beta  = beta,
    cll   = conforming_loan_limit,
    nobs  = nobs,
    abs_bias_hmda         = mean(abs(dfmc$coeff_hmda - dfmc$coeff_true)),
    abs_bias_lacourlittle = mean(abs(dfmc$coeff_lacourlittle - dfmc$coeff_true)),
    bias_lacourlittle     = mean(dfmc$coeff_lacourlittle - dfmc$coeff_true),
    bias_hmda             = mean(dfmc$coeff_hmda - dfmc$coeff_true),
    sd_hmda               = sd(dfmc$coeff_hmda - dfmc$coeff_true),
    sd_lacourlittle       = sd(dfmc$coeff_lacourlittle - dfmc$coeff_true)
  )
  
}

# simple description of the rounding problem -----------------------------------

# which method yields the smallest number of misclassifications?

# Example #1: Collier County

df1 <- generate_data_set(nobs = 1000, 
                        alpha = 0.2, 
                        beta  = 0.1, 
                        conforming_loan_limit = collier_fl_cll  / 1000)

ggplot(data=df1, aes(x = loanamount, y = loanamount_rounded)) +
  geom_point() +
  geom_vline(xintercept = collier_fl_cll  / 1000, lty = 2, col = "red") +
  # add black lines for the midpoints

# Example #2: Clay County

df2 <- generate_data_set(nobs = 1000, 
                        alpha = 0.2, 
                        beta  = 0.1, 
                        conforming_loan_limit = clay_fl_cll  / 1000)

ggplot(data=df2, aes(x = loanamount, y = loanamount_rounded)) +
  geom_point() +
  geom_vline(xintercept = clay_fl_cll  / 1000, lty = 2, col = "red") +
  # add black lines for the midpoints
  
# Make table with misclassifications by county

misdf <- rbind(  
            get_misclassification(nobs = 1000, 
                                  alpha = 0.2, 
                                  beta  = 0.1, 
                                  conforming_loan_limit = collier_fl_cll  / 1000) %>%
              mutate(county = "Collier County, FL"),
            get_misclassification(nobs = 1000, 
                                  alpha = 0.2, 
                                  beta  = 0.1, 
                                  conforming_loan_limit = clay_fl_cll  / 1000) %>%
              mutate(county = "Clay County, FL"))

# Beta_hat for many simulations but fixed values of alpha, beta, nobs ----------

nobs <- 1000
alpha <- 0.2
beta  <- 0.1

limits_array <- c(
  "Collier" = collier_fl_cll, 
  "Clay"    = clay_fl_cll
)

cll_idx <- 2

for (cll_idx in seq_len(length(limits_array))) {
  
  print(paste0(" Drawing density of beta_hat for county ", 
               names(limits_array)[cll_idx]))
  
  dfmc <- run_mc(nobs, alpha, beta, limits_array[cll_idx] / 1000)

  dens_hmda        <- density(abs(dfmc$coeff_hmda - dfmc$coeff_true))
  dens_lacourlitte <- density(abs(dfmc$coeff_lacourlittle - dfmc$coeff_true))

  range_x <- range(c(dens_hmda$x, dens_lacourlitte$x))

  pdf(paste0("figures/density_abs_beta_hat_", names(limits_array)[cll_idx] , ".pdf"),
      width = 10, height = 10)
  plot(dens_hmda, xlim=range_x, 
       xlab = "Absolute Difference Beta Estimated with Misclassification and True Beta", ylab = "Density",
       main = "", col = "blue")
  lines(dens_lacourlitte, col = "orange")
  lines(x = rep(0, 4001), y = 0:4000, lty = 2, col = "red")
  legend("topright",
         legend = c("HMDA Approach (Ouazad and Kahn, 2022)",
                    "LaCour-Little Approach"),
         col = c("blue", "orange"),
         lwd = c(1,1,1))
  dev.off()
  
  ##
  
  dens_hmda        <- density(dfmc$coeff_hmda - dfmc$coeff_true)
  dens_lacourlitte <- density(dfmc$coeff_lacourlittle - dfmc$coeff_true)
  
  range_x <- range(c(dens_hmda$x, dens_lacourlitte$x))
  
  pdf(paste0("figures/density_beta_hat_", names(limits_array)[cll_idx] , ".pdf"),
      width = 10, height = 10)
  plot(dens_hmda, xlim=range_x, 
       xlab = "Difference Beta Estimated with Misclassification and True Beta", ylab = "Density",
       main = "", col = "blue")
  lines(dens_lacourlitte, col = "orange")
  lines(x = rep(0, 4001), y = 0:4000, lty = 2, col = "red")
  legend("topright",
         legend = c("HMDA Approach (Ouazad and Kahn, 2022)",
                    "LaCour-Little Approach"),
         col = c("blue", "orange"),
         lwd = c(1,1,1))
  dev.off()
  
}

# Evolution of the bias and the standard deviation as sample size increases ----

nobs_min <- 50
nobs_max <- 2000
nobs_grid_pts <- 10
nobs_grid <- nobs_min + (nobs_max - nobs_min) * (0:(nobs_grid_pts-1))/(nobs_grid_pts-1)

dfnobs <- do.call(rbind,
        lapply(nobs_grid, function(chosen_nobs) {
          print(paste0(" observations : ", chosen_nobs))
          summarize_coeffs(nobs = floor(chosen_nobs), 
                           alpha = 0.2, 
                           beta  = 0.1, 
                           conforming_loan_limit = collier_fl_cll  / 1000) 
          }))

# speed of convergence of the estimators??

ggplot() +
  geom_point(data = dfnobs, aes(x = nobs, y = bias_hmda), col = "blue") +
  geom_point(data = dfnobs, aes(x = nobs, y = bias_lacourlittle), col = "orange")

ggplot(data = dfnobs, aes(x = nobs, y = bias_hmda)) +
  geom_point()

ggplot(data = dfnobs, aes(x = nobs, y = bias_lacourlittle)) +
  geom_point()

ggplot() +
  geom_point(data = dfnobs, aes(x = nobs, y = sd_hmda), col = "blue") +
  geom_line(data = dfnobs, aes(x = nobs, y = sd_hmda), col = "blue") +
  geom_point(data = dfnobs, aes(x = nobs, y = sd_lacourlittle), col = "orange")+
  geom_line(data = dfnobs, aes(x = nobs, y = sd_lacourlittle), col = "orange") +
  ylab("Standard Deviation of the Estimator") +
  xlab("Number of Observations in the Window Around the Conforming Loan Limit")

ggsave("figures/speed_convergence.pdf", 
       width =10, height = 10)

ggplot() +
  geom_point(data = dfnobs, aes(x = nobs, y = abs_bias_hmda), col = "blue") +
  geom_line(data = dfnobs, aes(x = nobs, y = abs_bias_hmda), col = "blue") +
  geom_point(data = dfnobs, aes(x = nobs, y = abs_bias_lacourlittle), col = "orange")+
  geom_line(data = dfnobs, aes(x = nobs, y = abs_bias_lacourlittle), col = "orange") +
  ylab("Absolute Deviation of the Estimator from True Value") +
  xlab("Number of Observations in the Window Around the Conforming Loan Limit")

ggsave("figures/speed_lower_bias.pdf", 
       width =10, height = 10)


# converges faster to the true value

lm(log(sd_hmda) ~ log(nobs), data = dfnobs) %>% summary()
  
lm(log(sd_lacourlittle) ~ log(nobs), data = dfnobs) %>% summary()


