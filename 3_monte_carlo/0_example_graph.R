
rm(list=ls())

library(tidyverse)
library(tidylog)

limits  <- c(
 "Clay County, FL" = 424100,
 "Collier County, FL" = 450800
)

limit_chosen <- 1


round_hmda <- function(x) {
  
  ce <- ceiling(x)
  fl <- floor(x)
  
  if (abs(x - ce) <= abs(x - fl)) {
    return(ce)
  }
  
  return(fl)
  
}

cll <- limits[limit_chosen]/1000

lb <- floor(cll)
ub <- ceiling(cll)

df <- data.frame(
  loan_amount = lb + (ub-lb) * (0:100)/100
) %>%
  mutate(loan_amount_rounded = sapply(loan_amount, round_hmda))

pdf(paste0("figures/example_illustrative_", gsub(" ", "_", names(limits)[limit_chosen]), "_.pdf"),
    width = 10, height = 10)

plot(y = df$loan_amount_rounded[df$loan_amount_rounded == min(df$loan_amount_rounded)], 
     x = df$loan_amount[df$loan_amount_rounded == min(df$loan_amount_rounded)],
     xlim = c(floor(cll), ceiling(cll)),
     ylim = c(min(df$loan_amount_rounded)-0.5, max(df$loan_amount_rounded)+0.5),
     type = "l",
     xlab = "Loan Amount",
     ylab = "Rounded Loan Amount (HMDA Approach, Nearest Thousand)",
     lwd  = 3)

lines(y = df$loan_amount_rounded[df$loan_amount_rounded == max(df$loan_amount_rounded)], 
     x = df$loan_amount[df$loan_amount_rounded == max(df$loan_amount_rounded)],
     xlim = c(floor(cll), ceiling(cll)),
     ylim = range(df$loan_amount_rounded),
     type = "l",
     lwd  = 3)

text(x = df$loan_amount[df$loan_amount_rounded == max(df$loan_amount_rounded)][1], 
     y = df$loan_amount_rounded[df$loan_amount_rounded == max(df$loan_amount_rounded)][1],
     labels = "[")

text(x = tail(df$loan_amount[df$loan_amount_rounded == min(df$loan_amount_rounded)], n=1), 
     y = tail(df$loan_amount_rounded[df$loan_amount_rounded == min(df$loan_amount_rounded)], n=1),
     labels = "[")

lines(y = min(df$loan_amount_rounded)-0.5 + (max(df$loan_amount_rounded)+0.5 - (min(df$loan_amount_rounded)-0.5))*(0:100)/100,
     x = rep(cll,101),
     lty = 2,
     col = "green")


lines(y = min(df$loan_amount_rounded)-0.5 + (max(df$loan_amount_rounded)+0.5 - (min(df$loan_amount_rounded)-0.5))*(0:100)/100,
      x = rep(ceiling(cll),101),
      lty = 2,
      col = "red")

legend("topright",
       legend = c("Actual Conforming Loan Limit",
                  "Loan Limit Rounded Up as in LaCour-Little (2022)"),
       lwd = c(1,1),
       col = c("green", "red"), ,bg = "white")

dev.off()





