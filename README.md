# Mortgage Securitization Dynamics in the Aftermath of Natural Disasters: A Reply

This archive provides the code for the paper _"Mortgage Securitization Dynamics in the Aftermath of Natural Disasters: A Reply"_, authored by Amine Ouazad, of Rutgers Business School and HEC Montreal, and Matthew E. Kahn, Provost Professor of Economics at the University of Southern California.

There are three parts to this code:

1. The TeX source of the reply. 
2. The Creation of the Sample of Mortgages and the Regression Discontinuity Analysis
3. The Monte Carlo simulations for the Analysis of the Rounding of HMDA Loans
4. The Analysis of the Errors of LaCour-Little et al. (2022)

## 1. The Paper

Includes the full TeX file and the BibTeX citations.

## 2. Regression Discontinuity: The Impact of Natural Disasters on the Approval, Origination of Loans Conforming with Fannie Mae and Freddie Mac's Rules, and the Securitization of these Loans

This section constructs the data set using (i)~Home Mortgage Disclosure Act data between 2000 and 2017, (ii)~the set of treated 5-digit ZIP codes, affected by one of the 15 billion dollar events.

## 3. Monte Carlo Simulation: When HMDA Loan Amounts are Rounded, How Should We Count Conforming Loans?

The R code in this folder studies the following question:

- Since loan amounts in Home Mortgage Disclosure Act data are rounded _to the nearest integer_, and since conforming loan limits are given at the dollar, how should we classify loans in the conforming vs. the jumbo segment?

The R code performs a Monte Carlo simulation. Assuming a _true model_ of a discontinuity in approval rates at the conforming loan limit, what is the specification using rounded HMDA loan amounts which best estimates such discontinuity? The Monte Carlo simulation suggests that the approach with the least bias and most precision uses HMDA loan amounts themselves rather than HMDA loan amounts rounded up, as in LaCour-Little et al. (2022).

## 4. Errors in LaCour-Little et al. (2022)

The R code in this folder relies on the LaCour-Little et al. archive stored either on their website, and with a snapshot available at [this link](http://www.ouazad.com/papers/lacour_little_data_archive.zip).

It produces Tables 1--3 and Figure 1. These show that (i)~hurricane treatment years are miscoded, likely because of a flaw in the event study design, (ii)~such miscoding is bunched at the conforming loan limit, thus affecting the estimates, (iii)~93,231 treated observations have no time indicator variable equal to 1. They are thus contributing to the identification of the control group.

## References

Ouazad, A. and Kahn, M.E., 2022. Mortgage finance and climate change: Securitization dynamics in the aftermath of natural disasters. The Review of Financial Studies, 35(8), pp.3617-3665.

