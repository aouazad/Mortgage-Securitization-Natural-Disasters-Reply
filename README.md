# Mortgage Securitization Dynamics in the Aftermath of Natural Disasters: A Reply

This archive provides the code for the paper [_"Mortgage Securitization Dynamics in the Aftermath of Natural Disasters: A Reply"_](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4445723), authored by Amine Ouazad, of Rutgers Business School and HEC Montreal, and Matthew E. Kahn, Provost Professor of Economics at the University of Southern California. This paper is a follow up of the paper published in 2022 in the Review of Financial Studies, [_"Mortgage Finance and Climate Change: Securitization Dynamics in the Aftermath of Natural Disasters"_](https://academic.oup.com/rfs/article-abstract/35/8/3617/6427560).

There are four parts to this archive:

1. The TeX source of the reply, posted as an online appendix to [NBER Working Paper 26322](https://www.nber.org/papers/w26322) and as an SSRN [Working](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4445723).
2. The Regression Discontinuity Analysis: Impact of Billion Dollar Disasters on the Discontinuity in Approval, Origination, Securitization Rates at the Conforming Loan Limit.
3. The Monte Carlo simulations for the Analysis of the Rounding of HMDA Loans
4. The Analysis of the Errors of LaCour-Little et al. (2022)

## 1. The Paper

Includes the full TeX file and the BibTeX citations.

## 2. Regression Discontinuity: Billion-Dollar Disasters, Lending and Securitization Standards in the Conforming Segment

This section constructs the data set using (i)~Home Mortgage Disclosure Act data between 2000 and 2017, (ii)~the set of treated 5-digit ZIP codes, affected by one of the 15 billion dollar events.

## 3. Monte Carlo Simulation: When HMDA Loan Amounts are Rounded, How Should We Count Conforming Loans?

The R code in this folder studies the following question:

- Since loan amounts in Home Mortgage Disclosure Act data are rounded _to the nearest integer_, and since conforming loan limits are given at the dollar, how should we classify loans in the conforming vs. the jumbo segment?

The R code performs a Monte Carlo simulation. Assuming a _true model_ of a discontinuity in approval rates at the conforming loan limit, what is the specification using rounded HMDA loan amounts which best estimates such discontinuity? The Monte Carlo simulation suggests that the approach with the least bias and most precision uses HMDA loan amounts themselves rather than HMDA loan amounts rounded up, as in LaCour-Little et al. (2022).

## 4. Errors in LaCour-Little et al. (2022)

The R code in this folder relies on the LaCour-Little et al. archive stored either on their website, and with a snapshot available at [this link](http://www.ouazad.com/paper/lacour_little_data_archive.zip).

It produces Tables 1--3 and Figure 1. These show that

1. hurricane treatment years are miscoded, likely because of a flaw in the event study design,
2. such miscoding is bunched at the conforming loan limit, thus affecting the estimates,
3. 93,231 treated observations have no time indicator variable equal to 1. They are thus contributing to the identification of the control group.

## References

Ouazad, A. and Kahn, M.E., 2022. [Mortgage finance and climate change: Securitization dynamics in the aftermath of natural disasters](https://academic.oup.com/rfs/article-abstract/35/8/3617/6427560). The Review of Financial Studies, 35(8), pp.3617-3665.

