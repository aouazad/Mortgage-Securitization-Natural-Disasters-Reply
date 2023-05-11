# Mortgage Securitization Dynamics in the Aftermath of Natural Disasters: A Reply

This folder produces the results of Figures 6-11, and Tables 5-7.

- 00_1_prepare_limits_by_county.R

This file extracts the conforming loan limits for each county and for each year.

- 01_1_make_hmda_individual_files.R

This scripts uses the raw files from the National Archives and the CFPB and imports them into CSVs.

- 02_1_make_hmda_individual_with_loan_limit.R

This script adds the conforming loan limits to HMDA.

- 02_2_make_panel_within_window.R

This script extracts observations within the +-20% window of the county-year specific conforming loan limit.

- 04_make_hmda_event_study_data_set.R

This script builds the event study data set, considering only observations within the -4 year to +4 year of each hurricane, and adds a control group.

- 05_make_estimation_sample.R

Using the previous sample, this builds the estimation sample.

- 06_lending_standards_regression_baseline_reply_2023.R

Estimates the results of Figure 7 of the reply.

- 07_lending_standards_regression_local_polynomial_regression.R

Produces Figures 8-10.

- 08_lending_standards_regression_baseline_reply_2023_RD_weighting.R

Estimates of Tables 5-7.



