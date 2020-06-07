# Quantifying the Affordable Housing Gap in Rochester, NY
By Adam Staveski and Andrea Ringer

## pums_import.R
* Imports single-year PUMS data from 2018 (File: https://www.census.gov/programs-surveys/acs/data/pums.html)
* Filters data to include only Rochester PUMAs
* Drops irrelevant variables to this analysis
* Sorts other variables into three groups: variables, flags, and weights
* Merges person and household-level data into a unified dataset
* Creates framework for data analysis, including:
   * How to construct weighted counts
   * How to construct weighted means and medians
   * How to construct weighted proportions
   * How to compute weighted standard errors
