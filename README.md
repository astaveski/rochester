# Quantifying the Affordable Housing Gap in Rochester, NY
By Adam Staveski and Andrea Ringer

## pums_import.R
* Imports single-year PUMS data from 2018
   * File: https://www.census.gov/programs-surveys/acs/data/pums.html
   * Documentation: https://www2.census.gov/programs-surveys/acs/tech_docs/pums/ACS2018_PUMS_README.pdf?#
* Filters data to include only Rochester PUMAs (00902 and 00903)
* Merges person and household-level data into a unified dataset
* Generates categorical income varibles based on:
   * HUD's AMI thresholds
   * Income quintiles
* Creates framework for data analysis, including:
   * How to construct weighted counts
   * How to construct weighted means and medians
   * How to construct weighted proportions
   * How to compute weighted standard errors
   * How to create histograms with weighted data

## right_sized.R
Analyzes whether Rochester's housing stock is "right-sized" for the types of households living in the City. This code does the following:
* Imports Rochester's single-year PUMS data from 2018
* Generates categorical income thresholds
* Conducts right-size analysis using two metrics:
   * Number of persons per bedroom (HUD threshold is a max of 2.0 persons per bedroom)
   * Number of persons per room (Typical standard is a max of 1.5 persons per room)
