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
* Imports and prepares 2018 PUMS data in the style of pums_import.R
* Conducts right-size analysis using person-level and household-level data
* Conducts right-size analysis using two metrics:
   * Number of persons per bedroom (HUD threshold is a max of 2.0 persons per bedroom)
   * Number of persons per room (Typical standard is a max of 1.5 persons per room)

### Persons per Bedroom
Although final determinations are made on a case-by-case basis, HUD typically defines "overcrowding" as having more than 2.0 people per livable bedroom. In accordance with this standard, I define a household as "overcrowded" if the number of people in the household divided by two is greater than the number of bedrooms in the household. I make an exception for one-person households, who I assume are comfortable living in zero bedroom studio/efficiency apartments. I also examine more spacious thresholds of 1.5 persons per bedroom and 1.0 persons per bedroom. 

### Persons per Room
An alternative criteria to determine whether a household is overcrowded is to analyze the number of persons per room. In the UK, government guidelines suggest that a household is overcrowded if it has more than 1.5 people per room. In accordance with this standard, I define a household as "overcrowded" if the number of people in the household divided by 1.5 is greater than the number of rooms in the household. I also examine alternative thresholds of 1.0 persons per room and 2.0 persons per room.

### Demographics
This file also examines the demographics of individuals living in overcrowded households. These characteristics include:
* Household income
* Sex
* Race
* Disability status
* Age
* College enrollment
* Citizenship status
* Number of people
* Homeownership status


## available.R
This file combines aspects of the affordability analysis and the right-sized analysis to estimate the so-called "right-sized affordable housing gap" in Rochester. This code does the following:
* Imports and prepares 2018 PUMS household data
* Analyzes the characteristics of Rochester's stock of vacant units
* Analyzes the space and cost needs of Rochester's renter households
* Estimates the right-sized affordable housing gap under current conditions
* Estimates the right-sized affordable housing gap under "perfect sorting" conditions

### Determining Right-Sized Affordable Housing
This analysis defines a unit as "affordable" if a household can spend 30% or less of its annual income on rent and utilities. This analysis defines a unit as "right-sized" if there are two people or less per bedroom, on average. Any unit that is not right-sized and/or affordable is counted toward the right-sized affordable housing gap.

### Estimating the Current Gap
Using 2018 PUMS household data, this file flags all households that are currently 1) unaffordable, according to the 30% of household income standard or 2) overcrowded, according to the 2.0 people per bedroom standard. The file then counts all households with at least one of the two flags. By this measure, 28,351 households are not right-sized and/or affordable.

### Estimating the Gap Under Perfect Sorting Conditions
Although 28,351 households are either unaffordable or overcrowded, part of this gap can be alleviated through perfect allocation of households to housing units. Under these "perfect sorting" conditions, the lowest-priced unit is allocated to the lowest-income family that can afford it while also avoiding overcrowding. This hypothetical scenario results in a right-sized affordable housing gap of 12,907 units, which indicates that over half of the gap can be filled through a more efficient allocation of households to housing units.
