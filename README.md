# Quantifying the Affordable Housing Gap in Rochester, NY
By Adam Staveski and Andrea Ringer

## pums_import.R
* Imports single-year PUMS data from 2018 and five-year PUMS data from 2014-2018:
   * Files: https://www.census.gov/programs-surveys/acs/data/pums.html
   * Documentation: https://www2.census.gov/programs-surveys/acs/tech_docs/pums/ACS2018_PUMS_README.pdf?#
* Filters data to include only Rochester PUMAs (00902 and 00903) and focuses specifically on Rochester's rental households (GRPIP>0)
* Merges person- and household-level data into a unified dataset
* Generates categorical varibles based for:
   * HUD's AMI thresholds
   * City's income quintiles
   * Hispanic binary
   * Race categories
   * Age categories
   * College enrollment binary
   * Citizenship status categories
   * Household type
   * Child presence
   * Occupation
* Creates framework for data analysis, including:
   * How to construct weighted counts
   * How to construct weighted means and medians
   * How to construct weighted proportions
   * How to create histograms with weighted data
* Includes standard error calculator that does the following:
   * Computes standard errors of counts 
   * Computes standard errors of proportions
   * Computes standard errors for all levels of a variable

## right_sized.R
Analyzes whether Rochester's housing stock is "right-sized" for the types of households living in the City. This code does the following:
* Imports and prepares 1-year / 5-year PUMS data in the style of pums_import.R
* Conducts right-size analysis using person-level and household-level data
* Conducts right-size analysis using two metrics:
   * Number of persons per bedroom (HUD standard is a max of 2.0 persons per livable bedroom)
   * Number of persons per room (HUD standard is a max of 1.0 persons per livable room)

### Persons per Bedroom
Although final determinations are made on a case-by-case basis, HUD typically defines "overcrowding" as having more than 2.0 people per livable bedroom. In accordance with this standard, I define a household as "overcrowded" if the number of people in the household divided by two is greater than the number of bedrooms in the household. I make an exception for one-person households, who I assume are comfortable living in zero bedroom studio/efficiency apartments. I also examine more spacious thresholds of 1.75 persons per bedroom and 1.5 persons per bedroom. 

### Persons per Room
An alternative criteria to determine whether a household is overcrowded is to analyze the number of persons per room. In the U.S., HUD guidelines sometimes define overcrowding as having more than 1.0 people per livable room. In accordance with this standard, I define a household as "overcrowded" if the number of people in the household is greater than the number of rooms in the household. I also examine alternative thresholds of 1.5 persons per room and 0.5 persons per room.

### Demographics
This file examines the demographics of individuals living in overcrowded households. These characteristics include:
* Household income
* Sex
* Race
* Disability status
* Age
* College enrollment
* Citizenship status
* Number of people in household
* Marital status
* Presence of children
* Occupation

## gap_analysis.R
This file combines aspects of the affordability analysis and the right-sized analysis to estimate the "right-sized affordable housing gap" in Rochester. This code does the following:
* Imports and prepares 1-year / 5-year PUMS household data
* Analyzes the characteristics of Rochester's stock of vacant units
* Analyzes the space and cost needs of Rochester's renter households
* Estimates the right-sized affordable housing gap under current conditions
* Estimates the right-sized affordable housing gap under "perfect sorting" conditions

### Determining Right-Sized Affordable Housing
A unit is defined as "affordable" if a household spends 30% or less of its annual gross income on rent and utilities. A unit is defined as "right-sized" if there are two people or less per bedroom, on average. Any household living in a unit that is not right-sized and/or affordable is counted as part of the right-sized affordable housing gap.

### Estimating the Current Gap
Using 2014-2018 PUMS household data, this file flags all households that are currently 1) unaffordable, according to the 30% of household income standard or 2) overcrowded, according to the 2.0 people per bedroom standard. The file then counts all households with at least one of the two flags. By this measure, 30,671 (+/- 1,433) households are not right-sized and/or affordable. This is 58.6% (+/- 2.2%) of all renter households in Rochester.

The file then examines the characteristics of gap households. We find that there is a lack of rental units affordable to households making less than 30% of Area Median Income (~$22,200). We also find that there is a lack of 0-1 bedroom apartments, which are appropriately sized for 1-2 person households.

### Estimating the Gap Under Perfect Sorting Conditions
Although 30,671 households are either unaffordable or overcrowded, part of this gap can be alleviated through perfect allocation of households to housing units. Under these "perfect sorting" conditions, the lowest-priced unit is allocated to the lowest-income family that can afford it, assuming the unit meets the household's space needs. According to this hypothetical scenario, the right-sized affordable housing gap is 14,839 units (+/- 1,237), or 28.3% (+/- 2.2%) of all renter households. In combination with our estimate of the current right-sized affordable housing gap, the "perfect sorting" estimate implies a need for better allocation of households to housing units.

The file then examines the characteristics of gap households. Even under "perfect sorting" conditions, we find that 99.8% of gap households earn less than 30% of Area Median Income. Likewise, we find that 63.7% of gap households are comprised of 1-2 people. This indicates a structural housing deficit of small, inexpensive rental units.
