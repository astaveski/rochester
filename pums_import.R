# ===============================================================================
# * FILE: pums_import.R
# * PURPOSE: Import PUMS data from 2018
# * AUTHORS: Adam Staveski
# * DATE CREATED: June 4, 2020
# * DATE LAST MODIFIED: June 15, 2020
# ===============================================================================
library(readr)
library(tidyverse)
library(survey)
library(Hmisc)
library(ggplot2)

options(scipen=999)

setwd("/Users/astav/Documents/Employment/Harvard-Bloomberg/Rochester/R/Data/PUMS")

#-------------------------------------------------------------------------------
# Load PUMS Household Data and Select Rochester PUMAs
#-------------------------------------------------------------------------------
pums_hh <- read_csv("psam_h36.csv", guess_max = 12000)
pums_p  <- read_csv("psam_p36.csv", guess_max = 12000)

hh_roc <- pums_hh %>%
  filter(PUMA == "00902" | PUMA == "00903")
p_roc <- pums_p %>%
  filter(PUMA == "00902" | PUMA == "00903")

rm("pums_hh", "pums_p")

#-------------------------------------------------------------------------------
# Merge Datasets
#-------------------------------------------------------------------------------
# Merge household and person datasets
roc=merge(roc_hh,roc_p, by="SERIALNO", all.x = TRUE, suffixes = c(".hh", ".p"))

# Split datasets into individual households
#hh=split(roc,roc$SERIALNO)

#-------------------------------------------------------------------------------
# Generate Variables
#-------------------------------------------------------------------------------
# Separate year and ID number from SERIALNO
roc$year=substr(as.character(roc$SERIALNO),1,4)
roc$id=substr(as.character(roc$SERIALNO),5,25)

# AMI Thresholds
ami30 <- 74000*0.3
ami50 <- 74000*0.5
ami80 <- 74000*0.8
ami100 <- 74000
ami120 <- 74000*1.2

AMI_CAT <- cut(roc$HINCP, breaks = c(0,ami30,ami50,ami80,ami120,10000000), labels = c(1,2,3,4,5), right = TRUE)
roc <- mutate(roc,AMI_CAT)

tapply(roc$PWGTP, list(roc$AMI_CAT), sum)                     # <30% AMI is largest category (28.6%)
prop.table(tapply(roc$PWGTP, list(roc$AMI_CAT), sum))         # Other categories are 16-19% of population each

# Income Quintiles
with(roc, Hmisc::wtd.quantile(HINCP, probs = c(0.2,0.4,0.6,0.8), weights=PWGTP))
INC_CAT <- cut(roc$HINCP, breaks = c(0,16200,30700,50700,82530,1000000), labels = c(1,2,3,4,5), right = TRUE)
roc <- mutate(roc,INC_CAT)

tapply(roc$PWGTP, list(roc$INC_CAT), sum)
prop.table(tapply(roc$PWGTP, list(roc$INC_CAT), sum))

#-------------------------------------------------------------------------------
# Data Analysis
#-------------------------------------------------------------------------------
## Counts ##
summarise(hh_roc, total_units=sum(WGTP))

hh_roc %>%
  filter(VACS==1) %>%
  summarise(for_rent=sum(WGTP))

hh_roc %>%
  filter(is.na(VACS)) %>%
  summarise(occupied=sum(WGTP))

## Weighted Means and Quantiles ##

# Housing Characteristics
with(hh_roc, Hmisc::wtd.mean(VALP, weights=WGTP))
with(hh_roc, Hmisc::wtd.quantile(VALP, weights=WGTP))   # Property value

with(hh_roc, Hmisc::wtd.quantile(RNTP, weights=WGTP))   # Rent paid (listed rent ONLY)
with(hh_roc, Hmisc::wtd.quantile(GRNTP, weights=WGTP))  # Gross rent paid (listed rent + utilities)
with(hh_roc, Hmisc::wtd.quantile(SMOCP, weights=WGTP))  # Selected monthly owner cost (mortgage, taxes, utilities)

# Demographic Characteristics
with(hh_roc, Hmisc::wtd.mean(NP, weights=WGTP))
with(hh_roc, Hmisc::wtd.quantile(NP, weights=WGTP))     # Number of People

## Proportions ##
num_rentals <- filter(hh_roc, (TEN==3) | (VACS==1)) %>% tally(wt=WGTP)
num_vacs <- filter(hh_roc, VACS==1) %>% tally(wt=WGTP)
100*(num_vacs/num_rentals)                              # Vacancy Rate

## Standard Errors ##
own <- filter(hh_roc, VACS==1)
pt.est <- sum(own$WGTP)
rep.names <- paste0('WGTP', 1:80)
rep.ests <- sapply(rep.names, function(n) sum(own[[n]]))
sqrt((4/80) * sum((rep.ests - pt.est)^2))

#-------------------------------------------------------------------------------
# Plots
#-------------------------------------------------------------------------------
# Histogram
ggplot(hh_roc, aes(x=NP, weight = WGTP)) + geom_histogram() + 
  stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust=-1.0)  # Number of Persons in Household

ggplot(hh_roc, aes(x=BDSP, weight = WGTP)) + geom_histogram() + 
  stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust=-1.0)  # Number of Bedrooms in Household

ggplot(hh_roc, aes(x=ACR, weight = WGTP)) + geom_histogram() + 
  stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust=-1.0)  # Lot size (single-family homes)

#-------------------------------------------------------------------------------
# Random
#-------------------------------------------------------------------------------
# Count by Category
hh_roc %>% 
  group_by(ACR) %>% 
  filter(ACR > 0) %>% 
  summarise(count = n())

# Tabulate
tabulate(hh_roc$ACR)

# Quantiles
quantile(hh_roc$VALP, na.rm = TRUE)