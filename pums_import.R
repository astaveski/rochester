# ===============================================================================
# * FILE: pums_import.R
# * PURPOSE: Import PUMS data from 2018
# * AUTHORS: Adam Staveski
# * DATE CREATED: June 4, 2020
# * DATE LAST MODIFIED: June 26, 2020
# ===============================================================================
library(readr)
library(tidyverse)
library(survey)
library(Hmisc)
library(ggplot2)

options(scipen=999)

setwd("/Users/astav/Documents/Employment/Harvard-Bloomberg/Rochester/R/Data/PUMS")

#===============================================================================
# Data Import and Preparation
#===============================================================================
#--------------------------------------
# Load PUMS Data
#--------------------------------------
pums_hh <- read_csv("psam_h36.csv", guess_max = 12000)
pums_p  <- read_csv("psam_p36.csv", guess_max = 12000)

#--------------------------------------
# Select Relevant PUMAs
#--------------------------------------
roc_hh <- pums_hh %>%
  filter(PUMA == "00902" | PUMA == "00903")
roc_p <- pums_p %>%
  filter(PUMA == "00902" | PUMA == "00903")

nys_hh <- pums_hh %>%
  filter(ST == 36)
nys_p <- pums_p %>%
  filter(ST == 36)

rm("pums_hh", "pums_p")

#--------------------------------------
# Merge Person and Household Datasets
#--------------------------------------
# Merge household and person datasets
roc <- merge(roc_hh,roc_p, by="SERIALNO", all.x = TRUE, suffixes = c(".hh", ".p"))



#===============================================================================
# Generate Variables for Analysis
#===============================================================================
#--------------------------------------
# AMI Thresholds
#--------------------------------------
ami30 <- ami*0.3
ami50 <- ami*0.5
ami80 <- ami*0.8
ami120 <- ami*1.2

AMI_CAT <- cut(rentals$HINCP, breaks = c(0,ami30,ami50,ami80,ami120,10000000), labels = c(1,2,3,4,5), right = TRUE)
rentals <- mutate(rentals,AMI_CAT)

# Summary Statistics
tapply(rentals$WGTP, list(rentals$AMI_CAT), sum)
prop.table(tapply(rentals$WGTP, list(rentals$AMI_CAT), sum))      # <30% AMI is largest category (44.2% of renter households)

# Standard Errors
pt.est <- sum(rentals$WGTP*(ifelse(rentals$AMI_CAT == 1,1,0)))    # Point Estimate:           22,822 <30% AMI renter households
rep.ests <- sapply(wrep.names, function(n) sum(rentals[[n]]*(ifelse(rentals$AMI_CAT == 1,1,0))))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2))                   # Standard Errors:           1,685 renter households
ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     # 90% Confidence Interval:  [20,059 -- 25,585]

#--------------------------------------
# Household Income Quintiles
#--------------------------------------
with(rentals, Hmisc::wtd.quantile(HINCP, probs = c(0.2,0.4,0.6,0.8), weights=WGTP))
INC_CAT <- cut(rentals$HINCP, breaks = c(-1,10000,20000,35000,61000,1000000), labels = c(1,2,3,4,5), right = TRUE)
rentals <- mutate(rentals,INC_CAT)

# Summary Statistics
tapply(rentals$WGTP, list(rentals$INC_CAT), sum)

# Standard Errors
pt.est <- sum(rentals$WGTP*(ifelse(rentals$INC_CAT == 1,1,0)))    # Point Estimate:           11,191 lowest quintile households
rep.ests <- sapply(wrep.names, function(n) sum(rentals[[n]]*(ifelse(rentals$INC_CAT == 1,1,0))))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2))                   # Standard Errors:           1,208 renter households
ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     # 90% Confidence Interval:  [9,210 -- 13,172]

#--------------------------------------
# Hispanic
#--------------------------------------
# Create Hispanic Categories
renters <- renters %>%
  mutate(HISP_CAT = ifelse(HISP=="01",0,1))

# Summary Statistics
tapply(renters$PWGTP, list(renters$HISP_CAT), sum)
prop.table(tapply(renters$PWGTP, list(renters$HISP_CAT), sum))    # 21.6% of renters are Hispanic

# Standard Errors
pt.est <- sum(renters$PWGTP*renters$HISP_CAT)                     # Point Estimate:           24,832 renters
rep.ests <- sapply(prep.names, function(n) sum(renters[[n]]*renters$HISP_CAT))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2))                   # Standard Errors:           2,397 renters
ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     # 90% Confidence Interval:  [20,901 -- 28,763]

#--------------------------------------
# Race
#--------------------------------------
# Create Race Categories
renters <- renters %>%
  mutate(RACE_CAT = ifelse(HISP=="01",RAC1P,10))

# Summary Statistics
tapply(renters$PWGTP, list(renters$RACE_CAT), sum)                # 27.8% of renters are white
prop.table(tapply(renters$PWGTP, list(renters$RACE_CAT), sum))    # 42.7% of renters are black

# Standard Errors
pt.est <- sum(renters$PWGTP*(ifelse(renters$RACE_CAT == 1,1,0)))  # Point Estimate:           31,947 white renters
rep.ests <- sapply(prep.names, function(n) sum(renters[[n]]*(ifelse(renters$RACE_CAT == 1,1,0))))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2))                   # Standard Errors:           2,305 renters
ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     # 90% Confidence Interval:  [28,167 -- 35,727]

#--------------------------------------
# College Student
#--------------------------------------
# Create College Categories
renters <- renters %>%
  mutate(COLLEGE = ifelse(SCHG== "15" | SCHG == "16",1,0)) %>%
  mutate(COLLEGE = ifelse(is.na(SCHG),0,COLLEGE))

# Summary Statistics
tapply(renters$PWGTP, list(renters$COLLEGE), sum)
prop.table(tapply(renters$PWGTP, list(renters$COLLEGE), sum))     # 10.6% of renters are in college

# Standard Errors
pt.est <- sum(renters$PWGTP*renters$COLLEGE)                      # Point Estimate:           12,144 renters
rep.ests <- sapply(prep.names, function(n) sum(renters[[n]]*renters$COLLEGE))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2))                   # Standard Errors:           1,568 renters
ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     # 90% Confidence Interval:  [9,573 -- 14,715]

#--------------------------------------
# Age Buckets
#--------------------------------------
# Create Age Categories
renters <- renters %>%
  mutate(AGE_CAT = cut(renters$AGEP, breaks = c(-1,18,40,65,100), labels = c(1,2,3,4), right = FALSE))

# Summary Statistics
tapply(renters$PWGTP, list(renters$AGE_CAT), sum)                 # 41.3% of renters are ages 18-39
prop.table(tapply(renters$PWGTP, list(renters$AGE_CAT), sum))     # 26.8% of renters are ages 0-17

# Standard Errors
pt.est <- sum(renters$PWGTP*(ifelse(renters$AGE_CAT == 1,1,0)))   # Point Estimate:           30,873 ages 0-17 renters
rep.ests <- sapply(prep.names, function(n) sum(renters[[n]]*(ifelse(renters$AGE_CAT == 1,1,0))))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2))                   # Standard Errors:           2,125 renters
ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     # 90% Confidence Interval:  [27,389 -- 34,357]

#--------------------------------------
# Citizenship Status
#--------------------------------------
# Create Citizenship Categories
renters <- renters %>%
  mutate(CIT_CAT = ifelse(CIT==1 | CIT==2 | CIT==3,1,0)) %>%
  mutate(CIT_CAT = ifelse(CIT==4,2,CIT_CAT)) %>%
  mutate(CIT_CAT = ifelse(CIT==5,3,CIT_CAT))

# Summary Statistics
tapply(renters$PWGTP, list(renters$CIT_CAT), sum)                 # 88.4% of renters were U.S. citizens at birth
prop.table(tapply(renters$PWGTP, list(renters$CIT_CAT), sum))     # 8.0% are not U.S. citizens

# Standard Errors
pt.est <- sum(renters$PWGTP*(ifelse(renters$CIT_CAT == 3,1,0)))   # Point Estimate:           9,248 non-citizens
rep.ests <- sapply(prep.names, function(n) sum(renters[[n]]*(ifelse(renters$CIT_CAT == 3,1,0))))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2))                   # Standard Errors:           2,235 renters
ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     # 90% Confidence Interval:  [5,583 -- 12,913]



#===============================================================================
# Narrow Analysis to Renter Households
#===============================================================================
#--------------------------------------
# Create Renter and Rental Datasets
#--------------------------------------
rentals <- roc_hh %>%
  filter(GRPIP > 0)

renters <- roc %>%
  filter(GRPIP > 0)

#--------------------------------------
# Summary Statistics: Rental Households
#--------------------------------------
pt.est <- sum(rentals$WGTP)                                       # Point Estimate:           51,655 rental households
wrep.names <- paste0('WGTP', 1:80)
rep.ests <- sapply(wrep.names, function(n) sum(rentals[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2))                   # Standard Errors:           2,097 rental households
ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     # 90% Confidence Interval:  [48,216 -- 55,094]
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se))                     # 95% Confidence Interval:  [47,545 -- 55,765]

#--------------------------------------
# Summary Statistics: Renters
#--------------------------------------
pt.est <- sum(renters$PWGTP)                                      # Point Estimate:           115,048 renters
prep.names <- paste0('PWGTP', 1:80)
rep.ests <- sapply(prep.names, function(n) sum(renters[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2))                   # Standard Errors:           4,368 renters
ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     # 90% Confidence Interval:  [107,884 -- 122,212]
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se))                     # 95% Confidence Interval:  [106,486 -- 123,610]



#===============================================================================
# Framework for Data Analysis
#===============================================================================
#--------------------------------------
# Counts
#--------------------------------------
summarise(roc_hh, total_units=sum(WGTP))

roc_hh %>%
  filter(VACS==1) %>%
  summarise(for_rent=sum(WGTP))

roc_hh %>%
  filter(is.na(VACS)) %>%
  summarise(occupied=sum(WGTP))

#--------------------------------------
# Weighted Means and Quantiles
#--------------------------------------
# Housing Characteristics
with(roc_hh, Hmisc::wtd.mean(VALP, weights=WGTP))
with(roc_hh, Hmisc::wtd.quantile(VALP, weights=WGTP))   # Property value

with(roc_hh, Hmisc::wtd.quantile(RNTP, weights=WGTP))   # Rent paid (listed rent ONLY)
with(roc_hh, Hmisc::wtd.quantile(GRNTP, weights=WGTP))  # Gross rent paid (listed rent + utilities)
with(roc_hh, Hmisc::wtd.quantile(SMOCP, weights=WGTP))  # Selected monthly owner cost (mortgage, taxes, utilities)

# Demographic Characteristics
with(roc_hh, Hmisc::wtd.mean(NP, weights=WGTP))
with(roc_hh, Hmisc::wtd.quantile(NP, weights=WGTP))     # Number of People

#--------------------------------------
# Weighted Proportions
#--------------------------------------
num_rentals <- filter(roc_hh, (TEN==3) | (VACS==1)) %>% tally(wt=WGTP)
num_vacs <- filter(roc_hh, VACS==1) %>% tally(wt=WGTP)
100*(num_vacs/num_rentals)                              # Vacancy Rate



#===============================================================================
# Framework for Computing Standard Errorss
#===============================================================================
prep.names <- paste0('PWGTP', 1:80)
wrep.names <- paste0('WGTP', 1:80)

#--------------------------------------
# Standard Errors of a Number (Person-Level)
#--------------------------------------
yopro <- roc_p %>%
  filter(AGEP >= 25 & AGEP <= 34 & RAC1P == 1)

pt.est <- sum(yopro$PWGTP)                                        # Point Estimate:
rep.names <- paste0('PWGTP', 1:80)
rep.ests <- sapply(rep.names, function(n) sum(yopro[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2))                   # Standard Error:
ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     # 90% Confidence Interval:
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se))                     # 95% Confidence Interval:

#--------------------------------------
# Standard Errors of a Number (HH-Level)
#--------------------------------------
poor <- roc_hh %>%
  filter(HINCP < 35000)

pt.est <- sum(poor$WGTP)                                          # Point Estimate:
rep.names <- paste0('WGTP', 1:80)
rep.ests <- sapply(rep.names, function(n) sum(poor[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2))                   # Standard Error:
ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     # 90% Confidence Interval:
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se))                     # 95% Confidence Interval:

#--------------------------------------
# Standard Errors of a Proportion
#--------------------------------------
# Where X is a subset of Y...
# SE(X/Y) = 1/Y * sqrt( SE(X)^2 - (X^2/Y^2)* SE(Y)^2 )
# X is a point estimate of X
# Y is a point estimate of Y
# SE(X) is the Standard Errors of the point estimate of X
# SE(Y) is the Standard Errors of the point estimate of Y

prop <- (pt.est1/pt.est2)
se_prop <- (1/pt.est2) * sqrt(se1^2 - (prop^2*se2^2))
se_ci90 <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))



#===============================================================================
# Graphs and Charts
#===============================================================================
#--------------------------------------
# Histogram
#--------------------------------------
ggplot(roc_hh, aes(x=NP, weight = WGTP)) + geom_histogram() + 
  stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust=-1.0)  # Number of Persons in Household

ggplot(roc_hh, aes(x=BDSP, weight = WGTP)) + geom_histogram() + 
  stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust=-1.0)  # Number of Bedrooms in Household

ggplot(roc_hh, aes(x=ACR, weight = WGTP)) + geom_histogram() + 
  stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust=-1.0)  # Lot size (single-family homes)

