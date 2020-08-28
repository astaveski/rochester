# ===============================================================================
# * FILE: pums_import.R
# * PURPOSE: Import PUMS data and create framework for data analysis
# * AUTHORS: Adam Staveski
# * DATE CREATED: June 4, 2020
# * DATE LAST MODIFIED: July 10, 2020
# ===============================================================================
library(readr)
library(tidyverse)
library(survey)
library(Hmisc)
library(ggplot2)

options(scipen=999)

#--------------------------------------
# Select Dataset
#--------------------------------------
pums <- 5             # Which PUMS dataset should be used?
                      # Options: 1 / 5 --> 1-year PUMS / 5-year PUMS

#--------------------------------------
# Select AMI Standard
#--------------------------------------
ami <- 74000          # What number should be used as "Area Median Income"?
                      # Format: 74000 --> $74,000



#===============================================================================
# Data Import and Preparation
#===============================================================================
#--------------------------------------
# Set Working Directory
#--------------------------------------
wd <- paste0("/Users/astav/Documents/Employment/Harvard-Bloomberg/Rochester/R/PUMS_",pums,"_Year/Data/")
setwd(wd)
rm(wd)

#--------------------------------------
# Load PUMS Data
#--------------------------------------
pums_hh <- read_csv("psam_h36.csv", guess_max = 12000, col_types = cols(.default = "?", SERIALNO = "c"))
pums_p <- read_csv("psam_p36.csv", guess_max = 12000, col_types = cols(.default = "?", SERIALNO = "c"))

#--------------------------------------
# Select Relevant PUMAs
#--------------------------------------
roc_hh <- pums_hh %>%
  filter(PUMA == "00902" | PUMA == "00903")
roc_p <- pums_p %>%
  filter(PUMA == "00902" | PUMA == "00903")

rm("pums_hh", "pums_p")

#--------------------------------------
# Save ROC Files For Future Use
#--------------------------------------
save(roc_hh, file = "./roc_hh.Rda")
save(roc_p, file = "./roc_p.Rda")

#--------------------------------------
# Load ROC Files (If Already Created)
#--------------------------------------
load("./roc_hh.Rda")
load("./roc_p.Rda")

#--------------------------------------
# Merge Person and Household Datasets
#--------------------------------------
# Merge household and person datasets
roc <- merge(roc_hh,roc_p, by="SERIALNO", all.x = TRUE, suffixes = c(".hh", ".p"))

#--------------------------------------
# Subset Into Rentals and Renters
#--------------------------------------
rentals <- roc_hh %>%
  filter(GRPIP > 0)

renters <- roc %>%
  filter(GRPIP > 0)



#===============================================================================
# Generate Variables for Analysis
#===============================================================================
#--------------------------------------
# Vector of Replicate Weights
#--------------------------------------
prep.names <- paste0('PWGTP', 1:80)                               # Person-level weights
wrep.names <- paste0('WGTP', 1:80)                                # Household-level weights

#--------------------------------------
# AMI Thresholds [1= <30% AMI, 2= 30-50% AMI, 3= 50-80% AMI, 4= 80-120% AMI, 5= >120% AMI]
#--------------------------------------
# Compute AMI Thresholds
ami30 <- ami*0.3
ami50 <- ami*0.5
ami80 <- ami*0.8
ami120 <- ami*1.2

# Cut Real Household Income Into AMI Buckets
rentals$RHINCP <- rentals$HINCP*(rentals$ADJINC/1000000)
rentals$AMI_CAT <- cut(rentals$RHINCP, breaks = c(0,ami30,ami50,ami80,ami120,15000000), labels = c(1,2,3,4,5), right = TRUE)

# Summary Statistics
tapply(rentals$WGTP, list(rentals$AMI_CAT), sum)
prop.table(tapply(rentals$WGTP, list(rentals$AMI_CAT), sum))      # <30% AMI is largest category (44.8% of renter households)

# Standard Errors
pt.est <- sum(rentals$WGTP*(ifelse(rentals$AMI_CAT == 1,1,0)))    # Point Estimate:           23,455 <30% AMI renter households
rep.ests <- sapply(wrep.names, function(n) 
  sum(rentals[[n]]*(ifelse(rentals$AMI_CAT == 1,1,0))))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2))                   # Standard Error:           628 renter households
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se))                     # 95% Confidence Interval:  [22,225 -- 24,685]

#--------------------------------------
# Household Income Quintiles: [1= 0-20%, 2= 20-40%, 3= 40-60%, 4= 60-80%, 5=80-100%]
#--------------------------------------
# Cut Real Household Income Into Income Quintiles
with(rentals, Hmisc::wtd.quantile(RHINCP, probs = c(0.2,0.4,0.6,0.8), weights=WGTP))
rentals$INC_CAT <- cut(rentals$RHINCP, breaks = c(-1,10400.17,19700.38,32334.32,54909.86,1500000), labels = c(1,2,3,4,5), right = TRUE)

# Summary Statistics
tapply(rentals$WGTP, list(rentals$INC_CAT), sum)

# Standard Errors
pt.est <- sum(rentals$WGTP*(ifelse(rentals$INC_CAT == 1,1,0)))    # Point Estimate:           10,403 lowest quintile households
rep.ests <- sapply(wrep.names, function(n) 
  sum(rentals[[n]]*(ifelse(rentals$INC_CAT == 1,1,0))))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2))                   # Standard Error:           448 renter households
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se))                     # 95% Confidence Interval:  [9,525 -- 11,281]

#--------------------------------------
# Hispanic: [0=Not Hispanic, 1=Hispanic]
#--------------------------------------
# Create Hispanic Categories
renters <- renters %>%
  mutate(HISP_CAT = ifelse(HISP=="01",0,1))

# Summary Statistics
tapply(renters$PWGTP, list(renters$HISP_CAT), sum)
prop.table(tapply(renters$PWGTP, list(renters$HISP_CAT), sum))    # 20.6% of renters are Hispanic

# Standard Errors
pt.est <- sum(renters$PWGTP*renters$HISP_CAT)                     # Point Estimate:           24,344 renters
rep.ests <- sapply(prep.names, function(n) 
  sum(renters[[n]]*renters$HISP_CAT))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2))                   # Standard Error:           962 renters
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se))                     # 95% Confidence Interval:  [22,458 -- 26,230]

#--------------------------------------
# Race: [1=White, 2=Black, 3=Hispanic, 4=Asian, 5=Other]
#--------------------------------------
# Create Race Categories
renters <- renters %>%
  mutate(RACE_CAT = 0) %>%
  mutate(RACE_CAT = ifelse(RAC1P=="1",1,RACE_CAT)) %>%
  mutate(RACE_CAT = ifelse(RAC1P=="2",2,RACE_CAT)) %>%
  mutate(RACE_CAT = ifelse(RAC1P=="6",4,RACE_CAT)) %>%
  mutate(RACE_CAT = ifelse(RAC1P==3 | RAC1P==4 | RAC1P==5 | RAC1P > 6,5,RACE_CAT)) %>%
  mutate(RACE_CAT = ifelse(HISP_CAT=="1",3,RACE_CAT))

# Summary Statistics
tapply(renters$PWGTP, list(renters$RACE_CAT), sum)                # 28.9% of renters are white
prop.table(tapply(renters$PWGTP, list(renters$RACE_CAT), sum))    # 44.4% of renters are black

# Standard Errors
pt.est <- sum(renters$PWGTP*(ifelse(renters$RACE_CAT == 1,1,0)))  # Point Estimate:           34,157 white renters
rep.ests <- sapply(prep.names, function(n) 
  sum(renters[[n]]*(ifelse(renters$RACE_CAT == 1,1,0))))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2))                   # Standard Error:           917 renters
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se))                     # 95% Confidence Interval:  [32,360 -- 35,954]

#--------------------------------------
# College Student: [0=Non-Student, 1=Student]
#--------------------------------------
# Create College Categories
renters <- renters %>%
  mutate(COLLEGE = ifelse(SCHG== "15" | SCHG == "16",1,0)) %>%
  mutate(COLLEGE = ifelse(is.na(SCHG),0,COLLEGE))

# Summary Statistics
tapply(renters$PWGTP, list(renters$COLLEGE), sum)
prop.table(tapply(renters$PWGTP, list(renters$COLLEGE), sum))     # 8.9% of renters are in college

# Standard Errors
pt.est <- sum(renters$PWGTP*renters$COLLEGE)                      # Point Estimate:           10,491 renters
rep.ests <- sapply(prep.names, function(n) 
  sum(renters[[n]]*renters$COLLEGE))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2))                   # Standard Error:           713 renters
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se))                     # 95% Confidence Interval:  [9,093 -- 11,889]

#--------------------------------------
# Age Buckets: [1= 0-17, 2= 18-39, 3= 40-64, 4= 65+]
#--------------------------------------
# Create Age Categories
renters <- renters %>%
  mutate(AGE_CAT = cut(renters$AGEP, breaks = c(-1,18,40,65,100), labels = c(1,2,3,4), right = FALSE))

# Summary Statistics
tapply(renters$PWGTP, list(renters$AGE_CAT), sum)                 # 41.4% of renters are ages 18-39
prop.table(tapply(renters$PWGTP, list(renters$AGE_CAT), sum))     # 28.1% of renters are ages 0-17

# Standard Errors
pt.est <- sum(renters$PWGTP*(ifelse(renters$AGE_CAT == 1,1,0)))   # Point Estimate:           33,269 ages 0-17 renters
rep.ests <- sapply(prep.names, function(n) 
  sum(renters[[n]]*(ifelse(renters$AGE_CAT == 1,1,0))))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2))                   # Standard Error:           776 renters
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se))                     # 95% Confidence Interval:  [31,748 -- 34,790]

#--------------------------------------
# Citizenship Status: [1=Citizen at Birth, 2=Naturalized Citizen, 3=Non-Citizen]
#--------------------------------------
# Create Citizenship Categories
renters <- renters %>%
  mutate(CIT_CAT = ifelse(CIT==1 | CIT==2 | CIT==3,1,0)) %>%
  mutate(CIT_CAT = ifelse(CIT==4,2,CIT_CAT)) %>%
  mutate(CIT_CAT = ifelse(CIT==5,3,CIT_CAT))

# Summary Statistics
tapply(renters$PWGTP, list(renters$CIT_CAT), sum)                 # 91.9% of renters were U.S. citizens at birth
prop.table(tapply(renters$PWGTP, list(renters$CIT_CAT), sum))     # 5.3% are not U.S. citizens

# Standard Error
pt.est <- sum(renters$PWGTP*(ifelse(renters$CIT_CAT == 3,1,0)))   # Point Estimate:           6,236 non-citizens
rep.ests <- sapply(prep.names, function(n) 
  sum(renters[[n]]*(ifelse(renters$CIT_CAT == 3,1,0))))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2))                   # Standard Error:           637 renters
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se))                     # 95% Confidence Interval:  [4,988 -- 7,484]

#--------------------------------------
# Household Type: [1=Married Couple, 2=Unmarried Couple, 3=Other Family Household, 4=Non-Family & Non-Partner]
#--------------------------------------
rentals <- rentals %>%
  mutate(HHT_CAT = ifelse(HHT==1,1,0)) %>%
  mutate(HHT_CAT = ifelse(HHT==2 | HHT==3,3,HHT_CAT)) %>%
  mutate(HHT_CAT = ifelse(HHT>3,4,HHT_CAT)) %>%
  mutate(HHT_CAT = ifelse(PARTNER>0,2,HHT_CAT))

# Summary Statistics                                             # 50.8% of renter households are non-family and non-partner households
tapply(rentals$WGTP, list(rentals$HHT_CAT), sum)                 # 10.0% of renter households are married couple households
prop.table(tapply(rentals$WGTP, list(rentals$HHT_CAT), sum))     # 10.2% of renter households are unmarried partner households

# Standard Error
pt.est <- sum(rentals$WGTP*(ifelse(rentals$HHT_CAT == 1,1,0)))   # Point Estimate:           5,233 married couple households
rep.ests <- sapply(wrep.names, function(n) 
  sum(rentals[[n]]*(ifelse(rentals$HHT_CAT == 1,1,0))))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2))                   # Standard Error:          316 rental households
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se))                     # 95% Confidence Interval: [4,613 -- 5,853]

#--------------------------------------
# Child Presence: [1=Child Under Six, 2=Child Not Under Six, 3=No Child]
#--------------------------------------
rentals <- rentals %>%
  mutate(KID_CAT = ifelse(HUPAC==1 | HUPAC==3,1,0)) %>%
  mutate(KID_CAT = ifelse(HUPAC==2,2,KID_CAT)) %>%
  mutate(KID_CAT = ifelse(HUPAC==4,3,KID_CAT))

# Summary Statistics
tapply(rentals$WGTP, list(rentals$KID_CAT), sum)                 # 14.9% of renter households have a child under six
prop.table(tapply(rentals$WGTP, list(rentals$KID_CAT), sum))     # 69.4% of renter households do not have children

# Standard Error
pt.est <- sum(rentals$WGTP*(ifelse(rentals$KID_CAT == 1,1,0)))   # Point Estimate:           7,783 households with child under six
rep.ests <- sapply(wrep.names, function(n) 
  sum(rentals[[n]]*(ifelse(rentals$KID_CAT == 1,1,0))))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2))                  # Standard Error:          449 rental households
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se))                    # 95% Confidence Interval: [6,904 -- 8,662]

#--------------------------------------
# Occupation: [27 OCC Categories]
#--------------------------------------
renters <- renters %>%
  mutate(JOB_CAT = ifelse(as.numeric(OCCP)>=0010 & as.numeric(OCCP)<=0440,1,0)) %>%          # Managerial
  mutate(JOB_CAT = ifelse(as.numeric(OCCP)>=0500 & as.numeric(OCCP)<=0750,2,JOB_CAT)) %>%    # Business
  mutate(JOB_CAT = ifelse(as.numeric(OCCP)>=0800 & as.numeric(OCCP)<=0960,3,JOB_CAT)) %>%    # Finance
  mutate(JOB_CAT = ifelse(as.numeric(OCCP)>=1005 & as.numeric(OCCP)<=1240,4,JOB_CAT)) %>%    # Communication
  mutate(JOB_CAT = ifelse(as.numeric(OCCP)>=1305 & as.numeric(OCCP)<=1560,5,JOB_CAT)) %>%    # Engineering
  mutate(JOB_CAT = ifelse(as.numeric(OCCP)>=1600 & as.numeric(OCCP)<=1980,6,JOB_CAT)) %>%    # Sciences
  mutate(JOB_CAT = ifelse(as.numeric(OCCP)>=2001 & as.numeric(OCCP)<=2060,7,JOB_CAT)) %>%    # Counseling
  mutate(JOB_CAT = ifelse(as.numeric(OCCP)>=2100 & as.numeric(OCCP)<=2180,8,JOB_CAT)) %>%    # Legal
  mutate(JOB_CAT = ifelse(as.numeric(OCCP)>=2205 & as.numeric(OCCP)<=2555,9,JOB_CAT)) %>%    # Education
  mutate(JOB_CAT = ifelse(as.numeric(OCCP)>=2600 & as.numeric(OCCP)<=2920,10,JOB_CAT)) %>%   # Entertainment
  mutate(JOB_CAT = ifelse(as.numeric(OCCP)>=3000 & as.numeric(OCCP)<=3550,11,JOB_CAT)) %>%   # Medical
  mutate(JOB_CAT = ifelse(as.numeric(OCCP)>=3601 & as.numeric(OCCP)<=3655,12,JOB_CAT)) %>%   # Health Services
  mutate(JOB_CAT = ifelse(as.numeric(OCCP)>=3700 & as.numeric(OCCP)<=3960,13,JOB_CAT)) %>%   # Protective Services
  mutate(JOB_CAT = ifelse(as.numeric(OCCP)>=4000 & as.numeric(OCCP)<=4160,14,JOB_CAT)) %>%   # Eatery
  mutate(JOB_CAT = ifelse(as.numeric(OCCP)>=4200 & as.numeric(OCCP)<=4255,15,JOB_CAT)) %>%   # Cleaning
  mutate(JOB_CAT = ifelse(as.numeric(OCCP)>=4330 & as.numeric(OCCP)<=4655,16,JOB_CAT)) %>%   # Personal Care
  mutate(JOB_CAT = ifelse(as.numeric(OCCP)>=4700 & as.numeric(OCCP)<=4965,17,JOB_CAT)) %>%   # Sales
  mutate(JOB_CAT = ifelse(as.numeric(OCCP)>=5000 & as.numeric(OCCP)<=5940,18,JOB_CAT)) %>%   # Office
  mutate(JOB_CAT = ifelse(as.numeric(OCCP)>=6005 & as.numeric(OCCP)<=6130,19,JOB_CAT)) %>%   # Fishing/Farming/Forestry
  mutate(JOB_CAT = ifelse(as.numeric(OCCP)>=6200 & as.numeric(OCCP)<=6765,20,JOB_CAT)) %>%   # Construction
  mutate(JOB_CAT = ifelse(as.numeric(OCCP)>=6800 & as.numeric(OCCP)<=6950,21,JOB_CAT)) %>%   # Extraction
  mutate(JOB_CAT = ifelse(as.numeric(OCCP)>=7000 & as.numeric(OCCP)<=7640,22,JOB_CAT)) %>%   # Repair
  mutate(JOB_CAT = ifelse(as.numeric(OCCP)>=7700 & as.numeric(OCCP)<=8990,23,JOB_CAT)) %>%   # Production
  mutate(JOB_CAT = ifelse(as.numeric(OCCP)>=9005 & as.numeric(OCCP)<=9760,24,JOB_CAT)) %>%   # Transportation
  mutate(JOB_CAT = ifelse(as.numeric(OCCP)>=9800 & as.numeric(OCCP)<=9830,25,JOB_CAT)) %>%   # Military
  mutate(JOB_CAT = ifelse(as.numeric(OCCP)==9920,26,JOB_CAT)) %>%                            # Unemployed
  mutate(JOB_CAT = ifelse(is.na(OCCP)==TRUE,27,JOB_CAT))                                     # Not in Labor Force

# Summary Statistics
tapply(renters$PWGTP, list(renters$JOB_CAT), sum)
prop.table(tapply(renters$PWGTP, list(renters$JOB_CAT), sum))



#===============================================================================
# High-Level Analysis of Renter Households
#===============================================================================
#--------------------------------------
# Summary Statistics: Rental Households
#--------------------------------------
pt.est <- sum(rentals$WGTP)                                       # Point Estimate:           52,366 rental households
wrep.names <- paste0('WGTP', 1:80)
rep.ests <- sapply(wrep.names, function(n) sum(rentals[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2))                   # Standard Errors:           771 rental households
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se))                     # 95% Confidence Interval:  [50,854 -- 53,878]

#--------------------------------------
# Summary Statistics: Renters
#--------------------------------------
pt.est <- sum(renters$PWGTP)                                      # Point Estimate:           118,357 renters
prep.names <- paste0('PWGTP', 1:80)
rep.ests <- sapply(prep.names, function(n) sum(renters[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2))                   # Standard Errors:           1,651 renters
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se))                     # 95% Confidence Interval:  [115,120 -- 121,594]



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
# General Framework for Computing Standard Errors
#===============================================================================
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
# Standard Errors of a Number (Household-Level)
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
# Standard Error Calculator -- One Variable
#===============================================================================
#--------------------------------------
# User Specifications
#--------------------------------------
var <- "COLLEGE"                      # Select a variable (e.g. "SEX" or "AMI_CAT")
val <- "1"                            # Select a value of the above variable (e.g. "1" or "2")
wgt <- "PWGTP"                        # Select person-level or household-level weights ("PWGTP" or "WGTP")
dta <- "renters"                      # Select a dataset to use (e.g. "rentals_crowd" or "renters_xcrowd")

#--------------------------------------
# Generate Standard Errors
#--------------------------------------
# Generate point estimate
se.est <- sum(ifelse(get(dta)[[var]]==val,get(dta)[[wgt]],0))               # Point Estimate

# Select appropriate replicate weights
if (wgt == "WGTP") {
  rep.names <- wrep.names
} else if (wgt == "PWGTP") {
  rep.names <- prep.names
}

# Compute standard errors
se.rep.ests <- sapply(rep.names, function(n) 
  sum(ifelse(get(dta)[[var]]==val,get(dta)[[n]],0)))
se <- sqrt((4/80) * sum((se.rep.ests - se.est)^2))                          # Standard Error
se_ci90 <- c(se.est-(1.64*se), se.est+(1.64*se))                            # 90% Confidence Interval
se_ci95 <- c(se.est-(1.96*se), se.est+(1.96*se))                            # 95% Confidence Interval

row_names <- var
col_names <- c("Point Estimate", "Standard Error", "90% CI", "95% CI")
se_table <- matrix(c(se.est, se, se_ci90, se_ci95), nrow = 1, ncol = 4, dimnames = list(row_names, col_names))



#===============================================================================
# Standard Error Calculator -- Loop
#===============================================================================
#--------------------------------------
# User Specifications
#--------------------------------------
var <- "HHT_CAT"                      # Select a variable (e.g. "SEX" or "AMI_CAT")
cat <- "4"                            # How many categories of this variable are there? (e.g. "2" or "4")
wgt <- "WGTP"                         # Select person-level or household-level weights ("PWGTP" or "WGTP")
dta <- "rentals_crowd"                # Select a dataset to use (e.g. "rentals_crowd" or "renters_xcrowd")

#--------------------------------------
# Generate Standard Errors
#--------------------------------------
# Initialize vectors
row_names <- vector()
col_names <- c("Point Estimate","Standard Error","95% CI Low", "95% CI High")
list <- vector()

# Prepare estimates, standard errors, and confidence intervals
for (val in 1:cat) {
  # Prepare unique names
  est <- paste0(var,val)
  est.se <- paste0(var,val,"_se")
  est.ci95 <- paste0(var,val,"_ci95")
  row_names <- c(row_names, est)
  
  # Copmute point estimate
  assign(est, sum(ifelse(get(dta)[[var]]==val,get(dta)[[wgt]],0)))
  
  # Select appropriate replicate weights
  if (wgt == "WGTP") {
    rep.names <- wrep.names
  } else if (wgt == "PWGTP") {
    rep.names <- prep.names
  }
  
  # Compute replicate weight estimates
  rep.ests <- sapply(rep.names, function(n) 
    sum(ifelse(get(dta)[[var]]==val,get(dta)[[n]],0)))
  
  # Compute standard error
  assign(est.se, sqrt((4/80) * sum((rep.ests - get(est))^2)))
  
  # Compute 95% confidence interval
  assign(est.ci95, c(get(est)-(1.96*get(est.se)), get(est)+(1.96*get(est.se))))
  
  # Combine in list
  list <- c(list, get(est), get(est.se), get(est.ci95))
}

#--------------------------------------
# Generate Table
#--------------------------------------
se_table <- matrix(list, nrow = as.numeric(cat), ncol = 4, dimnames = list(row_names, col_names), byrow = TRUE)



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



#===============================================================================
# Export to .CSV
#===============================================================================
write.csv(rentals, file = "./Data_Subsets/rentals.csv")
