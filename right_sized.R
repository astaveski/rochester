# ===============================================================================
# * FILE: right_sized.R
# * PURPOSE: Determine the extent of overcrowding in Rochester, NY
# * AUTHORS: Adam Staveski
# * DATE CREATED: June 10, 2020
# * DATE LAST MODIFIED: July 2, 2020
# ===============================================================================
library(readr)
library(tidyverse)
library(survey)
library(Hmisc)
library(ggplot2)

#--------------------------------------
# Select Dataset
#--------------------------------------
pums <- 5             # Which PUMS dataset should be used?
                      # Options: 1 / 5 --> 1-year PUMS / 5-year PUMS

#--------------------------------------
# Select standards for overcrowding
#--------------------------------------
bdrm  <- 2            # Options: 2 / 1.75 / 1.5 people per bedroom
rm    <- 1            # Options: 1.5 / 1 / 0.5 people per room

#--------------------------------------
# Select AMI number for analysis
#--------------------------------------
ami <- 74000          # Options: 74000 (Actual AMI) / 58900 (Monroe AMI) / 35000 (Rochester AMI)



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
# Load Rochester PUMS Data
#--------------------------------------
load("./roc_hh.Rda")
load("./roc_p.Rda")

#--------------------------------------
# Merge Household and Person-Level Data
#--------------------------------------
roc <- merge(roc_hh,roc_p, by="SERIALNO", all.x = TRUE, suffixes = c(".hh", ".p"))

#--------------------------------------
# Narrow Focus to Rental Units
#--------------------------------------
rentals <- roc_hh %>%
  filter(GRPIP > 0)

renters <- roc %>%
  filter(GRPIP > 0)



#===============================================================================
# Generate Variables for Analysis
#===============================================================================
prep.names <- paste0('PWGTP', 1:80)
wrep.names <- paste0('WGTP', 1:80)

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
rentals$AMI_CAT <- cut(rentals$RHINCP, breaks = c(0,ami30,ami50,ami80,ami120,1500000), labels = c(1,2,3,4,5), right = TRUE)

# Summary Statistics
tapply(rentals$WGTP, list(rentals$AMI_CAT), sum)
prop.table(tapply(rentals$WGTP, list(rentals$AMI_CAT), sum))      # <30% AMI is largest category (44.8% of renter households)

#--------------------------------------
# Household Income Quintiles: [1= 0-20%, 2= 20-40%, 3= 40-60%, 4= 60-80%, 5=80-100%]
#--------------------------------------
# Cut Real Household Income Into Income Quintiles
quintiles <- with(rentals, Hmisc::wtd.quantile(RHINCP, probs = c(0.2,0.4,0.6,0.8), weights=WGTP))
quintiles <- c(0, as.numeric(quintiles), 1500000)
rentals$INC_CAT <- cut(rentals$RHINCP, breaks = quintiles, labels = c(1,2,3,4,5), right = TRUE)

# Summary Statistics
tapply(rentals$WGTP, list(rentals$INC_CAT), sum)

#--------------------------------------
# Hispanic: [0= Not Hispanic, 1= Hispanic]
#--------------------------------------
# Create Hispanic Categories
renters <- renters %>%
  mutate(HISP_CAT = ifelse(HISP=="01",0,1))

# Summary Statistics
tapply(renters$PWGTP, list(renters$HISP_CAT), sum)
prop.table(tapply(renters$PWGTP, list(renters$HISP_CAT), sum))    # 20.6% of renters are Hispanic

#--------------------------------------
# Race: [1=White,2=Black,3=Native American,6=Asian,10=Hispanic]
#--------------------------------------
# Create Race Categories
renters <- renters %>%
  mutate(RACE_CAT = ifelse(HISP=="01",RAC1P,10))

# Summary Statistics
tapply(renters$PWGTP, list(renters$RACE_CAT), sum)                # 28.9% of renters are white
prop.table(tapply(renters$PWGTP, list(renters$RACE_CAT), sum))    # 44.4% of renters are black

#--------------------------------------
# College Student: [0= Non-Student, 1= Student]
#--------------------------------------
# Create College Categories
renters <- renters %>%
  mutate(COLLEGE = ifelse(SCHG== "15" | SCHG == "16",1,0)) %>%
  mutate(COLLEGE = ifelse(is.na(SCHG),0,COLLEGE))

# Summary Statistics
tapply(renters$PWGTP, list(renters$COLLEGE), sum)
prop.table(tapply(renters$PWGTP, list(renters$COLLEGE), sum))     # 8.9% of renters are in college

#--------------------------------------
# Age Buckets: [1= 0-17, 2= 18-39, 3= 40-64, 4= 65+]
#--------------------------------------
# Create Age Categories
renters <- renters %>%
  mutate(AGE_CAT = cut(renters$AGEP, breaks = c(-1,18,40,65,100), labels = c(1,2,3,4), right = FALSE))

# Summary Statistics
tapply(renters$PWGTP, list(renters$AGE_CAT), sum)                 # 41.4% of renters are ages 18-39
prop.table(tapply(renters$PWGTP, list(renters$AGE_CAT), sum))     # 28.1% of renters are ages 0-17

#--------------------------------------
# Citizenship Status: [1= Citizen at Birth, 2= Naturalized Citizen, 3= Non-Citizen]
#--------------------------------------
# Create Citizenship Categories
renters <- renters %>%
  mutate(CIT_CAT = ifelse(CIT==1 | CIT==2 | CIT==3,1,0)) %>%
  mutate(CIT_CAT = ifelse(CIT==4,2,CIT_CAT)) %>%
  mutate(CIT_CAT = ifelse(CIT==5,3,CIT_CAT))

# Summary Statistics
tapply(renters$PWGTP, list(renters$CIT_CAT), sum)                 # 91.9% of renters were U.S. citizens at birth
prop.table(tapply(renters$PWGTP, list(renters$CIT_CAT), sum))     # 5.3% are not U.S. citizens

#--------------------------------------
# Household Type: [1= Married Couple, 2= Unmarried Couple, 3= Family Household, 4= Non-Family & Non-Partner]
#--------------------------------------
rentals <- rentals %>%
  mutate(HHT_CAT = ifelse(HHT==1,1,0)) %>%
  mutate(HHT_CAT = ifelse(HHT==2 | HHT==3,3,HHT_CAT)) %>%
  mutate(HHT_CAT = ifelse(HHT>3,4,HHT_CAT)) %>%
  mutate(HHT_CAT = ifelse(PARTNER>0,2,HHT_CAT))

# Summary Statistics                                             # 50.8% of renter households are non-family and non-partner households
tapply(rentals$WGTP, list(rentals$HHT_CAT), sum)                 # 10.0% of renter households are married couple households
prop.table(tapply(rentals$WGTP, list(rentals$HHT_CAT), sum))     # 10.2% of renter households are unmarried partner households



#===============================================================================
# Identify Overcrowded Households
#===============================================================================
#--------------------------------------
# Using Bedroom Standard
#--------------------------------------
# Create BEDR variables indicating bed need
x <- ifelse(rentals$NP == 1, 0, 1)              # Identify 1-person households

rentals <- rentals %>%
  mutate(RENTAL = 1) %>%
  mutate(NP_adj = NP*x) %>%                     # Allows 1-person households to live in studio apartments
  mutate(BEDR2 = ceiling(NP_adj/2)) %>%         # Max 2 People Per Bedroom
  mutate(BEDR1.75 = ceiling(NP_adj/1.75)) %>%   # Max 1.75 People Per Bedroom
  mutate(BEDR1.5 = ceiling(NP_adj/1.5))         # Max 1.5 People Per Bedroom

# Calculate the difference between available beds and needed beds
rentals <- rentals %>%
  mutate(DIFF2 = BDSP-BEDR2) %>%
  mutate(DIFF1.75 = BDSP-BEDR1.75) %>%
  mutate(DIFF1.5 = BDSP-BEDR1.5)

# Identify crowded rental households
diff_bdrm <- paste0("DIFF", toString(bdrm))
rentals <- rentals %>%
  mutate(FLAG = ifelse(get(diff_bdrm) < 0, 1, 0), FLAG2 = ifelse(get(diff_bdrm) < -1, 1, 0))

# Count crowded rental households
rentals %>%
  summarise(all = sum(WGTP), crowd = sum(WGTP*FLAG), crowd_pct = crowd/all, vcrowd = sum(WGTP*FLAG2), vcrowd_pct = (vcrowd/all))

# Standard error of crowded rentals
pt.est <- sum(rentals$WGTP*rentals$FLAG)                          # Point Estimate:           1,501 rental households are crowded
rep.ests <- sapply(wrep.names, function(n) 
  sum(rentals[[n]]*rentals$FLAG))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2))                   # Standard Error:           189 rental households
ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     # 90% Confidence Interval:  [1,192 -- 1,810]
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se))                     # 95% Confidence Interval:  [1,131 -- 1,871]

# Isolate crowded rental households
rentals_crowd <- rentals %>%
  filter(FLAG == 1)

rentals_xcrowd <- rentals %>%
  filter(FLAG == 0)

#--------------------------------------
# Using Room Standard
#--------------------------------------
# Create ROOM variables indicating room need
rentals <- rentals %>%
  mutate(ROOM2 = ceiling(NP/2)) %>%           # Max 2 People Per Room
  mutate(ROOM1.5 = ceiling(NP/1.5)) %>%       # Max 1.5 People Per Room
  mutate(ROOM1 = NP)                          # Max 1 Person Per Room

# Calculate the difference between available rooms and needed rooms
rentals <- rentals %>%
  mutate(RDIFF2 = RMSP-ROOM2) %>%
  mutate(RDIFF1.5 = RMSP-ROOM1.5) %>%
  mutate(RDIFF1 = RMSP-ROOM1)

# Identify crowded rentals
diff_rm <- paste0("RDIFF", toString(rm))
rentals <- rentals %>%
  mutate(RFLAG = ifelse(get(diff_rm) < 0, 1, 0), RFLAG2 = ifelse(RDIFF1.5 < 0, 1, 0))

# Count crowded rental households
rentals %>%
  summarise(all = sum(WGTP), crowd = sum(WGTP*RFLAG), crowd_pct = crowd/all, vcrowd = sum(WGTP*RFLAG2), vcrowd_pct = (vcrowd/all))

# Standard error of crowded rentals
pt.est <- sum(rentals$WGTP*rentals$RFLAG)                         # Point Estimate:            485 rental households are crowded
rep.ests <- sapply(wrep.names, function(n) 
  sum(rentals[[n]]*rentals$RFLAG))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2))                   # Standard Error:            130 rental households
ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     # 90% Confidence Interval:  [272 -- 698]
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se))                     # 95% Confidence Interval:  [230 -- 740]

# Isolate crowded rentals
rentals_rcrowd <- rentals %>%
  filter(RFLAG == 1)

rentals_rxcrowd <- rentals %>%
  filter(RFLAG == 0)



#===============================================================================
# Identify People Living in Overcrowded Households
#===============================================================================
#--------------------------------------
# Bedroom Standard
#--------------------------------------
# Match crowded households with specific individuals
renters_crowd <- merge(rentals_crowd,renters, by="SERIALNO", all.x = TRUE, suffixes = c(".hh", ".p"))
renters_xcrowd <- merge(rentals_xcrowd,renters, by="SERIALNO", all.x = TRUE, suffixes = c(".hh", ".p"))

# Count crowded individuals
renters_crowd %>%
  summarise(crowd = sum(PWGTP), vcrowd = sum(PWGTP*FLAG2))        # There are 6,709 overcrowded renters (1,226 severely overcrowded)

renters %>%
  summarise(count = sum(PWGTP))                                   # There are 118,357 renters, so 5.7% are overcrowded (1.0% severely)

# Standard Error -- Crowded Renters
pt.est1 <- sum(renters_crowd$PWGTP)                               # Point Estimate:           6,709 renters
rep.ests <- sapply(prep.names, function(n) 
  sum(renters_crowd[[n]]))
se1 <- sqrt((4/80) * sum((rep.ests - pt.est1)^2))                 # Standard Error:            782 renters
ci90 <- c(pt.est1-(1.64*se1), pt.est1+(1.64*se1))                 # 90% Confidence Interval:  [5,426 -- 7,992]
ci95 <- c(pt.est1-(1.96*se1), pt.est1+(1.96*se1))                 # 95% Confidence Interval:  [5,176 -- 8,242]

# Standard Error -- Total Renters
pt.est2 <- sum(renters$PWGTP)                                     # Point Estimate:           118,357 renters
rep.ests <- sapply(prep.names, function(n) 
  sum(renters[[n]]))
se2 <- sqrt((4/80) * sum((rep.ests - pt.est2)^2))                 # Standard Error:           1,651 renters
ci90 <- c(pt.est2-(1.64*se2), pt.est2+(1.64*se2))                 # 90% Confidence Interval:  [115,649 -- 121,065]
ci95 <- c(pt.est2-(1.96*se2), pt.est2+(1.96*se2))                 # 95% Confidence Interval:  [115,120 -- 121,594]

# Standard Error -- Proportion: Crowded Renters/Total Renters
prop <- (pt.est1/pt.est2)                                         # Point Estimate:           5.7% of renters are overcrowded
se_prop <- (1/pt.est2) * sqrt(se1^2 - prop^2*se2^2 )              # Standard Error:           0.66 percentage points
se_ci90 <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))            # 90% Confidence Interval:  [4.6% -- 6.7%]
se_ci95 <- c(prop-(1.96*se_prop), prop+(1.96*se_prop))            # 95% Confidence Interval:  [4.4% -- 7.0%]


#--------------------------------------
# Room Standard
#--------------------------------------
# Match crowded households with specific individuals
renters_rcrowd <- merge(rentals_rcrowd,renters, by="SERIALNO", suffixes = c(".hh", ".p"))
renters_rxcrowd <- merge(rentals_rxcrowd,renters, by="SERIALNO", suffixes = c(".hh", ".p"))

# Count crowded individuals
renters_rcrowd %>%
  summarise(crowd = sum(PWGTP), vcrowd = sum(PWGTP*RFLAG2))       # There are 1,862 overcrowded renters (0 severely overcrowded)

renters %>%
  summarise(count = sum(PWGTP))                                   # There are 118,357 renters, so 1.6% are overcrowded (0% severely)

# Standard Error -- Crowded Renters
pt.est1 <- sum(renters_rcrowd$PWGTP)                              # Point Estimate:           1,862 renters
rep.ests <- sapply(prep.names, function(n) 
  sum(renters_rcrowd[[n]]))
se1 <- sqrt((4/80) * sum((rep.ests - pt.est1)^2))                 # Standard Error:           519 renters
ci90 <- c(pt.est1-(1.64*se1), pt.est1+(1.64*se1))                 # 90% Confidence Interval:  [1,011 -- 2,713]
ci95 <- c(pt.est1-(1.96*se1), pt.est1+(1.96*se1))                 # 95% Confidence Interval:  [845 -- 2,879]

# Standard Error -- Total Renters
pt.est2 <- sum(renters$PWGTP)                                     # Point Estimate:           118,357 renters
rep.ests <- sapply(prep.names, function(n) 
  sum(renters[[n]]))
se2 <- sqrt((4/80) * sum((rep.ests - pt.est2)^2))                 # Standard Error:           1,651 renters
ci90 <- c(pt.est2-(1.64*se2), pt.est2+(1.64*se2))                 # 90% Confidence Interval:  [115,649 -- 121,065]
ci95 <- c(pt.est2-(1.96*se2), pt.est2+(1.96*se2))                 # 95% Confidence Interval:  [115,120 -- 121,594]

# Standard Error -- Proportion: Crowded Renters/Total Renters
prop <- (pt.est1/pt.est2)                                         # Point Estimate:           1.6% of renters are overcrowded
se_prop <- (1/pt.est2) * sqrt(se1^2 - prop^2*se2^2 )              # Standard Error:           0.44 percentage points
se_ci90 <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))            # 90% Confidence Interval:  [0.9% -- 2.3%]
se_ci95 <- c(prop-(1.96*se_prop), prop+(1.96*se_prop))            # 90% Confidence Interval:  [0.7% -- 2.4%]

renters <- merge(rentals,renters, by="SERIALNO", all.x = TRUE, suffixes = c(".hh", ".p"))

rm(pt.est1, rep.ests, se1, ci90, ci95, pt.est2, se2, prop, se_prop, se_ci90, se_ci95)



#===============================================================================
# Demographics of Crowded Households -- Bedroom Standard
#===============================================================================
#-------------------------------------------------------------------------------
# AMI Thresholds: [1= <30% AMI, 2= 30-50% AMI, 3= 50-80% AMI, 4= 80-120% AMI, 5= >120% AMI]
#-------------------------------------------------------------------------------
# Crowded (Person-Level)
tapply(renters_crowd$PWGTP, list(renters_crowd$AMI_CAT), sum)
prop.table(tapply(renters_crowd$PWGTP, list(renters_crowd$AMI_CAT), sum))       # Category 4 has most crowded renters (36.2% of total)

# Uncrowded (Person-Level)
tapply(renters_xcrowd$PWGTP, list(renters_xcrowd$AMI_CAT), sum)
prop.table(tapply(renters_xcrowd$PWGTP, list(renters_xcrowd$AMI_CAT), sum)) 

# Crowded (Household-Level)
tapply(rentals_crowd$WGTP, list(rentals_crowd$AMI_CAT), sum)
prop.table(tapply(rentals_crowd$WGTP, list(rentals_crowd$AMI_CAT), sum))        # Category 4 has most crowded households (31.9% of total)

# Uncrowded (Household-Level)
tapply(rentals_xcrowd$WGTP, list(rentals_xcrowd$AMI_CAT), sum)
prop.table(tapply(rentals_xcrowd$WGTP, list(rentals_xcrowd$AMI_CAT), sum)) 

#-------------------------------------------------------------------------------
# Income Quintiles: [1 = 0-20%, 2 = 20-40%, 3 = 40-60%, 4 = 60-80%, 5 = 80-100%]
#-------------------------------------------------------------------------------
# Crowded (Person-Level)
tapply(renters_crowd$PWGTP, list(renters_crowd$INC_CAT), sum)
prop.table(tapply(renters_crowd$PWGTP, list(renters_crowd$INC_CAT), sum))       # Quintile 5 has most crowded people (38.7% of total)

# Uncrowded (Person-Level)
tapply(renters_xcrowd$PWGTP, list(renters_xcrowd$INC_CAT), sum)
prop.table(tapply(renters_xcrowd$PWGTP, list(renters_xcrowd$INC_CAT), sum))

# Crowded (Household-Level)
tapply(rentals_crowd$WGTP, list(rentals_crowd$INC_CAT), sum)
prop.table(tapply(rentals_crowd$WGTP, list(rentals_crowd$INC_CAT), sum))        # Quintile 5 has most crowded people (35.7% of total)

# Uncrowded (Household-Level)
tapply(rentals_xcrowd$WGTP, list(rentals_xcrowd$INC_CAT), sum)
prop.table(tapply(rentals_xcrowd$WGTP, list(rentals_xcrowd$INC_CAT), sum)) 

#-------------------------------------------------------------------------------
# Sex Tabulations: [1=Male,2=Female]
#-------------------------------------------------------------------------------
# Crowded (Person-Level)
tapply(renters_crowd$PWGTP, list(renters_crowd$SEX), sum)
prop.table(tapply(renters_crowd$PWGTP, list(renters_crowd$SEX), sum))           # Females and males are equally likely to be crowded

# Uncrowded (Person-Level)
tapply(renters_xcrowd$PWGTP, list(renters_xcrowd$SEX), sum)
prop.table(tapply(renters_xcrowd$PWGTP, list(renters_xcrowd$SEX), sum))

#-------------------------------------------------------------------------------
# Race Tabulations: [1=White,2=Black,3=Native American,6=Asian,10=Hispanic]
#-------------------------------------------------------------------------------
# Crowded (Person-Level)
tapply(renters_crowd$PWGTP, list(renters_crowd$RACE_CAT), sum)                  # White people make up 20.1% of crowded renters
prop.table(tapply(renters_crowd$PWGTP, list(renters_crowd$RACE_CAT), sum))      # Black people make up 55.6% of crowded renters

# Uncrowded (Person-Level)
tapply(renters_xcrowd$PWGTP, list(renters_xcrowd$RACE_CAT), sum)                # White people make up 28.2% of uncrowded renters
prop.table(tapply(renters_xcrowd$PWGTP, list(renters_xcrowd$RACE_CAT), sum))    # Black people make up 41.9% of uncrowded renters

#-------------------------------------------------------------------------------
# Disability Tabulations: [1=Disability,2=No Disability]
#-------------------------------------------------------------------------------
# Crowded (Person-Level)
tapply(renters_crowd$PWGTP, list(renters_crowd$DIS), sum)
prop.table(tapply(renters_crowd$PWGTP, list(renters_crowd$DIS), sum))           # Disabled people make up 14.5% of crowded renters

# Uncrowded (Person-Level)
tapply(renters_xcrowd$PWGTP, list(renters_xcrowd$DIS), sum)
prop.table(tapply(renters_xcrowd$PWGTP, list(renters_xcrowd$DIS), sum))         # Disabled people make up 21.5% of uncrowded renters

#-------------------------------------------------------------------------------
# Age Group Tabulations: [1= 0-17 years, 2= 18-39 years, 3= 40-64 years, 4= 65+ years]
#-------------------------------------------------------------------------------
# Crowded (Person-Level)
tapply(renters_crowd$PWGTP, list(renters_crowd$AGE_CAT), sum)                   # 18-39 age group makes up 34.0% of crowded renters
prop.table(tapply(renters_crowd$PWGTP, list(renters_crowd$AGE_CAT), sum))       # 0-17 age group makes up 45.5% of crowded renters

# Uncrowded (Person-Level)
tapply(renters_xcrowd$PWGTP, list(renters_xcrowd$AGE_CAT), sum)                 # 18-39 age group makes up 41.7% of uncrowded renters
prop.table(tapply(renters_xcrowd$PWGTP, list(renters_xcrowd$AGE_CAT), sum))     # 0-17 age group makes up 25.8% of uncrowded renters

#-------------------------------------------------------------------------------
# Child Presence: [1= Child<6, 2= Child 6-17, 3= Child<6 & Child 6-17, 4= No Children]
#-------------------------------------------------------------------------------
# Crowded (Household-Level)
tapply(rentals_crowd$WGTP, list(rentals_crowd$HUPAC), sum)
prop.table(tapply(rentals_crowd$WGTP, list(rentals_crowd$HUPAC), sum))

# Uncrowded (Household-Level)
tapply(rentals_xcrowd$WGTP, list(rentals_xcrowd$HUPAC), sum)
prop.table(tapply(rentals_xcrowd$WGTP, list(rentals_xcrowd$HUPAC), sum)) 

# Crowded (Person-Level)
tapply(renters_crowd$PWGTP, list(renters_crowd$HUPAC.hh), sum)
prop.table(tapply(renters_crowd$PWGTP, list(renters_crowd$HUPAC.hh), sum))

# Uncrowded (Person-Level)
tapply(renters_xcrowd$PWGTP, list(renters_xcrowd$HUPAC.hh), sum)
prop.table(tapply(renters_xcrowd$PWGTP, list(renters_xcrowd$HUPAC.hh), sum))

#-------------------------------------------------------------------------------
# College Enrollment Tabulations: [0=Not Attending,1=Attending]
#-------------------------------------------------------------------------------
# Crowded (Person-Level)
tapply(renters_crowd$PWGTP, list(renters_crowd$COLLEGE), sum)
prop.table(tapply(renters_crowd$PWGTP, list(renters_crowd$COLLEGE), sum))       # College students make up 7.1% of crowded renters

# Uncrowded (Person-Level)
tapply(renters_xcrowd$PWGTP, list(renters_xcrowd$COLLEGE), sum)
prop.table(tapply(renters_xcrowd$PWGTP, list(renters_xcrowd$COLLEGE), sum))     # College students make up 10.8% of uncrowded renters

#-------------------------------------------------------------------------------
# Citizenship Status Tabulations: [1=U.S. Citizen,2=Naturalized Citizen,3=Non-Citizen]
#-------------------------------------------------------------------------------
# Crowded (Person-Level)
tapply(renters_crowd$PWGTP, list(renters_crowd$CIT_CAT), sum)
prop.table(tapply(renters_crowd$PWGTP, list(renters_crowd$CIT_CAT), sum))       # Naturalized and non-citizens make up 29.0% of crowded renters

# Uncrowded (Person-Level)
tapply(renters_xcrowd$PWGTP, list(renters_xcrowd$CIT_CAT), sum)
prop.table(tapply(renters_xcrowd$PWGTP, list(renters_xcrowd$CIT_CAT), sum))     # Naturalized and non-citizens make up 10.6% of uncrowded renters

#-------------------------------------------------------------------------------
# Number of People Tabulations: [# = Count]
#-------------------------------------------------------------------------------
# Crowded (Household-Level)
tapply(rentals_crowd$WGTP, list(rentals_crowd$NP), sum)
prop.table(tapply(rentals_crowd$WGTP, list(rentals_crowd$NP), sum))             # 2-person households make up 55.6% of crowded rentals

# Uncrowded (Household-Level)
tapply(rentals_xcrowd$WGTP, list(rentals_xcrowd$NP), sum)
prop.table(tapply(rentals_xcrowd$WGTP, list(rentals_xcrowd$NP), sum))           # 2-person households make up 46.6% of uncrowded rentals

# Crowded (Person-Level)
tapply(renters_crowd$PWGTP, list(renters_crowd$NP.hh), sum)
prop.table(tapply(renters_crowd$PWGTP, list(renters_crowd$NP.hh), sum))

# Uncrowded (Person-Level)
tapply(renters_xcrowd$PWGTP, list(renters_xcrowd$NP.hh), sum)
prop.table(tapply(renters_xcrowd$PWGTP, list(renters_xcrowd$NP.hh), sum))

#-------------------------------------------------------------------------------
# Family Relationships: [HHT_CAT: 1 = Married, 2 = Unmarried Partner, 3 = Other Family, 4 = Non-Family, Non-Partner]
#-------------------------------------------------------------------------------
# Crowded (Household-Level)
tapply(rentals_crowd$WGTP, list(rentals_crowd$HHT_CAT), sum)
tapply(rentals_crowd$WGTP, list(rentals_crowd$NP, rentals_crowd$HHT_CAT), sum)      # 94 2-person married households are crowded

# Uncrowded (Household-Level)
tapply(rentals_xcrowd$WGTP, list(rentals_xcrowd$HHT_CAT), sum)
tapply(rentals_xcrowd$WGTP, list(rentals_xcrowd$NP, rentals_xcrowd$HHT_CAT), sum)   # 2,482 2-person married households are uncrowded

# Crowded (Person-Level)
tapply(renters_crowd$PWGTP, list(renters_crowd$HHT_CAT), sum)                       # 120 non-family, non-partner households are crowded

# Uncrowded (Person-Level)
tapply(renters_xcrowd$PWGTP, list(renters_xcrowd$HHT_CAT), sum)                     # 32,930 non-family, non-partner households are uncrowded



#===============================================================================
# Standard Error Calculator
#===============================================================================
#--------------------------------------
# User Specifications
#--------------------------------------
var <- "HUPAC.hh"                    # Select a variable (e.g. "SEX" or "AMI_CAT")
cat <- "4"                          # How many categories of this variable are there? (e.g. "2" or "4")
wgt <- "PWGTP"                      # Select person-level or household-level weights ("PWGTP" or "WGTP")
dta <- "renters_crowd"              # Select a dataset to use (e.g. "rentals_crowd" or "renters_xcrowd")

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
  
  # Compute point estimate
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
se_table



#===============================================================================
# Export to .CSV
#===============================================================================
#write.csv(rentals, file = "./Data_Subsets/rentals.csv")
#write.csv(rentals_crowd, file = "./Data_Subsets/rentals_crowd.csv")
#write.csv(rentals_xcrowd, file = "./Data_Subsets/rentals_xcrowd.csv")

#write.csv(renters, file = "./Data_Subsets/renters.csv")
#write.csv(renters_crowd, file = "./Data_Subsets/renters_crowd.csv")
#write.csv(renters_xcrowd, file = "./Data_Subsets/renters_xcrowd.csv")
