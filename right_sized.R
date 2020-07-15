# ===============================================================================
# * FILE: right_sized.R
# * PURPOSE: Determine the extent of overcrowding in Rochester, NY
# * AUTHORS: Adam Staveski
# * DATE CREATED: June 10, 2020
# * DATE LAST MODIFIED: July 10, 2020
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
# Select Standards for Overcrowding
#--------------------------------------
bdrm  <- 2            # Options: 2 / 1.75 / 1.5 people per bedroom
rm    <- 1            # Options: 1.5 / 1 / 0.5 people per room

#--------------------------------------
# Select AMI Number for Analysis
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
prop.table(tapply(rentals$WGTP, list(rentals$AMI_CAT), sum))

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
# Hispanic: [0=Not Hispanic, 1=Hispanic]
#--------------------------------------
# Create Hispanic Categories
renters <- renters %>%
  mutate(HISP_CAT = ifelse(HISP=="01",0,1))

# Summary Statistics
tapply(renters$PWGTP, list(renters$HISP_CAT), sum)
prop.table(tapply(renters$PWGTP, list(renters$HISP_CAT), sum))

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
tapply(renters$PWGTP, list(renters$RACE_CAT), sum)
prop.table(tapply(renters$PWGTP, list(renters$RACE_CAT), sum))

#--------------------------------------
# College Student: [1=Student, 2=Non-Student]
#--------------------------------------
# Create College Categories
renters <- renters %>%
  mutate(COLLEGE = ifelse(SCHG== "15" | SCHG == "16",1,2)) %>%
  mutate(COLLEGE = ifelse(is.na(SCHG),2,COLLEGE))

# Summary Statistics
tapply(renters$PWGTP, list(renters$COLLEGE), sum)
prop.table(tapply(renters$PWGTP, list(renters$COLLEGE), sum))

#--------------------------------------
# Age Buckets: [1= 0-17, 2= 18-39, 3= 40-64, 4= 65+]
#--------------------------------------
# Create Age Categories
renters <- renters %>%
  mutate(AGE_CAT = cut(renters$AGEP, breaks = c(-1,18,40,65,100), labels = c(1,2,3,4), right = FALSE))

# Summary Statistics
tapply(renters$PWGTP, list(renters$AGE_CAT), sum)
prop.table(tapply(renters$PWGTP, list(renters$AGE_CAT), sum))

#--------------------------------------
# Citizenship Status: [1= Citizen at Birth, 2= Naturalized Citizen, 3= Non-Citizen]
#--------------------------------------
# Create Citizenship Categories
renters <- renters %>%
  mutate(CIT_CAT = ifelse(CIT==1 | CIT==2 | CIT==3,1,0)) %>%
  mutate(CIT_CAT = ifelse(CIT==4,2,CIT_CAT)) %>%
  mutate(CIT_CAT = ifelse(CIT==5,3,CIT_CAT))

# Summary Statistics
tapply(renters$PWGTP, list(renters$CIT_CAT), sum)
prop.table(tapply(renters$PWGTP, list(renters$CIT_CAT), sum))

#--------------------------------------
# Household Type: [1=Married Couple, 2=Unmarried Couple, 3=Other Family Household, 4=Non-Family & Non-Partner]
#--------------------------------------
rentals <- rentals %>%
  mutate(HHT_CAT = ifelse(HHT==1,1,0)) %>%
  mutate(HHT_CAT = ifelse(HHT==2 | HHT==3,3,HHT_CAT)) %>%
  mutate(HHT_CAT = ifelse(HHT>3,4,HHT_CAT)) %>%
  mutate(HHT_CAT = ifelse(PARTNER>0,2,HHT_CAT))

# Summary Statistics
tapply(rentals$WGTP, list(rentals$HHT_CAT), sum)
prop.table(tapply(rentals$WGTP, list(rentals$HHT_CAT), sum))

#--------------------------------------
# Child Presence: [1=Child Under Six, 2=Child Not Under Six, 3=No Child]
#--------------------------------------
rentals <- rentals %>%
  mutate(KID_CAT = ifelse(HUPAC==1 | HUPAC==3,1,0)) %>%
  mutate(KID_CAT = ifelse(HUPAC==2,2,KID_CAT)) %>%
  mutate(KID_CAT = ifelse(HUPAC==4,3,KID_CAT))

# Summary Statistics
tapply(rentals$WGTP, list(rentals$KID_CAT), sum)
prop.table(tapply(rentals$WGTP, list(rentals$KID_CAT), sum))

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
# Identify Overcrowded Households -- Bedroom Standard
#===============================================================================
#--------------------------------------
# Define and Identify Crowded Households
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

# Isolate crowded rental households
rentals_crowd <- rentals %>%
  filter(FLAG == 1)

rentals_xcrowd <- rentals %>%
  filter(FLAG == 0)

#--------------------------------------
# Compute Standard Errors
#--------------------------------------
# Standard Error -- Crowded Rentals
pt.est1 <- sum(rentals_crowd$WGTP)                                # Point Estimate:           6,709 rentals
rep.ests <- sapply(wrep.names, function(n) 
  sum(rentals_crowd[[n]]))
se1 <- sqrt((4/80) * sum((rep.ests - pt.est1)^2))                 # Standard Error:            782 rentals
ci95 <- c(pt.est1-(1.96*se1), pt.est1+(1.96*se1))                 # 95% Confidence Interval:  [5,176 -- 8,242]

# Standard Error -- Total Rentals
pt.est2 <- sum(rentals$WGTP)                                      # Point Estimate:           52,366 rentals
rep.ests <- sapply(wrep.names, function(n) 
  sum(rentals[[n]]))
se2 <- sqrt((4/80) * sum((rep.ests - pt.est2)^2))                 # Standard Error:           771 rentals
ci95 <- c(pt.est2-(1.96*se2), pt.est2+(1.96*se2))                 # 95% Confidence Interval:  [50,854 -- 53,878]

# Standard Error -- Proportion: Crowded Rentals/Total Rentals
prop <- (pt.est1/pt.est2)                                         # Point Estimate:           5.7% of rentals are overcrowded
se_prop <- (1/pt.est2) * sqrt(se1^2 - prop^2*se2^2 )              # Standard Error:           0.66 percentage points
se_ci95 <- c(prop-(1.96*se_prop), prop+(1.96*se_prop))            # 95% Confidence Interval:  [4.4% -- 7.0%]



#===============================================================================
# Identify Overcrowded Households -- Room Standard
#===============================================================================
#--------------------------------------
# Define and Identify Crowded Households
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

# Isolate crowded rentals
rentals_rcrowd <- rentals %>%
  filter(RFLAG == 1)

rentals_rxcrowd <- rentals %>%
  filter(RFLAG == 0)

#--------------------------------------
# Compute Standard Errors
#--------------------------------------
# Standard Error -- Crowded Rentals
pt.est1 <- sum(rentals_rcrowd$WGTP)                               # Point Estimate:           1,092 rentals
rep.ests <- sapply(wrep.names, function(n) 
  sum(rentals_rcrowd[[n]]))
se1 <- sqrt((4/80) * sum((rep.ests - pt.est1)^2))                 # Standard Error:            175 rentals
ci95 <- c(pt.est1-(1.96*se1), pt.est1+(1.96*se1))                 # 95% Confidence Interval:  [750 -- 1,434]

# Standard Error -- Total Rentals
pt.est2 <- sum(rentals$WGTP)                                      # Point Estimate:           52,366 rentals
rep.ests <- sapply(wrep.names, function(n) 
  sum(rentals[[n]]))
se2 <- sqrt((4/80) * sum((rep.ests - pt.est2)^2))                 # Standard Error:           771 rentals
ci95 <- c(pt.est2-(1.96*se2), pt.est2+(1.96*se2))                 # 95% Confidence Interval:  [50,854 -- 53,878]

# Standard Error -- Proportion: Crowded Rentals/Total Rentals
prop <- (pt.est1/pt.est2)                                         # Point Estimate:           2.1% of rentals are overcrowded
se_prop <- (1/pt.est2) * sqrt(se1^2 - prop^2*se2^2 )              # Standard Error:           0.33 percentage points
se_ci95 <- c(prop-(1.96*se_prop), prop+(1.96*se_prop))            # 95% Confidence Interval:  [1.4% -- 2.7%]



#===============================================================================
# Identify People Living in Overcrowded Households -- Bedroom Standard
#===============================================================================
#--------------------------------------
# Define and Identify Crowded People
#--------------------------------------
# Match crowded households with specific individuals
renters_crowd <- merge(rentals_crowd,renters, by="SERIALNO", all.x = TRUE, suffixes = c(".hh", ".p"))
renters_xcrowd <- merge(rentals_xcrowd,renters, by="SERIALNO", all.x = TRUE, suffixes = c(".hh", ".p"))

# Count crowded individuals
renters_crowd %>%
  summarise(crowd = sum(PWGTP), vcrowd = sum(PWGTP*FLAG2))        # There are 6,709 overcrowded renters (1,226 severely overcrowded)

renters %>%
  summarise(count = sum(PWGTP))                                   # There are 118,357 renters, so 5.7% are overcrowded (1.0% severely)

#--------------------------------------
# Standard Errors
#--------------------------------------
# Standard Error -- Crowded Renters
pt.est1 <- sum(renters_crowd$PWGTP)                               # Point Estimate:           6,709 renters
rep.ests <- sapply(prep.names, function(n) 
  sum(renters_crowd[[n]]))
se1 <- sqrt((4/80) * sum((rep.ests - pt.est1)^2))                 # Standard Error:            782 renters
ci95 <- c(pt.est1-(1.96*se1), pt.est1+(1.96*se1))                 # 95% Confidence Interval:  [5,176 -- 8,242]

# Standard Error -- Total Renters
pt.est2 <- sum(renters$PWGTP)                                     # Point Estimate:           118,357 renters
rep.ests <- sapply(prep.names, function(n) 
  sum(renters[[n]]))
se2 <- sqrt((4/80) * sum((rep.ests - pt.est2)^2))                 # Standard Error:           1,651 renters
ci95 <- c(pt.est2-(1.96*se2), pt.est2+(1.96*se2))                 # 95% Confidence Interval:  [115,120 -- 121,594]

# Standard Error -- Proportion: Crowded Renters/Total Renters
prop <- (pt.est1/pt.est2)                                         # Point Estimate:           5.7% of renters are overcrowded
se_prop <- (1/pt.est2) * sqrt(se1^2 - prop^2*se2^2 )              # Standard Error:           0.66 percentage points
se_ci95 <- c(prop-(1.96*se_prop), prop+(1.96*se_prop))            # 95% Confidence Interval:  [4.4% -- 7.0%]


#===============================================================================
# Identify People Living in Overcrowded Households -- Room Standard
#===============================================================================
#--------------------------------------
# Define and Identify Crowded People
#--------------------------------------
# Match crowded households with specific individuals
renters_rcrowd <- merge(rentals_rcrowd,renters, by="SERIALNO", suffixes = c(".hh", ".p"))
renters_rxcrowd <- merge(rentals_rxcrowd,renters, by="SERIALNO", suffixes = c(".hh", ".p"))

# Count crowded individuals
renters_rcrowd %>%
  summarise(crowd = sum(PWGTP), vcrowd = sum(PWGTP*RFLAG2))       # There are 1,862 overcrowded renters (0 severely overcrowded)

renters %>%
  summarise(count = sum(PWGTP))                                   # There are 118,357 renters, so 1.6% are overcrowded (0% severely)

#--------------------------------------
# Standard Errors
#--------------------------------------
# Standard Error -- Crowded Renters
pt.est1 <- sum(renters_rcrowd$PWGTP)                              # Point Estimate:           5,999 renters
rep.ests <- sapply(prep.names, function(n) 
  sum(renters_rcrowd[[n]]))
se1 <- sqrt((4/80) * sum((rep.ests - pt.est1)^2))                 # Standard Error:           849 renters
ci95 <- c(pt.est1-(1.96*se1), pt.est1+(1.96*se1))                 # 95% Confidence Interval:  [4,335 -- 7,663]

# Standard Error -- Total Renters
pt.est2 <- sum(renters$PWGTP)                                     # Point Estimate:           118,357 renters
rep.ests <- sapply(prep.names, function(n) 
  sum(renters[[n]]))
se2 <- sqrt((4/80) * sum((rep.ests - pt.est2)^2))                 # Standard Error:           1,651 renters
ci95 <- c(pt.est2-(1.96*se2), pt.est2+(1.96*se2))                 # 95% Confidence Interval:  [115,120 -- 121,594]

# Standard Error -- Proportion: Crowded Renters/Total Renters
prop <- (pt.est1/pt.est2)                                         # Point Estimate:           5.1% of renters are overcrowded
se_prop <- (1/pt.est2) * sqrt(se1^2 - prop^2*se2^2 )              # Standard Error:           0.71 percentage points
se_ci95 <- c(prop-(1.96*se_prop), prop+(1.96*se_prop))            # 95% Confidence Interval:  [3.7% -- 6.5%]

#--------------------------------------
# Update Renters Dataframe
#--------------------------------------
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
# Race Tabulations: [1=White,2=Black,3=Hispanic,4=Asian,5=Other]
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

#-------------------------------------------------------------------------------
# Occupation Category: [1= AGR, 2= EXT/UTL, 3= MFG, 4= WHL/RET, 5= INFO/FIN/PRF, 6= EDU/MED, 7= ENT, 8= SRV, 9= ADMIN/MIL]
#-------------------------------------------------------------------------------
# Crowded (Person-Level)
tapply(renters_crowd$PWGTP, list(renters_crowd$JOB_CAT), sum)
prop.table(tapply(renters_crowd$PWGTP, list(renters_crowd$JOB_CAT), sum))       # EDU/MED have 27.8% of crowded

# Uncrowded (Person-Level)
tapply(renters_xcrowd$PWGTP, list(renters_xcrowd$JOB_CAT), sum)
prop.table(tapply(renters_xcrowd$PWGTP, list(renters_xcrowd$JOB_CAT), sum))     # EDU/MED have 32.1% of uncrowded



#===============================================================================
# Standard Error Calculator
#===============================================================================
#--------------------------------------
# User Specifications
#--------------------------------------
var <- "JOB_CAT"                    # Select a variable (e.g. "SEX" or "AMI_CAT")
cat <- "5"                       # How many categories of this variable are there? (e.g. "2" or "4")
wgt <- "PWGTP"                   # Select person-level or household-level weights ("PWGTP" or "WGTP")
dta <- "renters_crowd"                 # Select a dataset to use (e.g. "rentals_crowd" or "renters_xcrowd")

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
