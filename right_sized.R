# ===============================================================================
# * FILE: right_sized.R
# * PURPOSE: Determine the extent of overcrowding in Rochester, NY
# * AUTHORS: Adam Staveski
# * DATE CREATED: June 10, 2020
# * DATE LAST MODIFIED: June 15, 2020
# ===============================================================================
library(readr)
library(tidyverse)
library(survey)
library(Hmisc)
library(ggplot2)

setwd("/Users/astav/Documents/Employment/Harvard-Bloomberg/Rochester/R/Data/PUMS")

# Select standards for overcrowding
bdrm  <- 2          # Options are: 2 / 1.5 / 1 bedrooms per person
rm    <- 1.5        # Options are: 2 / 1.5 / 1 rooms per person

#===============================================================================
# Data Import and Preparation
#===============================================================================
#-------------------------------------------------------------------------------
# Load PUMS Household Data and Select Rochester PUMAs
#-------------------------------------------------------------------------------
pums_hh <- read_csv("psam_h36.csv", guess_max = 12000)
pums_p <- read_csv("psam_p36.csv", guess_max = 12000)

roc_hh <- pums_hh %>%
  filter(PUMA == "00902" | PUMA == "00903")
roc_p <- pums_p %>%
  filter(PUMA == "00902" | PUMA == "00903")

rm("pums_hh", "pums_p")

#-------------------------------------------------------------------------------
# Merge Datasets
#-------------------------------------------------------------------------------
# Merge household and person datasets
roc=merge(roc_hh,roc_p, by="SERIALNO", all.x = TRUE, suffixes = c(".hh", ".p"))

#-------------------------------------------------------------------------------
# Generate Variables
#-------------------------------------------------------------------------------
#--------------------------------------
# AMI Thresholds
#--------------------------------------
ami30 <- 74000*0.3
ami50 <- 74000*0.5
ami80 <- 74000*0.8
ami100 <- 74000
ami120 <- 74000*1.2

AMI_CAT <- cut(roc$HINCP, breaks = c(0,ami30,ami50,ami80,ami120,10000000), labels = c(1,2,3,4,5), right = TRUE)
roc <- mutate(roc,AMI_CAT)

AMI_CAT <- cut(roc_hh$HINCP, breaks = c(0,ami30,ami50,ami80,ami120,10000000), labels = c(1,2,3,4,5), right = TRUE)
roc_hh <- mutate(roc_hh,AMI_CAT)

tapply(roc$PWGTP, list(roc$AMI_CAT), sum)                     # <30% AMI is largest category (28.6%)
prop.table(tapply(roc$PWGTP, list(roc$AMI_CAT), sum))         # Other categories are 16-19% of population each

#--------------------------------------
# Household Income Quintiles
#--------------------------------------
INC_CAT <- cut(roc$HINCP, breaks = c(0,12700,26200,45000,77000,1000000), labels = c(1,2,3,4,5), right = TRUE)
roc <- mutate(roc,INC_CAT)

with(roc_hh, Hmisc::wtd.quantile(HINCP, probs = c(0.2,0.4,0.6,0.8), weights=WGTP))
INC_CAT <- cut(roc_hh$HINCP, breaks = c(0,12700,26200,45000,77000,1000000), labels = c(1,2,3,4,5), right = TRUE)
roc_hh <- mutate(roc_hh,INC_CAT)

tapply(roc$PWGTP, list(roc$INC_CAT), sum)
prop.table(tapply(roc$PWGTP, list(roc$INC_CAT), sum))

#--------------------------------------
# Hispanic
#--------------------------------------
roc <- roc %>%
  mutate(HISP_CAT = ifelse(HISP=="01",0,1))

tapply(roc$PWGTP, list(roc$HISP_CAT), sum)
prop.table(tapply(roc$PWGTP, list(roc$HISP_CAT), sum))      # 20.7% of population is Hispanic

#--------------------------------------
# Race
#--------------------------------------
roc <- roc %>%
  mutate(RACE_CAT = ifelse(HISP=="01",RAC1P,10))

roc %>%
  select(RAC1P, HISP, RACE_CAT)

tapply(roc$PWGTP, list(roc$RACE_CAT), sum)                  # 35.2% of population is white
prop.table(tapply(roc$PWGTP, list(roc$RACE_CAT), sum))      # 37.2% of population is black

#--------------------------------------
# College Student
#--------------------------------------
roc <- roc %>%
  mutate(COLLEGE = ifelse(SCHG== "15" | SCHG == "16",1,0)) %>%
  mutate(COLLEGE = ifelse(is.na(SCHG),0,COLLEGE))

roc %>%
  select(COLLEGE, SCHG)

tapply(roc$PWGTP, list(roc$COLLEGE), sum)                  # 10.3% of population is in college
prop.table(tapply(roc$PWGTP, list(roc$COLLEGE), sum))      # 21,260 students

#--------------------------------------
# Age Buckets
#--------------------------------------
roc <- roc %>%
  mutate(AGE_CAT = cut(roc$AGEP, breaks = c(-1,18,40,65,100), labels = c(1,2,3,4), right = FALSE))

roc %>%
  select(AGEP, AGE_CAT)

tapply(roc$PWGTP, list(roc$AGE_CAT), sum)                     # 18-39 is the largest age category (38.8% of population)
prop.table(tapply(roc$PWGTP, list(roc$AGE_CAT), sum))         # 40-64 is second largest category (27.6% of population)

#--------------------------------------
# Citizenship Status
#--------------------------------------
roc <- roc %>%
  mutate(CIT_CAT = ifelse(CIT==1 | CIT==2 | CIT==3,1,0)) %>%
  mutate(CIT_CAT = ifelse(CIT==4,2,CIT_CAT)) %>%
  mutate(CIT_CAT = ifelse(CIT==5,3,CIT_CAT))

roc %>%
  select(CIT, CIT_CAT)

tapply(roc$PWGTP, list(roc$CIT_CAT), sum)                     # 89.7% of the population was a U.S. citizen at birth
prop.table(tapply(roc$PWGTP, list(roc$CIT_CAT), sum))         # 4.1% are naturalized citizens



#===============================================================================
# Right-Sized Analysis
#===============================================================================
#-------------------------------------------------------------------------------
# Compute difference between bedroom space and bedrooms per person standard
#-------------------------------------------------------------------------------
# Create bed dataframe that includes necessary variables
bed <- roc %>%
  select(NP, BDSP, WGTP, PWGTP, SEX, AGE_CAT, CIT_CAT, MAR, COLLEGE, DIS, HISP, RAC1P, RACE_CAT, HINCP, AMI_CAT, INC_CAT)

bed_hh <- roc_hh %>%
  select(NP, BDSP, WGTP, HINCP, AMI_CAT, INC_CAT, TEN, HHT, PARTNER)

# Create BEDR variables indicating bed need
x <- ifelse(roc$NP == 1, 0, 1)                  # Recode 1-person households as needing 0 bedrooms
x_hh <- ifelse(roc_hh$NP == 1, 0, 1)            # Recode 1-person households as needing 0 bedrooms

bed <- bed %>%
  mutate(NP_adj = NP*x) %>%                     # Adjustment to account for studio apartments
  mutate(BEDR2 = ceiling(NP_adj/2)) %>%         # Max 2 People Per Bedroom
  mutate(BEDR1.5 = ceiling(NP_adj/1.5)) %>%     # Max 1.5 People Per Bedroom
  mutate(BEDR1 = NP_adj)                        # Max 1 Person Per Bedroom

bed_hh <- bed_hh %>%
  mutate(NP_adj = NP*x_hh) %>%                  # Adjustment to account for studio apartments
  mutate(BEDR2 = ceiling(NP_adj/2)) %>%         # Max 2 People Per Bedroom
  mutate(BEDR1.5 = ceiling(NP_adj/1.5)) %>%     # Max 1.5 People Per Bedroom
  mutate(BEDR1 = NP_adj)                        # Max 1 Person Per Bedroom

# Calculate the difference between available beds and needed beds
bed <- bed %>%
  mutate(DIFF2 = BDSP-BEDR2) %>%
  mutate(DIFF1.5 = BDSP-BEDR1.5) %>%
  mutate(DIFF1 = BDSP-BEDR1)

bed_hh <- bed_hh %>%
  mutate(DIFF2 = BDSP-BEDR2) %>%
  mutate(DIFF1.5 = BDSP-BEDR1.5) %>%
  mutate(DIFF1 = BDSP-BEDR1)

# Compute quantile estimates: Person-level data
with(bed, Hmisc::wtd.quantile(DIFF2, prob = c(0.00, 0.01, 0.02, 0.04, 0.05), weights=PWGTP))  #  4% overcrowded &  1% severely overcrowded
with(bed, Hmisc::wtd.quantile(DIFF1.5, prob = c(0.00, 0.03, 0.05, 0.1, 0.20), weights=PWGTP)) # 19% overcrowded &  3% severely overcrowded
with(bed, Hmisc::wtd.quantile(DIFF1, prob = c(0.00, 0.04, 0.08, 0.18, 0.41), weights=PWGTP))  # 40% overcrowded & 17% severely overcrowded

with(bed_hh, Hmisc::wtd.quantile(DIFF2, prob = c(0.00, 0.01, 0.02, 0.04, 0.05), weights=WGTP))  #  1% overcrowded & <1% severely overcrowded
with(bed_hh, Hmisc::wtd.quantile(DIFF1.5, prob = c(0.00, 0.01, 0.02, 0.09,0.1), weights=WGTP))  #  9% overcrowded &  1% severely overcrowded
with(bed_hh, Hmisc::wtd.quantile(DIFF1, prob = c(0.00, 0.06, 0.07, 0.18, 0.19), weights=WGTP))  # 18% overcrowded &  6% severely overcrowded

#-------------------------------------------------------------------------------
# Compute difference between room space and rooms per person standard
#-------------------------------------------------------------------------------
# Create room dataframe that includes necessary variables
room <- roc %>%
  select(NP, RMSP, WGTP, PWGTP, SEX, AGE_CAT, CIT_CAT, MAR, COLLEGE, DIS, HISP, RAC1P, RACE_CAT, HINCP, AMI_CAT, INC_CAT)

room_hh <- roc_hh %>%
  select(NP, RMSP, WGTP, HINCP, AMI_CAT, INC_CAT)

# Create ROOM variables indicating room need
room <- room %>%
  mutate(ROOM2 = ceiling(NP/2)) %>%           # Max 2 People Per Room
  mutate(ROOM1.5 = ceiling(NP/1.5)) %>%       # Max 1.5 People Per Room
  mutate(ROOM1 = NP)                          # Max 1 Person Per Room

room_hh <- room_hh %>%
  mutate(ROOM2 = ceiling(NP/2)) %>%           # Max 2 People Per Room
  mutate(ROOM1.5 = ceiling(NP/1.5)) %>%       # Max 1.5 People Per Room
  mutate(ROOM1 = NP)                          # Max 1 Person Per Room

# Calculate the difference between available rooms and needed rooms
room <- room %>%
  mutate(DIFF2 = RMSP-ROOM2) %>%
  mutate(DIFF1.5 = RMSP-ROOM1.5) %>%
  mutate(DIFF1 = RMSP-ROOM1)

room_hh <- room_hh %>%
  mutate(DIFF2 = RMSP-ROOM2) %>%
  mutate(DIFF1.5 = RMSP-ROOM1.5) %>%
  mutate(DIFF1 = RMSP-ROOM1)

# Compute quantile estimates: Person-level data
with(room, Hmisc::wtd.quantile(DIFF2, prob = c(0.00, 0.01, 0.03, 0.04, 0.05), weights=PWGTP))   # 0% overcrowded & 0% severely overcrowded
with(room, Hmisc::wtd.quantile(DIFF1.5, prob = c(0.00, 0.01, 0.02, 0.03, 0.05), weights=PWGTP)) # 2% overcrowded & 0% severely overcrowded
with(room, Hmisc::wtd.quantile(DIFF1, prob = c(0.00, 0.02, 0.03, 0.04, 0.05), weights=PWGTP))   # 4% overcrowded & 3% severely overcrowded

# Compute quantile estimates: Household-level data
with(room_hh, Hmisc::wtd.quantile(DIFF2, prob = c(0.00, 0.01, 0.03, 0.04, 0.05), weights=WGTP))   #  0% overcrowded &  0% severely overcrowded
with(room_hh, Hmisc::wtd.quantile(DIFF1.5, prob = c(0.00, 0.01, 0.02, 0.03, 0.05), weights=WGTP)) # <1% overcrowded &  0% severely overcrowded
with(room_hh, Hmisc::wtd.quantile(DIFF1, prob = c(0.00, 0.01, 0.02, 0.04, 0.05), weights=WGTP))   #  1% overcrowded & <1% severely overcrowded

#-------------------------------------------------------------------------------
# Identify people living in crowded households
#-------------------------------------------------------------------------------
#--------------------------------------
# Bedroom Standard
#--------------------------------------
diff_bdrm <- paste0("DIFF", toString(bdrm))               # This changes dynamically with user specification

bed <- bed %>%
  mutate(FLAG = ifelse(get(diff_bdrm) < 0, 1, 0))         # FLAG identifies people living in overcrowded households

bed_hh <- bed_hh %>%
  mutate(FLAG = ifelse(get(diff_bdrm) < 0, 1, 0))         # FLAG identifies people living in overcrowded households

tapply(bed_hh$WGTP, list(bed_hh$FLAG), sum)               # 1.786 households are overcrowded
prop.table(tapply(bed_hh$WGTP, list(bed_hh$FLAG), sum))   # 1.8% of the dataset

# Split data into crowded and uncrowded persons
bed_crowd <- bed %>%
  filter(get(diff_bdrm) < 0)
bed_xcrowd <- bed %>%
  filter(get(diff_bdrm) >= 0)

bed_hh_crowd <- bed_hh %>%
  filter(get(diff_bdrm) < 0)
bed_hh_xcrowd <- bed_hh %>%
  filter(get(diff_bdrm) >= 0)

#--------------------------------------
# Room Standard
#--------------------------------------
diff_rm <- paste0("DIFF", toString(rm))                             # This changes dynamically with user specification

# Flag people living in crowded households
room <- room %>%
  mutate(FLAG = ifelse(get(diff_rm) < 0, 1, 0))

room_hh <- room_hh %>%
  mutate(FLAG = ifelse(get(diff_rm) < 0, 1, 0))

tapply(room_hh$WGTP, list(room_hh$FLAG), sum)                        # 973 households are overcrowded
prop.table(tapply(room_hh$WGTP, list(room_hh$FLAG), sum))            # 0.1% of households

# Split data into crowded and uncrowded persons
room_crowd <- room %>%
  filter(get(diff_rm) < 0)
room_xcrowd <- room %>%
  filter(get(diff_rm) >= 0)

room_hh_crowd <- room_hh %>%
  filter(get(diff_rm) < 0)
room_hh_xcrowd <- room_hh %>%
  filter(get(diff_rm) >= 0)

#===============================================================================
# Demographic Analysis -- Bedrooms
#===============================================================================
#-------------------------------------------------------------------------------
# Income Tabulations: [1=30% AMI, 2=50% AMI, 3=80% AMI, 4=120% AMI, 5=121+% AMI]
#-------------------------------------------------------------------------------
#--------------------------------------
# AMI Thresholds
#--------------------------------------
# Person-Level Data
tapply(bed_crowd$PWGTP, list(bed_crowd$AMI_CAT), sum)
prop.table(tapply(bed_crowd$PWGTP, list(bed_crowd$AMI_CAT), sum))     # Category 4 has most crowded people (24.4% of total)

tapply(bed_xcrowd$PWGTP, list(bed_xcrowd$AMI_CAT), sum)
prop.table(tapply(bed_xcrowd$PWGTP, list(bed_xcrowd$AMI_CAT), sum))   # Category 1 has most uncrowded people (29.3% of total)

# Household-Level Data
tapply(bed_hh_crowd$WGTP, list(bed_hh_crowd$AMI_CAT), sum)
prop.table(tapply(bed_hh_crowd$WGTP, list(bed_hh_crowd$AMI_CAT), sum))     # Category 4 has most crowded people (26.4% of total)

tapply(bed_hh_xcrowd$WGTP, list(bed_hh_xcrowd$AMI_CAT), sum)
prop.table(tapply(bed_hh_xcrowd$WGTP, list(bed_hh_xcrowd$AMI_CAT), sum))   # Category 1 has most uncrowded people (33.1% of total)

#--------------------------------------
# Income Quintiles
#--------------------------------------
# Person-Level Data
tapply(bed_crowd$PWGTP, list(bed_crowd$INC_CAT), sum)
prop.table(tapply(bed_crowd$PWGTP, list(bed_crowd$INC_CAT), sum))     # Quintile 4 has most crowded people (31.5% of total)

tapply(bed_xcrowd$PWGTP, list(bed_xcrowd$INC_CAT), sum)
prop.table(tapply(bed_xcrowd$PWGTP, list(bed_xcrowd$INC_CAT), sum))   # Quintile 2 has most uncrowded people (20.5% of total)

# Household-Level Data
tapply(bed_hh_crowd$WGTP, list(bed_hh_crowd$INC_CAT), sum)
prop.table(tapply(bed_hh_crowd$WGTP, list(bed_hh_crowd$INC_CAT), sum))     # Quintile 4 has most crowded people (41.3% of total)

tapply(bed_hh_xcrowd$WGTP, list(bed_hh_xcrowd$INC_CAT), sum)
prop.table(tapply(bed_hh_xcrowd$WGTP, list(bed_hh_xcrowd$INC_CAT), sum))   # Quintile 2 has most uncrowded people (20.6% of total)

#-------------------------------------------------------------------------------
# Sex Tabulations: [1=Male,2=Female]
#-------------------------------------------------------------------------------
tapply(bed_crowd$PWGTP, list(bed_crowd$SEX), sum)
prop.table(tapply(bed_crowd$PWGTP, list(bed_crowd$SEX), sum))         # Females are marginally more crowded (51.4% of total)

tapply(bed_xcrowd$PWGTP, list(bed_xcrowd$SEX), sum)
prop.table(tapply(bed_xcrowd$PWGTP, list(bed_xcrowd$SEX), sum))       # Females are marginally more uncrowded (52.8% of total)

#-------------------------------------------------------------------------------
# Race Tabulations: [1=White,2=Black,6=Asian,10=Hispanic]
#-------------------------------------------------------------------------------
tapply(bed_crowd$PWGTP, list(bed_crowd$RACE_CAT), sum)                   # White people make up 20.5% of crowded population
prop.table(tapply(bed_crowd$PWGTP, list(bed_crowd$RACE_CAT), sum))       # Black people make up 42.1% of crowded population
                                                                         # Hisp  people make up 26.8% of crowded population

tapply(bed_xcrowd$PWGTP, list(bed_xcrowd$RACE_CAT), sum)                 # White people make up 35.0% of uncrowded population
prop.table(tapply(bed_xcrowd$PWGTP, list(bed_xcrowd$RACE_CAT), sum))     # Black people make up 37.7% of uncrowded population
                                                                         # Hisp  people make up 20.8% of uncrowded population

#-------------------------------------------------------------------------------
# Disability Tabulations: [1=Disability,2=No Disability]
#-------------------------------------------------------------------------------
tapply(bed_crowd$PWGTP, list(bed_crowd$DIS), sum)
prop.table(tapply(bed_crowd$PWGTP, list(bed_crowd$DIS), sum))           # Disabled people make up 10.5% of crowded population

tapply(bed_xcrowd$PWGTP, list(bed_xcrowd$DIS), sum)
prop.table(tapply(bed_xcrowd$PWGTP, list(bed_xcrowd$DIS), sum))         # Disabled people make up 19.5% of uncrowded population

#-------------------------------------------------------------------------------
# Age Group Tabulations: [1= 0-17 years, 2= 18-39 years, 3= 40-64 years, 4= 65+ years]
#-------------------------------------------------------------------------------
tapply(bed_crowd$PWGTP, list(bed_crowd$AGE_CAT), sum)
prop.table(tapply(bed_crowd$PWGTP, list(bed_crowd$AGE_CAT), sum))       # 0-17 age group makes up 48.6% of overcrowded population

tapply(bed_xcrowd$PWGTP, list(bed_xcrowd$AGE_CAT), sum)
prop.table(tapply(bed_xcrowd$PWGTP, list(bed_xcrowd$AGE_CAT), sum))     # 18-40 age group makes up 37.9% of uncrowded population

#-------------------------------------------------------------------------------
# College Enrollment Tabulations: [0=Not Attending,1=Attending]
#-------------------------------------------------------------------------------
tapply(bed_crowd$PWGTP, list(bed_crowd$COLLEGE), sum)
prop.table(tapply(bed_crowd$PWGTP, list(bed_crowd$COLLEGE), sum))       # College students make up 4.7% of crowded population

tapply(bed_xcrowd$PWGTP, list(bed_xcrowd$COLLEGE), sum)
prop.table(tapply(bed_xcrowd$PWGTP, list(bed_xcrowd$COLLEGE), sum))     # College students make up 8.5% of crowded population

#-------------------------------------------------------------------------------
# Citizenship Status Tabulations: [1=U.S. Citizen,2=Naturalized Citizen,3=Non-Citizen]
#-------------------------------------------------------------------------------
tapply(bed_crowd$PWGTP, list(bed_crowd$CIT_CAT), sum)
prop.table(tapply(bed_crowd$PWGTP, list(bed_crowd$CIT_CAT), sum))       # Naturalized and Non-Citizens make up 21.2% of crowded

tapply(bed_xcrowd$PWGTP, list(bed_xcrowd$CIT_CAT), sum)
prop.table(tapply(bed_xcrowd$PWGTP, list(bed_xcrowd$CIT_CAT), sum))     # Naturalized and Non-Citizens make up 9.4% of uncrowded

#-------------------------------------------------------------------------------
# Number of People Tabulations: [# = Count]
#-------------------------------------------------------------------------------
tapply(bed_hh_crowd$WGTP, list(bed_hh_crowd$NP), sum)
prop.table(tapply(bed_hh_crowd$WGTP, list(bed_hh_crowd$NP), sum))            # 2-person households make up 46.0% of overcrowded (822 HHs)

tapply(bed_hh_crowd$WGTP, list(bed_hh_crowd$NP, bed_hh_crowd$HHT), sum)      # Of the 822 crowded 2-person households, 238 are married couples
tapply(bed_hh_crowd$WGTP, list(bed_hh_crowd$NP, bed_hh_crowd$PARTNER), sum)  # Of the 822 crowded 2-person households, 318 are unmarried couples
                                                                             # Of the 822 crowded 2-person households, 556 (67.6%) are couples

tapply(bed_hh_xcrowd$WGTP, list(bed_hh_xcrowd$NP), sum)
prop.table(tapply(bed_hh_xcrowd$WGTP, list(bed_hh_xcrowd$NP), sum))          # 2-person households make up 23.4% of the uncrowded (22,655 HHs)

#-------------------------------------------------------------------------------
# Housing Tenure: [1 = Owned w/ Mortgage, 2 = Owned w/o Mortgage, 3 = Rented, 4 = Rented w/o Payment of Rent]
#-------------------------------------------------------------------------------
tapply(bed_hh_crowd$WGTP, list(bed_hh_crowd$TEN), sum)
prop.table(tapply(bed_hh_crowd$WGTP, list(bed_hh_crowd$TEN), sum))           # 82.8% of overcrowded households are rentals (1,478 HHs)
                                                                             # 37.6% of overcrowded rentals are couples living in a studio apt

tapply(bed_hh_xcrowd$WGTP, list(bed_hh_xcrowd$TEN), sum)
prop.table(tapply(bed_hh_xcrowd$WGTP, list(bed_hh_xcrowd$TEN), sum))         # 61.6% of non-crowded households are rentals

#===============================================================================
# Demographic Analysis -- Rooms
#===============================================================================
#-------------------------------------------------------------------------------
# Income Tabulations: [1=30% AMI, 2=50% AMI, 3=80% AMI, 4=120% AMI, 5=121+% AMI]
#-------------------------------------------------------------------------------
# AMI Thresholds
tapply(room_crowd$PWGTP, list(room_crowd$AMI_CAT), sum)
prop.table(tapply(room_crowd$PWGTP, list(room_crowd$AMI_CAT), sum))     # 

tapply(room_xcrowd$PWGTP, list(room_xcrowd$AMI_CAT), sum)
prop.table(tapply(room_xcrowd$PWGTP, list(room_xcrowd$AMI_CAT), sum))   # 

# Income Quintiles
tapply(room_crowd$PWGTP, list(room_crowd$INC_CAT), sum)
prop.table(tapply(room_crowd$PWGTP, list(room_crowd$INC_CAT), sum))     # 

tapply(room_xcrowd$PWGTP, list(room_xcrowd$INC_CAT), sum)
prop.table(tapply(room_xcrowd$PWGTP, list(room_xcrowd$INC_CAT), sum))   # 

#-------------------------------------------------------------------------------
# Sex Tabulations: [1=Male,2=Female]
#-------------------------------------------------------------------------------
tapply(room_crowd$PWGTP, list(room_crowd$SEX), sum)
prop.table(tapply(room_crowd$PWGTP, list(room_crowd$SEX), sum))         # 

tapply(room_xcrowd$PWGTP, list(room_xcrowd$SEX), sum)
prop.table(tapply(room_xcrowd$PWGTP, list(room_xcrowd$SEX), sum))       # 

#-------------------------------------------------------------------------------
# Race Tabulations: [1=White,2=Black,6=Asian,10=Hispanic]
#-------------------------------------------------------------------------------
tapply(room_crowd$PWGTP, list(room_crowd$RACE_CAT), sum)                   # 
prop.table(tapply(room_crowd$PWGTP, list(room_crowd$RACE_CAT), sum))       # 
# Hisp  people make up 26.8% of crowded population

tapply(room_xcrowd$PWGTP, list(room_xcrowd$RACE_CAT), sum)                 # 
prop.table(tapply(room_xcrowd$PWGTP, list(room_xcrowd$RACE_CAT), sum))     # 
# Hisp  people make up 20.8% of uncrowded population

#-------------------------------------------------------------------------------
# Disability Tabulations: [1=Disability,2=No Disability]
#-------------------------------------------------------------------------------
tapply(room_crowd$PWGTP, list(room_crowd$DIS), sum)
prop.table(tapply(room_crowd$PWGTP, list(room_crowd$DIS), sum))           # 

tapply(room_xcrowd$PWGTP, list(room_xcrowd$DIS), sum)
prop.table(tapply(room_xcrowd$PWGTP, list(room_xcrowd$DIS), sum))         # 

#-------------------------------------------------------------------------------
# Age Group Tabulations: [1= 0-17 years, 2= 18-39 years, 3= 40-64 years, 4= 65+ years]
#-------------------------------------------------------------------------------
tapply(room_crowd$PWGTP, list(room_crowd$AGE_CAT), sum)
prop.table(tapply(room_crowd$PWGTP, list(room_crowd$AGE_CAT), sum))       # 

tapply(room_xcrowd$PWGTP, list(room_xcrowd$AGE_CAT), sum)
prop.table(tapply(room_xcrowd$PWGTP, list(room_xcrowd$AGE_CAT), sum))     # 

#-------------------------------------------------------------------------------
# College Enrollment Tabulations: [0=Not Attending,1=Attending]
#-------------------------------------------------------------------------------
tapply(room_crowd$PWGTP, list(room_crowd$COLLEGE), sum)
prop.table(tapply(room_crowd$PWGTP, list(room_crowd$COLLEGE), sum))       # 

tapply(room_xcrowd$PWGTP, list(room_xcrowd$COLLEGE), sum)
prop.table(tapply(room_xcrowd$PWGTP, list(room_xcrowd$COLLEGE), sum))     # 

#-------------------------------------------------------------------------------
# Citizenship Status Tabulations: [1=U.S. Citizen,2=Naturalized Citizen,3=Non-Citizen]
#-------------------------------------------------------------------------------
tapply(room_crowd$PWGTP, list(room_crowd$CIT_CAT), sum)
prop.table(tapply(room_crowd$PWGTP, list(room_crowd$CIT_CAT), sum))       # 

tapply(room_xcrowd$PWGTP, list(room_xcrowd$CIT_CAT), sum)
prop.table(tapply(room_xcrowd$PWGTP, list(room_xcrowd$CIT_CAT), sum))     # 

#-------------------------------------------------------------------------------
# Number of People Tabulations: [# = Count]
#-------------------------------------------------------------------------------
tapply(room_crowd$PWGTP, list(room_crowd$NP), sum)
prop.table(tapply(room_crowd$PWGTP, list(room_crowd$NP), sum))                     # 6-person households make up 43.9% of overcrowded

tapply(room_xcrowd$PWGTP, list(room_xcrowd$NP), sum, na.rm = TRUE)
prop.table(tapply(room_xcrowd$PWGTP, list(room_xcrowd$NP), sum, na.rm = TRUE))     # 2-person households make up 24.6% of the uncrowded



#===============================================================================
# Export Dataframes to .CSV
#===============================================================================
#write.csv(roc, file = "./roc.csv")
#write.csv(roc_hh, file = "./roc_hh.csv")
#write.csv(roc_p, file = "./roc_p.csv")
