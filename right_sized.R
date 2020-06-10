# ===============================================================================
# * FILE: right_sized.R
# * PURPOSE: Conduct right-sized analysis for the City of Rochester
# * AUTHORS: Adam Staveski
# * DATE: June 10, 2020
# ===============================================================================
library(readr)
library(tidyverse)
library(survey)
library(Hmisc)
library(ggplot2)

setwd("/Users/astav/Documents/Employment/Harvard-Bloomberg/Rochester/R/Data/PUMS")

#-------------------------------------------------------------------------------
# Load PUMS Household Data and Select Rochester PUMAs
#-------------------------------------------------------------------------------
pums_hh <- read_csv("psam_h36.csv", guess_max = 12000)
pums_p <- read_csv("psam_p36.csv", guess_max = 12000)

hh_roc <- pums_hh %>%
  filter(PUMA == "00902" | PUMA == "00903")
p_roc <- pums_p %>%
  filter(PUMA == "00902" | PUMA == "00903")

rm("pums_hh", "pums_p")

#-------------------------------------------------------------------------------
# Merge Datasets
#-------------------------------------------------------------------------------
# Merge household and person datasets
roc=merge(hh_roc,p_roc, by="SERIALNO", suffixes = c(".hh", ".p"))

#-------------------------------------------------------------------------------
# Generate Variables
#-------------------------------------------------------------------------------
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
# Right-Sized: Bedrooms
#-------------------------------------------------------------------------------
# Create bed dataframe that includes necessary variables
bed <- roc %>%
  select(NP, BDSP, WGTP, PWGTP, SEX, AGEP, CIT, MAR, SCH, DIS, HISP, RAC1P, HINCP, INC_CAT)

# Create BEDR variables indicating bed need
x <- ifelse(roc$NP == 1, 0, 1)                  # Recode 1-person households as needing 0 bedrooms

bed <- bed %>%
  mutate(NP_adj = NP*x) %>%                     # Adjustment to account for studio apartments
  mutate(BEDR2 = ceiling(NP_adj/2)) %>%         # Max 2 People Per Bedroom
  mutate(BEDR1.5 = ceiling(NP_adj/1.5)) %>%     # Max 1.5 People Per Bedroom
  mutate(BEDR1 = NP_adj)                        # Max 1 Person Per Bedroom

# Calculate the difference between available beds and needed beds
bed <- bed %>%
  mutate(DIFF2 = BDSP-BEDR2) %>%
  mutate(DIFF1.5 = BDSP-BEDR1.5) %>%
  mutate(DIFF1 = BDSP-BEDR1)

# Compute quantile estimates -- Households
#with(bed, Hmisc::wtd.quantile(DIFF2, prob = c(0.00, 0.01, 0.02, 0.03, 0.05), weights=WGTP))  #  1% overcrowded & <1% severely overcrowded
#with(bed, Hmisc::wtd.quantile(DIFF1.5, prob = c(0.00, 0.01, 0.03, 0.05, 0.1), weights=WGTP)) #  9% overcrowded &  1% severely overcrowded
#with(bed, Hmisc::wtd.quantile(DIFF1, prob = c(0.00, 0.01, 0.03, 0.07, 0.19), weights=WGTP))  # 19% overcrowded &  3% severely overcrowded

# Compute quantile estimates -- People
with(bed, Hmisc::wtd.quantile(DIFF2, prob = c(0.00, 0.01, 0.02, 0.04, 0.05), weights=PWGTP))  #  4% overcrowded &  1% severely overcrowded
with(bed, Hmisc::wtd.quantile(DIFF1.5, prob = c(0.00, 0.03, 0.05, 0.1, 0.20), weights=PWGTP)) # 19% overcrowded &  3% severely overcrowded
with(bed, Hmisc::wtd.quantile(DIFF1, prob = c(0.00, 0.04, 0.08, 0.18, 0.41), weights=PWGTP))  # 40% overcrowded & 17% severely overcrowded

#===============================================================================
# Demographic Analysis -- Bedrooms
#===============================================================================
# Flag people living in crowded households
FLAG <- ifelse(bed$DIFF2 < 0, 1, 0)
bed <- mutate(bed,FLAG)

tapply(bed$PWGTP, list(bed$FLAG), sum)                        # 9,165 people living in crowded households
prop.table(tapply(bed$PWGTP, list(bed$FLAG), sum))            # 4.7% of the dataset

# Split data into crowded and uncrowded persons using DIFF2
bed_crowd <- bed %>%
  filter(DIFF2 < 0)
bed_xcrowd <- bed %>%
  filter(DIFF2 >= 0)

#-------------------------------------------------------------------------------
# Income Tabulations: [1=30% AMI, 2=50% AMI, 3=80% AMI, 4=120% AMI, 5=121+% AMI]
#-------------------------------------------------------------------------------
tapply(bed_crowd$PWGTP, list(bed_crowd$INC_CAT), sum)
prop.table(tapply(bed_crowd$PWGTP, list(bed_crowd$INC_CAT), sum))     # Category 4 has most crowded people

tapply(bed_xcrowd$PWGTP, list(bed_xcrowd$INC_CAT), sum)
prop.table(tapply(bed_xcrowd$PWGTP, list(bed_xcrowd$INC_CAT), sum))   # Category 1 has most uncrowded people

#-------------------------------------------------------------------------------
# Sex Tabulations: [1=Male,2=Female]
#-------------------------------------------------------------------------------
tapply(bed_crowd$PWGTP, list(bed_crowd$SEX), sum)
prop.table(tapply(bed_crowd$PWGTP, list(bed_crowd$SEX), sum))         # Females are marginally more crowded

tapply(bed_xcrowd$PWGTP, list(bed_xcrowd$SEX), sum)
prop.table(tapply(bed_xcrowd$PWGTP, list(bed_xcrowd$SEX), sum))       # Females are marginally more uncrowded

#-------------------------------------------------------------------------------
# Race Tabulations: [1=White,2=Black,6=Asian,9=Two or More Races]
#-------------------------------------------------------------------------------
tapply(bed_crowd$PWGTP, list(bed_crowd$RAC1P), sum)                   # White people make up 45.4% of crowded population
prop.table(tapply(bed_crowd$PWGTP, list(bed_crowd$RAC1P), sum))       # Black people make up 44.0% of crowded population

tapply(bed_xcrowd$PWGTP, list(bed_xcrowd$RAC1P), sum)                 # White people make up 47.4% of uncrowded population
prop.table(tapply(bed_xcrowd$PWGTP, list(bed_xcrowd$RAC1P), sum))     # Black people make up 39.6% of uncrowded population

#-------------------------------------------------------------------------------
# Disability Tabulations: [1=Disability,2=No Disability]
#-------------------------------------------------------------------------------
tapply(bed_crowd$PWGTP, list(bed_crowd$DIS), sum)
prop.table(tapply(bed_crowd$PWGTP, list(bed_crowd$DIS), sum))         # Disabled people make up 10.5% of crowded population

tapply(bed_xcrowd$PWGTP, list(bed_xcrowd$DIS), sum)
prop.table(tapply(bed_xcrowd$PWGTP, list(bed_xcrowd$DIS), sum))       # Disabled people make up 19.5% of uncrowded population

#-------------------------------------------------------------------------------
# School Enrollment Tabulations: [1=Not Attending,2=Attending Public,3=Attending Private]
#-------------------------------------------------------------------------------
tapply(bed_crowd$PWGTP, list(bed_crowd$SCH), sum)
prop.table(tapply(bed_crowd$PWGTP, list(bed_crowd$SCH), sum))