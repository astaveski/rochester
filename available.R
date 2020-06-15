# ===============================================================================
# * FILE: available.R
# * PURPOSE: Conduct availability analysis for the City of Rochester
# * AUTHORS: Adam Staveski
# * DATE: June 15, 2020
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

#===============================================================================
# Availability Analysis
#===============================================================================
#--------------------------------------
# Analysis of Vacant Properties
#--------------------------------------
open <- roc %>%
  filter(VACS == 1 | VACS == 3)                      # Dataframe of available rentals + purchases

for_rent <- roc %>%
  filter(VACS == 1)                                  # Dataframe of available rentals

tapply(roc$WGTP, list(roc$VACS), sum)                # There are 6,046 uninhabitable vacant properties (44.2% of total vacancies)
prop.table(tapply(roc$WGTP, list(roc$VACS), sum))    # There are 5,515 available rental units (40.3% of total vacancies)
                                                     # There are 751 inhabitable homes for sale (5.5% of total vacancies)

#--------------------------------------
# All Available Units
#--------------------------------------
tapply(open$WGTP, list(open$BDSP), sum)
prop.table(tapply(open$WGTP, list(open$BDSP), sum))     # 44.0% of vacant units are 1 bedroom or smaller

tapply(open$WGTP, list(open$RMSP), sum)
prop.table(tapply(open$WGTP, list(open$RMSP), sum))     # 34.7% of vacant rentals are 3 rooms or smaller

#--------------------------------------
# Rentals Only
#--------------------------------------
tapply(for_rent$WGTP, list(for_rent$BDSP), sum)
prop.table(tapply(for_rent$WGTP, list(for_rent$BDSP), sum))     # 50.0% of vacant rentals are 1 bedroom or smaller

tapply(for_rent$WGTP, list(for_rent$RMSP), sum)
prop.table(tapply(for_rent$WGTP, list(for_rent$RMSP), sum))     # 39.4% of vacant rentals are 3 rooms or smaller



#===============================================================================
# Quantify the Space Needs of Overcrowded Households
#===============================================================================
#--------------------------------------
# Identify Crowded Households
#--------------------------------------
bed_hh <- roc_hh %>%
  select(NP, BDSP, WGTP, HINCP)

# Create BEDR variables indicating bed need
x <- ifelse(bed_hh$NP == 1, 0, 1)               # Recode 1-person households as needing 0 bedrooms

bed_hh <- bed_hh %>%
  mutate(NP_adj = NP*x) %>%                     # Adjustment to account for studio apartments
  mutate(BEDR2 = ceiling(NP_adj/2)) %>%         # Max 2 People Per Bedroom
  mutate(BEDR1.5 = ceiling(NP_adj/1.5)) %>%     # Max 1.5 People Per Bedroom
  mutate(BEDR1 = NP_adj)                        # Max 1 Person Per Bedroom

# Calculate the difference between available beds and needed beds
bed_hh <- bed_hh %>%
  mutate(DIFF2 = BDSP-BEDR2) %>%
  mutate(DIFF1.5 = BDSP-BEDR1.5) %>%
  mutate(DIFF1 = BDSP-BEDR1)

#--------------------------------------
# Split Data Into Crowded/Uncrowded
#--------------------------------------
# User-specified definition of overcrowding
diff_bdrm <- paste0("DIFF", toString(bdrm))

# FLAG households that meet the overcrowding criteria
bed_hh <- bed_hh %>%
  mutate(FLAG = ifelse(get(diff_bdrm) < 0, 1, 0))

# Split data into crowded and uncrowded households
bed_hh_crowd <- bed_hh %>%
  filter(get(diff_bdrm) < 0)
bed_hh_xcrowd <- bed_hh %>%
  filter(get(diff_bdrm) >= 0)

#--------------------------------------
# Identify Bed Need of the Overcrowded
#--------------------------------------
bed_hh_crowd %>%
  summarise(bed_need = sum(WGTP))               # 1,786 households are overcrowded

# Initiate loop variables
bed_hh_crowd <- bed_hh_crowd %>%
  mutate(DIFF_LOOP = get(diff_bdrm), NP_LOOP = NP, free_agent = 0)

# Identify number of people per household who need to move to eliminate overcrowding
for (row in 1:nrow(bed_hh_crowd)) {
  while (bed_hh_crowd[row,"DIFF_LOOP"] < 0) {
    bed_hh_crowd[row, "NP_LOOP"] <- bed_hh_crowd[row, "NP_LOOP"]-1
    bed_hh_crowd[row, "NP_LOOP"] <- ifelse(bed_hh_crowd[row, "NP_LOOP"] == 1, 0, bed_hh_crowd[row, "NP_LOOP"])
    bed_hh_crowd[row, "free_agent"]  <- bed_hh_crowd[row, "free_agent"]+1
    bed_hh_crowd[row,"DIFF_LOOP"] <- bed_hh_crowd[row, "BDSP"] - ceiling(bed_hh_crowd[row,"NP_LOOP"]/2)
  }
  bed_hh_crowd[row, "NP_LOOP"] <- bed_hh_crowd[row, "NP"] - bed_hh_crowd[row, "free_agent"]
}

# Apply household weights to estimate overall number of movers
bed_hh_crowd %>%
  summarise(sum(WGTP*free_agent))                                         # 2,801 people need to move out of 1,786 households

tapply(bed_hh_crowd$WGTP, list(bed_hh_crowd$free_agent), sum)             # 1,310 households need just one person to move out

#===============================================================================
# Quantify the Available Space on the Market
#===============================================================================
#--------------------------------------
# Identify Available Bed Capacity
#--------------------------------------
# Create dataframe of available rental units
for_rent <- roc %>%
  filter(VACS == 1)

for_rent %>%
  summarise(bed_capacity = sum(WGTP*BDSP))                        # 9,612 bedrooms available

tapply(for_rent$WGTP, list(for_rent$BDSP), sum)                   # 2,758 vacant rentals are 1 bedroom or smaller
prop.table(tapply(for_rent$WGTP, list(for_rent$BDSP), sum))       # 50.0% of vacant rentals are 1 bedroom or smaller

#--------------------------------------
# Comparison Table -- 2 BR Standard
#--------------------------------------
# Quantify the need of overcrowded households
# 1 bedroom | 2 bedroom | 3 bedroom | 4 bedroom
# 1,480     | 306       | 0         | 0
tapply(bed_hh_crowd$WGTP, list(bed_hh_crowd$free_agent), sum)

# Quantify market availability
# 0 bedroom | 1 bedroom | 2 bedroom | 3 bedroom | 4 bedroom
# 638       | 2,120     | 1,086     | 1,364     | 307
tapply(for_rent$WGTP, list(for_rent$BDSP), sum)

# ---> Without taking cost into account, overcrowding can be eliminated under current market conditions <---

