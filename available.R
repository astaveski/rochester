# ===============================================================================
# * FILE: available.R
# * PURPOSE: Estimate the right-sized affordable housing gap
# * AUTHORS: Adam Staveski
# * DATE CREATED: June 12, 2020
# * DATE LAST MODIFIED: June 19, 2020
# ===============================================================================
library(readr)
library(tidyverse)
library(survey)
library(Hmisc)
library(cwhmisc)
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
monroe_hh <- pums_hh %>%
  filter(PUMA=="00901" | PUMA=="00902" | PUMA=="00903" | PUMA=="00904" | PUMA=="00905" | PUMA=="00906")

rm("pums_hh", "pums_p")

#-------------------------------------------------------------------------------
# Merge Datasets
#-------------------------------------------------------------------------------
# Merge household and person datasets
# roc=merge(roc_hh,roc_p, by="SERIALNO", all.x = TRUE, suffixes = c(".hh", ".p"))



#===============================================================================
# Analyze the Characteristics of Vacant Units
#===============================================================================
#--------------------------------------
# Types of Vacant Units
#--------------------------------------
open <- roc_hh %>%
  filter(VACS == 1 | VACS == 3)                            # Dataframe of available rentals + purchases

for_rent <- roc_hh %>%
  filter(VACS == 1)                                        # Dataframe of available rentals

tapply(roc_hh$WGTP, list(roc_hh$VACS), sum)                # There are 6,046 uninhabitable vacant properties (44.2% of total vacancies)
prop.table(tapply(roc_hh$WGTP, list(roc_hh$VACS), sum))    # There are 5,515 available rental units (40.3% of total vacancies)
                                                           # There are 751 inhabitable homes for sale (5.5% of total vacancies)

#--------------------------------------
# Vacant Homes and Rentals
#--------------------------------------
tapply(open$WGTP, list(open$BDSP), sum)
prop.table(tapply(open$WGTP, list(open$BDSP), sum))     # 44.0% of vacant units are 1 bedroom or smaller

tapply(open$WGTP, list(open$RMSP), sum)
prop.table(tapply(open$WGTP, list(open$RMSP), sum))     # 34.7% of vacant units are 3 rooms or smaller

#--------------------------------------
# Vacant Rentals
#--------------------------------------
tapply(for_rent$WGTP, list(for_rent$BDSP), sum)
prop.table(tapply(for_rent$WGTP, list(for_rent$BDSP), sum))     # 50.0% of vacant rentals are 1 bedroom or smaller

tapply(for_rent$WGTP, list(for_rent$RMSP), sum)
prop.table(tapply(for_rent$WGTP, list(for_rent$RMSP), sum))     # 39.4% of vacant rentals are 3 rooms or smaller



#===============================================================================
# Conduct Right-Sized Analysis
#===============================================================================
#-------------------------------------------------------------------------------
# Quantify the Space Needs of Overcrowded Households
#-------------------------------------------------------------------------------
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
# Identify Overcrowded Households
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
# De-Crowd Overcrowded Households
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

#--------------------------------------
# Analyze the Movers
#--------------------------------------
bed_hh_crowd %>%
  summarise(sum(WGTP*free_agent))                                         # 2,801 people need to move out of 1,786 households

tapply(bed_hh_crowd$WGTP, list(bed_hh_crowd$free_agent), sum)             # 1,310 households need just one person to move out

#-------------------------------------------------------------------------------
# Quantify the Available Space on the Market
#-------------------------------------------------------------------------------
#--------------------------------------
# Identify Available Bed Capacity
#--------------------------------------
for_rent %>%
  summarise(bed_capacity = sum(WGTP))                             # 5,515 rental units available on the open market

for_rent %>%
  summarise(bed_capacity = sum(WGTP*BDSP))                        # 9,612 rental bedrooms available on the open market

tapply(for_rent$WGTP, list(for_rent$BDSP), sum)                   # 2,758 vacant rentals are 1 bedroom or smaller
prop.table(tapply(for_rent$WGTP, list(for_rent$BDSP), sum))       # 50.0% of vacant rentals are 1 bedroom or smaller

#--------------------------------------
# Comparison Table -- 2 BR Standard
#--------------------------------------
# Quantify the bed need of overcrowded households
# 0-1 bedroom | 2 bedroom | 3 bedroom | 4 bedroom
# 1,480       | 306       | 0         | 0
tapply(bed_hh_crowd$WGTP, list(bed_hh_crowd$free_agent), sum)

# Quantify bed availability in the open market
# 0-1 bedroom | 2 bedroom | 3 bedroom | 4 bedroom
# 2,758       | 1,086     | 1,364     | 307
tapply(for_rent$WGTP, list(for_rent$BDSP), sum)



##############################################################################################################
#                                                                                                            #
#      Without taking cost into account, overcrowding can be eliminated under current market conditions      #
#                                                                                                            #
##############################################################################################################



#===============================================================================
# Conduct Affordability Analysis
#===============================================================================
#-------------------------------------------------------------------------------
# Subset to Occupied Rental Units
#-------------------------------------------------------------------------------
rentals <- roc_hh %>%
  filter(GRPIP > 0)

x <- ifelse(rentals$NP == 1, 0, 1)                    # Identify 1-person households

# Compute Bed Need for All Renter Households
rentals <- rentals %>%
  mutate(NP_adj = NP*x) %>%                           # Adjustment to account for studio apartments
  mutate(BEDR2 = ceiling(NP_adj/2)) %>%               # Max 2 People Per Bedroom
  mutate(BEDR1.5 = ceiling(NP_adj/1.5)) %>%           # Max 1.5 People Per Bedroom
  mutate(BEDR1 = NP_adj)                              # Max 1 Person Per Bedroom

# Calculate the difference between available beds and needed beds
rentals <- rentals %>%
  mutate(DIFF2 = BDSP-BEDR2) %>%
  mutate(DIFF1.5 = BDSP-BEDR1.5) %>%
  mutate(DIFF1 = BDSP-BEDR1)

#-------------------------------------------------------------------------------
# Calculate Maximum Income (% AMI) For Which a Unit is Affordable
#-------------------------------------------------------------------------------
#--------------------------------------
# Create Annual Gross Rent Variable
#--------------------------------------
rentals <- rentals %>%
  mutate(AGRNTP = GRNTP*12)

#--------------------------------------
# Calculate Area Median Income
#--------------------------------------
ami <- 74000                                                        # Actual AMI for Rochester MSA in 2018 is $74,000
roc_median_inc <- w.median(roc_hh$HINCP, roc_hh$WGTP)               # Median income in Rochester is $35,000
mon_median_inc <- w.median(monroe_hh$HINCP, monroe_hh$WGTP)         # Median income in Monroe County is $58,900

#--------------------------------------
# Compute Minimum Income Cutoffs (% AMI)
#--------------------------------------
rentals <- rentals %>%
  mutate(MIN_AMI = (AGRNTP / 0.3 / ami)*100) %>%
  mutate(MIN_AMI_ROC = (AGRNTP / 0.3 / roc_median_inc)*100) %>%     # Assuming no more than 30% of income should be spent on housing,
  mutate(MIN_AMI_MON = (AGRNTP / 0.3 / mon_median_inc)*100)         # what is the minimum income required to rent a given unit?

#--------------------------------------
# Group Income Cutoffs Into Buckets
#--------------------------------------
# [1 = <30% AMI | 2 = 30-50% AMI | 3 = 50-80% AMI | 4 = 80-120% AMI | 5 = >120% AMI]
rentals <- rentals %>%
  mutate(CAT_MIN_AMI = cut(MIN_AMI, breaks = c(-1, 30, 50, 80, 120, 10000), labels = c(1,2,3,4,5), right = TRUE)) %>%
  mutate(CAT_MIN_AMI_ROC = cut(MIN_AMI_ROC, breaks = c(-1, 30, 50, 80, 120, 10000), labels = c(1,2,3,4,5), right = TRUE)) %>%
  mutate(CAT_MIN_AMI_MON = cut(MIN_AMI_MON, breaks = c(-1, 30, 50, 80, 120, 10000), labels = c(1,2,3,4,5), right = TRUE))

#-------------------------------------------------------------------------------
# Calculate Maximum Rent (% AMI) That Each Household Can Afford
#-------------------------------------------------------------------------------
#--------------------------------------
# Compute Household Percent AMI
#--------------------------------------
rentals <- rentals %>%
  mutate(AMI_HH = (HINCP / ami)*100) %>%
  mutate(AMI_HH_ROC = (HINCP / roc_median_inc)*100) %>%
  mutate(AMI_HH_MON = (HINCP / mon_median_inc)*100)

#--------------------------------------
# Group Income Cutoffs Into Buckets
#--------------------------------------
rentals <- rentals %>%
  mutate(CAT_AMI_HH = cut(AMI_HH, breaks = c(-1, 30, 50, 80, 120, 10000), labels = c(1,2,3,4,5), right = TRUE)) %>%
  mutate(CAT_AMI_HH_ROC = cut(AMI_HH_ROC, breaks = c(-1, 30, 50, 80, 120, 10000), labels = c(1,2,3,4,5), right = TRUE)) %>%
  mutate(CAT_AMI_HH_MON = cut(AMI_HH_MON, breaks = c(-1, 30, 50, 80, 120, 10000), labels = c(1,2,3,4,5), right = TRUE))

#-------------------------------------------------------------------------------
# Summary Statistics: Affordability at Different Income Thresholds
#-------------------------------------------------------------------------------
#--------------------------------------
# Using Actual AMI of $74,000
#--------------------------------------
# How many units are affordable at each AMI threshold?
tapply(rentals$WGTP, list(rentals$CAT_MIN_AMI), sum)                          # 19.9% of units are affordable to 30% AMI
prop.table(tapply(rentals$WGTP, list(rentals$CAT_MIN_AMI), sum))              # 61.9% of units are affordable to 50% AMI 

# Cross-tabulations by number of bedrooms
tapply(rentals$WGTP, list(rentals$CAT_MIN_AMI, rentals$BDSP), sum)
prop.table(tapply(rentals$WGTP, list(rentals$CAT_MIN_AMI, rentals$BDSP), sum, default = 0))

# How many households encompass each AMI threshold?
tapply(rentals$WGTP, list(rentals$CAT_AMI_HH), sum)                           # 44.2% of households are 30% AMI or less
prop.table(tapply(rentals$WGTP, list(rentals$CAT_AMI_HH), sum))               # 62.7% of households are 50% AMI or less

# Cross-tabulations by number of bedrooms
tapply(rentals$WGTP, list(rentals$CAT_AMI_HH, rentals$BEDR2), sum)
prop.table(tapply(rentals$WGTP, list(rentals$CAT_AMI_HH, rentals$BEDR2), sum, default = 0))

#--------------------------------------
# Using Monroe County AMI of $58,900
#--------------------------------------
# How many units are affordable at each AMI threshold?
tapply(rentals$WGTP, list(rentals$CAT_MIN_AMI_MON), sum)                       # 14.1% of units are affordable to 30% AMI
prop.table(tapply(rentals$WGTP, list(rentals$CAT_MIN_AMI_MON), sum))           # 37.3% of units are affordable to 50% AMI 

# Cross-tabulations by number of bedrooms
tapply(rentals$WGTP, list(rentals$CAT_MIN_AMI_MON, rentals$BDSP), sum)
prop.table(tapply(rentals$WGTP, list(rentals$CAT_MIN_AMI_MON, rentals$BDSP), sum, default = 0))

# How many households encompass each AMI threshold?
tapply(rentals$WGTP, list(rentals$CAT_AMI_HH_MON), sum)                        # 36.1% of households are 30% AMI or less
prop.table(tapply(rentals$WGTP, list(rentals$CAT_AMI_HH_MON), sum))            # 53.7% of households are 50% AMI or less

# Cross-tabulations by number of bedrooms
tapply(rentals$WGTP, list(rentals$CAT_AMI_HH_MON, rentals$BEDR2), sum)
prop.table(tapply(rentals$WGTP, list(rentals$CAT_AMI_HH_MON, rentals$BEDR2), sum, default = 0))

#--------------------------------------
# Using Rochester AMI of $35,000
#--------------------------------------
# How many units are affordable at each AMI threshold?
tapply(rentals$WGTP, list(rentals$CAT_MIN_AMI_ROC), sum)                       # 6.9% of units are affordable to 30% AMI
prop.table(tapply(rentals$WGTP, list(rentals$CAT_MIN_AMI_ROC), sum))           # 14.0% of units are affordable to 50% AMI 

# Cross-tabulations by number of bedrooms
tapply(rentals$WGTP, list(rentals$CAT_MIN_AMI_ROC, rentals$BDSP), sum)
prop.table(tapply(rentals$WGTP, list(rentals$CAT_MIN_AMI_ROC, rentals$BDSP), sum, default = 0))

# How many households encompass each AMI threshold?
tapply(rentals$WGTP, list(rentals$CAT_AMI_HH_ROC), sum)                        # 22.7% of households are 30% AMI or less
prop.table(tapply(rentals$WGTP, list(rentals$CAT_AMI_HH_ROC), sum))            # 36.0% of households are 50% AMI or less

# Cross-tabulations by number of bedrooms
tapply(rentals$WGTP, list(rentals$CAT_AMI_HH_ROC, rentals$BEDR2), sum)
prop.table(tapply(rentals$WGTP, list(rentals$CAT_AMI_HH_ROC, rentals$BEDR2), sum, default = 0))



#===============================================================================
# Calculate the Right-Sized Affordable Housing Gap
#===============================================================================
#-------------------------------------------------------------------------------
# What is the Current Right-Sized Affordable Housing Gap?
#-------------------------------------------------------------------------------
# Create separate flags for size and affordability
rentals <- rentals %>%
  mutate(FLAG_SIZE = ifelse(get(diff_bdrm) < 0, 1, 0)) %>%
  mutate(FLAG_AFF = ifelse(GRPIP > 30, 1, 0))

flags <- rentals %>%
  select(WGTP, DIFF2, DIFF1.5, DIFF1, FLAG_SIZE, GRPIP, FLAG_AFF)

flags %>%
  summarise(overcrowded = sum(WGTP*FLAG_SIZE, na.rm = TRUE))            # 1,478 renter-occupied households are not right-sized

flags %>%
  summarise(rent_burden = sum(WGTP*FLAG_AFF, na.rm = TRUE))             # 27,380 renter-occupied households are not affordable

# Combine flags into a single measure
flags <- flags %>%
  mutate(FLAG = ifelse(FLAG_SIZE == 1 | FLAG_AFF == 1, 1, 0))

flags %>%
  summarise(need = sum(WGTP*FLAG, na.rm = TRUE))                        # 28,351 households are not right-sized and/or affordable

#-------------------------------------------------------------------------------
# How Large is the Gap Assuming Perfect Sorting Conditions?
#-------------------------------------------------------------------------------
bed_need <- paste0("BEDR", toString(bdrm))

#--------------------------------------
# Create HH/Unit-Specific Dataframes
#--------------------------------------
sort_hh <- rentals %>%
  select(SERIALNO, WGTP, HINCP, NP, BEDR2, BEDR1.5, BEDR1)

sort_unit <- rentals %>%
  select(SERIALNO, WGTP, GRNTP, AGRNTP, BDSP)

#--------------------------------------
# Sort Dataframes by HH Income/Unit Rent
#--------------------------------------
sort_hh <- sort_hh[order(sort_hh$HINCP, sort_hh$NP),]
sort_unit <- sort_unit[order(sort_unit$AGRNTP, sort_unit$BDSP),]

#--------------------------------------
# Apply Weights to Expand Dataframes
#--------------------------------------
HINCP_expand <- rep(sort_hh$HINCP, sort_hh$WGTP)
NP_expand    <- rep(sort_hh$NP, sort_hh$WGTP)
BEDR2_expand <- rep(sort_hh$BEDR2, sort_hh$WGTP)
BEDR1.5_expand <- rep(sort_hh$BEDR1.5, sort_hh$WGTP)
BEDR1_expand <- rep(sort_hh$BEDR1, sort_hh$WGTP)

AGRNTP_expand <- rep(sort_unit$AGRNTP, sort_unit$WGTP)
BDSP_expand <- rep(sort_unit$BDSP, sort_unit$WGTP)

#--------------------------------------
# Re-create Dataframe
#--------------------------------------
sort_hh <- data.frame(HINCP = HINCP_expand, NP = NP_expand, BEDR2 = BEDR2_expand, 
                      BEDR1.5 = BEDR1.5_expand, BEDR1 = BEDR1_expand)
sort_unit <- data.frame(AGRNTP = AGRNTP_expand, BDSP = BDSP_expand)

#--------------------------------------
# Initialize Loop Variables
#--------------------------------------
sort_hh <- sort_hh %>%
  mutate(MAX_RENT = HINCP*0.3, RENTED = 0, INDEX = 0)

sort_unit <- sort_unit %>%
  mutate(OCCUPIED = 0, INDEX = 0)

temp <- 1

#--------------------------------------
# Filter Datasets
#--------------------------------------
#sort_hh <- sort_hh %>%
#  filter(BEDR2 == 5 | BEDR2 == 6)

#sort_unit <- sort_unit %>%
#  filter(BDSP == 5 | BDSP == 6)

#-------------------------------------------------------------------------------
# Sorting Loop #1 -- Matching by Affordability and Size
#-------------------------------------------------------------------------------
# Match lowest-cost units to lowest income households if they can afford it
# WARNING: This loop takes ~20 minutes to run
for (row in 1:nrow(sort_unit)) {
  while (sort_unit[row, "AGRNTP"] > sort_hh[temp, "MAX_RENT"] | sort_hh[temp, "RENTED"] == 1) {
    temp <- temp + 1
  }
  
  if (temp %% 1000 == 0) {
    print(temp)
  }
  
  #--------------------------------------
  # When there is enough space
  #--------------------------------------
  if (sort_unit[row, "BDSP"] >= sort_hh[temp, bed_need]) {
    sort_unit[row, "INDEX"] <- temp
    sort_hh[temp, "INDEX"] <- row
    sort_hh[temp, "RENTED"] <- sort_hh[temp, "RENTED"] + 1
    sort_unit[row, "OCCUPIED"] <- sort_unit[row, "OCCUPIED"] + 1
    temp <- temp + 1
  }
  
  #--------------------------------------
  # When there is not enough space
  #--------------------------------------
  else if (sort_unit[row, "BDSP"] < sort_hh[temp, bed_need]) {
      temp2 <- temp + 1
      while (sort_unit[row, "BDSP"] < sort_hh[temp2, bed_need] | sort_hh[temp2, "RENTED"] == 1) {
        temp2 <- temp2 + 1
      }
      
      if (sort_unit[row, "AGRNTP"] < sort_hh[temp2, "MAX_RENT"]) {
        sort_unit[row, "INDEX"] <- temp2
        sort_hh[temp2, "INDEX"] <- row
        sort_hh[temp2, "RENTED"] <- sort_hh[temp2, "RENTED"] + 1
        sort_unit[row, "OCCUPIED"] <- sort_unit[row, "OCCUPIED"] + 1
      }
  }
  
  #--------------------------------------
  # When all units are rented
  #--------------------------------------
  if (sort_hh[nrow(sort_hh), "RENTED"] == 1) {
    break()
  }
}

#-------------------------------------------------------------------------------
# Results Analysis
#-------------------------------------------------------------------------------
#--------------------------------------
# Overall
#--------------------------------------
sort_hh$one <- 1
sort_unit$one <- 1

sort_hh %>%
  summarise(renters = sum(one), xGAP = sum(RENTED), GAP = sum(one)-sum(RENTED), GAP_pct = 1-(sum(RENTED)/sum(one)),
            pxGAP = sum(RENTED*NP), pGAP = sum(one*NP)-sum(RENTED*NP), pGAP_pct = 1-(sum(RENTED*NP)/sum(one*NP)))

sort_unit %>%
  summarise(rentals = sum(one), xGAP = sum(OCCUPIED), GAP = sum(one)-sum(OCCUPIED), GAP_pct = 1-(sum(OCCUPIED)/sum(one)))

#--------------------------------------
# 0-1 Bedroom
#--------------------------------------
sort_hh %>%
  filter(BEDR2 == 0 | BEDR2 == 1) %>%
  summarise(rentals = sum(one), xGAP = sum(RENTED), GAP = sum(one)-sum(RENTED), GAP_pct = 1-(sum(RENTED)/sum(one)),
            renters = sum(one*NP), pxGAP = sum(RENTED*NP), pGAP = sum(one*NP)-sum(RENTED*NP), pGAP_pct = 1-(sum(RENTED*NP)/sum(one*NP)))

sort_unit %>%
  filter(BDSP == 0 | BDSP == 1) %>%
  summarise(rentals = sum(one), xGAP = sum(OCCUPIED), GAP = sum(one)-sum(OCCUPIED), GAP_pct = 1-(sum(OCCUPIED)/sum(one)))

#--------------------------------------
# 2 Bedroom
#--------------------------------------
sort_hh %>%
  filter(BEDR2 == 2) %>%
  summarise(rentals = sum(one), xGAP = sum(RENTED), GAP = sum(one)-sum(RENTED), GAP_pct = 1-(sum(RENTED)/sum(one)),
            renters = sum(one*NP), pxGAP = sum(RENTED*NP), pGAP = sum(one*NP)-sum(RENTED*NP), pGAP_pct = 1-(sum(RENTED*NP)/sum(one*NP)))

sort_unit %>%
  filter(BDSP == 2) %>%
  summarise(rentals = sum(one), xGAP = sum(OCCUPIED), GAP = sum(one)-sum(OCCUPIED), GAP_pct = 1-(sum(OCCUPIED)/sum(one)))

#--------------------------------------
# 3 Bedroom
#--------------------------------------
sort_hh %>%
  filter(BEDR2 == 3) %>%
  summarise(rentals = sum(one), xGAP = sum(RENTED), GAP = sum(one)-sum(RENTED), GAP_pct = 1-(sum(RENTED)/sum(one)),
            renters = sum(one*NP), pxGAP = sum(RENTED*NP), pGAP = sum(one*NP)-sum(RENTED*NP), pGAP_pct = 1-(sum(RENTED*NP)/sum(one*NP)))

sort_unit %>%
  filter(BDSP == 3) %>%
  summarise(rentals = sum(one), xGAP = sum(OCCUPIED), GAP = sum(one)-sum(OCCUPIED), GAP_pct = 1-(sum(OCCUPIED)/sum(one)))

#--------------------------------------
# 4 Bedroom
#--------------------------------------
sort_hh %>%
  filter(BEDR2 == 4) %>%
  summarise(rentals = sum(one), xGAP = sum(RENTED), GAP = sum(one)-sum(RENTED), GAP_pct = 1-(sum(RENTED)/sum(one)),
            renters = sum(one*NP), pxGAP = sum(RENTED*NP), pGAP = sum(one*NP)-sum(RENTED*NP), pGAP_pct = 1-(sum(RENTED*NP)/sum(one*NP)))

sort_unit %>%
  filter(BDSP == 4) %>%
  summarise(rentals = sum(one), xGAP = sum(OCCUPIED), GAP = sum(one)-sum(OCCUPIED), GAP_pct = 1-(sum(OCCUPIED)/sum(one)))

#--------------------------------------
# 5+ Bedroom
#--------------------------------------
sort_hh %>%
  filter(BEDR2 == 5 | BEDR2 == 6) %>%
  summarise(rentals = sum(one), xGAP = sum(RENTED), GAP = sum(one)-sum(RENTED), GAP_pct = 1-(sum(RENTED)/sum(one)),
            renters = sum(one*NP), pxGAP = sum(RENTED*NP), pGAP = sum(one*NP)-sum(RENTED*NP), pGAP_pct = 1-(sum(RENTED*NP)/sum(one*NP)))

sort_unit %>%
  filter(BDSP == 5 | BDSP == 6) %>%
  summarise(rentals = sum(one), xGAP = sum(OCCUPIED), GAP = sum(one)-sum(OCCUPIED), GAP_pct = 1-(sum(OCCUPIED)/sum(one)))

#--------------------------------------
# Household Characteristics
#--------------------------------------
gap <- sort_hh %>%
  filter(RENTED == 0)

gap %>%
  summarise(max_income = max(HINCP))              # The max income for GAP households is $26,100

rentals %>%
  filter(HINCP <= 26100) %>%                      # In the entire rental universe, 26,025 households make <$26,100
  summarise(num = sum(WGTP))                      # Of these households, 12,907 (49.6%) are in the gap



#-------------------------------------------------------------------------------
# Sorting Loop #2 -- Matching By Rank Order
#-------------------------------------------------------------------------------
#--------------------------------------
# Recreate Dataframes
#--------------------------------------
bed_need <- paste0("BEDR", toString(bdrm))

sort_hh <- rentals %>%
  select(SERIALNO, WGTP, HINCP, NP, bed_need)

sort_unit <- rentals %>%
  select(SERIALNO, WGTP, GRNTP, AGRNTP, BDSP)

sort_hh <- sort_hh[order(sort_hh$HINCP, sort_hh$NP),]
sort_unit <- sort_unit[order(sort_unit$AGRNTP, sort_unit$BDSP),]

HINCP_expand <- rep(sort_hh$HINCP, sort_hh$WGTP)
NP_expand    <- rep(sort_hh$NP, sort_hh$WGTP)
BEDR2_expand <- rep(sort_hh$BEDR2, sort_hh$WGTP)

AGRNTP_expand <- rep(sort_unit$AGRNTP, sort_unit$WGTP)
BDSP_expand <- rep(sort_unit$BDSP, sort_unit$WGTP)

sort_hh <- data.frame(HINCP = HINCP_expand, NP = NP_expand, BEDR2 = BEDR2_expand)
sort_unit <- data.frame(AGRNTP = AGRNTP_expand, BDSP = BDSP_expand)

sort_hh <- sort_hh %>%
  mutate(MAX_RENT = HINCP*0.3, RENTED = 0, INDEX = 0)

sort_unit <- sort_unit %>%
  mutate(OCCUPIED = 0, INDEX = 0)

#--------------------------------------
# Filter by Bedroom Need/Size
#--------------------------------------
sort_hh <- sort_hh %>%
  filter(BEDR2 == 0 | BEDR2 == 1)

sort_unit <- sort_unit %>%
  filter(BDSP == 0 | BDSP == 1)

#--------------------------------------
# Run Loop
#--------------------------------------
for (row in 1:nrow(sort_unit)) {
  if (sort_unit[row, "AGRNTP"] <= sort_hh[row, "MAX_RENT"]) {
    sort_unit[row, "OCCUPIED"] <- 1
    sort_hh[row, "RENTED"] <- 1
  }
}

#--------------------------------------
# Analyze Results
#--------------------------------------
sort_hh$one <- 1
sort_unit$one <- 1

# Nonsensical result: 32,631 households are in the gap
sort_hh %>%
  summarise(renters = sum(one), xGAP = sum(RENTED), GAP = sum(one)-sum(RENTED), GAP_pct = 1-(sum(RENTED)/sum(one)),
            pxGAP = sum(RENTED*NP), pGAP = sum(one*NP)-sum(RENTED*NP), pGAP_pct = 1-(sum(RENTED*NP)/sum(one*NP)))

sort_unit %>%
  summarise(rentals = sum(one), xGAP = sum(OCCUPIED), GAP = sum(one)-sum(OCCUPIED), GAP_pct = 1-(sum(OCCUPIED)/sum(one)))

#write.csv(sort_hh, file = "./sort_hh.csv")
