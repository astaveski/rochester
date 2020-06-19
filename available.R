# ===============================================================================
# * FILE: available.R
# * PURPOSE: Conduct availability analysis for the City of Rochester
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
# Right-Sized Availability Analysis
#===============================================================================
#--------------------------------------
# Analysis of Vacant Properties
#--------------------------------------
open <- roc_hh %>%
  filter(VACS == 1 | VACS == 3)                      # Dataframe of available rentals + purchases

for_rent <- roc_hh %>%
  filter(VACS == 1)                                  # Dataframe of available rentals

tapply(roc_hh$WGTP, list(roc_hh$VACS), sum)                # There are 6,046 uninhabitable vacant properties (44.2% of total vacancies)
prop.table(tapply(roc_hh$WGTP, list(roc_hh$VACS), sum))    # There are 5,515 available rental units (40.3% of total vacancies)
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

#-------------------------------------------------------------------------------
# Quantify the Space Needs of Overcrowded Households
#-------------------------------------------------------------------------------
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

#-------------------------------------------------------------------------------
# Quantify the Available Space on the Market
#-------------------------------------------------------------------------------
#--------------------------------------
# Identify Available Bed Capacity
#--------------------------------------
# Create dataframe of available rental units
for_rent <- roc_hh %>%
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



##############################################################################################################
#                                                                                                            #
#      Without taking cost into account, overcrowding can be eliminated under current market conditions      #
#                                                                                                            #
##############################################################################################################



#===============================================================================
# Affordable Housing Availability Analysis
#===============================================================================
#--------------------------------------
# Subset Of All Occupied Rental Units
#--------------------------------------
rentals <- roc_hh %>%
  filter(GRPIP > 0)

x <- ifelse(rentals$NP == 1, 0, 1)              # Recode 1-person households as needing 0 bedrooms

# Compute Bed Need for All Renter Households
rentals <- rentals %>%
  mutate(NP_adj = NP*x) %>%                     # Adjustment to account for studio apartments
  mutate(BEDR2 = ceiling(NP_adj/2))             # Max 2 People Per Bedroom

#--------------------------------------
# Subset Of Unaffordable Rental Units
#--------------------------------------
unaffordable <- roc_hh %>%
  filter(GRPIP > 30.0)

unaffordable %>%
  summarise(expensive = sum(WGTP))            # 27,380 units are unaffordable

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
# Compute Income Cutoffs (% AMI)
#--------------------------------------
rentals <- rentals %>%
  mutate(AMI_PCT = (AGRNTP / 0.3 / ami)*100) %>%
  mutate(AMI_PCT_ROC = (AGRNTP / 0.3 / roc_median_inc)*100) %>%
  mutate(AMI_PCT_MON = (AGRNTP / 0.3 / mon_median_inc)*100)         # No more than 30% of income can be spent on rent

#--------------------------------------
# Group Income Cutoffs Into Buckets
#--------------------------------------
# [1 = <30% AMI | 2 = 30-50% AMI | 3 = 50-80% AMI | 4 = 80-120% AMI | 5 = >120% AMI]
rentals <- rentals %>%
  mutate(CAT_AMI_PCT = cut(AMI_PCT, breaks = c(-1, 30, 50, 80, 120, 10000), labels = c(1,2,3,4,5), right = TRUE)) %>%
  mutate(CAT_AMI_PCT_ROC = cut(AMI_PCT_ROC, breaks = c(-1, 30, 50, 80, 120, 10000), labels = c(1,2,3,4,5), right = TRUE)) %>%
  mutate(CAT_AMI_PCT_MON = cut(AMI_PCT_MON, breaks = c(-1, 30, 50, 80, 120, 10000), labels = c(1,2,3,4,5), right = TRUE))

#-------------------------------------------------------------------------------
# Calculate Maximum Rent (% AMI) That Each Household Can Afford
#-------------------------------------------------------------------------------
#--------------------------------------
# Compute Income Cutoffs (% AMI)
#--------------------------------------
rentals <- rentals %>%
  mutate(AMI_PCT_HH = (HINCP / ami)*100) %>%
  mutate(AMI_PCT_ROC_HH = (HINCP / roc_median_inc)*100) %>%
  mutate(AMI_PCT_MON_HH = (HINCP / mon_median_inc)*100)

#--------------------------------------
# Group Income Cutoffs Into Buckets
#--------------------------------------
rentals <- rentals %>%
  mutate(CAT_AMI_PCT_HH = cut(AMI_PCT_HH, breaks = c(-1, 30, 50, 80, 120, 10000), labels = c(1,2,3,4,5), right = TRUE)) %>%
  mutate(CAT_AMI_PCT_ROC_HH = cut(AMI_PCT_ROC_HH, breaks = c(-1, 30, 50, 80, 120, 10000), labels = c(1,2,3,4,5), right = TRUE)) %>%
  mutate(CAT_AMI_PCT_MON_HH = cut(AMI_PCT_MON_HH, breaks = c(-1, 30, 50, 80, 120, 10000), labels = c(1,2,3,4,5), right = TRUE))

#-------------------------------------------------------------------------------
# Summary Statistics
#-------------------------------------------------------------------------------
#--------------------------------------
# Using Actual AMI of $74,000
#--------------------------------------
# How many units are affordable at each AMI threshold?
tapply(rentals$WGTP, list(rentals$CAT_AMI_PCT), sum)                            # 19.9% of units are affordable to 30% AMI
prop.table(tapply(rentals$WGTP, list(rentals$CAT_AMI_PCT), sum))                # 61.9% of units are affordable to 50% AMI 

tapply(rentals$WGTP, list(rentals$CAT_AMI_PCT, rentals$BDSP), sum)
prop.table(tapply(rentals$WGTP, list(rentals$CAT_AMI_PCT, rentals$BDSP), sum, default = 0))

# How many households encompass each AMI threshold?
tapply(rentals$WGTP, list(rentals$CAT_AMI_PCT_HH), sum)                        # 44.2% of households are 30% AMI or less
prop.table(tapply(rentals$WGTP, list(rentals$CAT_AMI_PCT_HH), sum))            # 62.7% of households are 50% AMI or less

tapply(rentals$WGTP, list(rentals$CAT_AMI_PCT_HH, rentals$BEDR2), sum)
prop.table(tapply(rentals$WGTP, list(rentals$CAT_AMI_PCT_HH, rentals$BEDR2), sum, default = 0))

#--------------------------------------
# Using Monroe County AMI of $58,900
#--------------------------------------
# How many units are affordable at each AMI threshold?
tapply(rentals$WGTP, list(rentals$CAT_AMI_PCT_MON), sum)                       # 14.1% of units are affordable to 30% AMI
prop.table(tapply(rentals$WGTP, list(rentals$CAT_AMI_PCT_MON), sum))           # 37.3% of units are affordable to 50% AMI 

tapply(rentals$WGTP, list(rentals$CAT_AMI_PCT_MON, rentals$BDSP), sum)
prop.table(tapply(rentals$WGTP, list(rentals$CAT_AMI_PCT_MON, rentals$BDSP), sum, default = 0))

# How many households encompass each AMI threshold?
tapply(rentals$WGTP, list(rentals$CAT_AMI_PCT_MON_HH), sum)                    # 36.1% of households are 30% AMI or less
prop.table(tapply(rentals$WGTP, list(rentals$CAT_AMI_PCT_MON_HH), sum))        # 53.7% of households are 50% AMI or less

tapply(rentals$WGTP, list(rentals$CAT_AMI_PCT_MON_HH, rentals$BEDR2), sum)
prop.table(tapply(rentals$WGTP, list(rentals$CAT_AMI_PCT_MON_HH, rentals$BEDR2), sum, default = 0))

#--------------------------------------
# Using Rochester AMI of $35,000
#--------------------------------------
# How many units are affordable at each AMI threshold?
tapply(rentals$WGTP, list(rentals$CAT_AMI_PCT_ROC), sum)                       # 6.9% of units are affordable to 30% AMI
prop.table(tapply(rentals$WGTP, list(rentals$CAT_AMI_PCT_ROC), sum))           # 14.0% of units are affordable to 50% AMI 

tapply(rentals$WGTP, list(rentals$CAT_AMI_PCT_ROC, rentals$BDSP), sum)
prop.table(tapply(rentals$WGTP, list(rentals$CAT_AMI_PCT_ROC, rentals$BDSP), sum, default = 0))

# How many households encompass each AMI threshold?
tapply(rentals$WGTP, list(rentals$CAT_AMI_PCT_ROC_HH), sum)                    # 22.7% of households are 30% AMI or less
prop.table(tapply(rentals$WGTP, list(rentals$CAT_AMI_PCT_ROC_HH), sum))        # 36.0% of households are 50% AMI or less

tapply(rentals$WGTP, list(rentals$CAT_AMI_PCT_ROC_HH, rentals$BEDR2), sum)     # Cross-tabs with bed need


#===============================================================================
# Right-Sized Affordable Housing Gap
#===============================================================================
#-------------------------------------------------------------------------------
# Current Right-Sized Affordable Housing Deficit
#-------------------------------------------------------------------------------
# Create separate flags for size and affordability
roc_hh <- roc_hh %>%
  mutate(DIFF2 = bed_hh$DIFF2) %>%
  mutate(FLAG_SIZE = ifelse(get(diff_bdrm) < 0, 1, 0)) %>%
  mutate(FLAG_AFF = ifelse(GRPIP > 30, 1, 0))

flags <- roc_hh %>%
  select(WGTP, DIFF2, FLAG_SIZE, GRPIP, FLAG_AFF)

flags %>%
  summarise(overcrowded = sum(WGTP*FLAG_SIZE, na.rm = TRUE))            # 1,786 renter-occupied households are not right-sized

flags %>%
  summarise(rent_burden = sum(WGTP*FLAG_AFF, na.rm = TRUE))             # 27,380 renter-occupied households are not affordable

# Combine flags into a single measure
flags <- flags %>%
  mutate(FLAG = ifelse(FLAG_SIZE == 1 | FLAG_AFF == 1, 1, 0))

flags %>%
  summarise(need = sum(WGTP*FLAG, na.rm = TRUE))                        # 28,659 households are not right-sized and/or affordable

#-------------------------------------------------------------------------------
# Perfect Ordering Hypothetical
#-------------------------------------------------------------------------------
#--------------------------------------
# Data Prep
#--------------------------------------
# Create gap dataframe with relevant variables
sort_prep <- rentals %>%
  select(SERIALNO, WGTP, HINCP, AGRNTP, GRNTP, NP)

# Apply weights to each relevant variable
rent_expand <- rep(sort_prep$AGRNTP, sort_prep$WGTP)
inc_expand <- rep(sort_prep$HINCP, sort_prep$WGTP)
np_expand <- rep(sort_prep$NP, sort_prep$WGTP)

# Sort vectors from low to high
rent_expand <- sort(rent_expand, decreasing = FALSE)
inc_expand <- sort(inc_expand, decreasing = FALSE)
np_expand <- sort(np_expand, decreasing = FALSE)

# Initialize loop variables
sort <- data.frame(AGRNTP = rent_expand, HINCP = inc_expand, NP = np_expand)

sort <- sort %>%
  mutate(MAX_RENT = HINCP*0.3, OCCUPIED = 0, INDEX = 0, RENTED = 0)

temp <- 1

#--------------------------------------
# Sorting Loop
#--------------------------------------
# Match lowest-cost units to lowest income households if they can afford it
# WARNING: This loop takes ~30 seconds to run
for (row in 1:nrow(sort)) {
  while (sort[row, "AGRNTP"] >= sort[temp, "MAX_RENT"]) {
    temp <- temp + 1
  }
  if (sort[temp, "RENTED"] == 0) {
    sort[row, "INDEX"] <- temp
    sort[temp, "RENTED"] <- 1
    sort[row, "OCCUPIED"] <- 1
  }
  else if (sort[nrow(sort),"RENTED"] == 0) {
      temp <- temp + 1
      while (sort[temp, "RENTED"] == 1) {
        temp <- temp + 1
      }
      sort[row, "INDEX"] <- temp
      sort[temp, "RENTED"] <- 1
      sort[row, "OCCUPIED"] <- 1
  }
  else {
    # SKIP
  }
}

# FLAG households who fall into the gap
sort <- sort %>%
  mutate(FLAG = ifelse(RENTED == 0, 1, 0))

gap <- sort %>%
  filter(FLAG==1)

gapx <- sort %>%
  filter(FLAG==0)

#--------------------------------------
# Results Analysis
#--------------------------------------
### How big is the gap?
# 13,200 households lack affordable housing in a "perfect sort" scenario. This is 25.6% of all renters.
sort %>%
  summarise(rentals = nrow(sort), xGAP = sum(RENTED), GAP = sum(RENTED)-nrow(sort), GAP_pct = 1-(sum(RENTED)/nrow(sort)))

### What are the household characteristics of gap occupants?
gap %>%
  summarise(max_income = max(HINCP))              # The max income for GAP households is $22,000

rentals %>%
  filter(HINCP <= 22000) %>%                      # In the entire rental universe, 22,644 households make <$22,000
  summarise(num = sum(WGTP))                      # Of these households, 13,200 (58.3%) are in the gap


#write.csv(gap2, file = "./gap.csv")




# Temporary stuff

#x <- gap[order(gap$HINCP),]
#y <- gap[order(gap$AGRNTP),]

#gap2 <- data.frame(HINCP = x$HINCP,AGRNTP = y$AGRNTP)

#gap2 <- gap2 %>%
#  mutate(MAX_RENT = HINCP*0.3)

#gap2 <- gap2 %>%
#  mutate(OCCUPIED = 0, INDEX = 0, RENTED = 0)

# Can the n-th wealthiest household afford to rent the n-th most expensive unit?
#  for (row in 1:nrow(gap)) {
#    if (gap[row, "max_rent"] >= gap[row, "rent_expand"]) {
#      gap[row, "OCCUPIED"] <- 1
#      gap[row, "GAP"] <- 1
#    }
#  }
