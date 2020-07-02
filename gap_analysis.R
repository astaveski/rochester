# ===============================================================================
# * FILE: gap_analysis.R
# * PURPOSE: Estimate the right-sized affordable housing gap under current 
# *          conditions and "perfect sorting" conditions.
# * AUTHORS: Adam Staveski
# * DATE CREATED: June 12, 2020
# * DATE LAST MODIFIED: July 2, 2020
# ===============================================================================
library(readr)
library(tidyverse)
library(survey)
library(Hmisc)
library(cwhmisc)
library(ggplot2)

#--------------------------------------
# Select Dataset
#--------------------------------------
pums <- 5             # Which PUMS dataset should be used?
                      # Options: 1 / 5 --> 1-year PUMS / 5-year PUMS

#--------------------------------------
# Select Standard for Overcrowding
#--------------------------------------
bdrm  <- 2            # How many people per bedroom is "overcrowded"?
                      # Options: 2 / 1.75 / 1.5 people per bedroom

#--------------------------------------
# Select Standard for Affordability
#--------------------------------------
inc_pct <- 30         # What percent of household income is "affordable"? 
                      # Format: 30 --> 30% of household income

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
# Create Objects
#--------------------------------------
prep.names <- paste0('PWGTP', 1:80)
wrep.names <- paste0('WGTP', 1:80)

#--------------------------------------
# Load Rochester PUMS Data
#--------------------------------------
load("./roc_hh.Rda")
load("./roc_p.Rda")



#===============================================================================
# Analyze the Characteristics of Vacant Units
#===============================================================================
#--------------------------------------
# Types of Vacant Units
#--------------------------------------
open <- roc_hh %>%
  filter(VACS == 1 | VACS == 3)                                   # Dataframe of available rentals + purchases

for_rent <- roc_hh %>%
  filter(VACS == 1)                                               # Dataframe of available rentals

tapply(roc_hh$WGTP, list(roc_hh$VACS), sum)                       # There are 6,606 uninhabitable vacant properties (49.1% of total vacancies)
prop.table(tapply(roc_hh$WGTP, list(roc_hh$VACS), sum))           # There are 4,634 available rental units (34.4% of total vacancies)
                                                                  # There are 516 inhabitable homes for sale (3.8% of total vacancies)

#--------------------------------------
# Vacant Homes and Rentals
#--------------------------------------
tapply(open$WGTP, list(open$BDSP), sum)
prop.table(tapply(open$WGTP, list(open$BDSP), sum))               # 41.5% of vacant units are 1 bedroom or smaller

tapply(open$WGTP, list(open$RMSP), sum)
prop.table(tapply(open$WGTP, list(open$RMSP), sum))               # 34.7% of vacant units are 3 rooms or smaller

#--------------------------------------
# Vacant Rentals
#--------------------------------------
tapply(for_rent$WGTP, list(for_rent$BDSP), sum)
prop.table(tapply(for_rent$WGTP, list(for_rent$BDSP), sum))       # 46.1% of vacant rentals are 1 bedroom or smaller

tapply(for_rent$WGTP, list(for_rent$RMSP), sum)
prop.table(tapply(for_rent$WGTP, list(for_rent$RMSP), sum))       # 38.5% of vacant rentals are 3 rooms or smaller

rm(open)



#===============================================================================
# Quantify the Space Needs of Overcrowded Rentals
#===============================================================================
rentals <- roc_hh %>%
  filter(GRPIP > 0)

#--------------------------------------
# Create Overcrowding Variables
#--------------------------------------
x <- ifelse(rentals$NP == 1, 0, 1)               # Identify 1-person households

# Calculate bed need for each household
rentals <- rentals %>%
  mutate(NP_adj = NP*x) %>%                     # Allows 1-person households to live in studio apartments
  mutate(BEDR2 = ceiling(NP_adj/2)) %>%         # Max 2 People Per Bedroom
  mutate(BEDR1.75 = ceiling(NP_adj/1.75)) %>%   # Max 1.75 People Per Bedroom
  mutate(BEDR1.5 = ceiling(NP_adj/1.5))         # Max 1.5 People Per Bedroom

# Calculate difference between available beds and needed beds
rentals <- rentals %>%
  mutate(DIFF2 = BDSP-BEDR2) %>%
  mutate(DIFF1.75 = BDSP-BEDR1.75) %>%
  mutate(DIFF1.5 = BDSP-BEDR1.5)

#--------------------------------------
# Identify Overcrowded Households
#--------------------------------------
# User-specified definition of overcrowding
diff_bdrm <- paste0("DIFF", toString(bdrm))

# FLAG households that meet the overcrowding criteria
rentals <- rentals %>%
  mutate(FLAG_SIZE = ifelse(get(diff_bdrm) < 0, 1, 0))

# Split data into crowded and uncrowded households
rentals_crowd <- rentals %>%
  filter(get(diff_bdrm) < 0)
rentals_xcrowd <- rentals %>%
  filter(get(diff_bdrm) >= 0)

#--------------------------------------
# De-Crowd Households
#--------------------------------------
# Initiate loop variables
rentals_crowd <- rentals_crowd %>%
  mutate(DIFF_LOOP = get(diff_bdrm), NP_LOOP = NP, free_agent = 0)

# Identify number of people per household who need to move to eliminate overcrowding
for (row in 1:nrow(rentals_crowd)) {
  while (rentals_crowd[row,"DIFF_LOOP"] < 0) {
    rentals_crowd[row, "NP_LOOP"] <- rentals_crowd[row, "NP_LOOP"]-1
    rentals_crowd[row, "NP_LOOP"] <- ifelse(rentals_crowd[row, "NP_LOOP"] == 1, 0, rentals_crowd[row, "NP_LOOP"])
    rentals_crowd[row, "free_agent"]  <- rentals_crowd[row, "free_agent"]+1
    rentals_crowd[row,"DIFF_LOOP"] <- rentals_crowd[row, "BDSP"] - ceiling(rentals_crowd[row,"NP_LOOP"]/2)
  }
  rentals_crowd[row, "NP_LOOP"] <- rentals_crowd[row, "NP"] - rentals_crowd[row, "free_agent"]
}

#--------------------------------------
# Analyze Results (2 Bedroom Standard)
#--------------------------------------
rentals_crowd %>%
  summarise(movers = sum(WGTP*free_agent), movers_hh = sum(WGTP))         # 2,187 people need to move out of 1,501 households

tapply(rentals_crowd$WGTP, list(rentals_crowd$free_agent), sum)           # 1,080 households need just one person to move out



#===============================================================================
# Quantify the Available Space on the Market
#===============================================================================
#--------------------------------------
# Identify Available Bed Capacity
#--------------------------------------
for_rent %>%
  summarise(bed_capacity = sum(WGTP))                             # 4,634 rental units available on the open market

for_rent %>%
  summarise(bed_capacity = sum(WGTP*BDSP))                        # 8,359 rental bedrooms available on the open market

tapply(for_rent$WGTP, list(for_rent$BDSP), sum)                   # 2,137 vacant rentals are 1 bedroom or smaller
prop.table(tapply(for_rent$WGTP, list(for_rent$BDSP), sum))       # 46.1% of vacant rentals are 1 bedroom or smaller

#--------------------------------------
# Comparison Table (2 Bedroom Standard)
#--------------------------------------
# Quantify the bed need of overcrowded households
# 0-1 bedroom | 2 bedroom | 3 bedroom | 4+ bedroom
# 1,341       | 160       | 0         | 0
tapply(rentals_crowd$WGTP, list(rentals_crowd$free_agent), sum)

# Quantify bed availability in the open market
# 0-1 bedroom | 2 bedroom | 3 bedroom | 4+ bedroom
# 2,137       | 1,317     | 799     | 381
tapply(for_rent$WGTP, list(for_rent$BDSP), sum)



##############################################################################################################
#                                                                                                            #
#      Without taking cost into account, overcrowding can be eliminated under current market conditions      #
#                                                                                                            #
##############################################################################################################



#===============================================================================
# Calculate Maximum Income (% AMI) For Which a Unit is Affordable
#===============================================================================
#--------------------------------------
# Create Annual Gross Rent Variable
#--------------------------------------
rentals <- rentals %>%
  mutate(AGRNTP = GRNTP*(ADJHSG/1000000)*12)

#--------------------------------------
# Calculate Area Median Income
#--------------------------------------
roc_hh <- roc_hh %>%
  mutate(RHINCP = HINCP*(ADJINC/1000000))

ami_roc <- w.median(roc_hh$RHINCP, roc_hh$WGTP)                   # Median income in Rochester is $33,872

#--------------------------------------
# Compute Minimum Income Cutoffs (% AMI)
#--------------------------------------
inc_pct <- inc_pct/100

rentals <- rentals %>%
  mutate(MIN_AMI = (AGRNTP / inc_pct / ami)*100) %>%              # Assuming no more than inc_pct% of income should be spent on housing,
  mutate(MIN_AMI_ROC = (AGRNTP / inc_pct / ami_roc)*100)          # what is the minimum income required to rent a given unit?

#--------------------------------------
# Group Income Cutoffs Into Buckets
#--------------------------------------
# [1 = <30% AMI | 2 = 30-50% AMI | 3 = 50-80% AMI | 4 = 80-120% AMI | 5 = >120% AMI]
rentals <- rentals %>%
  mutate(CAT_MIN_AMI = cut(MIN_AMI, breaks = c(-1, 30, 50, 80, 120, 10000), labels = c(1,2,3,4,5), right = TRUE)) %>%
  mutate(CAT_MIN_AMI_ROC = cut(MIN_AMI_ROC, breaks = c(-1, 30, 50, 80, 120, 10000), labels = c(1,2,3,4,5), right = TRUE))



#===============================================================================
# Calculate Maximum Rent (% AMI) That Each Household Can Afford
#===============================================================================
#--------------------------------------
# Compute Household Percent AMI
#--------------------------------------
rentals <- rentals %>%
  mutate(RHINCP = HINCP*(ADJINC/1000000))

rentals <- rentals %>%
  mutate(AMI_HH = (RHINCP / ami)*100) %>%
  mutate(AMI_HH_ROC = (RHINCP / ami_roc)*100)

#--------------------------------------
# Group Income Cutoffs Into Buckets
#--------------------------------------
rentals <- rentals %>%
  mutate(CAT_AMI_HH = cut(AMI_HH, breaks = c(-1, 30, 50, 80, 120, 10000), labels = c(1,2,3,4,5), right = TRUE)) %>%
  mutate(CAT_AMI_HH_ROC = cut(AMI_HH_ROC, breaks = c(-1, 30, 50, 80, 120, 10000), labels = c(1,2,3,4,5), right = TRUE))

#-------------------------------------------------------------------------------
# Summary Statistics: Affordability at Different Income Thresholds
#-------------------------------------------------------------------------------
#--------------------------------------
# Using Actual AMI of $74,000
#--------------------------------------
# How many units are affordable at each AMI threshold?
tapply(rentals$WGTP, list(rentals$CAT_MIN_AMI), sum)                          # 17.2% of units are affordable to 30% AMI
prop.table(tapply(rentals$WGTP, list(rentals$CAT_MIN_AMI), sum))              # 62.4% of units are affordable to 50% AMI 

# Cross-tabulations by number of bedrooms
tapply(rentals$WGTP, list(rentals$BDSP, rentals$CAT_MIN_AMI), sum)
prop.table(tapply(rentals$WGTP, list(rentals$BDSP, rentals$CAT_MIN_AMI), sum, default = 0))

# How many households encompass each AMI threshold?
tapply(rentals$WGTP, list(rentals$CAT_AMI_HH), sum)                           # 44.8% of households are 30% AMI or less
prop.table(tapply(rentals$WGTP, list(rentals$CAT_AMI_HH), sum))               # 66.2% of households are 50% AMI or less

# Cross-tabulations by number of bedrooms
tapply(rentals$WGTP, list(rentals$BEDR2, rentals$CAT_AMI_HH), sum)
prop.table(tapply(rentals$WGTP, list(rentals$BEDR2, rentals$CAT_AMI_HH), sum, default = 0))

#--------------------------------------
# Using Rochester AMI of $33,872
#--------------------------------------
# How many units are affordable at each AMI threshold?
tapply(rentals$WGTP, list(rentals$CAT_MIN_AMI_ROC), sum)                       # 3.8% of units are affordable to 30% AMI
prop.table(tapply(rentals$WGTP, list(rentals$CAT_MIN_AMI_ROC), sum))           # 9.4% of units are affordable to 50% AMI 

# Cross-tabulations by number of bedrooms
tapply(rentals$WGTP, list(rentals$BDSP, rentals$CAT_MIN_AMI_ROC), sum)
prop.table(tapply(rentals$WGTP, list(rentals$BDSP, rentals$CAT_MIN_AMI_ROC), sum, default = 0))

# How many households encompass each AMI threshold?
tapply(rentals$WGTP, list(rentals$CAT_AMI_HH_ROC), sum)                        # 17.7% of households are 30% AMI or less
prop.table(tapply(rentals$WGTP, list(rentals$CAT_AMI_HH_ROC), sum))            # 34.4% of households are 50% AMI or less

# Cross-tabulations by number of bedrooms
tapply(rentals$WGTP, list(rentals$BEDR2, rentals$CAT_AMI_HH_ROC), sum)
prop.table(tapply(rentals$WGTP, list(rentals$BEDR2, rentals$CAT_AMI_HH_ROC), sum, default = 0))



##############################################################################################################
#                                                                                                            #
#   Rent prices are not available for vacant units. As a result, we cannot estimate the affordability gap.   #
#                                                                                                            #
##############################################################################################################



#===============================================================================
# Estimating the Current Right-Sized Affordable Housing Gap
#===============================================================================
#--------------------------------------
# Create Flags for Size and Affordability
#--------------------------------------
rentals <- rentals %>%
  mutate(FLAG_SIZE = ifelse(get(diff_bdrm) < 0, 1, 0)) %>%
  mutate(FLAG_AFF = ifelse(GRPIP > 30, 1, 0))

flags <- rentals %>%
  select(WGTP, DIFF2, DIFF1.75, DIFF1.5, FLAG_SIZE, GRPIP, FLAG_AFF)

flags %>%
  summarise(overcrowded = sum(WGTP*FLAG_SIZE, na.rm = TRUE))            # 1,501 renter-occupied households are not right-sized

flags %>%
  summarise(rent_burden = sum(WGTP*FLAG_AFF, na.rm = TRUE))             # 29,933 renter-occupied households are not affordable

#--------------------------------------
# Combine Flags Into a Single Measure
#--------------------------------------
flags <- flags %>%
  mutate(FLAG = ifelse(FLAG_SIZE == 1 | FLAG_AFF == 1, 1, 0))

flags %>%
  summarise(need = sum(WGTP*FLAG, na.rm = TRUE))                        # 30,671 households are not right-sized and/or affordable

#--------------------------------------
# Standard Error of Current Gap
#--------------------------------------
rentals <- rentals %>%
  mutate(FLAG = ifelse(FLAG_SIZE == 1 | FLAG_AFF == 1, 1, 0))

# Generate point estimate
se.est <- sum(ifelse(rentals$FLAG==1,rentals$WGTP,0))                   # Point Estimate: 30,671 households are not RSA

# Compute standard errors
se.rep.ests <- sapply(wrep.names, function(n) 
  sum(ifelse(rentals$FLAG==1,rentals[[n]],0)))
se <- sqrt((4/80) * sum((se.rep.ests - se.est)^2))                      # Standard Error: 731 households
se_ci90 <- c(se.est-(1.64*se), se.est+(1.64*se))                        # 90% Confidence Interval: [29,472 -- 31,870]
se_ci95 <- c(se.est-(1.96*se), se.est+(1.96*se))                        # 95% Confidence Interval: [29,238 -- 32,104]



#===============================================================================
# Estimating the Right-Sized Affordable Housing Gap Under "Perfect Sorting" Conditions
#===============================================================================
bed_need <- paste0("BEDR", toString(bdrm))

#--------------------------------------
# Create HH/Unit-Specific Dataframes
#--------------------------------------
sort_hh <- rentals %>%
  select(SERIALNO, WGTP, RHINCP, NP, BEDR2, BEDR1.75, BEDR1.5)

sort_unit <- rentals %>%
  select(SERIALNO, WGTP, GRNTP, AGRNTP, BDSP)

#--------------------------------------
# Sort Dataframes by HH Income/Unit Rent
#--------------------------------------
sort_hh <- sort_hh[order(sort_hh$RHINCP, sort_hh$NP),]
sort_unit <- sort_unit[order(sort_unit$AGRNTP, sort_unit$BDSP),]

#--------------------------------------
# Apply Weights to Expand Dataframes
#--------------------------------------
RHINCP_expand <- rep(sort_hh$RHINCP, sort_hh$WGTP)
NP_expand    <- rep(sort_hh$NP, sort_hh$WGTP)
BEDR2_expand <- rep(sort_hh$BEDR2, sort_hh$WGTP)
BEDR1.75_expand <- rep(sort_hh$BEDR1.75, sort_hh$WGTP)
BEDR1.5_expand <- rep(sort_hh$BEDR1.5, sort_hh$WGTP)

AGRNTP_expand <- rep(sort_unit$AGRNTP, sort_unit$WGTP)
BDSP_expand <- rep(sort_unit$BDSP, sort_unit$WGTP)

#--------------------------------------
# Re-create Dataframe
#--------------------------------------
sort_hh <- data.frame(RHINCP = RHINCP_expand, NP = NP_expand, BEDR2 = BEDR2_expand, 
                      BEDR1.5 = BEDR1.5_expand, BEDR1.75 = BEDR1.75_expand)
sort_unit <- data.frame(AGRNTP = AGRNTP_expand, BDSP = BDSP_expand)

#--------------------------------------
# Initialize Loop Variables
#--------------------------------------
sort_hh <- sort_hh %>%
  mutate(MAX_RENT = RHINCP*inc_pct, RENTED = 0, INDEX = 0)

sort_unit <- sort_unit %>%
  mutate(OCCUPIED = 0, INDEX = 0)

#-------------------------------------------------------------------------------
# Sorting Loop -- Takes About 75 Seconds to Run
#-------------------------------------------------------------------------------
for (row in 1:nrow(sort_unit)) {
  #--------------------------------------
  # Determine Unit Cost and Size Criteria
  #--------------------------------------
  rent <- sort_unit[row,"AGRNTP"]
  beds <- sort_unit[row,"BDSP"]
  
  #--------------------------------------
  # Match Unit to Minimum HH That Meets Criteria
  #--------------------------------------
  index <- which(sort_hh$MAX_RENT >= rent & sort_hh$RENTED == 0 & sort_hh[,bed_need] <= beds)[1]
  index <- ifelse(is.na(index),0,index)
  
  #--------------------------------------
  # Update Dataframes to Reflect the Match
  #--------------------------------------
  sort_hh[index,"INDEX"] <- row
  sort_hh[index,"RENTED"] <- sort_hh[index,"RENTED"] + 1
  
  sort_unit[row,"INDEX"] <- index
  sort_unit[row,"OCCUPIED"] <- ifelse(sort_unit[row,"INDEX"] == 0, 0, 1)
  
  #--------------------------------------
  # Break at Wealthiest Renter
  #--------------------------------------
  if (sort_hh[nrow(sort_hh), "RENTED"] == 1) {
    break()
  }
}

rm(x, row, index, rent, beds, AGRNTP_expand, BDSP_expand, RHINCP_expand, NP_expand)
rm(BEDR1.5_expand, BEDR1.75_expand, BEDR2_expand)

#-------------------------------------------------------------------------------
# Results Analysis
#-------------------------------------------------------------------------------
sort_hh$one <- 1
sort_unit$one <- 1

#--------------------------------------
# Overall
#--------------------------------------
sort_hh %>%
  summarise(rentals = sum(one), xGAP = sum(RENTED), GAP = sum(one)-sum(RENTED), GAP_pct = 1-(sum(RENTED)/sum(one)),
            renters = sum(one*NP), pxGAP = sum(RENTED*NP), pGAP = sum(one*NP)-sum(RENTED*NP), pGAP_pct = 1-(sum(RENTED*NP)/sum(one*NP)))

sort_unit %>%
  summarise(rentals = sum(one), xGAP = sum(OCCUPIED), GAP = sum(one)-sum(OCCUPIED), GAP_pct = 1-(sum(OCCUPIED)/sum(one)))

#--------------------------------------
# 0-1 Bedroom
#--------------------------------------
sort_hh %>%
  filter(get(bed_need) == 0 | get(bed_need) == 1) %>%
  summarise(rentals = sum(one), xGAP = sum(RENTED), GAP = sum(one)-sum(RENTED), GAP_pct = 1-(sum(RENTED)/sum(one)),
            renters = sum(one*NP), pxGAP = sum(RENTED*NP), pGAP = sum(one*NP)-sum(RENTED*NP), pGAP_pct = 1-(sum(RENTED*NP)/sum(one*NP)))

sort_unit %>%
  filter(BDSP == 0 | BDSP == 1) %>%
  summarise(rentals = sum(one), xGAP = sum(OCCUPIED), GAP = sum(one)-sum(OCCUPIED), GAP_pct = 1-(sum(OCCUPIED)/sum(one)))

#--------------------------------------
# 2 Bedroom
#--------------------------------------
sort_hh %>%
  filter(get(bed_need) == 2) %>%
  summarise(rentals = sum(one), xGAP = sum(RENTED), GAP = sum(one)-sum(RENTED), GAP_pct = 1-(sum(RENTED)/sum(one)),
            renters = sum(one*NP), pxGAP = sum(RENTED*NP), pGAP = sum(one*NP)-sum(RENTED*NP), pGAP_pct = 1-(sum(RENTED*NP)/sum(one*NP)))

sort_unit %>%
  filter(BDSP == 2) %>%
  summarise(rentals = sum(one), xGAP = sum(OCCUPIED), GAP = sum(one)-sum(OCCUPIED), GAP_pct = 1-(sum(OCCUPIED)/sum(one)))

#--------------------------------------
# 3 Bedroom
#--------------------------------------
sort_hh %>%
  filter(get(bed_need) == 3) %>%
  summarise(rentals = sum(one), xGAP = sum(RENTED), GAP = sum(one)-sum(RENTED), GAP_pct = 1-(sum(RENTED)/sum(one)),
            renters = sum(one*NP), pxGAP = sum(RENTED*NP), pGAP = sum(one*NP)-sum(RENTED*NP), pGAP_pct = 1-(sum(RENTED*NP)/sum(one*NP)))

sort_unit %>%
  filter(BDSP == 3) %>%
  summarise(rentals = sum(one), xGAP = sum(OCCUPIED), GAP = sum(one)-sum(OCCUPIED), GAP_pct = 1-(sum(OCCUPIED)/sum(one)))

#--------------------------------------
# 4 Bedroom
#--------------------------------------
sort_hh %>%
  filter(get(bed_need) == 4) %>%
  summarise(rentals = sum(one), xGAP = sum(RENTED), GAP = sum(one)-sum(RENTED), GAP_pct = 1-(sum(RENTED)/sum(one)),
            renters = sum(one*NP), pxGAP = sum(RENTED*NP), pGAP = sum(one*NP)-sum(RENTED*NP), pGAP_pct = 1-(sum(RENTED*NP)/sum(one*NP)))

sort_unit %>%
  filter(BDSP == 4) %>%
  summarise(rentals = sum(one), xGAP = sum(OCCUPIED), GAP = sum(one)-sum(OCCUPIED), GAP_pct = 1-(sum(OCCUPIED)/sum(one)))

#--------------------------------------
# 5+ Bedroom
#--------------------------------------
sort_hh %>%
  filter(get(bed_need) >= 5) %>%
  summarise(rentals = sum(one), xGAP = sum(RENTED), GAP = sum(one)-sum(RENTED), GAP_pct = 1-(sum(RENTED)/sum(one)),
            renters = sum(one*NP), pxGAP = sum(RENTED*NP), pGAP = sum(one*NP)-sum(RENTED*NP), pGAP_pct = 1-(sum(RENTED*NP)/sum(one*NP)))

sort_unit %>%
  filter(BDSP >= 5) %>%
  summarise(rentals = sum(one), xGAP = sum(OCCUPIED), GAP = sum(one)-sum(OCCUPIED), GAP_pct = 1-(sum(OCCUPIED)/sum(one)))

#--------------------------------------
# Household Characteristics
#--------------------------------------
sort_hh %>%
  filter(RENTED == 0) %>%                                                              # The max income for GAP households is $26,442
  summarise(max_inc = max(RHINCP), avg_inc = mean(RHINCP), med_inc = median(RHINCP))   # The median income for GAP households is $10,188

rentals %>%
  filter(RHINCP <= 26442) %>%                     # In the entire rental universe, 27,058 households make <$26,442
  summarise(num = sum(WGTP))                      # Of these households, 14,839 (54.8%) are in the gap

rentals %>%
  filter(RHINCP <= 10188) %>%                     # In the entire rental universe, 9,465 households make <= $10,188
  summarise(num = sum(WGTP))                      # Of these households, 7,420 (78.4%) are in the gap

sort_hh %>%
  filter(RENTED == 0 & RHINCP <= 10188) %>%
  summarise(gap_poor = sum(one))



#===============================================================================
# Standard Error of the Right-Sized Affordable Housing Gap
#===============================================================================
gap_count <- sum(ifelse(sort_hh$RENTED==0,1,0))
xgap_count <- sum(sort_hh$RENTED)
rent_count <- nrow(sort_hh)

#--------------------------------------
# Create SE Dataframes
#--------------------------------------
prep_hh <- rentals %>%
  select(SERIALNO, WGTP, RHINCP, NP, BEDR2, BEDR1.75, BEDR1.5, wrep.names)

prep_unit <- rentals %>%
  select(SERIALNO, WGTP, GRNTP, AGRNTP, BDSP, wrep.names)

#--------------------------------------
# Sort Dataframes
#--------------------------------------
prep_hh <- prep_hh[order(prep_hh$RHINCP, prep_hh$NP),]
prep_unit <- prep_unit[order(prep_unit$AGRNTP, prep_unit$BDSP),]

#--------------------------------------
# Apply Replicate Weights
#--------------------------------------
for (i in 1:80) {
  wgt <- paste0("WGTP", i)
  
  # Apply weights to expand dataframes
  RHINCP_expand <- rep(prep_hh$RHINCP, prep_hh[[wgt]])
  NP_expand    <- rep(prep_hh$NP, prep_hh[[wgt]])
  BEDR2_expand <- rep(prep_hh$BEDR2, prep_hh[[wgt]])
  
  AGRNTP_expand <- rep(prep_unit$AGRNTP, prep_unit[[wgt]])
  BDSP_expand <- rep(prep_unit$BDSP, prep_unit[[wgt]])

  # Re-create dataframes
  rep_hh <- data.frame(MAX_RENT = RHINCP_expand*inc_pct, NP = NP_expand, BEDR2 = BEDR2_expand, RENTED = 0, INDEX = 0)
  rep_unit <- data.frame(AGRNTP = AGRNTP_expand, BDSP = BDSP_expand, OCCUPIED = 0, INDEX = 0)
  
  #--------------------------------------
  # Sorting Loop With Replicate Weights
  #--------------------------------------
  for (row in 1:nrow(rep_unit)) {
    #--------------------------------------
    # Determine Unit Cost and Size Criteria
    #--------------------------------------
    rent <- rep_unit[row,"AGRNTP"]
    beds <- rep_unit[row,"BDSP"]
    
    #--------------------------------------
    # Match Unit to Minimum HH That Meets Criteria
    #--------------------------------------
    index <- which(rep_hh$MAX_RENT >= rent & rep_hh$RENTED == 0 & rep_hh[,bed_need] <= beds)[1]
    index <- ifelse(is.na(index),0,index)
    
    #--------------------------------------
    # Update Dataframes to Reflect the Match
    #--------------------------------------
    rep_hh[index,"INDEX"] <- row
    rep_hh[index,"RENTED"] <- rep_hh[index,"RENTED"] + 1
    
    rep_unit[row,"INDEX"] <- index
    rep_unit[row,"OCCUPIED"] <- ifelse(rep_unit[row,"INDEX"] == 0, 0, 1)
    
    #--------------------------------------
    # Break at Wealthiest Renter
    #--------------------------------------
    if (rep_hh[nrow(rep_hh), "RENTED"] == 1) {
      break()
    }
  }
  xgap_est <- sum(rep_hh$RENTED)
  xgap_count <- c(xgap_count,xgap_est)
  
  gap_est <- sum(ifelse(rep_hh$RENTED==0,1,0))
  gap_count <- c(gap_count,gap_est)
  
  rent_est <- sum(xgap_est,gap_est)
  rent_count <- c(rent_count,rent_est)
}

#--------------------------------------
# Standard Error Computations
#--------------------------------------
# Affordable Housing Gap
gap_est <- sum(ifelse(sort_hh$RENTED==0,1,0))                               # Point Estimate:           14,839 households
gap_se <- sqrt((4/80) * sum((gap_count - gap_est)^2))                       # Standard Error:              631 households
gap_ci90 <- c(gap_est-(1.64*gap_se), gap_est+(1.64*gap_se))                 # 90% Confidence Interval: [13,803 -- 15,875]
gap_ci95 <- c(gap_est-(1.96*gap_se), gap_est+(1.96*gap_se))                 # 95% Confidence Interval: [13,601 -- 16,077]

# Affordable Housing XGap
xgap_est <- sum(sort_hh$RENTED)                                             # Point Estimate:           37,527 households
xgap_se <- sqrt((4/80) * sum((xgap_count - xgap_est)^2))                    # Standard Error:              879 households
xgap_ci90 <- c(xgap_est-(1.64*xgap_se), xgap_est+(1.64*xgap_se))            # 90% Confidence Interval: [36,086 -- 38,968]
xgap_ci95 <- c(xgap_est-(1.96*xgap_se), xgap_est+(1.96*xgap_se))            # 95% Confidence Interval: [35,805 -- 39,249]

# Total Renters
rent_est <- nrow(sort_hh)                                                   # Point Estimate:           52,366 households
rent_se <- sqrt((4/80) * sum((rent_count - rent_est)^2))                    # Standard Error:              771 households
rent_ci90 <- c(rent_est-(1.64*rent_se), rent_est+(1.64*rent_se))            # 90% Confidence Interval: [51,101 -- 53,631]
rent_ci95 <- c(rent_est-(1.96*rent_se), rent_est+(1.96*rent_se))            # 95% Confidence Interval: [50,854 -- 53,878]

# Proportion of Renters in Gap
prop <- (gap_est/rent_est)                                                  # Point Estimate:            28.3%
prop_se <- (1/rent_est) * sqrt(gap_se^2 - (prop^2*rent_se^2))               # Standard Error:             1.1%
prop_ci90 <- c(prop-(1.64*prop_se), prop+(1.64*prop_se))                    # 90% Confidence Interval:  [26.5% -- 30.2%]
prop_ci95 <- c(prop-(1.96*prop_se), prop+(1.96*prop_se))                    # 95% Confidence Interval:  [26.1% -- 30.6%]



#===============================================================================
# Export to .CSV
#===============================================================================
#write.csv(sort_hh, file = "./Data_Subsets/sort_hh.csv")
#write.csv(sort_unit, file = "./Data_Subsets/sort_unit.csv")
