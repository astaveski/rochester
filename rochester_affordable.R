# ===============================================================================
# * FILE: 03_rochester.R
# * PURPOSE: Import and Analyze PUMS data from 2018
# * AUTHORS: Adam Staveski, Andrea Ringer
# * DATE CREATED: June 9, 2020
# * DATE LAST MODIFIED: June 16, 2020
# ===============================================================================
library(readr)
library(tidyverse)
library(survey)
library(Hmisc)
library(cwhmisc)

setwd("/Users/andrearinger/Documents/PUMS Data")

#-------------------------------------------------------------------------------
# Load PUMS Household Data and Select Rochester PUMAs
#-------------------------------------------------------------------------------
pums_hh <- read_csv("psam_h36_monroe.csv", guess_max=12000)
pums_all <- read_csv("psam_all_monroe.csv", guess_max = 12000)

hh_roc <- pums_hh %>%
  filter(PUMA == "902" | PUMA == "903")
all_roc <- pums_all %>%
  filter(PUMA == "902" | PUMA == "903")
hh_monroe <- pums_hh

#-------------------------------------------------------------------------------
# Clean Person and Household Data
#-------------------------------------------------------------------------------
# Separate year and ID number from SERIALNO
p_roc$year=substr(as.character(p_roc$SERIALNO),1,4)
p_roc$id=substr(as.character(p_roc$SERIALNO),5,25)

hh_roc$year=substr(as.character(hh_roc$SERIALNO),1,4)
hh_roc$id=substr(as.character(hh_roc$SERIALNO),5,25)


#-------------------------------------------------------------------------------
# GRPIP and HINCP: Gross rent as a percentage of HH income; HH income
#-------------------------------------------------------------------------------
gross_rent_perc <- select(hh_roc, GRPIP, WGTP) %>% tally(wt=WGTP)

ggplot(hh_roc, aes(x=GRPIP, weight = WGTP)) + 
  geom_histogram() + 
  stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust=-1.0)  # Gross rent as % HH income
         # Why do so many people spend over 100% of income on rent?

# For those with GRPIP==101, weighted histogram of household incomes over last 12 months
hh_roc %>%
  filter(GRPIP==101) %>%
  ggplot(aes(x=HINCP, weight = WGTP)) + geom_histogram() + 
  stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust=-1.0)

# For those with GRPIP==101, weighted summary of household incomes over last 12 months
hh_roc %>%
  select(WGTP, GRPIP, HINCP) %>%
  filter(GRPIP==101) %>%
  summary(HINCP, wt=WGTP)

# Generate categories for rent burden
hh_roc$GRPIP_cat <- cut(hh_roc$GRPIP, breaks = c(0, 30, 50, 60, 80, 100, 10000000), labels = c(1,2,3,4,5,6), right = TRUE)
summary(hh_roc$GRPIP_cat)
tapply(hh_roc$WGTP, list(hh_roc$GRPIP_cat), sum)
prop.table(tapply(hh_roc$WGTP, list(hh_roc$GRPIP_cat), sum))
     # <30% income:     42.8%
     # 30-50% income:   24.8%
     # 50-60% income:   5.7%
     # 60-80% income:   7.9%
     # 80-100% income:  5.2%
     # >100% income:    13.5%

# Graph of income related to rent burden
hh_roc %>%
  ggplot() +
  geom_point(aes(x=HINCP, y=GRPIP, size=WGTP), shape=21) +
  xlim(0,100000)   # graph view restricted to HINCP from $0-$100,000
    # Further analysis: put in 30% line, calculate percent of HHs above line

# Histogram of household income
hh_roc %>%
  ggplot(aes(x=HINCP, weight=WGTP)) +
  geom_histogram() +
  stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust=-1.0)

#-------------------------------------------------------------------------------
# Characteristics of those who are rent burdened - HH data
#-------------------------------------------------------------------------------
hh_rental <- hh_roc %>%
  filter(hh_roc$TEN==3)         # Rental HHs (excludes homeowner HHs)

rent_bur <- hh_roc %>%
  filter(GRPIP_cat %in% 2:5)    # >=30% income and <=100% income on rent
summary(rent_bur$GRPIP_cat)

rent_bur_101 <- hh_roc %>%
  filter(GRPIP_cat==6)          # >100% income on rent
summary(rent_bur_101$GRPIP_cat)

rent_bur_30to50 <- hh_roc %>%
  filter(GRPIP_cat==2)        # >=30% and <50% income on rent
summary(rent_bur_30to50$GRPIP_cat)

rent_bur_50to100 <- hh_roc %>%
  filter(GRPIP_cat %in% 3:5)
summary(rent_bur_50to100$GRPIP_cat)

# ---------------- FES: Family Type and Employment Status ----------------------
# 1-4 Married Couple Family
#      1 = Husband and wife in LF; 2 = Husband in LF, wife not in LF
#      3 = Husband not in LF, wife in LF; 4 = Neither husband nor wife in LF
# 5-8 Other Family
#      5 = Male HHer, no wife present, LF; 6 = Male HHer, no wife present, not in LF
#      7 = Female HHer, no husband present, LF; 8 = Female HHer, no husband present, not in LF

# All HHs
prop.table(tapply(hh_roc$WGTP, list(hh_roc$FES), sum))
     # Largest categories: 1 (20.2%), 7 (33.2%), 8 (17.8%)

# Rent burdened households (>=30% income and <=100%)
prop.table(tapply(rent_bur$WGTP, list(rent_bur$FES), sum))
     # Largest categories: 7 (52.2%), 8 (23.8%)

# Severely rent burdened households >100%
prop.table(tapply(rent_bur_101$WGTP, list(rent_bur_101$FES), sum))
     # Largest categories: 7 (34.7%), 8 (54.2%)

# Highly rent burdened households (>=50% and <=100%)
prop.table(tapply(rent_bur_50to100$WGTP, list(rent_bur_50to100$FES), sum))
     # Largest categories: 7 (51.2%), 8 (30.0%)

# Slightly rent burdened households (>=30% and <50%)
tapply(rent_bur_30to50$WGTP, list(rent_bur_30to50$FES), sum)
prop.table(tapply(rent_bur_30to50$WGTP, list(rent_bur_30to50$FES), sum))
     # Largest categories: 5 (7.5%), 7 (53.0%), 8 (18.7%)

# ------------------- HHT: Household/Family Type -------------------------------
# 1 = Married couple HH
# 2 = Other family HH: Male HHer, no spouse present
# 3 = Other family HH: Female HHer, no spouse present
# 4-7 Non family HH
#     4 = Male HHer, living alone; 5 = Male HHer, not living alone
#     6 = Female HHer, living alone; 7 = Female HHer, not living alone

# All HHs 
prop.table(tapply(hh_roc$WGTP, list(hh_roc$HHT), sum))
       # Largest categories: 1 (19.1%), 3 (24.3%), 4 (20.4%), 6 (20.7%)

# Rent burdened households (>=30% income and <=100%)
prop.table(tapply(rent_bur$WGTP, list(rent_bur$HHT), sum))
       # Largest categories: 3 (34.7%), 4 (23.8%), 6 (23.7%)

# Severely rent burdened households >100%
prop.table(tapply(rent_bur_101$WGTP, list(rent_bur_101$HHT), sum))
       # Largest categories: 3 (49.0%), 4 (18.0%), 6 (21.9%)

# Highly rent burdened households (>=50% and <=100%)
prop.table(tapply(rent_bur_50to100$WGTP, list(rent_bur_50to100$HHT), sum))
       # Largest categories: 3 (38.8%), 4 (21.1%), 6 (26.2%)

# Slightly rent burdened households (>=30% and <50%)
prop.table(tapply(rent_bur_30to50$WGTP, list(rent_bur_30to50$HHT), sum))
       # Largest categories: 1 (7.7%), 3 (31.5%), 4 (25.9%), 6 (21.7%)

# I find it interesting that:
       # 7.7% of married couples are slightly rent burdened
       # Those not living alone are not likely to be rent burdened
       # Moving from slightly to highly rent burdened, proportion of single male HHs
       # decreases while proportion of single female HHs increases

# ---------------------- HHL: Household Language -------------------------------
# (1) English only, (2) Spanish, (3) Other Indo-European languages
# (4) Asian and Pacific Island languages, (5) Other languages

# All HHs 
prop.table(tapply(hh_roc$WGTP, list(hh_roc$HHL), sum))
      # Largest categories: 1 (79.6%), 2 (13.5%), (3.6%)

# Rent burdened households (>=30% income and <=100%)
prop.table(tapply(rent_bur$WGTP, list(rent_bur$HHL), sum))
      # Largest categories: 3 (78.8%), 2 (16.8%), 3 (2.5%)

# Severely rent burdened households >100%
prop.table(tapply(rent_bur_101$WGTP, list(rent_bur_101$HHL), sum))
      # Largest categories: 1 (77.0%), 2 (18.5%), 3 (2.1%)

# Highly rent burdened households (>=50% and <=100%)
prop.table(tapply(rent_bur_50to100$WGTP, list(rent_bur_50to100$HHL), sum))
     # Largest categories: 1 (75.7%), 2 (18.8%), 3 (3.8%)

# Slightly rent burdened households (>=30% and <50%)
prop.table(tapply(rent_bur_30to50$WGTP, list(rent_bur_30to50$HHL), sum))
      # Largest categories: 1 (81.1%), 2 (15.3%), 3 (1.6%)

# I find it interesting that:
          # Moving from slightly to highly rent burdened, proportion of households
          # that are English speaking decreases, and proportion that are Spanish and 
          # Indo-European speaking increases

# ---------------------- MV: When moved into apartment -------------------------
# 1 = 12 mos or less; 2 = 13 to 23 mos; 3 = 2-4 years; 4 = 5-9 years
# 5 = 10-19 years; 6 = 20-29 years; 7 = 30+ years

# All Rental HHs 
prop.table(tapply(hh_rental$WGTP, list(hh_rental$MV), sum))
        # Largest categories: 1 (28.8%), 2 (11.3%), 3 (28.8%), 4 (16.7%), 5 (10.5%)

# Rent burdened households (>=30% income and <=100%)
prop.table(tapply(rent_bur$WGTP, list(rent_bur$MV), sum))
        # Largest categories: 1 (26.3%), 2 (10.7%), 3 (30.4%), 4 (17.7%), 5 (10.9%)

# Severely rent burdened households >100%
prop.table(tapply(rent_bur_101$WGTP, list(rent_bur_101$MV), sum))
        # Largest categories: 1 (36.4%), 2 (10.7%), 3 (26.9%), 4 (13.7%), 5 (9.6%)

# Highly rent burdened households (>=50% and <=100%)
prop.table(tapply(rent_bur_50to100$WGTP, list(rent_bur_50to100$MV), sum))
        # Largest categories: 1 (28.4%), 2 (11.9%), 3 (31.1%), 4 (14.3%), 5 (10.9%)

# Slightly rent burdened households (>=30% and <50%)
prop.table(tapply(rent_bur_30to50$WGTP, list(rent_bur_30to50$MV), sum))
        # Largest categories: 1 (24.7%), 2 (9.7%), 3 (29.9%), 4 (20.3%), 5 (10.9%)

# I find it surprising that:
      # the breakdown is relatively stable across rent burden. I expected that those
      # who are more rent burdened would have lived in their apartment for less time.
# I find it interesting that:
      # moving from highly to severely rent burdened, the proportion of people who moved
      # in within the 12 mos or less increases significantly


#-------------------------------------------------------------------------------
# Characteristics of those who are rent burdened - merged data
#-------------------------------------------------------------------------------
# Generate categories for rent burden in merged data
all_roc$GRPIP_cat <- cut(all_roc$GRPIP, breaks = c(0, 30, 50, 60, 80, 100, 10000000), labels = c(1,2,3,4,5,6), right = TRUE)
summary(all_roc$GRPIP_cat)
prop.table(tapply(all_roc$WGTP_HH, list(all_roc$GRPIP_cat), sum))
# Sanity check: the proportions are the same as when I used just the HH data,
# which means the HH weights I created in the merged dataset are correct
# (I created the correct weight variable in Stata. The variable is "WGTP_HH")

# Generate rent burdened category variables
rent_bur_all <- all_roc %>% filter(GRPIP_cat %in% 2:5)  # >=30% income and <=100% income on rent
rent_bur_severe <- all_roc %>% filter(GRPIP_cat==6)     # >100% income on rent
rent_bur_slight <- all_roc %>% filter(GRPIP_cat==2)     # >=30% and <50% income on rent
rent_bur_high <- all_roc %>% filter(GRPIP_cat %in% 3:5) # >=50% and <=100% income on rent
rent_all <- all_roc %>% filter(TEN==3)  # All rental HHs

# --------------------------- AGEP: AGE ----------------------------------------
# What is the average age of male and female single rental HHs? (see HHT section above)
hh_single <- all_roc %>% filter((HHT==4 | HHT==6) & TEN==3)
hh_single_f <- all_roc %>% filter(HHT==6 & TEN==3)
hh_single_m <- all_roc %>%  filter(HHT==4 & TEN ==3)

# Rent-burdened single HHs
rent_bur_single <- rent_bur_all %>% filter(HHT==4 | HHT==6)
rent_bur_single_f <- rent_bur_all %>% filter(HHT==6)
rent_bur_single_m <- rent_bur_all %>% filter(HHT==4)

# Average age of single HHs, by gender
weighted.mean(hh_single$AGEP, hh_single$PWGTP)       # all: 48.8 years old
weighted.mean(hh_single_f$AGEP, hh_single_f$PWGTP)   # female: 50.1 years old
weighted.mean(hh_single_m$AGEP, hh_single_m$PWGTP)   # male: 47.6 years old

# Average age of rent-burdened single HHs 
weighted.mean(rent_bur_single$AGEP, rent_bur_single$PWGTP)   # all: 51.1 years old
weighted.mean(rent_bur_single_f$AGEP, rent_bur_single_f$PWGTP)   # female: 51.9 years old
weighted.mean(rent_bur_single_m$AGEP, rent_bur_single_m$PWGTP)   # male: 50.3 years old

# Create age categories
hh_single$age_cat <- cut(hh_single$AGEP, breaks = c(0, 20, 30, 50, 70, 10000000), labels = c(1,2,3,4,5), right = TRUE)
hh_single_f$age_cat <- cut(hh_single_f$AGEP, breaks = c(0, 20, 30, 50, 70, 10000000), labels = c(1,2,3,4,5), right = TRUE)
hh_single_m$age_cat <- cut(hh_single_m$AGEP, breaks = c(0, 20, 30, 50, 70, 10000000), labels = c(1,2,3,4,5), right = TRUE)
rent_bur_single$age_cat <- cut(rent_bur_single$AGEP, breaks = c(0, 20, 30, 50, 70, 10000000), labels = c(1,2,3,4,5), right = TRUE)
rent_bur_single_f$age_cat <- cut(rent_bur_single_f$AGEP, breaks = c(0, 20, 30, 50, 70, 10000000), labels = c(1,2,3,4,5), right = TRUE)
rent_bur_single_m$age_cat <- cut(rent_bur_single_m$AGEP, breaks = c(0, 20, 30, 50, 70, 10000000), labels = c(1,2,3,4,5), right = TRUE)

# Single renter HHs: all, female, male
prop.table(tapply(hh_single$PWGTP, list(hh_single$age_cat), sum))
      # Age <20:   0.6%
      # Age 20-30: 18.8%
      # Age 30-50: 31.4%
      # Age 50-70: 39.2%
      # Age 70+:   9.9%

prop.table(tapply(hh_single_f$PWGTP, list(hh_single_f$age_cat), sum)) # female: much larger prop. 70+
prop.table(tapply(hh_single_m$PWGTP, list(hh_single_m$age_cat), sum)) # male: much larger prop. 30-50

# Rent-burdened single renter HHs
prop.table(tapply(rent_bur_single$PWGTP, list(rent_bur_single$age_cat), sum))
      # Age <20:   1.0%
      # Age 20-30: 14.6%
      # Age 30-50: 29.3%
      # Age 50-70: 43.0%
      # Age 70+:   12.1%

# Rent-burdened single renter HHs: Female
prop.table(tapply(rent_bur_single_f$PWGTP, list(rent_bur_single_f$age_cat), sum))
      # Age <20:   1.7%
      # Age 20-30: 16.9%
      # Age 30-50: 22.8%
      # Age 50-70: 42.5%
      # Age 70+:   16.0%

# Rent-burdened single renter HHs: Male
prop.table(tapply(rent_bur_single_m$PWGTP, list(rent_bur_single_m$age_cat), sum))
      # Age <20:   0.3%
      # Age 20-30: 12.4%
      # Age 30-50: 35.6%
      # Age 50-70: 43.5%
      # Age 70+:   8.2%

# ---------------------------------- RACE --------------------------------------
# Create RACE
# 1 = White alone; 2 = Black alone; 3 = American Indian alone; 4 = Alaska Native alone
# 5 = American Indian & Alaskan Native; 6 = Asian alone; 7 = Native Hawaiian / Pacific Islander alone
# 8 = Some other race alone; 9 = Two or more races; 10 = Hispanic

# Race proportions in Rochester population
all_roc$RACE = ifelse(all_roc$HISP == 01, all_roc$RAC1P, 10)
prop.table(tapply(all_roc$PWGTP, list(all_roc$RACE), sum))
       # 36.6% White, 38.3% Black, 18.4% Hispanic

# For now I'll look at the population. I need to figure out how to collapse at the
# HH level after creating the RACE variable, to do the HH analysis (will be more accurate)

# Race of renter household population
rent_all$RACE = ifelse(rent_all$HISP == 01, rent_all$RAC1P, 10)
tapply(rent_all$PWGTP, list(rent_all$RACE), sum)
prop.table(tapply(rent_all$PWGTP, list(rent_all$RACE), sum))
       # 22.6% White, 49.1% Black, 22.2% Hispanic

# Race of rent burdened population
rent_bur_all$RACE = ifelse(rent_bur_all$HISP == 01, rent_bur_all$RAC1P, 10)
prop.table(tapply(rent_bur_all$PWGTP, list(rent_bur_all$RACE), sum))
       # 22.6% White, 49.1% Black, 22.2% Hispanic

# Race of slightly rent burdened population
rent_bur_slight$RACE = ifelse(rent_bur_slight$HISP == 01, rent_bur_slight$RAC1P, 10)
prop.table(tapply(rent_bur_slight$PWGTP, list(rent_bur_slight$RACE), sum))
      # 24.7% White, 49.5% Black, 20.3% Hispanic

# Race of highly rent burdened population
rent_bur_high$RACE = ifelse(rent_bur_high$HISP == 01, rent_bur_high$RAC1P, 10)
prop.table(tapply(rent_bur_high$PWGTP, list(rent_bur_high$RACE), sum))
      # 19.9% White, 48.5% Black, 24.6% Hispanic
 
# Race of severely rent burdened population
rent_bur_severe$RACE = ifelse(rent_bur_severe$HISP == 01, rent_bur_severe$RAC1P, 10)
prop.table(tapply(rent_bur_severe$PWGTP, list(rent_bur_severe$RACE), sum))
      # 16.8% White, 50.0% Black, 27.7% Hispanic

# Race of single renter HHs (see section HHT above)
hh_single$RACE = ifelse(hh_single$HISP == 01, hh_single$RAC1P, 10)
prop.table(tapply(hh_single$PWGTP, list(hh_single$RACE), sum))
      # 48.1% White, 35.1% Black, 12.3% Hispanic

# Race of rent-burdened single renter HHs
rent_bur_single$RACE = ifelse(rent_bur_single$HISP == 01, rent_bur_single$RAC1P, 10)
prop.table(tapply(rent_bur_single$PWGTP, list(rent_bur_single$RACE), sum))
      # 44.6% White, 39.0% Black, 13.5% Hispanic

# ---------------------- MAR: Marital Status -----------------------------------
# Marital status of single-renter HHs
# 1 = Married, 2 = Widowed, 3 = Divorced, 4 = Separated, 5 = Never married

# All single-renter HHs
tapply(hh_single$PWGTP, list(hh_single$MAR), sum)
prop.table(tapply(hh_single$PWGTP, list(hh_single$MAR), sum))
    # Widowed: 10.1%, Divorced: 20.3%, Separated: 7.0%, Never married: 59.9%

# Female single-renter HHs
tapply(hh_single_f$PWGTP, list(hh_single_f$MAR), sum)
prop.table(tapply(hh_single_f$PWGTP, list(hh_single_f$MAR), sum))
    # Widowed: 14.4%, Divorced: 22.5%, Separated: 6.6%, Never married: 54.4%

# Male single-renter HHs
tapply(hh_single_m$PWGTP, list(hh_single_m$MAR), sum)
prop.table(tapply(hh_single_m$PWGTP, list(hh_single_m$MAR), sum))
    # Widowed: 6.0%, Divorced: 18.1%, Separated: 7.4%, Never married: 65.2%

# Rent-burdened single-renter HHs
tapply(rent_bur_single$PWGTP, list(rent_bur_single$MAR), sum)
prop.table(tapply(rent_bur_single$PWGTP, list(rent_bur_single$MAR), sum))
    # Widowed: 12.8%, Divorced: 20.8%, Separated: 6.5%, Never married: 57.9%

# Rent-burdened single-renter female HHs
tapply(rent_bur_single_f$PWGTP, list(rent_bur_single_f$MAR), sum)
prop.table(tapply(rent_bur_single_f$PWGTP, list(rent_bur_single_f$MAR), sum))
    # Widowed: 18.1%, Divorced: 20.7%, Separated: 5.7%, Never married: 53.1%

# Rent-burdened single-renter male HHs
tapply(rent_bur_single_m$PWGTP, list(rent_bur_single_m$MAR), sum)
prop.table(tapply(rent_bur_single_m$PWGTP, list(rent_bur_single_m$MAR), sum))
    # Widowed: 7.7%, Divorced: 20.8%, Separated: 7.2%, Never married: 62.6%

# ---------------- Marital Status Single-Renter HHs by Age ---------------------
single_50to70 <- hh_single %>% filter(age_cat==4)
single_50to70_f <- hh_single_f %>% filter(age_cat==4)
single_50to70_m <- hh_single_m %>% filter(age_cat==4)
single_bur_50to70 <- rent_bur_single %>% filter(age_cat==4)
single_bur_50to70_f <- rent_bur_single_f %>% filter(age_cat==4)
single_bur_50to70_m <- rent_bur_single_m %>% filter(age_cat==4)

# All single-renter HHs ages 50-70
tapply(single_50to70$PWGTP, list(single_50to70$MAR), sum)
prop.table(tapply(single_50to70$PWGTP, list(single_50to70$MAR), sum))
    # Widowed: 12.0%, Divorced: 31.6%, Separated: 11.5%, Never married: 40.9%

# Female single-renter HHs ages 50-70
tapply(single_50to70_f$PWGTP, list(single_50to70_f$MAR), sum)
prop.table(tapply(single_50to70_f$PWGTP, list(single_50to70_f$MAR), sum))
    # Widowed: 16.1%, Divorced: 34.3%, Separated: 10.9%, Never married: 35.6%

# Male single-renter HHs ages 50-70
tapply(single_50to70_m$PWGTP, list(single_50to70_m$MAR), sum)
prop.table(tapply(single_50to70_m$PWGTP, list(single_50to70_m$MAR), sum))
    # Widowed: 7.7%, Divorced: 28.9%, Separated: 12.2%, Never married: 46.3%

# All rent-burdened single-renter HHs ages 50-70
tapply(single_bur_50to70$PWGTP, list(single_bur_50to70$MAR), sum)
prop.table(tapply(single_bur_50to70$PWGTP, list(single_bur_50to70$MAR), sum))
    # Widowed: 12.9%, Divorced: 32.3%, Separated: 10.1%, Never married: 42.5%

# Female rent_burdened single-renter HHs ages 50-70
tapply(single_bur_50to70_f$PWGTP, list(single_bur_50to70_f$MAR), sum)
prop.table(tapply(single_bur_50to70_f$PWGTP, list(single_bur_50to70_f$MAR), sum))
    # Widowed: 18.5%, Divorced: 34.7%, Separated: 8.9%, Never married: 34.0%

# Male rent_burdened single-renter HHs ages 50-70
tapply(single_bur_50to70_m$PWGTP, list(single_bur_50to70_m$MAR), sum)
prop.table(tapply(single_bur_50to70_m$PWGTP, list(single_bur_50to70_m$MAR), sum))
    # Widowed: 7.4%, Divorced: 30.0%, Separated: 11.3%, Never married: 50.7%

# ----------- Occupation of Female-Headed in Labor Force Family HHs ------------
female_lf <- rent_all %>% filter(FES==7) # Rental HH families lead by single female in LF
female_lf_bur <- rent_bur_all %>% filter(FES==7)

# Occupation code:   
  
  
  
#-------------------------------------------------------------------------------
# OCPIP and HINCP: Gross owner costs as % of HH income; HH income
#-------------------------------------------------------------------------------
# Histogram of owner costs as % of HH income
hh_roc %>%
ggplot(aes(x=OCPIP, weight = WGTP)) + 
  geom_histogram() + 
  stat_bin(binwidth=5, geom="text", aes(label=..count..), vjust=-1.0)  # Gross rent as % HH income

# Generate categories for home ownership cost burden
hh_roc$OCPIP_cat <- cut(hh_roc$OCPIP, breaks = c(0, 30, 50, 60, 80, 100, 10000000), labels = c(1,2,3,4,5,6), right = TRUE)
summary(hh_roc$OCPIP_cat)
tapply(hh_roc$WGTP, list(hh_roc$OCPIP_cat), sum)
prop.table(tapply(hh_roc$WGTP, list(hh_roc$OCPIP_cat), sum))
     # <30% income:     77.2%
     # 30-50% income:   13.0%
     # 50-60% income:   2.8%
     # 60-80% income:   2.2%
     # 80-100% income:  1.6%
     # >100% income:    3.4%

# Graph of OCPIP compared to HH income
hh_roc %>%
  ggplot() +
  geom_point(aes(x=HINCP, y=OCPIP, size=WGTP), shape=21) +
  xlim(0,400000)   # graph view restricted to HINCP from $0-$75,000
    # Note: compared to graph of GRPIP, this one is far more bottom left,
    # meaning far fewer owners are housing burdened

#-------------------------------------------------------------------------------
# Household income by AMI
#-------------------------------------------------------------------------------
# Compute Rochester median income
w_median_roc <- w.median(hh_roc$HINCP, hh_roc$WGTP) # command from cwhmisc package

# Compute Monroe County median income
w_median_mon <- w.median(hh_monroe$HINCP, hh_monroe$WGTP)
   # Calculate AMI for Rochester Metro Area 
   # Look at HUD definition of AMI based on family size

# Generate AMI based on Rochester median income
hh_roc$ami_roc = hh_roc$HINCP/w_median_roc

# Generate Rochester AMI categories
hh_roc$ami_roc_cat <- cut(hh_roc$ami_roc, breaks = c(0, 0.3, 0.5, 0.6, 0.8, 1.0, 1.2, 10000000), labels = c(1,2,3,4,5,6,7), right = TRUE)
summary(hh_roc$ami_roc_cat)
tapply(hh_roc$WGTP, list(hh_roc$ami_roc_cat), sum)
prop.table(tapply(hh_roc$WGTP, list(hh_roc$ami_roc_cat), sum))
     # <30% AMI:     12.9%
     # 30-50% AMI:   12.2%
     # 50-60% AMI:   4.9%
     # 60-80% AMI:   10.0%
     # 80-100% AMI:  9.0%
     # 100-120% AMI: 7.1%
     # >=120% AMI:   43.8%

# Generate AMI based on Monroe County median income
hh_roc$ami_mon = hh_roc$HINCP/w_median_mon

# Generate Monroe County AMI categories
hh_roc$ami_mon_cat <- cut(hh_roc$ami_mon, breaks = c(0, 0.3, 0.5, 0.6, 0.8, 1.0, 1.2, 10000000), labels = c(1,2,3,4,5,6,7), right = TRUE)
summary(hh_roc$ami_mon_cat)
tapply(hh_roc$WGTP, list(hh_roc$ami_mon_cat), sum)
prop.table(tapply(hh_roc$WGTP, list(hh_roc$ami_mon_cat), sum))
     # <30% AMI:     25.5%
     # 30-50% AMI:   16.6%
     # 50-60% AMI:   7.9%
     # 60-80% AMI:   11.5%
     # 80-100% AMI:  8.9%
     # 100-120% AMI: 6.5%
     # >=120% AMI:   23.1%

#-------------------------------------------------------------------------------
# Gross Rent
#-------------------------------------------------------------------------------
# GRNTP = monthly gross rent. Create AGRNTP = annual gross rent
hh_roc$AGRNTP = hh_roc$GRNTP*12
with(hh_roc, Hmisc::wtd.quantile(GRNTP, weights=WGTP))
with(hh_roc, Hmisc::wtd.quantile(AGRNTP, weights=WGTP))

# Calculate income for which the unit is affordable (<=30% HH income)
# Formula: percent AMI = (Gross rent / 0.3 / AMI) * 100

# Rochester AMI
hh_roc$aff_inc_roc = (hh_roc$AGRNTP / 0.3 / w_median_roc)*100
with(hh_roc, Hmisc::wtd.quantile(aff_inc_roc, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), weights=WGTP))
    # At the Rochester AMI, less than 10% of units are affordable for 30% AMI

# Monroe County AMI
hh_roc$aff_inc_mon = (hh_roc$AGRNTP / 0.3 / w_median_mon)*100
with(hh_roc, Hmisc::wtd.quantile(aff_inc_mon, probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1), weights=WGTP))
   # At Monroe County AMI, approx. 10% of units are affordable for 30% AMI

# Generate Monroe County AMI affordability categories for rental units
hh_roc$aff_inc_cat <- cut(hh_roc$aff_inc_mon, breaks = c(0, 30, 50, 60, 80, 100, 120, 10000000), labels = c(1,2,3,4,5,6,7), right = TRUE)
summary(hh_roc$aff_inc_cat)
tapply(hh_roc$WGTP, list(hh_roc$aff_inc_cat), sum)
prop.table(tapply(hh_roc$WGTP, list(hh_roc$aff_inc_cat), sum))
    # 10.1% of units exclusively fall within <30% AMI affordability
    # 24.2% of units exclusively fall within 30-50% AMI affordability
    # 19.1% of units exclusively fall within 50-60% AMI affordability
    # 29.9% of units exclusively fall within 60-80% AMI affordability
    # 9.9% of units exclusively fall within 80-100% AMI affordability
    # 3.6% of units exclusively fall within 100-120% AMI affordability
    # 3.1 % of units exclusively fall within >=120% AMI affordability

# Histogram of rental units at Monroe County AMI
hh_roc %>%
  ggplot(aes(x=aff_inc_mon, weight = WGTP)) + 
  geom_histogram() + 
  stat_bin(binwidth=10, geom="text", aes(label=..count..), vjust=-1.0)  # AMI needed for units


# Generate Rochester AMI affordability categories for rental units
hh_roc$aff_inc_cat_roc <- cut(hh_roc$aff_inc_roc, breaks = c(0, 30, 50, 60, 80, 100, 120, 10000000), labels = c(1,2,3,4,5,6,7), right = TRUE)
summary(hh_roc$aff_inc_cat_roc)
tapply(hh_roc$WGTP, list(hh_roc$aff_inc_cat_roc), sum)
prop.table(tapply(hh_roc$WGTP, list(hh_roc$aff_inc_cat_roc), sum))
     # 3.9% of units exclusively fall within <30% AMI affordability
     # 6.0% of units exclusively fall within 30-50% AMI affordability
     # 3.8% of units exclusively fall within 50-60% AMI affordability
     # 15.8% of units exclusively fall within 60-80% AMI affordability
     # 22.7% of units exclusively fall within 80-100% AMI affordability
     # 20.6% of units exclusively fall within 100-120% AMI affordability
     # 27.2% of units exclusively fall within >=120% AMI affordability
# The category percentages are different when I use Rochester AMI because absolute
# rent prices are included in the calculation, which aren't based on AMI
