# ===============================================================================
# * FILE: 03_rochester.R
# * PURPOSE: Import and Analyze PUMS data from 2018
# * AUTHORS: Adam Staveski, Andrea Ringer
# * DATE CREATED: June 9, 2020
# * DATE LAST MODIFIED: June 9, 2020
# ===============================================================================
library(readr)
library(tidyverse)
library(survey)
library(Hmisc)
library(cwhmisc)

setwd("/Users/andrearinger/Google Drive/HKS/Bloomberg Internship/MLD412 Rochester NY/06 Summer Project/Data")

#-------------------------------------------------------------------------------
# Load PUMS Household Data and Select Rochester PUMAs
#-------------------------------------------------------------------------------
pums_hh <- read_csv("csv_hny/psam_h36.csv", guess_max = 12000)
pums_p <- read_csv("csv_pny/psam_p36.csv", guess_max = 12000)

hh_roc <- pums_hh %>%
  filter(PUMA == "00902" | PUMA == "00903")
p_roc <- pums_p %>%
  filter(PUMA == "00902" | PUMA == "00903")
hh_monroe <- pums_hh %>%
  filter(PUMA=="00901" | PUMA=="00902" | PUMA=="00903" | PUMA=="00904" | PUMA=="00905" | PUMA=="00906")

rm("pums_hh", "pums_p")

#-------------------------------------------------------------------------------
# Clean Person and Household Data
#-------------------------------------------------------------------------------
# Separate year and ID number from SERIALNO
p_roc$year=substr(as.character(p_roc$SERIALNO),1,4)
p_roc$id=substr(as.character(p_roc$SERIALNO),5,25)

hh_roc$year=substr(as.character(hh_roc$SERIALNO),1,4)
hh_roc$id=substr(as.character(hh_roc$SERIALNO),5,25)

# Sort Household Variables
hh_roc_vars <- subset(hh_roc, select = c(SERIALNO,PUMA,ADJHSG,ADJINC,WGTP,NP,TYPE,ACR,BDSP,BLD,CONP,ELEFP,ELEP,FS,FULFP,FULP,GASFP,GASP,HFL,INSP,
                                         MRGI,MRGP,MRGT,MRGX,RMSP,RNTM,RNTP,SMP,TEN,VACS,VALP,WATFP,WATP,YBL,FES,FINCP,FPARC,GRNTP,GRPIP,HHT,
                                         HINCP,HUPAC,KIT,LNGI,MV,NOC,NPF,OCPIP,PARTNER,PLM,R18,R65,SMOCP,SMX,TAXAMT,WIF,WORKSTAT))

hh_roc_wght <- subset(select(hh_roc, contains("SERIALNO"), starts_with("WGTP")))
hh_roc_clean <- merge(hh_roc_vars, hh_roc_wght)
rm(hh_roc_vars, hh_roc_wght)

# Sort Person Variables
p_roc_vars <- subset(p_roc, select = c(SERIALNO,SPORDER,PUMA,ADJINC,PWGTP,AGEP,CIT,CITWP,ENG,INTP,MAR,NWLA,NWLK,OIP,PAP,RELP,RETP,SCH,SCHG,
                                       SCHL,SEMP,SEX,SSIP,SSP,WAGP,WKHP,WKL,WKW,DECADE,DIS,ESR,HICOV,HISP,NATIVITY,NOP,PERNP,PINCP,RAC1P))

p_roc_wght <- subset(select(p_roc, contains("SERIALNO"), contains("SPORDER"), starts_with("PWGTP")))
p_roc_clean <- merge(p_roc_vars, p_roc_wght)
rm(p_roc_vars, p_roc_wght)

#-------------------------------------------------------------------------------
# Data Analysis
#-------------------------------------------------------------------------------
summarise(hh_roc, total_units=sum(WGTP))

## Proportions ##
num_rentals <- filter(hh_roc, (TEN==3) | (VACS==1)) %>% tally(wt=WGTP)
num_vacs <- filter(hh_roc, VACS==1) %>% tally(wt=WGTP)
vacancy_rate <- 100*(num_vacs/num_rentals)              # Vacancy rate

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
# OCPIP and HINCP: Gross owner costs as % of HH income; HH income
#-------------------------------------------------------------------------------
# Histogram of owner costs as % of HH income
hh_roc %>%
ggplot(aes(x=OCPIP, weight = WGTP)) + 
  geom_histogram() + 
  stat_bin(binwidth=5, geom="text", aes(label=..count..), vjust=-1.0)  # Gross rent as % HH income

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

# Generate AMI based on Rochester median income
hh_roc$ami_roc = hh_roc$HINCP/w_median_roc

# Create Rochester AMI categories
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

# Create Monroe County AMI categories
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





