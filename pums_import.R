# ===============================================================================
# * FILE: pums_import.R
# * PURPOSE: Import PUMS data from 2018
# * AUTHORS: Adam Staveski
# * DATE: June 7, 2020
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
# Clean Person and Household Data
#-------------------------------------------------------------------------------
# Separate year and ID number from SERIALNO
p_roc$year=substr(as.character(p_roc$SERIALNO),1,4)
p_roc$id=substr(as.character(p_roc$SERIALNO),5,25)

hh_roc$year=substr(as.character(hh_roc$SERIALNO),1,4)
hh_roc$id=substr(as.character(hh_roc$SERIALNO),5,25)

# Sort Household Variables
hh_roc_vars <- subset(hh_roc, select = c(SERIALNO,PUMA,ADJHSG,ADJINC,WGTP,NP,TYPE,ACR,BDSP,BLD,CONP,ELEFP,ELEP,FS,INSP,MRGI,MRGP,MRGT,MRGX,
                                         RMSP,RNTM,RNTP,SMP,TEN,VACS,VALP,WATFP,WATP,YBL,FES,FINCP,FPARC,GRNTP,GRPIP,HHT, HINCP,HUPAC,KIT,LNGI,
                                         MV,NOC,NPF,OCPIP,PARTNER,PLM,R18,R65,SMOCP,SMX,TAXAMT,WIF,WORKSTAT))

#hh_roc_flag <- subset(hh_roc, select = c(FACRP,FBDSP,FBLDP,FCONP,FELEP,FFINCP,FFSP,FFULP,FGASP,FGRNTP,FHFLP,FHINCP,FINSP,FKITP,FMRGIP,FMRGP,
#                                        FMRGTP,FMRGXP,FMVP,FPLMP,FRMSP,FRNTMP,FRNTP,FSMOCP,FSMP,FSMXHP,FSMXSP,FTAXP,FTENP,FVACSP,FVALP,FWATP,
#                                        FYBLP))

hh_roc_wght <- subset(select(hh_roc, contains("SERIALNO"), starts_with("WGTP")))
hh_roc_clean <- merge(hh_roc_vars, hh_roc_wght)
rm(hh_roc_vars, hh_roc_wght)

# Sort Person Variables
p_roc_vars <- subset(p_roc, select = c(SERIALNO,SPORDER,PUMA,ADJINC,PWGTP,AGEP,CIT,CITWP,ENG,INTP,MAR,NWLA,NWLK,OIP,PAP,RELP,RETP,SCH,SCHG,
                                       SCHL,SEMP,SEX,SSIP,SSP,WAGP,WKHP,WKL,WKW,DECADE,DIS,ESR,HICOV,HISP,NATIVITY,NOP,PERNP,PINCP,RAC1P))

#p_roc_flag <- subset(p_roc, select = c(FAGEP,FCITP,FCITWP,FDISP,FENGP,FINTP,FMARP,FOIP,FPAP,FPERNP,FPINCP,FRACP,FRETP,FSCHGP,FSCHLP,FSCHP,
#                                       FSEMP,FSEXP,FSSIP,FSSP,FWAGP,FWKHP,FWKLP,FWKWP))

p_roc_wght <- subset(select(p_roc, contains("SERIALNO"), contains("SPORDER"), starts_with("PWGTP")))
p_roc_clean <- merge(p_roc_vars, p_roc_wght)
rm(p_roc_vars, p_roc_wght)

#-------------------------------------------------------------------------------
# Merge Datasets
#-------------------------------------------------------------------------------
# Merge household and person datasets
roc=merge(hh_roc_clean,p_roc_clean, by="SERIALNO", suffixes = c(".hh", ".p"))

# Split datasets into individual households
#hh=split(roc,roc$SERIALNO)

#-------------------------------------------------------------------------------
# Data Analysis
#-------------------------------------------------------------------------------
## Counts ##
summarise(hh_roc, total_units=sum(WGTP))

hh_roc %>%
  filter(VACS==1) %>%
  summarise(for_rent=sum(WGTP))

## Weighted Means and Quantiles ##

# Housing Characteristics
with(hh_roc, Hmisc::wtd.mean(VALP, weights=WGTP))
with(hh_roc, Hmisc::wtd.quantile(VALP, weights=WGTP))   # Property value

with(hh_roc, Hmisc::wtd.quantile(RNTP, weights=WGTP))   # Rent paid (listed rent ONLY)
with(hh_roc, Hmisc::wtd.quantile(GRNTP, weights=WGTP))  # Gross rent paid (listed rent + utilities)
with(hh_roc, Hmisc::wtd.quantile(SMOCP, weights=WGTP))  # Selected monthly owner cost (mortgage, taxes, utilities)

# Demographic Characteristics
with(hh_roc, Hmisc::wtd.quantile(NP, weights=WGTP))     # Number of People

## Proportions ##
num_rentals <- filter(hh_roc, (TEN==3) | (VACS==1)) %>% tally(wt=WGTP)
num_vacs <- filter(hh_roc, VACS==1) %>% tally(wt=WGTP)
100*(num_vacs/num_rentals)                              # Vacancy Rate

## Standard Errors ##
own <- filter(hh_roc, VACS==1)
pt.est <- sum(own$WGTP)
rep.names <- paste0('WGTP', 1:80)
rep.ests <- sapply(rep.names, function(n) sum(own[[n]]))
sqrt((4/80) * sum((rep.ests - pt.est)^2))

#-------------------------------------------------------------------------------
# Plots
#-------------------------------------------------------------------------------
# Histogram
ggplot(hh_roc, aes(x=NP, weight = WGTP)) + geom_histogram() + 
  stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust=-1.0)  # Number of Persons in Household
ggplot(hh_roc, aes(x=ACR, weight = WGTP)) + geom_histogram() + 
  stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust=-1.0)  # Lot size (single-family homes)

hh_roc %>% group_by(ACR) %>% filter(ACR > 0) %>% summarise(counted = n())