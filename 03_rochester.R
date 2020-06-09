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
hh_roc %>%
ggplot(aes(x=OCPIP, weight = WGTP)) + 
  geom_histogram() + 
  stat_bin(binwidth=5, geom="text", aes(label=..count..), vjust=-1.0)  # Gross rent as % HH income

hh_roc %>%
  ggplot() +
  geom_point(aes(x=HINCP, y=OCPIP, size=WGTP), shape=21) +
  xlim(0,400000)   # graph view restricted to HINCP from $0-$75-,000
    # Note: compared to graph of GRPIP, this one is far more bottom left,
    # meaning far fewer owners are housing burdened


# To investigate:
# income distribution of people who rent v. people who buy
# distribution of rent costs v. distribution of income on same x axis
