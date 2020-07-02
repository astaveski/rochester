# ===============================================================================
# * FILE: 03_rochester.R
# * PURPOSE: Import and Analyze PUMS data from 2018
# * AUTHORS: Andrea Ringer
# * DATE CREATED: June 9, 2020
# * DATE LAST MODIFIED: July 2, 2020
# ===============================================================================
library(readr)
library(tidyverse)
library(survey)
library(srvyr)
library(Hmisc)
library(cwhmisc)
library(collapse)

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
all_roc$year=substr(as.character(p_roc$SERIALNO),1,4)
all_roc$id=substr(as.character(p_roc$SERIALNO),5,25)

hh_roc$year=substr(as.character(hh_roc$SERIALNO),1,4)
hh_roc$id=substr(as.character(hh_roc$SERIALNO),5,25)


#-------------------------------------------------------------------------------
# GRPIP and HINCP: Gross rent as a percentage of HH income; HH income
#-------------------------------------------------------------------------------
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

# Create adjusted household income variable (using HINCP, ADJINC)
hh_roc$ADJINC_1 <- hh_roc$ADJINC/1000000
hh_roc$HINCP_adj <- hh_roc$HINCP*hh_roc$ADJINC_1

# Compute Monroe County median income
hh_monroe$ADJINC_1 <- hh_monroe$ADJINC/1000000
hh_monroe$HINCP_adj <- hh_monroe$HINCP*hh_roc$ADJINC_1
w_median_mon <- w.median(hh_monroe$HINCP_adj, hh_monroe$WGTP)

# Generate Monroe County AMI categories
hh_roc$ami_mon = hh_roc$HINCP_adj/w_median_mon
hh_roc$ami_mon_cat <- cut(hh_roc$ami_mon, breaks = c(0, 0.3, 0.5, 0.8, 10000000), labels = c(1,2,3,4), right = TRUE)

tapply(hh_roc$WGTP, list(hh_roc$ami_mon_cat), sum)
prop.table(tapply(hh_roc$WGTP, list(hh_roc$ami_mon_cat), sum))


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


#===============================================================================
# Characteristics of those who are rent burdened - HH data
#===============================================================================

hh_rental <- hh_roc %>%
  filter(hh_roc$TEN==3)         # Rental HHs (excludes homeowner HHs)

rent_bur_no <- hh_roc %>%       # <30% income on rent
  filter(GRPIP_cat==1)

rent_bur <- hh_roc %>%
  filter(GRPIP_cat %in% 2:6)    # >=30% income on rent
summary(rent_bur$GRPIP_cat)

rent_bur_30to50 <- hh_roc %>%
  filter(GRPIP_cat==2)          # >=30% and <50% income on rent
summary(rent_bur_30to50$GRPIP_cat)

rent_bur_50to101 <- hh_roc %>%
  filter(GRPIP_cat %in% 3:6)    # >50% income on rent
summary(rent_bur_50to101$GRPIP_cat)

#-------------------------------------------------------------------------------
# FES: Family Type and Employment Status
#-------------------------------------------------------------------------------

# 1-4 Married Couple Family
#      1 = Husband and wife in LF; 2 = Husband in LF, wife not in LF
#      3 = Husband not in LF, wife in LF; 4 = Neither husband nor wife in LF
# 5-8 Other Family
#      5 = Male HHer, no wife present, LF; 6 = Male HHer, no wife present, not in LF
#      7 = Female HHer, no husband present, LF; 8 = Female HHer, no husband present, not in LF

# All rental HHs
tapply(hh_rental$WGTP, list(hh_rental$FES), sum)
prop.table(tapply(hh_rental$WGTP, list(hh_rental$FES), sum))

# Any rent burdened households (>=30% income)
tapply(rent_bur$WGTP, list(rent_bur$FES), sum)
prop.table(tapply(rent_bur$WGTP, list(rent_bur$FES), sum))

# Non rent burdened households (<30% income)
tapply(rent_bur_no$WGTP, list(rent_bur_no$FES), sum)
prop.table(tapply(rent_bur_no$WGTP, list(rent_bur_no$FES), sum))

# Rent burdened households (>=30% and <50% income)
tapply(rent_bur_30to50$WGTP, list(rent_bur_30to50$FES), sum)
prop.table(tapply(rent_bur_30to50$WGTP, list(rent_bur_30to50$FES), sum))

# Severely rent burdened households >=50%
tapply(rent_bur_50to101$WGTP, list(rent_bur_50to101$FES), sum)
prop.table(tapply(rent_bur_50to101$WGTP, list(rent_bur_50to101$FES), sum))


#--------------- STANDARD ERRORS - All Rental Family HHs -----------------------
rep.names <- paste0('WGTP', 1:80)

# All Rental Family HHs
all <- hh_rental %>% filter(FES %in% 1:8)
pt.est_all <- sum(all$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(all[[n]]))
se_all <- sqrt((4/80) * sum((rep.ests - pt.est_all)^2)) # SE=623.25

ci90 <- c(pt.est_all-(1.64*se_all), pt.est_all+(1.64*se_all)) # [21961, 24005]                    
ci95 <- c(pt.est_all-(1.96*se_all), pt.est_all+(1.96*se_all)) # [21761, 24205]                  

# Rental HHs: married couple, at least one HHer in LF
x <- hh_rental %>% filter(FES %in% 1:3)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=284.39

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [3523, 4637]              

# Proportion standard error: married couple, at least one HHer in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0114

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.155, 0.2]                 

# Rental HHs: single male headed in LF
x <- hh_rental %>% filter(FES==5)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=216.69

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [1179, 2029]              

# Proportion standard error: single male headed in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.00924

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.0517, 0.0879]                 

# Rental HHs: single male headed not in LF
x <- hh_rental %>% filter(FES==6)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=137.26

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [587, 1125]              

# Proportion standard error: single male headed not in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.00589

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.0257, 0.0488]                 

# Rental HHs: single female headed in LF
x <- hh_rental %>% filter(FES==7)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=422.68

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [8890, 10546]              

# Proportion standard error: single female headed in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0144

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.395, 0.451]                 

# Rental HHs: single female headed not in LF
x <- hh_rental %>% filter(FES==8)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=428.09

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [4882, 6560]              

# Proportion standard error: single female headed not in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0174

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.215, 0.283]                 

# Rental HHs: married couple family not in LF
x <- hh_rental %>% filter(FES==4)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=167.22

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [676, 1332]              

# Proportion standard error: married couple family not in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) #0.00718

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.0296, 0.0578]                 


#--------------- STANDARD ERRORS - Any Rent-Burdened Family HHs ----------------
rep.names <- paste0('WGTP', 1:80)

# Any Rent Burdened Family HHs (>=30% of income on rent)
all <- rent_bur %>% filter(FES %in% 1:8)
pt.est_all <- sum(all$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(all[[n]]))
se_all <- sqrt((4/80) * sum((rep.ests - pt.est_all)^2)) # SE=536.06

ci90 <- c(pt.est_all-(1.64*se_all), pt.est_all+(1.64*se_all)) # [13445, 15203]                    
ci95 <- c(pt.est_all-(1.96*se_all), pt.est_all+(1.96*se_all)) # [13273, 15375]                  

# Any Rent-burdened family HHs: married couple, at least one HHer in LF
x <- rent_bur %>% filter(FES %in% 1:3)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=138.06

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [856, 1398]              

# Prop SE: Any rent-burdened family, married couple, at least one HHer in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.00918

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.0607, 0.0967]                 

# Any rent-burdened family HHs: single male headed in LF
x <- rent_bur %>% filter(FES==5)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=126.58

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [362, 858]              

# Prop SE: Any rent-burdened family, single male headed in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.00869

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.0255, 0.0596]                 

# Any rent-burdened family HHs: single male headed not in LF
x <- rent_bur %>% filter(FES==6)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=119.48

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [326, 794]              

# Prop SE: Any rent-burdened family, single male headed not in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.00821

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.023, 0.0552]                 

# Any rent-burdened family HHs: single female headed in LF
x <- rent_bur %>% filter(FES==7)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=435.02

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [5941, 7647]              

# Prop SE: Any rent-burdened family single female headed in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0246

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.426, 0.523]                 

# Any rent-burdened family HHs: single female headed not in LF
x <- rent_bur %>% filter(FES==8)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=401.75

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [3809, 5383]              

# Prop SE: Any rent-burdened family, single female headed not in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0253

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.271, 0.371]                 

# Any rent-burdened family HHs: married couple family not in LF
x <- rent_bur %>% filter(FES==4)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=127.30

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [387, 887]              

# Prop SE: Any rent-burdened family, married couple family not in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) #0.00873

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.0274, 0.0616]                 


#--------------- STANDARD ERRORS - Non Rent Burdened Family HHs ----------------
rep.names <- paste0('WGTP', 1:80)

# Non-Rent Burdened Family HHs
all <- rent_bur_no %>% filter(FES %in% 1:8)
pt.est_all <- sum(all$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(all[[n]]))
se_all <- sqrt((4/80) * sum((rep.ests - pt.est_all)^2)) # SE=455.87

ci90 <- c(pt.est_all-(1.64*se_all), pt.est_all+(1.64*se_all)) # [7569, 9065]                    
ci95 <- c(pt.est_all-(1.96*se_all), pt.est_all+(1.96*se_all)) # [7423, 9211]                  

# Non Rent-burdened family HHs: married couple, at least one HHer in LF
x <- rent_bur_no %>% filter(FES %in% 1:3)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=273.28

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [2417, 3489]              

# Prop SE: Non Rent-burdened family, married couple, at least one HHer in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0265

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.303, 0.407]                 

# Non Rent-burdened family HHs: single male headed in LF
x <- rent_bur_no %>% filter(FES==5)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=175.46

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [650, 1338]              

# Prop SE: Non rent-burdened family, single male headed in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0201

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.0802, 0.1588]                 

# Non Rent-burdened family HHs: single male headed not in LF
x <- rent_bur_no %>% filter(FES==6)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=73.79

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [108, 398]              

# Prop SE: Non rent-burdened family, single male headed not in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.00871

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.0133, 0.0475]                 

# Non Rent-burdened family HHs: single female headed in LF
x <- rent_bur_no %>% filter(FES==7)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=296.99

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [2330, 3494]              

# Prop SE: Non rent-burdened family single female headed in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0301

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.291, 0.409]                 

# Non Rent-burdened family HHs: single female headed not in LF
x <- rent_bur_no %>% filter(FES==8)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=160.19

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [546, 1174]              

# Prop SE: Non rent-burdened family, single female headed not in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0184

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.0673, 0.1395]                 

# Non Rent-burdened family HHs: married couple family not in LF
x <- rent_bur_no %>% filter(FES==4)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=93.71

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [161, 529]              

# Prop SE: Non rent-burdened family, married couple family not in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) #0.0110

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.0199, 0.0631]                 


#--------------- STANDARD ERRORS - Rent Burdened Family HHs --------------------
rep.names <- paste0('WGTP', 1:80)

# Rent Burdened Family HHs (>=30% and <50% income on rent)
all <- rent_bur_30to50 %>% filter(FES %in% 1:8)
pt.est_all <- sum(all$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(all[[n]]))
se_all <- sqrt((4/80) * sum((rep.ests - pt.est_all)^2)) # SE=336.43

ci90 <- c(pt.est_all-(1.64*se_all), pt.est_all+(1.64*se_all)) # [5141, 6245]                    
ci95 <- c(pt.est_all-(1.96*se_all), pt.est_all+(1.96*se_all)) # [5034, 6352]                  

# Rent-burdened family HHs: married couple, at least one HHer in LF
x <- rent_bur_30to50 %>% filter(FES %in% 1:3)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=102.92

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [417, 821]              

# Prop SE: Rent-burdened family, married couple, at least one HHer in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0169

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.0756, 0.1419]                 

# Rent-burdened family HHs: single male headed in LF
x <- rent_bur_30to50 %>% filter(FES==5)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=104.47

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [221, 631]              

# Prop SE: rent-burdened family, single male headed in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0178

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.0399, 0.1097]                 

# Rent-burdened family HHs: single male headed not in LF
x <- rent_bur_30to50 %>% filter(FES==6)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=75.45

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [53.1, 348.9]              

# Prop SE: rent-burdened family, single male headed not in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0131

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.00965, 0.06096]                 

# Rent-burdened family HHs: single female headed in LF
x <- rent_bur_30to50 %>% filter(FES==7)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=300.64

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [2431, 3609]              

# Prop SE: rent-burdened family single female headed in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0425

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.447, 0.614]                 

# Rent-burdened family HHs: single female headed not in LF
x <- rent_bur_30to50 %>% filter(FES==8)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=141.70

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [785, 1341]              

# Prop SE: rent-burdened family, single female headed not in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0223

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.143, 0.23]                 

# Rent-burdened family HHs: married couple family not in LF
x <- rent_bur_30to50 %>% filter(FES==4)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=92.89

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [182, 546]              

# Prop SE: rent-burdened family, married couple family not in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) #0.0159

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.0328, 0.095]                 


#--------------- STANDARD ERRORS - Severely Rent-Burdened Family HHs -----------
rep.names <- paste0('WGTP', 1:80)

# Severely Rent Burdened Family HHs (>=30% of income on rent)
all <- rent_bur_50to101 %>% filter(FES %in% 1:8)
pt.est_all <- sum(all$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(all[[n]]))
se_all <- sqrt((4/80) * sum((rep.ests - pt.est_all)^2)) # SE=471.60

ci90 <- c(pt.est_all-(1.64*se_all), pt.est_all+(1.64*se_all)) # [7858, 9404]                    
ci95 <- c(pt.est_all-(1.96*se_all), pt.est_all+(1.96*se_all)) # [7707, 9555]                  

# Severely Rent-burdened family HHs: married couple, at least one HHer in LF
x <- rent_bur_50to101 %>% filter(FES %in% 1:3)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=109.73

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [293, 723]              

# Prop SE: Severely rent-burdened family, married couple, at least one HHer in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0123

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.0347, 0.083]                 

# Severely rent-burdened family HHs: single male headed in LF
x <- rent_bur_50to101 %>% filter(FES==5)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=67.65

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [51.4, 316.6]              

# Prop SE: Severely rent-burdened family, single male headed in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0078

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.0061, 0.0365]                 

# Severely rent-burdened family HHs: single male headed not in LF
x <- rent_bur_50to101 %>% filter(FES==6)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=101.40

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [160, 558]              

# Prop SE: Severely rent-burdened family, single male headed not in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0115

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.019, 0.0642]                 

# Severely rent-burdened family HHs: single female headed in LF
x <- rent_bur_50to101 %>% filter(FES==7)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=316.17

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [3154, 4394]              

# Prop SE: Severely rent-burdened family single female headed in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0278

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.383, 0.492]                 

# Severely rent-burdened family HHs: single female headed not in LF
x <- rent_bur_50to101 %>% filter(FES==8)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=370.62

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [2807, 4259]              

# Prop SE: Severely rent-burdened family, single female headed not in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0367

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.337, 0.481]                 

# Severely rent-burdened family HHs: married couple family not in LF
x <- rent_bur_50to101 %>% filter(FES==4)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=75.07

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [126, 420]              

# Prop SE: Severely rent-burdened family, married couple family not in LF / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) #0.00852

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.0149, 0.04830]                 


#-------------------------------------------------------------------------------
# HHT: Household/Family Type
#-------------------------------------------------------------------------------
# 1 = Married couple HH
# 2 = Other family HH: Male HHer, no spouse present
# 3 = Other family HH: Female HHer, no spouse present
# 4-7 Non family HH
#     4 = Male HHer, living alone; 5 = Male HHer, not living alone
#     6 = Female HHer, living alone; 7 = Female HHer, not living alone

# All rental HHs 
tapply(hh_rental$WGTP, list(hh_rental$HHT), sum)
prop.table(tapply(hh_rental$WGTP, list(hh_rental$HHT), sum))

# Any rent burdened households (>=30% income)
tapply(rent_bur$WGTP, list(rent_bur$HHT), sum)
prop.table(tapply(rent_bur$WGTP, list(rent_bur$HHT), sum))

# Non rent burdened households (>=30% and <50%)
tapply(rent_bur_no$WGTP, list(rent_bur_no$HHT), sum)
prop.table(tapply(rent_bur_no$WGTP, list(rent_bur_no$HHT), sum))

# Rent burdened households (>=30% and <50%)
tapply(rent_bur_30to50$WGTP, list(rent_bur_30to50$HHT), sum)
prop.table(tapply(rent_bur_30to50$WGTP, list(rent_bur_30to50$HHT), sum))

# Severely rent burdened households >=50%
tapply(rent_bur_50to101$WGTP, list(rent_bur_50to101$HHT), sum)
prop.table(tapply(rent_bur_50to101$WGTP, list(rent_bur_50to101$HHT), sum))

#------------------ STANDARD ERRORS - All Rental HHs ---------------------------
rep.names <- paste0('WGTP', 1:80)

# All Rental HHs
all <- hh_rental %>% filter(HHT %in% 1:7)
pt.est_all <- sum(all$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(all[[n]]))
se_all <- sqrt((4/80) * sum((rep.ests - pt.est_all)^2)) # SE=772.67

ci90 <- c(pt.est_all-(1.64*se_all), pt.est_all+(1.64*se_all)) # [52581, 55115]                    
ci95 <- c(pt.est_all-(1.96*se_all), pt.est_all+(1.96*se_all)) # [52334, 55362]                  

# Rental HHs: Married couple
x <- hh_rental %>% filter(HHT==1)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=317.50

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [4633, 5877]              

# Prop SE: married couple / all rental HHs
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.00572

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.0864, 0.1088]                 

# Rental HHs: single female headed
x <- hh_rental %>% filter(HHT==3)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=497.80

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [14463, 16415]              

# Prop SE: single female headed / all rental HHs
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.00828

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.27, 0.303]                 

# Rental HHs: male living alone
x <- hh_rental %>% filter(HHT==4)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=507.56

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [11324, 13314]              

# Prop SE: male living alone / all rental HHs
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.00884

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.211, 0.246]                 

# Rental HHs: female living alone
x <- hh_rental %>% filter(HHT==6)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=508.73

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [10750, 12744]              

# Prop SE: female living alone / all rental HHs
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0089

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.201, 0.236]                 

# Rental HHs: other HH type
x <- hh_rental %>% filter(HHT==2 | HHT==5 | HHT==7)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=407.67

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [8289, 9887]              

# Prop SE: other HH type / all rental HHs
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.00717

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.155, 0.1830]                 


#------------------ STANDARD ERRORS - Any rent-burdened HHs --------------------
rep.names <- paste0('WGTP', 1:80)

# Any rent-burdened HHs (>=30% of income on rent)
all <- rent_bur %>% filter(HHT %in% 1:7)
pt.est_all <- sum(all$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(all[[n]]))
se_all <- sqrt((4/80) * sum((rep.ests - pt.est_all)^2)) # SE=735.47

ci90 <- c(pt.est_all-(1.64*se_all), pt.est_all+(1.64*se_all)) # [28727, 31139]                    
ci95 <- c(pt.est_all-(1.96*se_all), pt.est_all+(1.96*se_all)) # [28491, 31375]                  

# Any rent-burdened HHs: Married couple
x <- rent_bur %>% filter(HHT==1)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=179.92

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [1441, 2147]              

# Prop SE: any rent-burdened married couple / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.00583

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.0485, 0.0714]                 

# Any rent-burdened HHs: single female headed
x <- rent_bur %>% filter(HHT==3)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=539.32

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [10333, 12447]              

# Prop SE: Any rent-burdened single female headed / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0154

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.35, 0.411]                 

# Any rent-burdened HHs: male living alone
x <- rent_bur %>% filter(HHT==4)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=432.26

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [5871, 7565]              

# Prop SE: Any rent-burdened male living alone / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0134

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.198, 0.251]                 

# Any rent-burdened HHs: female living alone
x <- rent_bur %>% filter(HHT==6)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=418.25

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [6136, 7776]              

# Prop SE: any rent-burdened female living alone / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0128

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.207, 0.257]                 

# Any rent-burdened HHs: other HH type
x <- rent_bur %>% filter(HHT==2 | HHT==5 | HHT==7)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=245.90

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [2593, 3557]              

# Prop SE: any rent-burdened other HH type / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0078

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.0874, 0.1181]                 


#------------------ STANDARD ERRORS - Non rent-burdened HHs --------------------
rep.names <- paste0('WGTP', 1:80)

# Non rent-burdened HHs (<30% of income on rent)
all <- rent_bur_no %>% filter(HHT %in% 1:7)
pt.est_all <- sum(all$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(all[[n]]))
se_all <- sqrt((4/80) * sum((rep.ests - pt.est_all)^2)) # SE=801.20

ci90 <- c(pt.est_all-(1.64*se_all), pt.est_all+(1.64*se_all)) # [21119, 23747]                    
ci95 <- c(pt.est_all-(1.96*se_all), pt.est_all+(1.96*se_all)) # [20863, 24003]                  

# Non rent-burdened HHs: Married couple
x <- rent_bur_no %>% filter(HHT==1)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=284.47

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [2881, 3997]              

# Prop SE: non rent-burdened married couple / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0114

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.131, 0.176]                 

# Non rent-burdened HHs: single female headed
x <- rent_bur_no %>% filter(HHT==3)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=327.07

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [3131, 4413]              

# Prop SE: Non rent-burdened single female headed / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0133

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.142, 0.194]                 

# Non rent-burdened HHs: male living alone
x <- rent_bur_no %>% filter(HHT==4)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=415.52

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [4206, 5834]              

# Prop SE: Non rent-burdened male living alone / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0167

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.191, 0.257]                 

# Non rent-burdened HHs: female living alone
x <- rent_bur_no %>% filter(HHT==6)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=375.42

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [3647, 5119]              

# Prop SE: Non rent-burdened female living alone / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0152

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.166, 0.225]                 

# Non rent-burdened HHs: other HH type
x <- rent_bur_no %>% filter(HHT==2 | HHT==5 | HHT==7)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=391.06

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [5053, 6585]              

# Prop SE: non rent-burdened other HH type / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0148

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.23, 0.288]                 


#------------------ STANDARD ERRORS - Rent-burdened HHs ------------------------
rep.names <- paste0('WGTP', 1:80)0

# Rent-burdened HHs (>=30% and <50% of income on rent)
all <- rent_bur_30to50 %>% filter(HHT %in% 1:7)
pt.est_all <- sum(all$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(all[[n]]))
se_all <- sqrt((4/80) * sum((rep.ests - pt.est_all)^2)) # SE=548.70

ci90 <- c(pt.est_all-(1.64*se_all), pt.est_all+(1.64*se_all)) # [12065, 13865]                    
ci95 <- c(pt.est_all-(1.96*se_all), pt.est_all+(1.96*se_all)) # [11890, 14040]                  

# Rent-burdened HHs: Married couple
x <- rent_bur_30to50 %>% filter(HHT==1)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=134.31

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [733, 1259]              

# Prop SE: rent-burdened married couple / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0098

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.0575, 0.0961]                 

# Rent-burdened HHs: single female headed
x <- rent_bur_30to50 %>% filter(HHT==3)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=303.21

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [3489, 4677]              

# Prop SE: Rent-burdened single female headed / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0192

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.277, 0.353]                 

# Rent-burdened HHs: male living alone
x <- rent_bur_30to50 %>% filter(HHT==4)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=314.68

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [2738, 3972]              

# Prop SE: Rent-burdened male living alone / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0217

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.216, 0.301]                 

# Rent-burdened HHs: female living alone
x <- rent_bur_30to50 %>% filter(HHT==6)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=269.37

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [2291, 3347]              

# Prop SE: Rent-burdened female living alone / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0186

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.181, 0.254]                 

# Rent-burdened HHs: other HH type
x <- rent_bur_30to50 %>% filter(HHT==2 | HHT==5 | HHT==7)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=216.78

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [1287, 2137]              

# Prop SE: rent-burdened other HH type / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0158

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.101, 0.163]                 


#------------------ STANDARD ERRORS - Severely Rent-burdened HHs ---------------
rep.names <- paste0('WGTP', 1:80)0

# Severely rent-burdened HHs (>=50% income on rent)
all <- rent_bur_50to101 %>% filter(HHT %in% 1:7)
pt.est_all <- sum(all$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(all[[n]]))
se_all <- sqrt((4/80) * sum((rep.ests - pt.est_all)^2)) # SE=579.98

ci90 <- c(pt.est_all-(1.64*se_all), pt.est_all+(1.64*se_all)) # [16017, 17919]                    
ci95 <- c(pt.est_all-(1.96*se_all), pt.est_all+(1.96*se_all)) # [15831, 18105]                  

# Severely rent-burdened HHs: Married couple
x <- rent_bur_50to101 %>% filter(HHT==1)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=130.55

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [542, 1054]              

# Prop SE: Severely rent-burdened married couple / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0075

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.0323, 0.0618]                 

# Severely rent-burdened HHs: single female headed
x <- rent_bur_50to101 %>% filter(HHT==3)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=472.02

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [6382, 8232]              

# Prop SE: Severely rent-burdened single female headed / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0236

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.384, 0.477]                 

# Severely rent-burdened HHs: male living alone
x <- rent_bur_50to101 %>% filter(HHT==4)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=340.45

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [2696, 4030]              

# Prop SE: Severely rent-burdened male living alone / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0189

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.161, 0.235]                 

# Severely rent-burdened HHs: female living alone
x <- rent_bur_50to101 %>% filter(HHT==6)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=289.44

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [3570, 4704]              

# Prop SE: Severely rent-burdened female living alone / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0149

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.215, 0.273]                 

# Severely rent-burdened HHs: other HH type
x <- rent_bur_50to101 %>% filter(HHT==2 | HHT==5 | HHT==7)
pt.est <- sum(x$WGTP)
rep.ests <- sapply(rep.names, function(n) sum(x[[n]]))
se <- sqrt((4/80) * sum((rep.ests - pt.est)^2)) # SE=175.04

ci90 <- c(pt.est-(1.64*se), pt.est+(1.64*se))                     
ci95 <- c(pt.est-(1.96*se), pt.est+(1.96*se)) # [1020, 1706]              

# Prop SE: Severely rent-burdened other HH type / all
se_prop <- (1/pt.est_all)*(sqrt(se^2-((pt.est^2/pt.est_all^2)*se_all^2))) # 0.0099

prop <- (pt.est/pt.est_all)
ci90_prop <- c(prop-(1.64*se_prop), prop+(1.64*se_prop))                     
ci95_prop <- c(prop-(1.96*se_prop), prop+(1.96*se_prop)) # [0.061, 0.010]                 


#------------------------------------------------------------------------------
# HHL: Household Language
#------------------------------------------------------------------------------
# (1) English only, (2) Spanish, (3) Other Indo-European languages
# (4) Asian and Pacific Island languages, (5) Other languages

# All Rental HHs
tapply(hh_rental$WGTP, list(hh_rental$HHL), sum)
prop.table(tapply(hh_rental$WGTP, list(hh_rental$HHL), sum))
      # Largest categories: 1 (79%), 2 (15%), (3%)

# Any rent burdened households (>=30% income)
tapply(rent_bur$WGTP, list(rent_bur$HHL), sum)
prop.table(tapply(rent_bur$WGTP, list(rent_bur$HHL), sum))
      # Largest categories: 1 (79%), 2 (17%), 3 (2%)

# Non rent burdened households (>=30% income)
tapply(rent_bur_no$WGTP, list(rent_bur_no$HHL), sum)
prop.table(tapply(rent_bur_no$WGTP, list(rent_bur_no$HHL), sum))
      # Largest categories: 1 (81%), 2 (11%), 3 (4%)

# Rent burdened households (>=30% and <50%)
tapply(rent_bur_30to50$WGTP, list(rent_bur_30to50$HHL), sum)
prop.table(tapply(rent_bur_30to50$WGTP, list(rent_bur_30to50$HHL), sum))
      # Largest categories: 1 (81%), 2 (15%), 3 (2%)

# Severely rent burdened households >=50%
tapply(rent_bur_50to101$WGTP, list(rent_bur_50to101$HHL), sum)
prop.table(tapply(rent_bur_50to101$WGTP, list(rent_bur_50to101$HHL), sum))
      # Largest categories: 1 (76%), 2 (19%), 3 (3%)


# I find it interesting that:
          # Moving from rent burdened to severely rent burdened, proportion of households
          # that are English speaking decreases, and proportion that are Spanish and speaking increases


#------------------------------------------------------------------------------
# MV: Rent Burden by Length of Time in Unit
#------------------------------------------------------------------------------
# 1 = 12 mos or less; 2 = 13 to 23 mos; 3 = 2-4 years; 4 = 5-9 years
# 5 = 10-19 years; 6 = 20-29 years; 7 = 30+ years

# All Rental HHs 
tapply(hh_rental$WGTP, list(hh_rental$MV), sum)
prop.table(tapply(hh_rental$WGTP, list(hh_rental$MV), sum))

# Any rent burdened households (>=30% income)
tapply(rent_bur$WGTP, list(rent_bur$MV), sum)
prop.table(tapply(rent_bur$WGTP, list(rent_bur$MV), sum))

# Non rent burdened households (>=30% income)
tapply(rent_bur_no$WGTP, list(rent_bur_no$MV), sum)
prop.table(tapply(rent_bur_no$WGTP, list(rent_bur_no$MV), sum))

# Rent burdened households (>=30% and <50%)
tapply(rent_bur_30to50$WGTP, list(rent_bur_30to50$MV), sum)
prop.table(tapply(rent_bur_30to50$WGTP, list(rent_bur_30to50$MV), sum))

# Severely rent burdened households >50%
tapply(rent_bur_50to101$WGTP, list(rent_bur_50to101$MV), sum)
prop.table(tapply(rent_bur_50to101$WGTP, list(rent_bur_50to101$MV), sum))

# Calculate average and standard error for length of time in rental unit
time_in_unit <- select(hh_roc, MV, WGTP) %>% tally(wt=WGTP)
weighted.mean(all_roc$MV, all_roc$WGTP_HH, na.rm==TRUE) # 3.56 (mean between 2-4 years)
w.median(all_roc$MV, all_roc$WGTP_HH) # 3 (median between 2-4 years)


#------------------------------------------------------------------------------
# MV: Length of Time in Unit by Income
#------------------------------------------------------------------------------

# Use AMI categories created previously (ami_mon_cat)
# 1=0-30%, 2=30-50%, 3=50-60%, 4=60-80%, 5=80-100%, 6=100-120%, 7=120%+

d <- hh_rental %>% filter(ami_mon_cat %in% 1:4)

#-------------------------- All Rental HHs
z <- d
w <- z$WGTP
v <- z$ami_mon_cat
tapply(w, list(v), sum)
prop.table(tapply(w, list(v), sum))
weighted.mean(z$HINCP_adj, w) # $36,361.46
var <- wtd.var(z$HINCP_adj, w) # from HMISC package
sd <- sqrt(var) # $41,857.30

# use "survey" package to set survey design and specify replicate weights
pumsd_hh <- z %>%
  as_survey_rep(weights = WGTP, repweights = starts_with("WGTP"), combined_weights = TRUE)

# calculate mean and std. error of HINCP_adj
pumsd_hh %>% filter(!is.na(HINCP_adj)) %>%
  summarise(survey_mean(HINCP_adj, na.rm = TRUE)) # mean=36361, se=454

# ------------------------- HHs in unit 12 mos or less
z <- d %>% filter(MV==1)
w <- z$WGTP
v <- z$ami_mon_cat
tapply(w, list(v), sum)
prop.table(tapply(w, list(v), sum))
weighted.mean(z$HINCP_adj, w) # $36,626.06
var <- wtd.var(z$HINCP_adj, w)
sd <- sqrt(var) # $33,459.45

# use "survey" package to set survey design and specify replicate weights
pumsd_hh <- z %>%
  as_survey_rep(weights = WGTP, repweights = starts_with("WGTP"), combined_weights = TRUE)

# calculate mean and std. error of HINCP_adj
pumsd_hh %>% filter(!is.na(HINCP_adj)) %>%
  summarise(survey_mean(HINCP_adj, na.rm = TRUE)) # mean=36626, se=601

# -------------------------- HHs in unit 13-23 mos
z <- d %>% filter(MV==2)
w <- z$WGTP
v <- z$ami_mon_cat
tapply(w, list(v), sum)
prop.table(tapply(w, list(v), sum))
weighted.mean(z$HINCP_adj, w) # $43,721.19
var <- wtd.var(z$HINCP_adj, w)
sd <- sqrt(var) # $49,303.48

# use "survey" package to set survey design and specify replicate weights
pumsd_hh <- z %>%
  as_survey_rep(weights = WGTP, repweights = starts_with("WGTP"), combined_weights = TRUE)

# calculate mean and std. error of HINCP_adj
pumsd_hh %>% filter(!is.na(HINCP_adj)) %>%
  summarise(survey_mean(HINCP_adj, na.rm = TRUE)) # mean=43721, se=1594

# -------------------------- HHs in unit 2-4 years
z <- d %>% filter(MV==3)
w <- z$WGTP
v <- z$ami_mon_cat
tapply(w, list(v), sum)
prop.table(tapply(w, list(v), sum))
weighted.mean(z$HINCP_adj, w) # $35,487.76
var <- wtd.var(z$HINCP_adj, w)
sd <- sqrt(var) # $35,914.70

# use "survey" package to set survey design and specify replicate weights
pumsd_hh <- z %>%
  as_survey_rep(weights = WGTP, repweights = starts_with("WGTP"), combined_weights = TRUE)

# calculate mean and std. error of HINCP_adj
pumsd_hh %>% filter(!is.na(HINCP_adj)) %>%
  summarise(survey_mean(HINCP_adj, na.rm = TRUE)) # mean=35488, se=817

# --------------------------- HHs in unit 5-9 years
z <- d %>% filter(MV==4)
w <- z$WGTP
v <- z$ami_mon_cat
tapply(w, list(v), sum)
prop.table(tapply(w, list(v), sum))
weighted.mean(z$HINCP_adj, w) # $35,991.89
var <- wtd.var(z$HINCP_adj, w)
sd <- sqrt(var) # $60,767.78

# use "survey" package to set survey design and specify replicate weights
pumsd_hh <- z %>%
  as_survey_rep(weights = WGTP, repweights = starts_with("WGTP"), combined_weights = TRUE)

# calculate mean and std. error of HINCP_adj
pumsd_hh %>% filter(!is.na(HINCP_adj)) %>%
  summarise(survey_mean(HINCP_adj, na.rm = TRUE)) # mean=35992, se=1389

# ---------------------------- HHs in unit 10+ years
z <- d %>% filter(MV %in% 5:7)
w <- z$WGTP
v <- z$ami_mon_cat
tapply(w, list(v), sum)
prop.table(tapply(w, list(v), sum))
weighted.mean(z$HINCP_adj, w) # $32,305.18
var <- wtd.var(z$HINCP_adj, w)
sd <- sqrt(var) # $32,915.49
stderror <- sd/sqrt(sum(w)) # 378

# use "survey" package to set survey design and specify replicate weights
pumsd_hh <- z %>%
  as_survey_rep(weights = WGTP, repweights = starts_with("WGTP"), combined_weights = TRUE)

# calculate mean and std. error of HINCP_adj
pumsd_hh %>% filter(!is.na(HINCP_adj)) %>%
  summarise(survey_mean(HINCP_adj, na.rm = TRUE)) # mean=32035, se=901


#------------------------------------------------------------------------------
# MV: Length of Time in Rental Unit by Presence of Children (HUPAC)
#------------------------------------------------------------------------------
# HUPAC
# 1=Children under 6 only; 2=Children 6-17 only; 3=Children <6 & 6-17; 4=No children

d <- hh_rental %>% filter(HUPAC %in% 1:4)

#-------------------------- All Rental HHs
z <- d
w <- z$WGTP
v <- z$HUPAC
tapply(w, list(v), sum)
prop.table(tapply(w, list(v), sum))

# ------------------------- HHs in unit 12 mos or less
z <- d %>% filter(MV==1)
w <- z$WGTP
v <- z$HUPAC
tapply(w, list(v), sum)
prop.table(tapply(w, list(v), sum))

# ------------------------- HHs in unit 13-23 mos
z <- d %>% filter(MV==2)
w <- z$WGTP
v <- z$HUPAC
tapply(w, list(v), sum)
prop.table(tapply(w, list(v), sum))

# ------------------------- HHs in unit 2-4 years
z <- d %>% filter(MV==3)
w <- z$WGTP
v <- z$HUPAC
tapply(w, list(v), sum)
prop.table(tapply(w, list(v), sum))

# ------------------------- HHs in unit 5-9 years
z <- d %>% filter(MV==4)
w <- z$WGTP
v <- z$HUPAC
tapply(w, list(v), sum)
prop.table(tapply(w, list(v), sum))

# ------------------------- HHs in unit 10+ years
z <- d %>% filter(MV %in% 5:7)
w <- z$WGTP
v <- z$HUPAC
tapply(w, list(v), sum)
prop.table(tapply(w, list(v), sum))


#===============================================================================
# Characteristics of those who are rent burdened - merged data
#===============================================================================

# Generate categories for rent burden in merged data
all_roc$GRPIP_cat <- cut(all_roc$GRPIP, breaks = c(0, 30, 50, 60, 80, 100, 10000000), labels = c(1,2,3,4,5,6), right = TRUE)
summary(all_roc$GRPIP_cat)
prop.table(tapply(all_roc$WGTP_HH, list(all_roc$GRPIP_cat), sum))
# Sanity check: the proportions are the same as when I used just the HH data,
# which means the HH weights I created in the merged dataset are correct
# (I created the correct weight variable in Stata. The variable is "WGTP_HH")

# Generate categories for age in merged data
all_roc$age_cat <- cut(all_roc$AGEP, breaks = c(0, 20, 30, 50, 70, 10000000), labels = c(1,2,3,4,5), right = TRUE)

# Generate categories for race in merged data
all_roc$RACE = ifelse(all_roc$HISP == 01, all_roc$RAC1P, 10)

# Generate categories for employment status in merged data
# ESR (employment status recode)
# 1=civilian employed, at work; 2=civilian employed, with a job but not at work
# 3=unemployed; 4=Armed forces, at work; 5=Armed forces, with a job but not at work
# 6=Not in labor force
all_roc$EMP = ifelse(all_roc$ESR %in% 1:2 | all_roc$ESR %in% 4:5, 1, all_roc$ESR) #EMP=1,3,6
all_roc$EMP = ifelse(all_roc$EMP == 6, 4, all_roc$EMP) #EMP: 1=employed, 3=unemployed, 4=out of LF

# Generate part-time and full-time for EMP
all_roc$EMP = ifelse(all_roc$EMP==1 & all_roc$WKHP<40, 2, all_roc$EMP)
summary(all_roc$EMP)
#EMP: 1=full-time employed, 2=part-time employed, 3=unemployed, 4=out of labor force

# Generate rent burdened category variables
rent_all <- all_roc %>% filter(TEN==3)  # All rental HHs
rent_bur_all <- all_roc %>% filter(GRPIP_cat %in% 2:6)  # >=30% income on rent
rent_bur_non <- all_roc %>% filter(GRPIP_cat==1)
rent_bur_slight <- all_roc %>% filter(GRPIP_cat==2)     # >=30% and <50% income on rent
rent_bur_severe <- all_roc %>% filter(GRPIP_cat %in% 3:6)     # >=50% income on rent

#-------------------------------------------------------------------------------
# Length of time in rental unit by age (MV, age_cat)
#-------------------------------------------------------------------------------
# 1 = 12 mos or less; 2 = 13 to 23 mos; 3 = 2-4 years; 4 = 5-9 years
# 5 = 10-19 years; 6 = 20-29 years; 7 = 30+ years

# Restrict dataset to head of household (HOH)
# Use SPORDER to collapse data and create one obs. per HH of head of household
rent_hoh <- rent_all %>%
  arrange(SERIALNO, SPORDER) %>% 
  filter(SPORDER==1)

# Generate age categories for <30, 30-50, 50-70, 70+
rent_hoh$age_cat_2 <- cut(rent_hoh$AGEP, breaks = c(0, 30, 50, 70, 10000000), labels = c(1,2,3,4), right = TRUE)

# All Rental HOHs
z <- rent_hoh
tapply(z$PWGTP, list(z$age_cat_2), sum)
prop.table(tapply(z$PWGTP, list(z$age_cat_2), sum))

# HOHs in unit 12 mos or less
z <- rent_hoh %>% filter(MV==1)
tapply(z$PWGTP, list(z$age_cat_2), sum)
prop.table(tapply(z$PWGTP, list(z$age_cat_2), sum))

# HOHs in unit 13-23 mos
z <- rent_hoh %>% filter(MV==2)
tapply(z$PWGTP, list(z$age_cat_2), sum)
prop.table(tapply(z$PWGTP, list(z$age_cat_2), sum))

# HOHs in unit 2-4 years
z <- rent_hoh %>% filter(MV==3)
tapply(z$PWGTP, list(z$age_cat_2), sum)
prop.table(tapply(z$PWGTP, list(z$age_cat_2), sum))

# HOHs in unit 5-9 years
z <- rent_hoh %>% filter(MV==4)
tapply(z$PWGTP, list(z$age_cat_2), sum)
prop.table(tapply(z$PWGTP, list(z$age_cat_2), sum))

# HOHs in unit 10+ years
z <- rent_hoh %>% filter(MV %in% 5:7)
tapply(z$PWGTP, list(z$age_cat_2), sum)
prop.table(tapply(z$PWGTP, list(z$age_cat_2), sum))


#-------------------------------------------------------------------------------
# Length of time in rental unit by race (MV, RACE)
#-------------------------------------------------------------------------------
# RACE: 1=White, 2=Black, 10=Hispanic

# All Rental HOHs
z <- rent_hoh
tapply(z$PWGTP, list(z$RACE), sum)
prop.table(tapply(z$PWGTP, list(z$RACE), sum))

# HOHs in unit 12 mos or less
z <- rent_hoh %>% filter(MV==1)
tapply(z$PWGTP, list(z$RACE), sum)
prop.table(tapply(z$PWGTP, list(z$RACE), sum))

# HOHs in unit 13-23 mos
z <- rent_hoh %>% filter(MV==2)
tapply(z$PWGTP, list(z$RACE), sum)
prop.table(tapply(z$PWGTP, list(z$RACE), sum))

# HOHs in unit 2-4 years
z <- rent_hoh %>% filter(MV==3)
tapply(z$PWGTP, list(z$RACE), sum)
prop.table(tapply(z$PWGTP, list(z$RACE), sum))

# HOHs in unit 5-9 years
z <- rent_hoh %>% filter(MV==4)
tapply(z$PWGTP, list(z$RACE), sum)
prop.table(tapply(z$PWGTP, list(z$RACE), sum))

# HOHs in unit 10+ years
z <- rent_hoh %>% filter(MV %in% 5:7)
tapply(z$PWGTP, list(z$RACE), sum)
prop.table(tapply(z$PWGTP, list(z$RACE), sum))


#-------------------------------------------------------------------------------
# Length of time in rental unit by employment status (MV; EMP)
#-------------------------------------------------------------------------------
# EMP: 1=full-time employed, 2=part-time employed, 3=unemployed, 4=out of labor force

# Restrict to head of households only (HOH)
d <- rent_hoh %>% filter(EMP %in% 1:4)

#-------------------------- All Rental HHs
z <- d
w <- z$PWGTP
v <- z$EMP
tapply(w, list(v), sum)
prop.table(tapply(w, list(v), sum))

# ------------------------- HHs in unit 12 mos or less
z <- d %>% filter(MV==1)
w <- z$PWGTP
v <- z$EMP
tapply(w, list(v), sum)
prop.table(tapply(w, list(v), sum))

# ------------------------- HHs in unit 13-23 mos
z <- d %>% filter(MV==2)
w <- z$PWGTP
v <- z$EMP
tapply(w, list(v), sum)
prop.table(tapply(w, list(v), sum))

# ------------------------- HHs in unit 2-4 years
z <- d %>% filter(MV==3)
w <- z$PWGTP
v <- z$EMP
tapply(w, list(v), sum)
prop.table(tapply(w, list(v), sum))

# ------------------------- HHs in unit 5-9 years
z <- d %>% filter(MV==4)
w <- z$PWGTP
v <- z$EMP
tapply(w, list(v), sum)
prop.table(tapply(w, list(v), sum))

# ------------------------- HHs in unit 10+ years
z <- d %>% filter(MV %in% 5:7)
w <- z$PWGTP
v <- z$EMP
tapply(w, list(v), sum)
prop.table(tapply(w, list(v), sum))


#-------------------------------------------------------------------------------
# AGEP: Age of Single-Renter HHs
#-------------------------------------------------------------------------------
# What is the average age of male and female single rental HHs? (see HHT section above)
hh_single <- all_roc %>% filter((HHT==4 | HHT==6) & TEN==3)
hh_single_f <- all_roc %>% filter(HHT==6 & TEN==3)
hh_single_m <- all_roc %>%  filter(HHT==4 & TEN ==3)

# Single HHs rent-burden categories
rent_bur_all_s <- rent_bur_all %>% filter(HHT==4 | HHT==6)
rent_bur_non_s <- rent_bur_non %>% filter(HHT==4 | HHT==6)
rent_bur_slight_s <- rent_bur_slight %>% filter(HHT==4 | HHT==6)
rent_bur_severe_s <- rent_bur_severe %>% filter(HHT==4 | HHT==6)

# Female single HHs rent-burden categories
rent_bur_all_sf <- rent_bur_all %>% filter(HHT==6)
rent_bur_non_sf <- rent_bur_non %>% filter(HHT==6)
rent_bur_slight_sf <- rent_bur_slight %>% filter(HHT==6)
rent_bur_severe_sf <- rent_bur_severe %>% filter(HHT==6)

# Male single HHs rent-burden categories
rent_bur_all_sm <- rent_bur_all %>% filter(HHT==4)
rent_bur_non_sm <- rent_bur_non %>% filter(HHT==4)
rent_bur_slight_sm <- rent_bur_slight %>% filter(HHT==4)
rent_bur_severe_sm <- rent_bur_severe %>% filter(HHT==4)

# Average age of single HHs, by gender
weighted.mean(hh_single$AGEP, hh_single$WGTP_HH)       # all: 48.8 years old
weighted.mean(hh_single$AGEP, hh_single$PWGTP)       # all: 48.8 years old
weighted.mean(hh_single_f$AGEP, hh_single_f$PWGTP)   # female: 50.1 years old
weighted.mean(hh_single_m$AGEP, hh_single_m$PWGTP)   # male: 47.6 years old

# Average age of rent-burdened single HHs 
weighted.mean(rent_bur_single$AGEP, rent_bur_single$PWGTP)   # all: 51.1 years old
weighted.mean(rent_bur_single_f$AGEP, rent_bur_single_f$PWGTP)   # female: 51.9 years old
weighted.mean(rent_bur_single_m$AGEP, rent_bur_single_m$PWGTP)   # male: 50.3 years old

#----------------------------- Single renter HHs
tapply(hh_single$PWGTP, list(hh_single$age_cat), sum)
prop.table(tapply(hh_single$PWGTP, list(hh_single$age_cat), sum))

# Any rent-burdened single renter HHs
tapply(rent_bur_all_s$PWGTP, list(rent_bur_all_s$age_cat), sum)
prop.table(tapply(rent_bur_all_s$PWGTP, list(rent_bur_all_s$age_cat), sum))

# Non rent-burdened single renter HHs
tapply(rent_bur_non_s$PWGTP, list(rent_bur_non_s$age_cat), sum)
prop.table(tapply(rent_bur_non_s$PWGTP, list(rent_bur_non_s$age_cat), sum))

# Rent-burdened single renter HHs
tapply(rent_bur_slight_s$PWGTP, list(rent_bur_slight_s$age_cat), sum)
prop.table(tapply(rent_bur_slight_s$PWGTP, list(rent_bur_slight_s$age_cat), sum))

# Severely rent-burdened single renter HHs
tapply(rent_bur_severe_s$PWGTP, list(rent_bur_severe_s$age_cat), sum)
prop.table(tapply(rent_bur_severe_s$PWGTP, list(rent_bur_severe_s$age_cat), sum))


#-------------------------------- Female single renter HHs
tapply(hh_single_f$PWGTP, list(hh_single_f$age_cat), sum)
prop.table(tapply(hh_single_f$PWGTP, list(hh_single_f$age_cat), sum))

# Any rent-burdened female single renter HHs
tapply(rent_bur_all_sf$PWGTP, list(rent_bur_all_sf$age_cat), sum)
prop.table(tapply(rent_bur_all_sf$PWGTP, list(rent_bur_all_sf$age_cat), sum))

# Non rent-burdened female single renter HHs
tapply(rent_bur_non_sf$PWGTP, list(rent_bur_non_sf$age_cat), sum)
prop.table(tapply(rent_bur_non_sf$PWGTP, list(rent_bur_non_sf$age_cat), sum))

# Rent-burdened female single renter HHs
tapply(rent_bur_slight_sf$PWGTP, list(rent_bur_slight_sf$age_cat), sum)
prop.table(tapply(rent_bur_slight_sf$PWGTP, list(rent_bur_slight_sf$age_cat), sum))

# Severely rent-burdened female single renter HHs
tapply(rent_bur_severe_sf$PWGTP, list(rent_bur_severe_sf$age_cat), sum)
prop.table(tapply(rent_bur_severe_sf$PWGTP, list(rent_bur_severe_sf$age_cat), sum))


#-------------------------------- Male single renter HHs
tapply(hh_single_m$PWGTP, list(hh_single_m$age_cat), sum)
prop.table(tapply(hh_single_m$PWGTP, list(hh_single_m$age_cat), sum))

# Any rent-burdened male single renter HHs
tapply(rent_bur_all_sm$PWGTP, list(rent_bur_all_sm$age_cat), sum)
prop.table(tapply(rent_bur_all_sm$PWGTP, list(rent_bur_all_sm$age_cat), sum))

# Non rent-burdened male single renter HHs
tapply(rent_bur_non_sm$PWGTP, list(rent_bur_non_sm$age_cat), sum)
prop.table(tapply(rent_bur_non_sm$PWGTP, list(rent_bur_non_sm$age_cat), sum))

# Rent-burdened male single renter HHs
tapply(rent_bur_slight_sm$PWGTP, list(rent_bur_slight_sm$age_cat), sum)
prop.table(tapply(rent_bur_slight_sm$PWGTP, list(rent_bur_slight_sm$age_cat), sum))

# Severely rent-burdened male single renter HHs
tapply(rent_bur_severe_sm$PWGTP, list(rent_bur_severe_sm$age_cat), sum)
prop.table(tapply(rent_bur_severe_sm$PWGTP, list(rent_bur_severe_sm$age_cat), sum))



#-------------------------------------------------------------------------------
# RACE: Race of All Renter HHs
#-------------------------------------------------------------------------------
# RACE variable
# 1 = White alone; 2 = Black alone; 3 = American Indian alone; 4 = Alaska Native alone
# 5 = American Indian & Alaskan Native; 6 = Asian alone; 7 = Native Hawaiian / Pacific Islander alone
# 8 = Some other race alone; 9 = Two or more races; 10 = Hispanic
# (Categories for race generated in previous section in merged data)

# Race proportions in Rochester population
prop.table(tapply(all_roc$PWGTP, list(all_roc$RACE), sum))
       # 36.6% White, 38.3% Black, 18.4% Hispanic

# For now I'll look at the population. I need to figure out how to collapse at the
# HH level after creating the RACE variable, to do the HH analysis (will be more accurate)

# Race of renter household population
tapply(rent_all$PWGTP, list(rent_all$RACE), sum)
prop.table(tapply(rent_all$PWGTP, list(rent_all$RACE), sum))

# Race of all rent burdened population (>=30% income)
tapply(rent_bur_all$PWGTP, list(rent_bur_all$RACE), sum)
prop.table(tapply(rent_bur_all$PWGTP, list(rent_bur_all$RACE), sum))

# Race of non rent-burdened population (<30% income)
tapply(rent_bur_non$PWGTP, list(rent_bur_non$RACE), sum)
prop.table(tapply(rent_bur_non$PWGTP, list(rent_bur_non$RACE), sum))

# Race of slightly rent burdened population (>=30% and <50% income)
tapply(rent_bur_slight$PWGTP, list(rent_bur_slight$RACE), sum)
prop.table(tapply(rent_bur_slight$PWGTP, list(rent_bur_slight$RACE), sum))

# Race of severely rent burdened population (>=50% income)
tapply(rent_bur_severe$PWGTP, list(rent_bur_severe$RACE), sum)
prop.table(tapply(rent_bur_severe$PWGTP, list(rent_bur_severe$RACE), sum))


# Race of single renter HHs (see section HHT above)
prop.table(tapply(hh_single$PWGTP, list(hh_single$RACE), sum))

# Race of rent-burdened single renter HHs
prop.table(tapply(rent_bur_single$PWGTP, list(rent_bur_single$RACE), sum))


#-------------------------------------------------------------------------------
# MAR: Marital Status of Single-Renter HHs
#-------------------------------------------------------------------------------
# Marital status of single-renter HHs
# 1 = Married, 2 = Widowed, 3 = Divorced, 4 = Separated, 5 = Never married


#----------------------------- Single renter HHs
tapply(hh_single$PWGTP, list(hh_single$MAR), sum)
prop.table(tapply(hh_single$PWGTP, list(hh_single$MAR), sum))

# Any rent-burdened single renter HHs
tapply(rent_bur_all_s$PWGTP, list(rent_bur_all_s$MAR), sum)
prop.table(tapply(rent_bur_all_s$PWGTP, list(rent_bur_all_s$MAR), sum))

# Non rent-burdened single renter HHs
tapply(rent_bur_non_s$PWGTP, list(rent_bur_non_s$MAR), sum)
prop.table(tapply(rent_bur_non_s$PWGTP, list(rent_bur_non_s$MAR), sum))

# Rent-burdened single renter HHs
tapply(rent_bur_slight_s$PWGTP, list(rent_bur_slight_s$MAR), sum)
prop.table(tapply(rent_bur_slight_s$PWGTP, list(rent_bur_slight_s$MAR), sum))

# Severely rent-burdened single renter HHs
tapply(rent_bur_severe_s$PWGTP, list(rent_bur_severe_s$MAR), sum)
prop.table(tapply(rent_bur_severe_s$PWGTP, list(rent_bur_severe_s$MAR), sum))


#-------------------------------- Female single renter HHs
tapply(hh_single_f$PWGTP, list(hh_single_f$MAR), sum)
prop.table(tapply(hh_single_f$PWGTP, list(hh_single_f$MAR), sum))

# Any rent-burdened female single renter HHs
tapply(rent_bur_all_sf$PWGTP, list(rent_bur_all_sf$MAR), sum)
prop.table(tapply(rent_bur_all_sf$PWGTP, list(rent_bur_all_sf$MAR), sum))

# Non rent-burdened female single renter HHs
tapply(rent_bur_non_sf$PWGTP, list(rent_bur_non_sf$MAR), sum)
prop.table(tapply(rent_bur_non_sf$PWGTP, list(rent_bur_non_sf$MAR), sum))

# Rent-burdened female single renter HHs
tapply(rent_bur_slight_sf$PWGTP, list(rent_bur_slight_sf$MAR), sum)
prop.table(tapply(rent_bur_slight_sf$PWGTP, list(rent_bur_slight_sf$MAR), sum))

# Severely rent-burdened female single renter HHs
tapply(rent_bur_severe_sf$PWGTP, list(rent_bur_severe_sf$MAR), sum)
prop.table(tapply(rent_bur_severe_sf$PWGTP, list(rent_bur_severe_sf$MAR), sum))


#-------------------------------- Male single renter HHs
tapply(hh_single_m$PWGTP, list(hh_single_m$MAR), sum)
prop.table(tapply(hh_single_m$PWGTP, list(hh_single_m$MAR), sum))

# Any rent-burdened male single renter HHs
tapply(rent_bur_all_sm$PWGTP, list(rent_bur_all_sm$MAR), sum)
prop.table(tapply(rent_bur_all_sm$PWGTP, list(rent_bur_all_sm$MAR), sum))

# Non rent-burdened male single renter HHs
tapply(rent_bur_non_sm$PWGTP, list(rent_bur_non_sm$MAR), sum)
prop.table(tapply(rent_bur_non_sm$PWGTP, list(rent_bur_non_sm$MAR), sum))

# Rent-burdened male single renter HHs
tapply(rent_bur_slight_sm$PWGTP, list(rent_bur_slight_sm$MAR), sum)
prop.table(tapply(rent_bur_slight_sm$PWGTP, list(rent_bur_slight_sm$MAR), sum))

# Severely rent-burdened male single renter HHs
tapply(rent_bur_severe_sm$PWGTP, list(rent_bur_severe_sm$MAR), sum)
prop.table(tapply(rent_bur_severe_sm$PWGTP, list(rent_bur_severe_sm$MAR), sum))



#-------------------------------------------------------------------------------
# Marital Status of Single-Renter HHs Ages 50-70
#-------------------------------------------------------------------------------

#----------------------------- 50-70 Single renter HHs
z <- hh_single %>% filter(age_cat==4)
tapply(z$PWGTP, list(z$MAR), sum)
prop.table(tapply(z$PWGTP, list(z$MAR), sum))

# Any rent-burdened 50-70 single renter HHs
z <- rent_bur_all_s %>% filter(age_cat==4)
tapply(z$PWGTP, list(z$MAR), sum)
prop.table(tapply(z$PWGTP, list(z$MAR), sum))

# Non rent-burdened 50-70 single renter HHs
z <- rent_bur_non_s %>% filter(age_cat==4)
tapply(z$PWGTP, list(z$MAR), sum)
prop.table(tapply(z$PWGTP, list(z$MAR), sum))

# Rent-burdened 50-70 single renter HHs
z <- rent_bur_slight_s %>% filter(age_cat==4)
tapply(z$PWGTP, list(z$MAR), sum)
prop.table(tapply(z$PWGTP, list(z$MAR), sum))

# Severely rent-burdened 50-70single renter HHs
z <- rent_bur_severe_s %>% filter(age_cat==4)
tapply(z$PWGTP, list(z$MAR), sum)
prop.table(tapply(z$PWGTP, list(z$MAR), sum))


#-------------------------------- Female single renter HHs
z <- single_50to70_f
tapply(z$PWGTP, list(z$MAR), sum)
prop.table(tapply(z$PWGTP, list(z$MAR), sum))

# Any rent-burdened female 50-70 single renter HHs
z <- rent_bur_all_sffifty
tapply(z$PWGTP, list(z$MAR), sum)
prop.table(tapply(z$PWGTP, list(z$MAR), sum))

# Non rent-burdened female 50-70 single renter HHs
z <- rent_bur_non_sffifty
tapply(z$PWGTP, list(z$MAR), sum)
prop.table(tapply(z$PWGTP, list(z$MAR), sum))

# Rent-burdened female 50-70 single renter HHs
z <- rent_bur_slight_sffifty
tapply(z$PWGTP, list(z$MAR), sum)
prop.table(tapply(z$PWGTP, list(z$MAR), sum))

# Severely rent-burdened female 50-70 single renter HHs
z <- rent_bur_severe_sffifty
tapply(z$PWGTP, list(z$MAR), sum)
prop.table(tapply(z$PWGTP, list(z$MAR), sum))


#-------------------------------- Male single renter HHs
z <- single_50to70_m
tapply(z$PWGTP, list(z$MAR), sum)
prop.table(tapply(z$PWGTP, list(z$MAR), sum))

# Any rent-burdened male 50-70 single renter HHs
z <- rent_bur_all_smfifty
tapply(z$PWGTP, list(z$MAR), sum)
prop.table(tapply(z$PWGTP, list(z$MAR), sum))

# Non rent-burdened male 50-70 single renter HHs
z <- rent_bur_non_smfifty
tapply(z$PWGTP, list(z$MAR), sum)
prop.table(tapply(z$PWGTP, list(z$MAR), sum))

# Rent-burdened male 50-70 single renter HHs
z <- rent_bur_slight_smfifty
tapply(z$PWGTP, list(z$MAR), sum)
prop.table(tapply(z$PWGTP, list(z$MAR), sum))

# Severely rent-burdened male 50-70 single renter HHs
z <- rent_bur_severe_smfifty
tapply(z$PWGTP, list(z$MAR), sum)
prop.table(tapply(z$PWGTP, list(z$MAR), sum))



#----------------- Occupation of Female-Headed in Labor Force Family HHs ------------
female_lf <- rent_all %>% filter(FES==7) # Rental HH families lead by single female in LF

# Restrict data to HH head only
# Try function from "collapse" package
hh_female_lf_2 <- collap(female_lf, ~ SERIALNO, ffirst) # keep only first obs of SERIALNO
hh_test <- collap(all_roc, ~ SERIALNO, ffirst) # Test collapse with "all_roc" to see if I get # of obs in "hh_roc" data
    # Not sure that this works correctly

# Use SPORDER to collapse data and create one obs. per HH
female_lf <- arrange(female_lf, SERIALNO, SPORDER)
hh_female_lf <- female_lf %>% filter(SPORDER==1)

# Occupation code: INDP, NAICSP
tapply(hh_female_lf$PWGTP, list(hh_female_lf$INDP), sum)
prop.table(tapply(hh_female_lf$PWGTP, list(hh_female_lf$INDP), sum))

# Create occupation categories (based on INDP codes, see data dictionary)
hh_female_lf$ind_cat <- cut(hh_female_lf$INDP, breaks = c(0, 300, 500, 700, 1000, 4000, 4600, 6000, 6400, 6800, 7200, 7800, 7900, 8300, 8500, 8700, 9300, 9600, 9900, 10000000), 
                            labels = c("AGR", "EXT", "UTL", "CON", "MFG", "WHL", "RET", "TRN", "INF", "FIN", "PRF", "EDU", "MED", "SCA", "ENT", "SRV", "ADM", "MIL", "UEM"), right = TRUE)

# Create subsets of female single-headed HHs based on rent burden
hh_female_lf_bur <- hh_female_lf %>% filter(GRPIP_cat %in% 2:5)
hh_female_lf_bur_slight <- hh_female_lf %>% filter(GRPIP_cat==2)
hh_female_lf_bur_high <- hh_female_lf %>% filter(GRPIP_cat %in% 3:5)
hh_female_lf_bur_severe <- hh_female_lf %>% filter(GRPIP_cat==6)

# All single-female-headed-HHs in LF occupation categories
tapply(hh_female_lf$PWGTP, list(hh_female_lf$ind_cat), sum)

# Rent-burdened single-female-headed-HHs in LF occupation categories
tapply(hh_female_lf_bur$PWGTP, list(hh_female_lf_bur$ind_cat), sum)

# Slightly rent-burdened single-female-headed-HHs in LF occupation categories
tapply(hh_female_lf_bur_slight$PWGTP, list(hh_female_lf_bur_slight$ind_cat), sum)

# Highly rent-burdened single-female-headed-HHs in LF occupation categories
tapply(hh_female_lf_bur_high$PWGTP, list(hh_female_lf_bur_high$ind_cat), sum)

# Severely rent-burdened single-female-headed-HHs in LF occupation categories
tapply(hh_female_lf_bur_severe$PWGTP, list(hh_female_lf_bur_severe$ind_cat), sum)


#----------------- Rent Burden of Different Occupations -------------------------
# Create occupation categories (based on INDP codes, see data dictionary)
all_roc$ind_cat <- cut(all_roc$INDP, breaks = c(0, 300, 500, 700, 1000, 4000, 4600, 6000, 6400, 6800, 7200, 7800, 7900, 8300, 8500, 8700, 9300, 9600, 9900, 10000000), 
labels = c("AGR", "EXT", "UTL", "CON", "MFG", "WHL", "RET", "TRN", "INF", "FIN", "PRF", "EDU", "MED", "SCA", "ENT", "SRV", "ADM", "MIL", "UEM"), right = TRUE)

# Manufacturing
mfg <- all_roc %>% filter(ind_cat=="MFG")
tapply(mfg$PWGTP, list(mfg$GRPIP_cat), sum)
prop.table(tapply(mfg$PWGTP, list(mfg$GRPIP_cat), sum)) # 36.8% are rent burdened

# Retail
ret <- all_roc %>% filter(ind_cat=="RET")
tapply(ret$PWGTP, list(ret$GRPIP_cat), sum)
prop.table(tapply(ret$PWGTP, list(ret$GRPIP_cat), sum)) # 51.7% are rent burdened

# Professional
prf <- all_roc %>% filter(ind_cat=="PRF")
tapply(prf$PWGTP, list(prf$GRPIP_cat), sum)
prop.table(tapply(prf$PWGTP, list(prf$GRPIP_cat), sum)) # 47.2% are rent burdened

# Medical
med <- all_roc %>% filter(ind_cat=="MED")
tapply(med$PWGTP, list(med$GRPIP_cat), sum)
prop.table(tapply(med$PWGTP, list(med$GRPIP_cat), sum)) # 40.5% are rent burdened

# Social services and care
sca <- all_roc %>% filter(ind_cat=="SCA")
tapply(sca$PWGTP, list(sca$GRPIP_cat), sum)
prop.table(tapply(sca$PWGTP, list(sca$GRPIP_cat), sum)) # 47.1% are rent burdened

# Standard Error Example
own <- filter(hh_roc, VACS==1)
pt.est <- sum(own$WGTP)
rep.names <- paste0('WGTP', 1:80)
rep.ests <- sapply(rep.names, function(n) sum(own[[n]]))
sqrt((4/80) * sum((rep.ests - pt.est)^2))


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

#-------------------------------------------------------------------------------
# Miscellaneous
#-------------------------------------------------------------------------------

#**************************************
# Standard errors attempt - create proportions dataset
all <- hh_roc %>% 
  select(FES,
         starts_with("WGTP"),
         starts_with("GRPIP")
  )

sub <- hh_rental %>% 
  select(FES,
         starts_with("WGTP"),
         starts_with("GRPIP")
  )

prop <- sub/all
#**************************************

#**************************************
# Standard errors attempt - calculation of MV variable
# use "survey" package to set survey design and specify replicate weights
pumsd_hh <- hh_roc %>%
  as_survey_rep(weights = WGTP, repweights = starts_with("WGTP"), combined_weights = TRUE)

# calculate mean and std. error of "length of time" variable
pumsd_hh %>% filter(!is.na(MV)) %>%
  summarise(survey_mean(MV, na.rm = TRUE))

# mean=3.56, se=0.0131
# mean same as with previous method, so this is correct

x <- sqrt(wtd.var(hh_roc$MV, hh_roc$WGTP))
y <- x/sqrt(sum(hh_roc$WGTP))
#*****************************************

#*****************************************
# Standard Errors Attempt - survey package to calculate SE of person-level data, age

# Calculate mean and std. error of age overall and by category
# use "survey" package to set survey design and specify replicate weights
pumsd_all <- all_roc %>%
  as_survey_rep(
    weights = PWGTP, 
    repweights = starts_with("PWGTP"),
    combined_weights = TRUE
  )

# calculate mean and std. error of age
pumsd_all %>%
  filter(!is.na(AGEP)) %>%
  summarise(
    survey_mean(AGEP, na.rm = TRUE)
  )
# mean=, se=
# error message: Error in qr.default(weights(design, "analysis"), tol = 1e-05) : 
# NA/NaN/Inf in foreign function call (arg 1)

# Another try
pumsd_all <-
  svrepdesign(
    weight = ~PWGTP ,
    repweights = 'PWGTP[0-9]+' ,
    scale = 4 / 80 ,
    rscales = rep( 1 , 80 ) ,
    mse = TRUE ,
    type = 'JK1' ,
    data = all_roc
  )

#*************************************

