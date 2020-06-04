# Rochester Data Analysis

install.packages("tidyverse")
library(tidyverse)

# Section 1: we grab the set of puma codes we want. ROC == the two PUMAs for Rochester. 
   # Monroe== the six PUMAs for Monroe county
   # 9county == the nine counties that create a greater Rochester area.
   # Note, some PUMAS contain more than one county, e.g. Livingston + Wyoming pooled together. 
   # Thus there is no set of PUMAs that give you only the six counties traditionally considered the 'Rochester MSA'

rm(list = ls())
pcode9county = c(800, 901, 902, 903, 904, 905, 906, 1000, 1300, 1400)
pcodeMonroe = c(901, 902, 903, 904, 905, 906)
pcodeROC = c(902, 903)
pcode = pcodeMonroe

#The census state code for New York 
state.code = 36

#Section 2: We tell the script where we unzipped the household and person files for NYS 2018 PUMS
#Takes the NYS PUMS data and splits out Monroe county, saves the result. This takes a while...

setwd("/Users/andrearinger/Google Drive/HKS/Bloomberg Internship/MLD412 Rochester NY/06 Summer Project/Data")
files = c(
  "csv_hny/psam_h36.csv",
  "csv_pny/psam_p36.csv")

# The files above were downloaded from:
# https://www2.census.gov/programs-surveys/acs/data/pums/


# Section 3: We crop the data to just the PUMAs we're interested in.
# This section should work for either state-level data download or whole US files.
g = list()
if (1) {
  # files = files[1]
  for (i in files) {
    j = sub("\\.csv","_monroe.csv",i)
    cat(i,"\n",j,"\n\n", sep="")
    a = read.csv(i)
    print(dim(a))
    l = (a$ST==state.code)&(a$PUMA %in% pcode)
    print(sum(l))
    b = a[l,]
    print(dim(b))
    g[[i]] = b
    write.csv(b, file=j, row.names = FALSE, na ='', quote = FALSE)
  }
}

hny = g[[1]]
pny = g[[2]]
save(file = "pums18.RData", hny, pny)


# Section 4: Load back the cropped data,
rm(list = ls())
load("pums18.RData")

# The SERIALNO contains both the year and the id of a household sampled
pny$year = substr(as.character(pny$SERIALNO),1,4)
pny$id = substr(as.character(pny$SERIALNO),5,25)

hny$year = substr(as.character(hny$SERIALNO),1,4)
hny$id = substr(as.character(hny$SERIALNO),5,25)

# Two big data structures created here: one has the person and household attributes 
# merged into one big table. The other is the above table split into chunks,
# one for each household.

d = merge(hny,pny,by = "SERIALNO")
#dl = split(d,d$SERIALNO) #This can eat your memory, be careful
save(d, 
     #dl, 
     file = "dataset.RData")





