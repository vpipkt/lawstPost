#Test consumption class reading

#set working dir to 
source('src/readWriteConsumption.R')

# Read then write the CC with no changes

setwd('exampleGames/K/exe')
k <- readLawstConfig("../_kdemo.lconfig")

cc1 <- readConsumptionClass(k)
View(cc1)

writeConsumptionClass(cc1)

# Externally diff the two files