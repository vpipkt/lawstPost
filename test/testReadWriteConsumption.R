# test consumption class reading and writing

source('src/readWriteConsumption.R')

#working from relative directory of the example game exe
setwd('exampleGames/k/exe')
kconfig <- readLawstConfig('../_kdemo.lconfig')
kcc <- readConsumptionClass(kconfig)

summary(kcc)

head(kcc, 10)

kcc$Description[2] <- "Stryker bde class jtb 20150505"
kcc$RequirementQty[4] <- 44444

