#test post processing functions

rm(list=ls())

source('src/LognodeSupply.R')

#test the config file with absolute paths
a <- sapply(readLawstConfig('exampleGames/K/_kdemo.lconfig')$files, file.exists )
a #should be all true

#test a config file with paths relative to LAWST executable.
setwd('exampleGames/K/exe')
rel <- readLawstConfig("../_kdemo.lconfig")
b <- sapply(rel$files, file.exists )
b
mean(b)

rel.u <- readUnit(rel)
summary(rel.u)
head(rel.u)


str(readUnitScript(rel))
