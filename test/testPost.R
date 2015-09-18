#Test and demonstrate example uses of post processing functions

# Todo: 
#   * Test/demonstrate absolute path example

#Remove all objects!!
rm(list=ls())

# Working directory should be lawstPost repo
getwd()

# Read all source files into the environment.
source('src/readLawstGame.R')
source('src/writeLawstGame.R')
source('src/readWriteConsumption.R')
source('src/combineDeliveries.R')
source('src/logNodeSupply.R')
source('src/viz/LogNodeStatusHistory.R')
source('src/viz/LognodeSupply.R')
source('src/viz/productFlow.R')

# To use any of these functions, you must read in a game configuration file using `readLawstConfig`. If the game is saved using relative paths, set the working directory to the expected LAWST executable directory.

# Read unclass Korea demo
setwd('exampleGames')
kdemo <- readLawstConfig("k/_kdemo.lconfig")
# Note the functions below also expect the working directory to be the same unless the game is saved with absolute paths.

# Check the file paths are all good
mean(sapply(kdemo$files, file.exists)) # should be 1.00

# There are many `read[object]()` functions. They usually return a data frame
readSupplyTypes(kdemo)
# Some return a list
summary(readUnitScript(kdemo))
summary(readUnitScript(kdemo)$Script)

## Go through the visualizations, some are supported by data extraction functions which will have further utility.

#A basic history of log node's inventory, product flow and transport use
plotLogNodeHistory(kdemo, 'ANDONG SUPPLY DEPOT')

#What is a log node's supported units on hand inventory?
plotClientOnHand(kdemo, 'ANDONG SUPPLY DEPOT')
# Can look at many levels below; use levels = 0 for all
plotClientOnHand(kdemo, 'ANDONG SUPPLY DEPOT', levels = 0)

# Source data for plotClientOnHand; note it contains demand and consumed amount.
View(getLognodeClientSupply(kdemo, 'OBJ BLUE SUPPLY POINT'))
# How does it crawl the log tree?
View(findDependents(readUnitScript(kdemo)$SupplyScript, 'OBJ BLUE SUPPLY POINT'))

# A more abstract view of downstream 'risk' with all units overplotted
plotClientRisk(kdemo, 'ANDONG SUPPLY DEPOT')

# A viz of cargo flows by weight
flowMap(kdemo)
# Options to limit to a selection of days
flowMap(kdemo, 8:12)
# Can limit to an area of interest, 
flowMap(kdemo, day = 15, xLim = c(128.7, 129.6))
flowMap(kdemo, 15, xLim = c(128.5, 129.6), yLim = c(34.9, 36.1) )
# Can also tune the contrast; 0 is transparent and 1 is opaque
flowMap(kdemo, 15, alphaRange = c(0.01, 0.1))
flowMap(kdemo, 15, alphaRange = c(0.1, 0.5))
# Object returned is a ggplot, so we can add to it.
flowMap(kdemo,19) + geom_hline(yintercept = 36, colour = 'red') #simple annotation
flowMap(kdemo,14) + 
    geom_point(data = subset(readUnitScript(kdemo)$Script, Day == 14), 
               aes(x = Longitude, y = Latitude), colour = 'blue')

#An alternate visualization which sets transparency by log(Volume)
flowMapLog(kdemo, 14)

# Underlying data of the flow map
View(flowData(kdemo))
# Also useful is a stack of all deliveries 
summary(readCombinedDeliveries(kdemo)) 
library(plyr)
library(reshape2)
dcast(melt(readCombinedDeliveries(kdemo)), 
      variable ~ Type + SupplyType, sum, 
      subset = .(variable %in% c('Volume', 'FuelAmountExpended')))

# Plot of the deliveries
qplot(x = log(Volume), y = log(FuelAmountExpended), data = readCombinedDeliveries(kdemo), 
      colour = TransportType)


# Reference table of lognode product flows
View(subset(getLognodeBalance(kdemo), TopLogNode == 'OBJ BLUE SUPPLY POINT' & SupplyType == 'FUEL'))
