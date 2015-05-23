
#Returns a data frame summary of product received and distributed by a lognode
getLognodeBalance <- function(lconfig){
    summary(getLognodeClientSupply(kdemo, 'OBJ BLUE SUPPLY POINT'))
    summary(readCombinedDeliveries(kdemo))
    
    
    downstream <- summaryLognodeClientSupply(configFile = lconfig)
}

summaryLognodeSketch <- function(configFile){
    #return a data frame with columns: lognode, supply type, day, #units direct, numUnitsTotal, NumLevels, consumption, consumption downstream, distro consumption, distro consumption downstream, demand, demand downstream, distributed, received
    
    units <- readUnit(configFile)
    units <- subset(units, IsLogNode == TRUE)
    
    rv <- data.frame()
    
    # Terrible loop to build the adjacency list into a decent structure
    for(ln in units$UnitName){
        # call findDependents to build the tree for this unit
    }
    
    # Merge unit history for consumed
    
    # merge deliveries to get
}

#not quite working out
summaryLognodeClientSupply <- function(configFile){
    #Return a data frame of consumption and demand by level
    require(reshape2)
    require(plyr)
    
    #get downstream demand for all lognodes
    units <- readUnit(configFile)
    units <- subset(units, IsLogNode == TRUE)

    rv <- data.frame()
    
    # Terrible loop
    for(ln in units$UnitName){
        client <- dcast(melt(getLognodeClientSupply(configFile, ln, levels = 0), 
                         id.vars = 1:6),
                    Day + SupplyType + level ~ variable, sum,
                    subset = .(variable %in% c('DemandAmount', 'ConsumedAmount')))
        
        client$Lognode <- ln
        rv <- rbind(rv, client)
    }
    
    rv$Lognode <- as.factor(rv$Lognode)
    rv <- rv[,c(6,1:5)]
    names(rv)[5:6] <- c('DemandDownstream', 'ConsumedDownstream')
    
    #Append the level 'zero' demand/consumptin, the lognode's own consumptino
    unitSupply <- readUnitSupplyHistory(configFile)
    unitSupply <- subset(unitSupply, UnitName %in% units$UnitName,
                         select = c(Day, UnitName, SupplyType, 
                            DemandAmount, ConsumedAmount))
    names(unitSupply)[2] <- 'Lognode'
    unitSupply$level <- 'Level 0'
    
    merge(rv, unitSupply, all.x = TRUE, all.y = TRUE)
    
}

#Return a data frame of consumption, demand, and supply for units supported by a selected lognode.
getLognodeClientSupply <- function(configFile, logNodeName, levels = 0) {
    #read unit script
    unitScript <- readUnitScript(configFile)
    
    #get supplying lognode relation sets for logNodeName; note this recurses over the tree
    deps <- findDependents(unitScript$SupplyScript, logNodeName, maxLevel = levels)
    
    #merge deps object with risk histories; note lognodes may appear in unit
    lnSupply   <- readLogNodeSupplyHistory(configFile)
    lnSupply   <- subset(lnSupply,  select = c(Day, UnitName, SupplyType, PpnStorageEmpty))
    unitSupply <- readUnitSupplyHistory(configFile)
    unitSupply <- subset(unitSupply, select = c(Day, UnitName, SupplyType, 
                                                DemandAmount, ConsumedAmount, RiskScore))
    
    deps <- merge(deps, unitSupply, all.x = TRUE)
    deps <- merge(deps, lnSupply, all.x = TRUE)
    
    # Now have two columns to bring together; take lognode if conflict
    deps$PpnStorageEmpty[is.na(deps$PpnStorageEmpty)] <- deps$RiskScore[is.na(deps$PpnStorageEmpty)]
    deps$RiskScore <- NULL
    deps$level <- paste('Level',deps$level) #if continuous, colours are on a gradient  
    return(deps)
}


findDependents <- function(supplyScript, lognode, level = 1L, maxLevel = 0L){
    rv <- subset(supplyScript, SupplyingLogNode == lognode)
    if(nrow(rv)){
        rv$level = level
    }
    
    if(!maxLevel || level < maxLevel){
        for(client in unique(rv$UnitName)){
            rv <- rbind(rv, findDependents(supplyScript, client, level + 1, maxLevel))
        }
    }    
    
    return(rv)
}