# TODO: replace NAs with zero in getLogNodeBalance.

#Returns a data frame summary of product received and distributed by a lognode
getLognodeBalance <- function(lconfig){
    require(reshape2)
    
    tree <- treeRollup(lconfig)
    
    #merge tree with unit history
    unitHistory <- readUnitSupplyHistory(lconfig)
    unitHistory <- subset(unitHistory, 
                          select = c(Day, UnitName, SupplyType, DemandAmount, ConsumedAmount))
    
    # merge tree by LinkUnit for downstream consumption
    ds <- merge(tree, unitHistory, 
                by.x = c('LinkUnit', 'Day', 'SupplyType'), by.y = c('UnitName', 'Day', 'SupplyType'))
    ds <- dcast(melt(ds, measure.vars = c('DemandAmount', 'ConsumedAmount')),
                TopLogNode + Day + SupplyType ~ variable, sum)
    #rename for merging
    ds$DownstreamDemand  <- ds$DemandAmount 
    ds$DemandAmount      <- NULL
    ds$DownstreamConsume <- ds$ConsumedAmount
    ds$ConsumedAmount    <- NULL
    
    # merge tree by TopLogNode for organic consumption
    ds <- merge(ds, unitHistory, by.x = c('TopLogNode', 'Day', 'SupplyType'),
                by.y = c('UnitName', 'Day', 'SupplyType'))
    
    # get consolidated deliveries and compute supply received, distributed, and fuel consumption in distribution
    
    del <- readCombinedDeliveries(lconfig)
    # Compute supply received by the lognode.
    rec <- dcast(melt(del, measure.vars = c('DeliveredAmount')),
                 OriginatingUnit + SupplyType + Day ~ variable, sum)
    rec$Received <- rec$DeliveredAmount
    rec$DeliveredAmount <- NULL
    
    rv <- merge(ds, rec, by.x = c('TopLogNode', 'SupplyType', 'Day'),
                by.y = c('OriginatingUnit', 'SupplyType', 'Day'), all.x = TRUE)
    
    # Compute supply distributed by the lognode
    dis <- dcast(melt(del, measure.vars = c('DeliveredAmount')),
                 SupplyingLogNode + SupplyType + Day ~ variable, sum)
    dis$Delivered <- dis$DeliveredAmount
    dis$DeliveredAmount <- NULL
    
    rv <- merge(rv, dis, by.x = c('TopLogNode', 'SupplyType', 'Day'),
                by.y = c('SupplyingLogNode', 'SupplyType', 'Day'), all.x = TRUE)
    
    # To compute delivered amount downstream, merge dis and tree 
    distree <- merge(tree, dis, by.x = c('LinkUnit', 'SupplyType', 'Day'), 
                     by.y = c('SupplyingLogNode', 'SupplyType', 'Day'))
    disroll <- dcast(melt(distree, measure.vars = 'Delivered'),
                     TopLogNode + SupplyType + Day ~ variable, sum)
    
    rv <- merge(rv, subset(disroll, select = c(TopLogNode, SupplyType, Day, Delivered)),
                all.x = TRUE)
    
    # Compute amount consumed in distributing supply; first at the lognode
    distroCon <- dcast(melt(del, measure.vars = c('FuelAmountExpended')),
                       SupplyingLogNode + FuelTypeExpended + Day ~ variable, sum)
    distroCon$DistroConsume <- distroCon$FuelAmountExpended
    distroCon$FuelAmountExpended <- NULL
    
    rv <- merge(rv, distroCon, by.x = c('TopLogNode', 'SupplyType', 'Day'),
                by.y = c('SupplyingLogNode', 'FuelTypeExpended', 'Day'), all.x = TRUE)
    
    # Compute downstream amount consumed distributing supply
    dctree <- merge(tree, distroCon, by.x =  c('LinkUnit', 'SupplyType', 'Day'),
                    by.y = c('SupplyingLogNode', 'FuelTypeExpended', 'Day'))
    dcRoll <- dcast(melt(dctree, measure.vars = 'DistroConsume'),
                    TopLogNode + SupplyType + Day ~ variable, sum)
    dcRoll$DistroConsDownstream <- dcRoll$DistroConsume 
    dcRoll$DistroConsume <- NULL
    
    rv <- merge(rv, dcRoll, all.x = TRUE)
    
    return(rv)
}

#TODO: 
#   * Pass the supply script down the call stack rather than read it each time
treeRollup <- function(lawstConfig, lognodes = NULL, level = 1L, maxLevel = 0L){
    ## Return a data frame of 'TopLogNode' and its subordinates at various levels  
    
    # Inefficient to read this on each recursive call...
    supplyScript <- readUnitScript(lawstConfig)$SupplyScript
    
    # default call examines entire game tree
    if(is.null(lognodes) & level == 1L){
        lns <- as.factor(subset(readUnit(lawstConfig), 
                                IsLogNode == TRUE)$UnitName)
        lognodes <- data.frame(TopLogNode = lns)
        rv <- merge(lognodes, supplyScript, by.y = 'SupplyingLogNode',
                    by.x = 'TopLogNode')
        
    } else
    {
        lognodes <- subset(lognodes, select = c(TopLogNode, Day, SupplyType, LinkUnit))
        rv <- merge(lognodes, supplyScript, by.x=c('Day', 'SupplyType', 'LinkUnit'),
                    by.y = c('Day', 'SupplyType', 'SupplyingLogNode'))
        
    }
    
    if(nrow(rv)){
        rv$level = level
        rv$LinkUnit <- rv$UnitName
        rv$UnitName <- NULL
        rv$SupplyIncrement <- NULL
        
        if(!maxLevel || level < maxLevel){
            rv <- rbind(rv, 
                        treeRollup(lawstConfig, rv, level + 1, maxLevel))
        }    
    }
    
    return(rv)
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