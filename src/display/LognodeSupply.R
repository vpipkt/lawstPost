
#Define a function to plot the relative supply levels of children of a LogNode
#run this from exe directory if config file uses relative paths


plotClientOnHand <- function(config, logNodeName, levels = 1) {
    require(ggplot2)
    
    #get the data in 'risk' terms (unit risk and lognode %onhand)
    deps <- getLognodeClientSupply(configFile, logNodeName, levels)

    #panel plot 
    pcoh <- ggplot(deps)
}

# Plot overall dependent unit on-hand risk over time
# For all levels supplied, use levels = 0. Otherwise levels = 1 is children; 2 is children and grandchildren...
# TODOs: 
#   * Better legend for colours (doesn't appear for small N)
#   * Set alpha proportional to 1 / number of units at the level.
#   * Extract only dependents by supply type supported. This only seems to impact grandchildren and deeper in the tree. [This seems already to work.]
plotClientRisk <- function(configFile, logNodeName, levels = 0) {
    require(ggplot2)
    
    #get the data in 'risk' terms (unit risk and lognode %onhand)
    deps <- getLognodeClientSupply(configFile, logNodeName, levels)
    
    gg <- ggplot(deps, aes(x = Day, y = PpnStorageEmpty, group = UnitName, colour = level))
    gg + geom_line(alpha = 0.3, lwd = 1) + facet_grid(SupplyType ~ .) + 
        ggtitle(paste('Units supported by',logNodeName)) + ylab('Supply Quasi-Risk')  
}

getLognodeClientSupply <- function(configFile, logNodeName, levels = 0) {
    #read unit script
    unitScript <- readUnitScript(configFile)
    
    #get supplying lognode relation sets for logNodeName; note this recurses over the tree
    deps <- findDependents(unitScript$SupplyScript, logNodeName, maxLevel = levels)
    
    #merge deps object with risk histories; note lognodes may appear in unit
    lnSupply <-   readLogNodeSupplyHistory(configFile)
    lnSupply <- subset(lnSupply,  select = c(Day, UnitName, SupplyType, PpnStorageEmpty))
    unitSupply <- readUnitSupplyHistory(configFile)
    unitSupply <- subset(unitSupply, select = c(Day, UnitName, SupplyType, RiskScore))
    
    deps <- merge(deps, unitSupply, all.x = TRUE)
    deps <- merge(deps, lnSupply, all.x = TRUE)
    
    # Now have two columns to bring together; take lognode if conflict
    deps$PpnStorageEmpty[is.na(deps$PpnStorageEmpty)] <- deps$RiskScore[is.na(deps$PpnStorageEmpty)]
    deps$level <- as.factor(deps$level) #if continuous, colours are on a gradient  
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