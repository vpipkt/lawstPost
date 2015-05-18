
#Define a function to plot the relative supply levels of children of a LogNode
#run this from exe directory if config file uses relative paths


plotLogNodeDependentSupplyLevel <- function(configFile = 'game_config.csv', logNodeName, levels = 1) {
    
    currentLevel <- 1
    require(ggplot2)
    
    #read config file
    cfg <- readLawstConfig(configFile)
    
    #read unit script
    unitScript <- readUnitScript(cfg)

    #get supplying lognode relation sets for logNodeName only
    unitScriptLN <- subset(unitScript$supplyScript, SupplyingLogNode == logNodeName)
    
    
    #get unit supply output
    #subset for units of concern (and supplies and days?)
    
    #get lognode supply file
    #subset for lognodes of concern (days; supply types)
    
    #recurse the function on those lognodes if level checks okay
    
    #merge/stack results
    
    #plot outputs, (using icons?)
}

# Plot overall dependent unit on-hand risk over time
# For all levels supplied, use levels = 0. Otherwise levels = 1 is children; 2 is children and grandchildren...
# TODO: better legend for colours ()
plotDependentRisk <- function(configFile, logNodeName, levels = 0) {
    
    currentLevel <- 1
    require(ggplot2)
    
    #read unit script
    unitScript <- readUnitScript(configFile)
    
    #get supplying lognode relation sets for logNodeName; note this recurses over the tree
    deps <- findDependents(unitScript$SupplyScript, logNodeName)
    
    #merge deps object with risk histories; note lognodes may appear in unit
    lnSupply <-   readLogNodeSupplyHistory(configFile)
    lnSupply <- subset(lnSupply,  select = c(Day, UnitName, SupplyType, PpnStorageEmpty))
    unitSupply <- readUnitSupplyHistory(configFile)
    unitSupply <- subset(unitSupply, select = c(Day, UnitName, SupplyType, RiskScore))
    
    deps <- merge(deps, unitSupply, all.x = TRUE)
    deps <- merge(deps, lnSupply, all.x = TRUE)
    
    # Now have two columns to bring together
    deps$PpnStorageEmpty[is.na(deps$PpnStorageEmpty)] <- deps$RiskScore[is.na(deps$PpnStorageEmpty)]
    deps$level <- as.factor(deps$level) #if continuous, colours are on a gradient
    
    gg <- ggplot(deps, aes(x = Day, y = PpnStorageEmpty, group = UnitName, colour = level))
    gg + geom_line(alpha = 0.3, lwd = 1) + facet_grid(SupplyType ~ .) + 
        ggtitle(paste('Units supported by',logNodeName)) + ylab('Quasi-risk')  
}

#Todo: append level on each call
findDependents <- function(supplyScript, lognode, level = 1L, maxLevel = 0L){
    rv <- subset(supplyScript, SupplyingLogNode == lognode)
    if(nrow(rv)){
        rv$level = level
    }
    
    if(!maxLevel || maxLevel <= level){
        for(client in unique(rv$UnitName)){
            rv <- rbind(rv, findDependents(supplyScript, client, level + 1, maxLevel))
        }
    }    
       
    return(rv)
}