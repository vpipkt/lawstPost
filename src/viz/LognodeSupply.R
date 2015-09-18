
#Define a function to plot the relative supply levels of children of a LogNode
#run this from exe directory if config file uses relative paths
plotClientOnHand <- function(config, logNodeName, levels = 1) {
    require(ggplot2)
    
    #get the data in 'risk' terms (unit risk and lognode 1 - %onhand)
    deps <- getLognodeClientSupply(config, logNodeName, levels)
    deps$OnHandRel <- 1 - deps$PpnStorageEmpty
    #panel plot by unit name and level
    pcoh <- ggplot(deps, aes(x = Day, y = OnHandRel, colour = SupplyType))
    pcoh + geom_line(lwd = 1) + facet_grid(UnitName ~ level) + ylim(c(0, 1)) + 
        ylab('On hand amount, percent') +  ggtitle(paste('Units supported by',logNodeName))
}

# Plot overall dependent unit on-hand risk over time
# For all levels supplied, use levels = 0. Otherwise levels = 1 is children; 2 is children and grandchildren...
plotClientRisk <- function(configFile, logNodeName, levels = 0) {
    require(ggplot2)
    
    #get the data in 'risk' terms (unit risk and lognode %onhand)
    deps <- getLognodeClientSupply(configFile, logNodeName, levels)
    
    gg <- ggplot(deps, aes(x = Day, y = PpnStorageEmpty, group = UnitName, colour = level))
    gg + geom_line(alpha = 0.3, lwd = 1) + facet_grid(SupplyType ~ .) + 
        ggtitle(paste('Units supported by',logNodeName)) + ylab('Supply Quasi-Risk')  
}

