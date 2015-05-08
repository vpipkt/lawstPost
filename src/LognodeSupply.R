
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
