#Define a function to plot the relative supply levels of children of a LogNode
#run this from exe directory if config file uses relative paths

## TODOs
#   * Move general purpose functions to a new script and call source
#   * Investigate using unit icons in charts
#   * 

readLawstConfig <- function(configFile = 'game_config.csv',caseName = 'case',timestamp = date()){
    #returns a list of configuration file contents
    
    cfg <- read.table(configFile, header = FALSE, sep = ",", fill = TRUE,
                      colClasses = rep("character", 3), col.names = c("field", "value1", "value2"))
    cfg$field <- toupper(cfg$field)
    #Replace any space with underscore
    cfg$field <- gsub(" ", "_", cfg$field, fixed = TRUE)
    
    #This assumes structure of the game file is stable
    return(list(GAME_NAME = cfg$value1[cfg$field == "GAME_NAME"],
                CASE = caseName,
                TIMESTAMP = timestamp,
                GAME_DURATION = as.numeric(cfg$value1[cfg$field == "GAME_DURATION"]),
                GAME_START_DATE = list(month = cfg$value1[cfg$field == "GAME_START_DATE"],
                                       day =  as.numeric(cfg$value2[cfg$field == "GAME_START_DATE"])),
                VOLUME_UOM = cfg$value1[cfg$field == "VOLUME_UOM"],
                #5
                MASS_UOM =  cfg$value1[cfg$field == "MASS_UOM"],
                                DISTANCE_UOM =  cfg$value1[cfg$field == "DISTANCE_UOM"],
                SUPPLY_ADJUDICATION_METHODOLOGY =  cfg$value1[cfg$field == "SUPPLY_ADJUDICATION_METHODOLOGY"],
                HIGHWAY_REACH = as.numeric(cfg$value1[cfg$field == "HIGHWAY_REACH"]),
                ZERO_RISK_UTILIZATION =  as.numeric(cfg$value1[cfg$field == "ZERO_RISK_UTILIZATION"]),
                GROUND_ESCORT_FUEL_RATE =  as.numeric(cfg$value1[cfg$field == "GROUND_ESCORT_FUEL_RATE"]),
                #10
                SEA_ESCORT_FUEL_RATE =  as.numeric(cfg$value1[cfg$field == "SEA_ESCORT_FUEL_RATE"]),
                AIR_ESCORT_FUEL_RATE =  as.numeric(cfg$value1[cfg$field == "AIR_ESCORT_FUEL_RATE"]),
                files = list(
                #Begin file names. Replace the windows style slash with /
                SUPPLY_TYPES = gsub('\\', '/', cfg$value1[cfg$field == "SUPPLY_TYPES"], fixed = TRUE),
                POSTURES =  gsub('\\', '/', cfg$value1[cfg$field == "POSTURES"], fixed = TRUE),
                CONSUMPTION_CLASSES =  gsub('\\', '/', cfg$value1[cfg$field == "CONSUMPTION_CLASSES"], fixed = TRUE),
                #15
                PRIORITIZATION_CLASSES =  gsub('\\', '/', cfg$value1[cfg$field == "PRIORITIZATION_CLASSES"], fixed = TRUE),
                MAPS =  gsub('\\', '/', cfg$value1[cfg$field == "MAPS"], fixed = TRUE),
                NODES =  gsub('\\', '/', cfg$value1[cfg$field == "NODES"], fixed = TRUE),
                ARCS =  gsub('\\', '/', cfg$value1[cfg$field == "ARCS"], fixed = TRUE),
                ARC_SCRIPTS =  gsub('\\', '/', cfg$value1[cfg$field == "ARC_SCRIPTS"], fixed = TRUE),
                #20
                TRANSPORTATION_ASSETS =  gsub('\\', '/', cfg$value1[cfg$field == "TRANSPORTATION_ASSETS"], fixed = TRUE),
                TRANSPORTATION_MODE_EXCLUSIONS =  gsub('\\', '/', cfg$value1[cfg$field == "TRANSPORTATION_MODE_EXCLUSIONS"], fixed = TRUE),
                UNITS =  gsub('\\', '/', cfg$value1[cfg$field == "UNITS"], fixed = TRUE),
                UNIT_SCRIPTS =  gsub('\\', '/', cfg$value1[cfg$field == "UNIT_SCRIPTS"], fixed = TRUE),
                LOG_NODE_SCRIPTS =  gsub('\\', '/', cfg$value1[cfg$field == "LOG_NODE_SCRIPTS"], fixed = TRUE),
                #25
                PIPELINES =  gsub('\\', '/', cfg$value1[cfg$field == "PIPELINES"], fixed = TRUE),
                PIPELINE_SCRIPTS =  gsub('\\', '/', cfg$value1[cfg$field == "PIPELINE_SCRIPTS"], fixed = TRUE),
                WEATHER =  gsub('\\', '/', cfg$value1[cfg$field == "WEATHER"], fixed = TRUE),
                SCRIPTED_SORTIES =  gsub('\\', '/', cfg$value1[cfg$field == "SCRIPTED_SORTIES"], fixed = TRUE)
                )
                ))
}

readUnit <- function(config){
    u <- read.csv(config$files$UNITS, 
                  colClasses = c("factor", rep("character", 3), "logical"))
    #not sure if I can actually exploit the icons
    #Standardize the names for post processing use
    names(u) <- c('UnitName', 'Description', 'UnitType', 'UnitIcon', 'IsLogNode')
    return(u)
}

readSupplyTypes <- function(config){
    st <- read.csv(config$files$SUPPLY_TYPES, colClasses = c('factor', 'character',
                                                         'factor', 'logical', 'numeric'),
                   col.names = c('SupplyType', 'Description', 
                                 'SupplyClass', 'IsLiquid', 'Density'))
    return(st)
}

readUnitScript <- function(config){
    #assume reading units and supply types is cheap
    u <- readUnit(config)
    st <- readSupplyTypes(config)
    nDay <- config$GAME_DURATION
    
    # This function will return a list of two data frames. One has supply-dependent 
    #   fields. This is the other.
    us <- expand.grid(UnitName = u$UnitName, Day = seq(from = 0, to = nDay, by = 1))
    
    us.supply <- expand.grid(UnitName = u$UnitName, SupplyType = st$SupplyType,
                             Day = seq(from = 0, to = nDay, by = 1))
    
    #read the unit script file
    raw <- read.csv(config$files$UNIT_SCRIPTS, fill = TRUE,
                   colClasses = c('factor', 'numeric', rep('character', 3)),
                   col.names = c('UnitName', 'Day', 'Field', 'V1', 'V2'))
    
    # 'fields' all uppercase and replace space with underscore
    raw$Field <- toupper(raw$Field)
    raw$Field <- gsub(" ", "_", raw$Field, fixed = TRUE)
    
    # Subsetting by field, merge into the normalized dataframe
    
    #LATITUDE
    us <- merge(us, subset(raw, Field == 'LOCATION', c('UnitName', 'Day', 'V1')), 
                by = c('UnitName', 'Day'), all.x = TRUE)
    names(us)[dim(us)[2]] <- 'Latitude'
    us$Latitude <- as.numeric(us$Latitude)
    
    #Longitude
    us <- merge(us, subset(raw, Field == 'LOCATION', c('UnitName', 'Day', 'V2')), 
                by = c('UnitName', 'Day'), all.x = TRUE)    
    names(us)[dim(us)[2]] <- 'Longitude'
    us$Longitude <- as.numeric(us$Longitude)
    
    #Strength 
    us <- merge(us, subset(raw, Field == 'STRENGTH', c('UnitName', 'Day', 'V1')), 
                by = c('UnitName', 'Day'), all.x = TRUE)    
    names(us)[dim(us)[2]] <- 'Strength'
    us$Strength <- as.numeric(us$Strength)
       
    #'POSTURE'
    us <- merge(us, subset(raw, Field == 'POSTURE', c('UnitName', 'Day', 'V1')), 
                by = c('UnitName', 'Day'), all.x = TRUE)    
    names(us)[dim(us)[2]] <- 'Posture'
    us$Posture <- as.factor(us$Posture)
    
    #Consumption Class 
    us <- merge(us, subset(raw, Field == 'CONSUMPTION_CLASS', c('UnitName', 'Day', 'V1')), 
                by = c('UnitName', 'Day'), all.x = TRUE)    
    names(us)[dim(us)[2]] <- 'ConsumptionClass'
    us$ConsumptionClass <- as.factor(us$ConsumptionClass)
    
    #Prioritization Class
    us <- merge(us, subset(raw, Field == 'PRIORITIZATION_CLASS', c('UnitName', 'Day', 'V1')), 
                by = c('UnitName', 'Day'), all.x = TRUE)    
    names(us)[dim(us)[2]] <- 'PrioritizationClass'
    us$PrioritizationClass <- as.factor(us$PrioritizationClass)
    
    # REQUIRED DAYS OF SUPPLY'
    us <- merge(us, subset(raw, Field == 'REQUIRED_DAYS_OF_SUPPLY', c('UnitName', 'Day', 'V1')), 
                by = c('UnitName', 'Day'), all.x = TRUE)    
    names(us)[dim(us)[2]] <- 'ReqDaysSupply'
    us$ReqDaysSupply <- as.numeric(us$ReqDaysSupply)
    
    #'DOMAIN'
    us <- merge(us, subset(raw, Field == 'DOMAIN', c('UnitName', 'Day', 'V1')), 
                by = c('UnitName', 'Day'), all.x = TRUE)    
    names(us)[dim(us)[2]] <- 'Domain'
    us$Domain <- as.factor(us$Domain)    
    
   #us.supply
    'SUPPLYING_LOG_NODE' 
    'US_BDE_MECH'
    
    # fill in "missing" days with previous day's value
    
    
    return(list(script = us, supplyScript = us.supply))
}

plotLogNodeDependentSupplyLevel <- function(configFile = 'game_config.csv', logNodeName, levels = 1) {
    
    currentLevel <- 1
    require(ggplot2)
    
    #read config file
    cfg <- readLawstConfig(configFile)
    #get output directory from game name
    outdir <- paste(cfg$GAME_NAME, 'OUTPUT', sep = '_')
    #get unit script directory, read unit script
    
    #get supplying lognode relation sets for logNodeName only
    
    
    
    #get unit supply output
    #subset for units of concern (and supplies and days?)
    
    #get lognode supply file
    #subset for lognodes of concern (days; supply types)
    
    #recurse the function on those lognodes if level checks okay
    
    #merge/stack results
    
    #plot outputs
}