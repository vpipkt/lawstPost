# Read a LAWST game, both inputs and outputs into a set of data frames

## Completed
#  * Read config into a list structure
#  * Read unit and supply type inputs 
#  * Read unit script for supply relationships

## TODOs
#   * Write a function to read everything into one structure.
#   * Finish unit script - rolling joins
#   * Finish reading in other object types - see list at bottom of file
#   * Add game name, case name and timestamp to all data frames
#   * Finish reading scripts: lognode, pipeline, arc
#   * Read output data
#   * Investigate using unit icons in charts

readLawstConfig <- function(configFile = 'game_config.csv', caseName = 'case', timestamp = date()){
    #returns a list of configuration file contents
    cfg <- read.table(configFile, header = FALSE, sep = ",", fill = TRUE,
                      colClasses = rep("character", 3), col.names = c("field", "value1", "value2"))
    cfg$field <- toupper(cfg$field)
    #Replace any space with underscore
    cfg$field <- gsub(" ", "_", cfg$field, fixed = TRUE)
    
    #Get the path to the _OUTPUT folder
    outDir <- paste(dirname(configFile), '/', cfg$value1[cfg$field == "GAME_NAME"], 
                    "_OUTPUT/", sep = '')
    
    #This assumes structure of the game file is stable
    return(list(GAME_NAME = cfg$value1[cfg$field == "GAME_NAME"],
                CASE = caseName,
                TIMESTAMP = timestamp,
                OUTPUT_DIR = outDir,
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
    #Standardize the field names for post processing use
    names(u) <- c('UnitName', 'Description', 'UnitType', 'UnitIcon', 'IsLogNode')
    return(u)
}

readSupplyTypes <- function(config){
    st <- read.csv(config$files$SUPPLY_TYPES, 
                   colClasses = c('factor', 'character', 'factor', 'logical', 'numeric'),
                   col.names = c('SupplyType', 'Description', 
                                 'SupplyClass', 'IsLiquid', 'Density'))
    return(st)
}

readUnitScript <- function(config){
    require(data.table)
    
    #assume reading units and supply types is cheap
    u <- readUnit(config)
    st <- readSupplyTypes(config)
    nDay <- config$GAME_DURATION - 1
    
    # This function will return a list of two data frames. One has supply-dependent 
    #   fields. This is the other.
    us <- expand.grid(UnitName = u$UnitName, Day = seq(from = 0, to = nDay, by = 1))
    
    us.supply <- expand.grid(UnitName = u$UnitName, SupplyType = st$SupplyType,
                             Day = seq(from = 0, to = nDay, by = 1))
    
    #read the unit script file
    raw <- read.csv(config$files$UNIT_SCRIPTS, fill = TRUE,
                   colClasses = c('factor', 'numeric', rep('character', 3)),
                   col.names = c('UnitName', 'Day', 'Field', 'V1', 'V2'),
                   header = FALSE, skip = 1) #use this header & skip combination to avoid a warning; the header has 1 too few columns.
    
    # 'fields' all uppercase and replace space with underscore
    raw$Field <- toupper(raw$Field)
    raw$Field <- gsub(" ", "_", raw$Field, fixed = TRUE)
    
    #convert raw and expand.grids into data.tables to take advantage of rolling merges
    us.dt <- data.table(us)
    setkey(us.dt, UnitName, Day)
    us.supply.dt <- data.table(us.supply)
    setkey(us.supply.dt, UnitName, SupplyType, Day)
    raw.dt <- data.table(raw)
            
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
    
    # Supply type dependent 
    #SUPPLYING LOG NODE
    setnames(raw.dt, c('UnitName', 'Day', 'Field', 'SupplyType', 'V2'))
    setkey(raw.dt,  UnitName, SupplyType, Day)
    #this reassignment may not be the most efficient   
    us.supply.dt <- raw.dt[Field=="SUPPLYING_LOG_NODE"][us.supply.dt, roll = TRUE]
    us.supply.dt[, SupplyingLogNode := as.factor(V2)]
    us.supply.dt[, SupplyType := as.factor(SupplyType)]
    us.supply.dt[, V2 := NULL]
    us.supply.dt[, Field := NULL]
    setkey(us.supply.dt, UnitName, SupplyType, Day)
             
    #merge in the supply intrements without rolling, as a left outer join
    us.supply.dt[raw.dt[Field == 'SUPPLY_INCREMENT'], 
                 SupplyIncrement := as.numeric(i.V2), nomatch = NA ]
  
    #Supply increment is now numeric; also would rather have supplyingLN be FACTOR.
    
    return(list(Script = us, 
                SupplyScript = as.data.frame(us.supply.dt)
                ))
}

# Objects to read`
readArcs <- function(config){}
#consumption classes = done
readMaps <- function(config){} #not sure this is really needed
readNodes <- function(config){} 
readPipelines <- function(config){}
readPostures <- function(config){}
readPriorityClass <- function(config){}  # this structure is the same as consumption class
readScriptedSorties <- function(config){}
readTransports <- function(config){}
readWeather <- function(config){} #low priority

#scripts to read
readArcScript <- function(config){}
readLogNodeScript <- function(config){}
readPipelineScript <- function(config){}

# Read output data

#utility function to locate the output folder.

readLogNodeSupplyHistory <- function(config){
    read.csv(paste(config$OUTPUT_DIR, 'LogNodeSupplyHistory.csv', sep = ''),
             colClasses = c('integer', rep('factor', 2), rep('numeric', 13)))
}

readLogNodeTransportHistory <- function(config){
    read.csv(paste(config$OUTPUT_DIR, 'LogNodeTransportHistory.csv', sep = ''),
             colClasses = c('integer', rep('factor', 2), rep('numeric', 6)))
}

readUnitSupplyHistory <- function(config){
    read.csv(paste(config$OUTPUT_DIR, 'UnitHistory.csv', sep = ''),
             colClasses = c('integer', rep('factor',2), 'character', rep('numeric', 16)))
}
