# Read a LAWST game, both inputs and outputs into a set of data frames

## Completed
#  * Read config into a list structure
#  * Most output files
#  * Read unit script with all fields rolling

## TODOs
#   * Write a function to read object data and outputs into one data structure.
#   * Refactor function signature(s) to read a list or vector of configs.
#   * Where one table's factor refers to another, ensure same levels
#   * Finish reading in other object types - see list at bottom of file
#   * Add game name, case name and timestamp to all data frames
#   * Finish reading scripts: lognode, pipeline, arc
#   * Read output data arc and pipe history
#   * Investigate using unit icons in charts

readLawstConfig <- function(configFile = 'game.lconfig', caseName = 'case', timestamp = date()){
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
                WORK_DIR = getwd(),
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
                  colClasses = c(rep("character", 4), "logical"))
    #not sure if this project will exploit the icons
    u <- data.frame(GameName = config$GAME_NAME, u, stringsAsFactors = FALSE)
    #Standardize the field names for post processing use
    names(u)[c(2:5)] <- c('UnitName', 'UnitDesc', 'UnitType', 'UnitIcon')
    return(u)
}

readSupplyTypes <- function(config){
    st <- read.csv(config$files$SUPPLY_TYPES, 
                   colClasses = c(rep('character', 3), 'logical', 'numeric'),
                   col.names = c('SupplyType', 'SupplyDesc', 
                                 'SupplyClass', 'IsLiquid', 'Density'))
    st <- data.frame(GameName = config$GAME_NAME, st, stringsAsFactors = FALSE)
    st[st$IsLiquid == FALSE,'Density'] <- 1
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
    us <- expand.grid(GameName = config$GAME_NAME, 
                      UnitName = u$UnitName, Day = seq(from = 0, to = nDay, by = 1))
    
    us.supply <- expand.grid(GameName = config$GAME_NAME, 
                             UnitName = u$UnitName, SupplyType = st$SupplyType,
                             Day = seq(from = 0, to = nDay, by = 1))
    
    #read the unit script file
    raw <- read.csv(config$files$UNIT_SCRIPTS, fill = TRUE,
                   colClasses = c('character', 'numeric', rep('character', 3)),
                   col.names = c('UnitName', 'Day', 'Field', 'V1', 'V2'),
                   header = FALSE, skip = 1) #use this header & skip combination to avoid a warning; the header has 1 too few columns.
    
    # 'fields' all uppercase and replace space with underscore
    raw$Field <- toupper(raw$Field)
    raw$Field <- gsub(" ", "_", raw$Field, fixed = TRUE)
    raw <- data.frame(GameName = config$GAME_NAME, raw, stringsAsFactors = FALSE)
    
    #convert raw and expand.grids into data.tables to take advantage of rolling merges
    us.dt <- data.table(us)
    setkey(us.dt, GameName, UnitName, Day)
    us.supply.dt <- data.table(us.supply)
    setkey(us.supply.dt, GameName, UnitName, SupplyType, Day)
    raw.dt <- data.table(raw)
    setkey(raw.dt, GameName, UnitName, Day) 
    
    # Subsetting by field, merge into the normalized data table with rolling join
    
    #LATITUDE
    us.dt <- raw.dt[Field=="LOCATION"][us.dt, roll = TRUE]
    us.dt[, Latitude := as.numeric(V1)]
    us.dt[, Longitude := as.numeric(V2)]
    us.dt[, V1 := NULL]
    us.dt[, V2 := NULL]
    us.dt[, Field := NULL]
    setkey(us.dt, GameName, UnitName, Day)
    
    #Strength 
    us.dt <- raw.dt[Field=="STRENGTH"][us.dt, roll = TRUE]
    us.dt[, Strength := as.numeric(V1)]
    us.dt[, V1 := NULL]
    us.dt[, V2 := NULL]
    us.dt[, Field := NULL]
    setkey(us.dt, GameName, UnitName, Day)
           
    #'POSTURE'
    us.dt <- raw.dt[Field=="POSTURE"][us.dt, roll = TRUE]
    us.dt[, Posture := V1]
    us.dt[, V1 := NULL]
    us.dt[, V2 := NULL]
    us.dt[, Field := NULL]
    setkey(us.dt, GameName, UnitName, Day)
    
    #Consumption Class 
    us.dt <- raw.dt[Field=="CONSUMPTION_CLASS"][us.dt, roll = TRUE]
    us.dt[, ConsumptionClass := V1]
    us.dt[, V1 := NULL]
    us.dt[, V2 := NULL]
    us.dt[, Field := NULL]
    setkey(us.dt, GameName, UnitName, Day)
        
    #Prioritization Class
    us.dt <- raw.dt[Field=="PRIORITIZATION_CLASS"][us.dt, roll = TRUE]
    us.dt[, PrioritizationClass := V1]
    us.dt[, V1 := NULL]
    us.dt[, V2 := NULL]
    us.dt[, Field := NULL]
    setkey(us.dt, GameName, UnitName, Day)
    
    # REQUIRED DAYS OF SUPPLY'
    us.dt <- raw.dt[Field=="REQUIRED_DAYS_OF_SUPPLY"][us.dt, roll = TRUE]
    us.dt[, ReqDaysSupply := as.numeric(V1)]
    us.dt[, V1 := NULL]
    us.dt[, V2 := NULL]
    us.dt[, Field := NULL]
    setkey(us.dt, GameName, UnitName, Day)
      
    #'DOMAIN'
    us.dt <- raw.dt[Field=="DOMAIN"][us.dt, roll = TRUE]
    us.dt[, Domain := V1]
    us.dt[, V1 := NULL]
    us.dt[, V2 := NULL]
    us.dt[, Field := NULL]
    setkey(us.dt, GameName, UnitName, Day)
        
    # Supply type dependent 
    #SUPPLYING LOG NODE
    setnames(raw.dt, old = 'V1', new = 'SupplyType')
    setkey(raw.dt,  GameName, UnitName, SupplyType, Day) 
    us.supply.dt <- raw.dt[Field == "SUPPLYING_LOG_NODE"][us.supply.dt, roll = TRUE]
    us.supply.dt[, SupplyingLogNode := V2]
    us.supply.dt[, SupplyType := SupplyType]
    us.supply.dt[, V2 := NULL]
    us.supply.dt[, Field := NULL]
    setkey(us.supply.dt, GameName, UnitName, SupplyType, Day)
             
    #merge in the supply intrements without rolling, as a left outer join
    us.supply.dt[raw.dt[Field == 'SUPPLY_INCREMENT'], 
                 SupplyIncrement := as.numeric(i.V2), nomatch = NA ]
    
    return(list(Script = as.data.frame(us.dt), 
                SupplyScript = as.data.frame(us.supply.dt)
                ))
}

#TODO 
#    * read the max loads as a data frame
#    * read exclusions as a data frame
readTransports <- function(config){
    t <- read.csv(config$files$TRANSPORTATION_ASSETS,
                  skip = 1, header = FALSE, fill = TRUE, stringsAsFactors = FALSE)
    t <- data.frame(GameName = config$GAME_NAME, t, stringsAsFactors = FALSE)
    
    #transport types
    tr <- t[, c(1:3, 5:6)]
    names(tr)[2:5] <- c('TransportType', 'TransDesc', 'Category', 'Availability')
    #now compress out the configurations.
    tr <- unique(tr)
    
    #configurations 
    c <- t[, 1:10]  
    names(c)[2:10] <- c('TransportType', 'TransDesc', 'ConfigurationName', 'Category', 
                        'Availability', 'Fuel Type', 'Fuel Efficiency', 'Average Speed', 'Max Range')
    
    #the capacities are a simpler idea of the prioritization class / consumption class 
    
    list(transports = tr,
         configurations = c,
         capacities = data.frame(),
         exclusions = data.frame()
    )
}

# TODO: read intermediate points.
readArcs <- function(config){
    a <- read.csv(config$files$ARCS, skip = 1, header = FALSE, 
                  colClasses = c(rep('character', 5), rep('numeric', 2)))
    a <- a[, 1:7] # Filter out the intermediate points for now.
    a <- data.frame(GameName = config$GAME_NAME, a, stringsAsFactors = FALSE)
    names(a)[1 + 1:7] <- c('ArcName', 'ArcDesc', 'Node1', 'Node2', 'Mode', 'True Length', 'Max Speed')
    return(a)
}


readNodes <- function(config){
    data.frame(GameName = config$GAME_NAME, 
               read.csv(config$files$NODES, 
                        colClasses = c(rep('character', 2), rep('numeric', 2), 'character'),
                        col.names = c('NodeName', 'NodeDesc', 'Latitude', 'Longitude', 'Domain')),
               stringsAsFactors = FALSE)
} 

readPipelines <- function(config){
    # HEADER := Name,Description,Supply Type, Max Throughput,Max Distance,Fuel Type,Fuel Rate,Sea State Degrade Multipliers {0;1;2;3;4;5;6;7;8;9}
    p <- read.csv(config$files$PIPELINES,
                  colClasses = c(rep('character', 3), rep('numeric', 2), 'character', rep('numeric', 11)),
                  col.names = c('Pipeline', 'PipeDesc', 'SupplyType', 'Throughput', 'MaxDistance', 'FuelType', 'FuelRate', paste('SeaState', 0:9, sep = '')),
                  header = FALSE, skip = 1, stringsAsFactors = FALSE)
    
    return(data.frame(GameName = config$GAME_NAME, p, stringsAsFactors = FALSE))
}

#consumption classes - see separate source file

# Other objects to read.
readMaps <- function(config){} #not sure this is really needed
readWeather <- function(config){} #low priority
readPostures <- function(config){}
readScriptedSorties <- function(config){}



#scripts to read
readArcScript <- function(config){}
readLogNodeScript <- function(config){}
readPipelineScript <- function(config){}

# Read output data

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

readSupplyRequests <- function(config){
    read.csv(paste(config$OUTPUT_DIR, 'SupplyRequests.csv', sep = ''),
             colClasses = c(rep('integer', 2), rep('factor', 2), rep('numeric', 3),
                            'integer', rep('numeric', 2))
    )
}

readTransportDeliveries <- function(config){
    # HEADER := TransportDeliveryID,RequestID,Day,SupplyingLogNode,TransportType,TransportConfiguration,DeliveredAmount,DeliveredWeight,FuelTypeExpended,FuelAmountExpended,ConvoyProtectionFuelExpended,Distance,Sorties,TransportTimeSpent,SortieDurationPer,Scripted
    td <- read.csv(paste(config$OUTPUT_DIR, 'TransportDeliveries.csv', sep = ''),
             colClasses = c(rep('integer',3), rep('factor', 3), rep('numeric', 2), 
                            'factor', rep('numeric', 6), 'logical'))
    #Set factor levels to align with rest of game data
    u <- readUnit(config)
    t <- readTransports(config)
    levels(td$SupplyingLogNode) <- u$UnitName[u$IsLogNode]
    levels(td$TransportType) <- levels(t$transports$Name)
    levels(td$TransportConfiguration) <- levels(t$transports$ConfigurationName)
    levels(td$FuelTypeExpended) <- readSupplyTypes(config)$SupplyType
    return(td)
}

readIncrementDeliveries <- function(config){
    read.csv(paste(config$OUTPUT_DIR, 'ScriptedIncrementDeliveries.csv', sep = ''),
        colClasses = c(rep('integer', 2), 'numeric')         
    )
}

readPipelineDeliveries <- function(config){
    # header line :=  RequestID,Day,Pipeline,SupplyingLogNode,DeliveredAmount,FuelTypeExpended,FuelAmountExpended,Distance
        pd <- read.csv(paste(config$OUTPUT_DIR, 'PipelineDeliveries.csv', sep = ''),
             colClasses = c(rep('integer', 2), rep('factor', 2), 'numeric', 
                            'factor', rep('numeric', 2)))
        levels(pd$Pipeline) <- readPipelines(config)$Pipeline
        u <- readUnit(config)
        levels(pd$SupplyingLogNode) <- u$UnitName[u$IsLogNode]
        levels(pd$FuelTypeExpended) <- readSupplyTypes(config)$SupplyType
        
        return(pd)
}

readDeliveryArcs <- function(config){
    da <- read.csv(paste(config$OUTPUT_DIR, 'DeliveryArcs.csv', sep = ''),
             colClasses = c('integer', 'factor')
             )
    # merge to get start/end locations 
    n <- readNodes(config)
    a <- readArcs(config)
    a <- merge(a, n, by.x = 'Node1', by.y = 'Name', suffixes = c('', '.o'))
    a <- merge(a, n, by.x = 'Node2', by.y = 'Name', suffixes = c('', '.d'))
    a$Description <- NULL
    a$Description.o <- NULL
    a$Description.d <- NULL
    merge(da, a, by.x = 'ArcID', by.y = 'Name', all.x = TRUE, all.y = FALSE)
}

#Arc history
#pipelinHistory
