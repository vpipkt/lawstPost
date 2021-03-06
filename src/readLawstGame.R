# Read a LAWST game, both inputs and outputs into a set of data frames

## Completed
#  * Read config into a list structure
#  * Most output files
#  * Read unit script with all fields rolling

## TODOs
#   * Write a function to read everything into one structure.
#   * Where one table's factor refers to another, ensure same levels
#   * Finish reading in other object types - see list at bottom of file
#   * Add game name, case name and timestamp to all data frames
#   * Finish reading scripts: lognode, pipeline, arc
#   * Read output data arc and pipe history
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
    setkey(raw.dt,  UnitName, Day) 
    
    # Subsetting by field, merge into the normalized data table with rolling join
    
    #LATITUDE
    us.dt <- raw.dt[Field=="LOCATION"][us.dt, roll = TRUE]
    us.dt[, Latitude := as.numeric(V1)]
    us.dt[, Longitude := as.numeric(V2)]
    us.dt[, V1 := NULL]
    us.dt[, V2 := NULL]
    us.dt[, Field := NULL]
    setkey(us.dt, UnitName, Day)
    
    #Strength 
    us.dt <- raw.dt[Field=="STRENGTH"][us.dt, roll = TRUE]
    us.dt[, Strength := as.numeric(V1)]
    us.dt[, V1 := NULL]
    us.dt[, V2 := NULL]
    us.dt[, Field := NULL]
    setkey(us.dt, UnitName, Day)
           
    #'POSTURE'
    us.dt <- raw.dt[Field=="POSTURE"][us.dt, roll = TRUE]
    us.dt[, Posture := as.factor(V1)]
    us.dt[, V1 := NULL]
    us.dt[, V2 := NULL]
    us.dt[, Field := NULL]
    setkey(us.dt, UnitName, Day)
    
    #Consumption Class 
    us.dt <- raw.dt[Field=="CONSUMPTION_CLASS"][us.dt, roll = TRUE]
    us.dt[, ConsumptionClass := as.factor(V1)]
    us.dt[, V1 := NULL]
    us.dt[, V2 := NULL]
    us.dt[, Field := NULL]
    setkey(us.dt, UnitName, Day)
        
    #Prioritization Class
    us.dt <- raw.dt[Field=="PRIORITIZATION_CLASS"][us.dt, roll = TRUE]
    us.dt[, PrioritizationClass := as.factor(V1)]
    us.dt[, V1 := NULL]
    us.dt[, V2 := NULL]
    us.dt[, Field := NULL]
    setkey(us.dt, UnitName, Day)
    
    # REQUIRED DAYS OF SUPPLY'
    us.dt <- raw.dt[Field=="REQUIRED_DAYS_OF_SUPPLY"][us.dt, roll = TRUE]
    us.dt[, ReqDaysSupply := as.numeric(V1)]
    us.dt[, V1 := NULL]
    us.dt[, V2 := NULL]
    us.dt[, Field := NULL]
    setkey(us.dt, UnitName, Day)
      
    #'DOMAIN'
    us.dt <- raw.dt[Field=="DOMAIN"][us.dt, roll = TRUE]
    us.dt[, Domain := as.factor(V1)]
    us.dt[, V1 := NULL]
    us.dt[, V2 := NULL]
    us.dt[, Field := NULL]
    setkey(us.dt, UnitName, Day)
        
    # Supply type dependent 
    #SUPPLYING LOG NODE
    setnames(raw.dt, c('UnitName', 'Day', 'Field', 'SupplyType', 'V2'))
    setkey(raw.dt,  UnitName, SupplyType, Day) 
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
    
    return(list(Script = as.data.frame(us.dt), 
                SupplyScript = as.data.frame(us.supply.dt)
                ))
}

#TODO 
#    * read the max loads as a data frame
#    * read exclusions as a data frame
readTransports <- function(config){
    t <- read.csv(config$files$TRANSPORTATION_ASSETS,
                  skip = 1, header = FALSE, fill = TRUE)
    t <- t[, 1:9]
    names(t) <- c('Name', 'Description', 'ConfigurationName', 'Category', 'Availability', 
                  'Fuel Type', 'Fuel Efficiency', 'Average Speed', 'Max Range')
    
    list(transports = t,
         capacities = data.frame(),
         exclusions = data.frame()
    )
}

# TODO: read intermediate points.
#   * read fields as character then convert to appropriate type.
readArcs <- function(config){
    a <- read.csv(config$files$ARCS, skip = 1, header = FALSE)
    a <- a[, 1:7]
    names(a) <- c('Name', 'Description', 'Node1', 'Node2', 'Mode', 'True Length', 'Max Speed')
    return(a)
}

readNodes <- function(config){
    read.csv(config$files$NODES, colClasses = c('factor', 'character', rep('numeric', 2), 'factor'))
} 

#consumption classes = done

# Objects to read`
readMaps <- function(config){} #not sure this is really needed
readPipelines <- function(config){}
readPostures <- function(config){}
readPriorityClass <- function(config){}  # this structure is the same as consumption class
readScriptedSorties <- function(config){}

readWeather <- function(config){} #low priority

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
    read.csv(paste(config$OUTPUT_DIR, 'TransportDeliveries.csv', sep = ''),
             colClasses = c(rep('integer',3), rep('factor', 3), rep('numeric', 2), 
                            'factor', rep('numeric', 6), 'logical'))
}

readIncrementDeliveries <- function(config){
    read.csv(paste(config$OUTPUT_DIR, 'ScriptedIncrementDeliveries.csv', sep = ''),
        colClasses = c(rep('integer', 2), 'numeric')         
    )
}

readPipelineDeliveries <- function(config){
    read.csv(paste(config$OUTPUT_DIR, 'PipelineDeliveries.csv', sep = ''),
             colClasses = c(rep('integer', 2), rep('factor', 2), 'numeric', 
                            'factor', rep('numeric', 2))
    )
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
