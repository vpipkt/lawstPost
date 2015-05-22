#Reads the three delivery files from the working directory and returns a data frame
# containing the common elements of these data sets 
readCombinedDeliveries <- function(lconfig){
    #Read transport deliveries
    trans.delivery      <- readTransportDeliveries(lconfig)
    trans.delivery$Type <- 'Transport'
    trans.delivery$DelivID <- trans.delivery$TransportDeliveryID
    
    # Merge transport definitions for type: vessel, vehicle, a/c
    ttypes <- readTransports(lconfig)$transports
    ttypes <- subset(ttypes, , c('Name', 'ConfigurationName', 'Category'))
    trans.delivery <- merge(trans.delivery, ttypes, 
                            by.x = c('TransportType', 'TransportConfiguration'),
                            by.y = c('Name', 'ConfigurationName'))
        
    #Read
    increments                    <- readIncrementDeliveries(lconfig)
    increments$Distance           <- 0
    increments$FuelAmountExpended <- 0
    increments$SortieDurationPer  <- 0
    increments$SupplyingLogNode   <- factor(NA,levels=levels(trans.delivery$SupplyingLogNode))
    increments$FuelTypeExpended   <- factor(NA,levels=levels(trans.delivery$FuelTypeExpended))
    increments$Type               <- increments$Category <- 'Increment'
    increments$TransportType      <- NA
    increments$DelivID            <- NA
    
    pipe.delivery                    <- readPipelineDeliveries(lconfig)
    pipe.delivery$SortieDurationPer  <- 0
    pipe.delivery$FuelTypeExpended   <- factor(NA,levels=levels(trans.delivery$FuelTypeExpended))
    pipe.delivery$Type               <- pipe.delivery$Category <- 'Pipeline'
    pipe.delivery$TransportType      <- NA
    pipe.delivery$DelivID            <- NA
    
    columns <- c('RequestID', 'DelivID', 'Day', 'SupplyingLogNode','DeliveredAmount',
                    'Distance','FuelAmountExpended','SortieDurationPer',
                    'FuelTypeExpended','Type', 'Category', 'TransportType')
    delivery <- increments[columns]
    delivery <- rbind(delivery, pipe.delivery[columns])
    delivery <- rbind(delivery, trans.delivery[columns])
    
    #merge with requests to get the requesting unit and supply type
    request <- readSupplyRequests(lconfig)
    
    m <- merge(delivery, request, by.x=c('RequestID', 'Day'), by.y = c('ID', 'Day'), 
               all.x = TRUE)
    
    columns <- c('OriginatingUnit', 'SupplyType', columns)
    
    return(m[columns])
    
}

# Read LAWST outputs and return a data frame of detailed supply deliveries with geographic start and end points
flowData <- function(lconfig){
    # read all the deliveries
    d <- readCombinedDeliveries(lconfig)
    
    # read delivery arcs, which automagically merges in the endpoing coordinates
    da <- readDeliveryArcs(lconfig)
    
    # merge delivery arcs with deliveries, then sum cargo volume over location pairs
    st <- readSupplyTypes(lconfig)
    st[st$IsLiquid == FALSE,'Density'] <- 1
    
    d <- merge(d, st, all.x = TRUE)
    d$Volume <- d$Density * d$DeliveredAmount
    
    #  At this point only tranpsort deliveries are printed with an ID.
    da <- merge(da, d, by.x = 'TransportDeliveryID', by.y = 'DelivID')
    
    # subset deliveries with no arcs - air and pipes, also will have scripted increment
    airdel <- subset(d, !(DelivID %in% da$TransportDeliveryID))
    # merge no-arc unit locations
    unitLoc <- readUnitScript(lconfig)
    unitLoc <- unitLoc$Script[,c('UnitName', 'Day', 'Latitude', 'Longitude')]    
    
    # location of requesting unit
    airdel <- merge(airdel, unitLoc, by.x = c('Day', 'OriginatingUnit'),
                    by.y = c('Day', 'UnitName'), suffixes = c('', '.o'))
    # location of supplying unit
    airdel <- merge(airdel, unitLoc, by.x = c('Day', 'SupplyingLogNode'),
                    by.y = c('Day', 'UnitName'), suffixes = c('', '.d'))
    
    cols <- intersect(names(da), names(airdel))
    return(rbind(da[cols], airdel[cols]))
}