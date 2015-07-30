#Reads the three delivery files from the working directory and returns a data frame
# containing the common elements of these data sets 
readCombinedDeliveries <- function(lconfig){
    #Read increments (freebies) 
    increments                    <- readIncrementDeliveries(lconfig)
    increments$Distance           <- 0
    increments$FuelAmountExpended <- 0
    increments$SortieDurationPer  <- 0
    increments$SupplyingLogNode   <- NA
    increments$FuelTypeExpended   <- NA
    increments$Type               <- increments$Category <- 'Increment'
    increments$TransportType      <- NA
    increments$DelivID            <- paste('Incr', row.names(increments), sep = '')
    
    columns <- c('RequestID', 'DelivID', 'Day', 'SupplyingLogNode','DeliveredAmount',
                 'Distance','FuelAmountExpended','SortieDurationPer',
                 'FuelTypeExpended','Type', 'Category', 'TransportType')
    delivery <- increments[columns]
    
    #Read transport deliveries
    trans.delivery      <- readTransportDeliveries(lconfig)
    if(nrow(trans.delivery)){
        trans.delivery$Type <- 'Transport'
        trans.delivery$DelivID <- trans.delivery$TransportDeliveryID
        
        # Merge transport definitions for type: vessel, vehicle, a/c
        ttypes <- readTransports(lconfig)$transports
        ttypes <- subset(ttypes, , c('Name', 'ConfigurationName', 'Category'))
        
        trans.delivery <- merge(trans.delivery, ttypes, 
                                by.x = c('TransportType', 'TransportConfiguration'),
                                by.y = c('Name', 'ConfigurationName'))
        delivery <- rbind(delivery, trans.delivery[columns])
    } 
       
    pipe.delivery                    <- readPipelineDeliveries(lconfig)
    if(nrow(pipe.delivery)){
        pipe.delivery$SortieDurationPer  <- 0
        pipe.delivery$Type               <- pipe.delivery$Category <- 'Pipeline'
        pipe.delivery$TransportType      <- NA
        pipe.delivery$DelivID            <- paste('Pipe', row.names(pipe.delivery), sep = '')
        delivery <- rbind(delivery, pipe.delivery[columns])
    }   
          
    #merge with requests to get the requesting unit and supply type
    request <- readSupplyRequests(lconfig)
    st <- readSupplyTypes(lconfig)
    request <- merge(request, st)
    
    m <- merge(delivery, request, by.x=c('RequestID', 'Day'), by.y = c('ID', 'Day'), 
               all.x = TRUE)
    m$DeliveredWeight <- m$Density * m$DeliveredAmount
        
    columns <- c('OriginatingUnit', 'SupplyType', columns, 'DeliveredWeight')
    return(m[columns])
}

# Read LAWST outputs and return a data frame of detailed supply deliveries with geographic start and end points
flowData <- function(lconfig){
    # read all the deliveries
    d <- readCombinedDeliveries(lconfig)
    
    # read delivery arcs, which automagically merges in the endpoing coordinates
    da <- readDeliveryArcs(lconfig)
      
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