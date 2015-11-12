#Reads the three delivery files from the working directory and returns a data frame
# containing the common elements of these data sets 
readCombinedDeliveries <- function(lconfig){
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
    }
    
    
    #Set up which columns are going to be output
    columns <- c('RequestID', 'DelivID', 'Day', 'SupplyingLogNode','DeliveredAmount',
                 'Distance','FuelAmountExpended','SortieDurationPer',
                 'FuelTypeExpended','Type', 'Category', 'TransportType')
        
    #Read output files
    increments                    <- readIncrementDeliveries(lconfig)
    if(nrow(increments)){
        increments$Distance           <- 0
        increments$FuelAmountExpended <- 0
        increments$SortieDurationPer  <- 0
        increments$SupplyingLogNode   <- factor(NA,levels=levels(trans.delivery$SupplyingLogNode))
        increments$FuelTypeExpended   <- factor(NA,levels=levels(trans.delivery$FuelTypeExpended))
        increments$Type               <- increments$Category <- 'Increment'
        increments$TransportType      <- NA
        increments$DelivID            <- NA
        
        delivery <- increments[columns]    
    }
    
    
    pipe.delivery                    <- readPipelineDeliveries(lconfig)    
    if(nrow(pipe.delivery)){
        pipe.delivery$Type           <- pipe.delivery$Category <- 'Pipeline'
        pipe.delivery$SortieDurationPer  <- numeric(nrow(pipe.delivery))
        pipe.delivery$TransportType      <- NA
        pipe.delivery$DelivID            <- NA
        
        if(exists('delivery')){
            delivery <- rbind(delivery, pipe.delivery[columns])     
        }else{
            delivery <- pipe.delivery[columns]
        }
    }
          
    if(nrow(trans.delivery)){
        if(exists('delivery')){
            delivery <- rbind(delivery, trans.delivery[columns])
        } else {
            delivery <- trans.delivery[columns]
        }
    }
        
          
    #merge with requests to get the requesting unit and supply type
    request <- readSupplyRequests(lconfig)
    st <- readSupplyTypes(lconfig)
    request <- merge(request, st)
    
    if(!exists('delivery'))
        return(NULL)
    
    m <- merge(delivery, request, by.x=c('RequestID', 'Day'), by.y = c('ID', 'Day'), 
               all.x = TRUE)
    m$Volume <- m$Density * m$DeliveredAmount
        
    columns <- c('OriginatingUnit', 'SupplyType', columns, 'Volume')
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