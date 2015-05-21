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
    increments$Type               <- 'Increment'
    increments$Category           <- 'Increment'
    increments$TransportType      <- NA
    increments$DelivID            <- NA
    
    pipe.delivery                    <- readPipelineDeliveries(lconfig)
    pipe.delivery$FuelAmountExpended <- 0
    pipe.delivery$SortieDurationPer  <- 0
    pipe.delivery$FuelTypeExpended   <- factor(NA,levels=levels(trans.delivery$FuelTypeExpended))
    pipe.delivery$Type               <- 'Pipeline'
    pipe.delivery$Category           <- 'Pipeline'
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