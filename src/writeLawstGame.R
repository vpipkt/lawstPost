#Partial implementation of writing LAWST game based on a set of objects returned from analogous read*() functions

writeLawstConfig <- function(path = 'game.lconfig', config){
    #break the config up into 4 pieces writing the list out, relying on list order from readLawstConfig. This may write extra garbage to the config file output which lawst.exe should ignore
    
    #four pieces are: game name thru duration
    # date, a list of 2
    # rest of items except
    # files
    conn <- file(path, open = 'w')
    
    m <- matrix(unlist(c(names(config)[1:5], config[1:5])), ncol = 2)
    towrite  <- paste(m[, 1], m[, 2], sep = ',')
    writeLines(text = towrite, con = conn)
    
    writeLines(paste('GAME_START_DATE', config$GAME_START_DATE$month, 
                     config$GAME_START_DATE$day, sep = ','), conn)
    
    m <- matrix(unlist(c(names(config)[7:15], config[7:15])), ncol = 2)
    towrite  <- paste(m[, 1], m[, 2], sep = ',')
    writeLines(text = towrite, con = conn)
    
    m <- matrix(unlist(c(names(config$files), config$files)), ncol = 2)
    towrite  <- paste(m[, 1], m[, 2], sep = ',')
    writeLines(text = towrite, con = conn)
    
    close(conn)
}

writeUnitScript <- function(path = 'UnitScripts.csv', unitScript){
    # Writes a unit script file (csv) for use in LAWST, based on the `unitScript` object passed in.
    
    # The expected object is a list of two data frames
    #     List of 2
    #     $ Script      :'data.frame':    x obs. of  10 variables:
    #     ..$ UnitName           : Factor  
    #     ..$ Day                : num  
    #     ..$ Latitude           : num  
    #     ..$ Longitude          : num  
    #     ..$ Strength           : num  
    #     ..$ Posture            : Factor  
    #     ..$ ConsumptionClass   : Factor  
    #     ..$ PrioritizationClass: Factor 
    #     ..$ ReqDaysSupply      : num  
    #     ..$ Domain             : Factor  
    #     $ SupplyScript:'data.frame':	x obs. of  5 variables:
    #         ..$ UnitName        : Factor  
    #     ..$ Day             : num  
    #     ..$ SupplyType      : Factor  
    #     ..$ SupplyingLogNode: Factor  
    #     ..$ SupplyIncrement : num  
    

    #shorthand for the script
    s <- unitScript$Script
    sup <- unitScript$SupplyScript
    
    #open up the file connection for writing.
    conn <- file(path, open = 'w')
    writeLines('Unit ID,Day,Field,Value', conn) 

    # Define a function within this closure to handle writing, not ref to `conn`
    writeVector <- function(fieldname, vector){
        notna <- !is.na(vector)
        towrite <- paste(s$UnitName[notna], s$Day[notna], toupper(fieldname), 
                         vector[notna], sep = ',')
        writeLines( towrite, conn)
        return() 
    }
    
    writeTwoVector <- function(fieldname, df, vector1, vector2){
        notna <- !is.na(vector1) & !is.na(vector2)
        towrite <- paste(df$UnitName[notna], df$Day[notna],  toupper(fieldname), 
                         vector1[notna], vector2[notna], sep = ',')
        writeLines(towrite, conn)
        return()                        
    }
    
    writeTwoVector('Location',          s, s$Latitude, s$Longitude)
    
    writeVector('Strength',             s$Strength)
    writeVector('Posture',              s$Posture)
    writeVector('Consumption Class',    s$ConsumptionClass)
    writeVector('PRIORITIZATION CLASS', s$PrioritizationClass)
    writeVector('REQUIRED DAYS OF SUPPLY', s$ReqDaysSupply)
    writeVector('Domain',               s$Domain)
    
    writeTwoVector('SUPPLYING LOG NODE', sup, sup$SupplyType, sup$SupplyingLogNode)
    writeTwoVector('SUPPLY INCREMENT',   sup, sup$SupplyType, sup$SupplyIncrement)
    
    close(conn)
    return()
    
}

