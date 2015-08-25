### Read and write LAWST consumption class  files


# Read priority class from the config list produced by readLawstConfig()
readPrioritizationClass <- function(config){
    path <- config$files$PRIORITIZATION_CLASSES
    #open a connection to the file. Don't forget to close it
    conn <- file(path, open = 'r')
    #read and throw away the header line.
    pc <- readCCPC(conn)
    #fix names to make it a CC not a PC
    names(pc) <- c('PriorityClass', 'PriClassDesc', 'SupplyType', 'Posture', 'Priority')
    close(conn)
    return(data.frame(GameName = config$GAME_NAME, pc, stringsAsFactors = FALSE))
}

readConsumptionClass <- function(config){
    path <- config$files$CONSUMPTION_CLASSES
    #open a connection to the file. Don't forget to close it
    conn <- file(path, open = 'r')
    #read and throw away the header line.
    cc <- readCCPC(conn)
    #fix names to make it a CC not a PC
    names(cc) <- c('ConsumptionClass', 'ConsClassDescr', 'SupplyType', 'Posture', 'RequirementQty')
    close(conn)
    return(data.frame(GameName = config$GAME_NAME, cc, stringsAsFactors = FALSE))
}

readCCPC <- function(conn){
    
    #read and throw away the header line.
    head <- scan(conn, nlines = 1, what = 'character', quiet = TRUE)
    rm(head)
    
    #create a data frame to hold the tidy data
    tidy <- data.frame()
    #read each line in sequence, appending results to the df
    cc <- readCCPCLine(conn)
    while(is.data.frame(cc)){
        tidy <- rbind(tidy, cc)
        cc <- readCCPCLine(conn)
    }
    # return the data frame
    return(tidy)
}

#read one line from the connection passed in, which is presumed to be a 
#   LAWST configuration class file or priority class.
readCCPCLine <- function(conn){
    #read the whole line into a character vector
    ccLine <- scan(conn, sep = ',', what = 'character', nlines = 1, quiet = TRUE)
        
    if(length(ccLine) == 0)
        return(NA)
    
    #get name of consumption class and its description text
    cc <- ccLine[1:2]
    
    #define numeric and character vectors for each part of the triples
    supply  <- character()
    posture <- character()
    amount  <- numeric()
    
    if(length(ccLine) > 2){
        for(i in seq(from = 3, to = length(ccLine), by = 3)){
            supply  <- c(supply,  ccLine[i + 0])
            posture <- c(posture, ccLine[i + 1])
            amount  <- c(amount, 
                         as.numeric(ccLine[i + 2]))
        }
    } else {
        #trivial consumption class
        supply  <- NA
        posture <- NA
        amount  <- NA
    }
    
    return(data.frame(Class = cc[1],
                      Desc = cc[2],
                      SupplyType  = supply,
                      Posture     = posture,
                      Qty = amount,
                      stringsAsFactors = FALSE))
}


#Function to write a tidy data frame for consumption class
# The data frame is as produced by readConsumptionClass
# The csv file is to be read by LAWST


writePrioritizationClass <- function(dataFrame, path = 'tidyPC.csv'){
    conn <- file(path, open = 'w')
    writeLines('Name,Description,{SupplyType,Posture,Priority}', conn)
    ccs <- unique(dataFrame$PriorityClass)
    for(c in ccs){
        sub <- subset(dataFrame, PriorityClass == c)
        line <- c(as.character(sub$PriorityClass[1]), 
                  as.character(sub$PriClassDesc[1]))
        
        for(i in seq_len(nrow(sub))){
            if(!(is.na(sub$Priority[i])))
                line <- c(line, 
                          as.character(sub$SupplyType[i]), 
                          as.character(sub$Posture[i]),
                          sub$Priority[i])
        }
        writeLines(paste(line, collapse = ','), conn) 
    }
    close(conn)
}

    
writeConsumptionClass <- function(dataFrame, path = 'tidyCC.csv'){
    conn <- file(path, open = 'w')
    writeLines('Name,Description,{SupplyType,Posture,RequirementQty}', conn)
    ccs <- unique(dataFrame$ConsumptionClass)
    for(c in ccs){
        sub <- subset(dataFrame, ConsumptionClass == c)
        line <- c(as.character(sub$ConsumptionClass[1]), 
                  as.character(sub$ConsClassDescr[1]))
        
        for(i in seq_len(nrow(sub))){
            if(!(is.na(sub$RequirementQty[i])))
                line <- c(line, 
                          as.character(sub$SupplyType[i]), 
                          as.character(sub$Posture[i]),
                          sub$RequirementQty[i])
        }
        writeLines(paste(line, collapse = ','), conn) 
    }
    close(conn)
}