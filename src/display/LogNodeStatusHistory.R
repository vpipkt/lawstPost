# A basic plot of lognode status over time
# This expands the log node info panel on the app


#TODO's: 
# * how to get supply and transport plot X-axes to perfectly line up (has to do with legend width?)

plotLogNodeHistory <- function(lawstCfg, logNodeName = 'New Lognode'){
    require(ggplot2)
    require(gridExtra)
    
    #Read the supply history
    supply <- readLogNodeSupplyHistory(lawstCfg)
        
    #Read the transport history
    trans <- readLogNodeTransportHistory(lawstCfg)
    
    supply <- subset(supply, UnitName == logNodeName)
    trans  <- subset(trans,  UnitName == logNodeName)
    
    onhand <- ggplot(supply, aes(x = Day, y = PpnStorageFull, colour = SupplyType)) +
        geom_line(lwd = 1) + ylab('Proportion On Hand') + ggtitle(logNodeName) + 
        theme(axis.title.x = element_blank()) + xlim(c(0, lawstCfg$GAME_DURATION - 1))
        
    flow <- ggplot(supply, aes(x = Day, y = PpnFlow, colour = SupplyType)) +
        geom_line(lwd = 1) + ylab('Proportion Throughput') + 
        theme(axis.title.x = element_blank()) + xlim(c(0, lawstCfg$GAME_DURATION - 1))
    
    tu <- ggplot(trans, aes(x = Day, y = Utilization, colour = TransportType)) +
        geom_line(lwd = 1) + ylab('Transport Utilization') + xlim(c(0, lawstCfg$GAME_DURATION - 1)) + 
        scale_color_discrete( h = c(0, 360) + 45)
    
    grid.arrange(onhand, flow, tu, nrow = 3)
            
}


# Alternate version taking an entire game's data structure (instead of reading the files again)

# A second alternate comparing any N games' data structures 

