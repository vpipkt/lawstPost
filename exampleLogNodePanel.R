#This is one of the most frequently used plots in LAWST post processing

library(ggplot2)

#Adjudicate, save, and read the outputs
dir <- '~/dropbox/work/E2O Support/LAWST Distribution and supply sim/K demo/Korea unclass demo_OUTPUT/'

lnSupply <- read.csv(paste(dir, 'LogNodeSupplyHistory.csv', sep=''),
                     colClasses = c('integer', rep('factor', 2), rep('numeric', 11)))

#subset for units of interest if desired
lngg <- subset(lnSupply, UnitName %in% c('ANDONG SUPPLY DEPOT', 
                                         'OBJ BLUE SUPPLY POINT', 'OBJ YELLOW SUPPLY POINT' ))

#plot log node inventory level over time for multiple lognodes
gg <- ggplot(lngg, aes(x = Day, y = PpnStorageFull, colour = SupplyType))
#this line actually produces the plot
gg + geom_line(lwd = 1) + facet_grid(UnitName ~ .)
