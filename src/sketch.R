#blue <- findDependents(readUnitScript(config = kdemo)$SupplyScript, 'OBJ BLUE SUPPLY POINT')
#subset(blue, SupplyType == 'FUEL')


#getLognodeClientSupply(kdemo, 'OBJ BLUE SUPPLY POINT')


#Returns a data frame summary of product received and distributed by a lognode
#getLognodeBalance <- function(lconfig){
lconfig<-kdemo

require(reshape2)

    tree <- treeRollup(lconfig)
    
    #merge tree with unit history
    unitHistory <- readUnitSupplyHistory(lconfig)
    unitHistory <- subset(unitHistory, select = c(Day, UnitName, 
                                              SupplyType, DemandAmount, ConsumedAmount))

    # merge tree by LinkUnit for downstream consumption
    ds <- merge(tree, unitHistory, 
                 by.x = c('LinkUnit', 'Day', 'SupplyType'), by.y = c('UnitName', 'Day', 'SupplyType'))
ds <- dcast(melt(ds, measure.vars = c('DemandAmount', 'ConsumedAmount')),
      TopLogNode + Day + SupplyType ~ variable, sum)
#rename for merging
ds$DownstreamDemand  <- ds$DemandAmount 
ds$DemandAmount      <- NULL
ds$DownstreamConsume <- ds$ConsumedAmount
ds$ConsumedAmount    <- NULL

# merge tree by TopLogNode for organic consumption
ds <- merge(ds, unitHistory, by.x = c('TopLogNode', 'Day', 'SupplyType'),
            by.y = c('UnitName', 'Day', 'SupplyType'))

# get consolidated deliveries and compute supply received, distributed, and fuel consumption in distribution

del <- readCombinedDeliveries(lconfig)
# Compute supply received by the lognode.
rec <- dcast(melt(del, measure.vars = c('DeliveredAmount')),
             OriginatingUnit + SupplyType + Day ~ variable, sum)
rec$Received <- rec$DeliveredAmount
rec$DeliveredAmount <- NULL

rv <- merge(ds, rec, by.x = c('TopLogNode', 'SupplyType', 'Day'),
            by.y = c('OriginatingUnit', 'SupplyType', 'Day'), all.x = TRUE)

# Compute supply distributed by the lognode
dis <- dcast(melt(del, measure.vars = c('DeliveredAmount')),
             SupplyingLogNode + SupplyType + Day ~ variable, sum)
dis$Delivered <- dis$DeliveredAmount
dis$DeliveredAmount <- NULL

rv <- merge(rv, dis, by.x = c('TopLogNode', 'SupplyType', 'Day'),
            by.y = c('SupplyingLogNode', 'SupplyType', 'Day'), all.x = TRUE)

# To compute delivered amount downstream, merge dis and tree 
distree <- merge(tree, dis, by.x = c('LinkUnit', 'SupplyType', 'Day'), 
                 by.y = c('SupplyingLogNode', 'SupplyType', 'Day'))
disroll <- dcast(melt(distree, measure.vars = 'Delivered'),
                 TopLogNode + SupplyType + Day ~ variable, sum)

rv <- merge(rv, subset(disroll, select = c(TopLogNode, SupplyType, Day, Delivered)),
                       all.x = TRUE)

# Compute amount consumed in distributing supply; first at the lognode
distroCon <- dcast(melt(del, measure.vars = c('FuelAmountExpended')),
                   SupplyingLogNode + FuelTypeExpended + Day ~ variable, sum)
distroCon$DistroConsume <- distroCon$FuelAmountExpended
distroCon$FuelAmountExpended <- NULL

rv <- merge(rv, distroCon, by.x = c('TopLogNode', 'SupplyType', 'Day'),
            by.y = c('SupplyingLogNode', 'FuelTypeExpended', 'Day'), all.x = TRUE)

# Compute downstream amount consumed distributing supply
dctree <- merge(tree, distroCon, by.x =  c('LinkUnit', 'SupplyType', 'Day'),
                by.y = c('SupplyingLogNode', 'FuelTypeExpended', 'Day'))
dcRoll <- dcast(melt(dctree, measure.vars = 'DistroConsume'),
                TopLogNode + SupplyType + Day ~ variable, sum)
dcRoll$DistroConsDownstream <- dcRoll$DistroConsume 
dcRoll$DistroConsume <- NULL

rv <- merge(rv, dcRoll, all.x = TRUE)

#}
rm(lconfig,tree)



if(FALSE){
    kad <- findAllDependents(kdemo)
    kad[order(kad$TopLogNode, kad$Day, kad$SupplyType),]
    nrow(kad)
    summary(kad)
    kada <-subset(kad, TopLogNode== 'ANDONG SUPPLY DEPOT')
    
}
