#Plot of overall product flow in the study area, as a Sankey diagram

# map background
# http://stackoverflow.com/questions/9558040/ggplot-map-with-l

# flow map of journeys; note this one has alpha instead of width for volume.
# http://spatial.ly/2015/03/mapping-flows/

# Use `day` argument to subset to the selected day. If day is less than 0, all days are shown in a filmstrip.
#First cut: flows along delivery arcs, ignoring temp connection; just overall flow don't try to color or complicate
# TODO
#   * Add map background
#   * Add unit icons at locations
#   * ? Add temp arcs to units
#   * options to color supply types or transports differently, or subset
#   * refactor a function to produce teh needed data frame
flowMap <- function(lconfig, day = -1, xlim = NULL, ylim = NULL){
    require(ggplot2)
    require(reshape2)
    require(plyr)
    
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
    
    m<- melt(da, measure.vars = c('DeliveredAmount', 'FuelAmountExpended', 'Volume'))
    
    surface <- dcast(m, Day + ArcID + Latitude + Longitude + Latitude.d + Longitude.d ~ variable, sum,
                     subset = .(variable == 'Volume'))
    
    
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
     
    #airpipe <- dcast(melt(airdel, measure.vars = c('DeliveredAmount', 'FuelAmountExpended', 'Volume')),
    #                 Day + Latitude + Longitude + Latitude.d + Longitude.d ~ variable, sum,
    #                 subset = .(variable == 'Volume')))
    cols <- intersect(names(da), names(airdel))
    m <- melt(rbind(da[cols], airdel[cols]), 
              measure.vars = c('DeliveredAmount', 'FuelAmountExpended', 'Volume'))
    flows <- dcast(m, Day + Latitude + Longitude + Latitude.d + Longitude.d ~ variable, sum,
                   subset = .(variable == 'Volume'))
    
    if(day >= 0)
        flows <- subset(flows, Day == day)
    
    fm <- ggplot(flows, aes(x = Longitude, y = Latitude))
    fm <- fm + geom_segment(aes(xend = Longitude.d, yend = Latitude.d, alpha = Volume), 
                            colour = 'white', lwd = 1) +
        scale_alpha_continuous(range = c(0.03, 0.3)) + coord_equal() + 
        scale_x_continuous('', breaks=NULL, limits = xlim) + 
        scale_y_continuous('', breaks=NULL, limits = ylim) +
        theme(panel.background = element_rect(fill = 'black', colour = 'black')) 
    
    if(day < 0)
        fm + facet_wrap( ~ Day)
    else
        fm
    
    #return(rbind(da[cols], airdel[cols]))
}



##### Example 1 rCharts ##### 
#Uses rCharts package which is not published on CRAN

# http://rcharts.io/viewer/?6001601#.VVnkzvlVikp
# this could be tough on the class system
if(FALSE)
{
    require(devtools)
    install_github('rCharts', 'ramnathv')  #github repo ramnathv/rCharts@master
    
    require(rCharts)
    require(rjson)
    #get source from original example
    #this is a JSON, so will need to translate
    #this is complicated and unnecessary but feel I need to replicate
    #for completeness
    #expect most data to come straight from R
    #in form of source, target, value
    links <- matrix(unlist(
        rjson::fromJSON(
            file = "http://bost.ocks.org/mike/sankey/energy.json"
        )$links
    ),ncol = 3, byrow = TRUE)
    nodes <- unlist(
        rjson::fromJSON(
            file = "http://bost.ocks.org/mike/sankey/energy.json"
        )$nodes
    )
    #convert to data.frame so souce and target can be character and value numeric
    links <- data.frame(links)
    colnames(links) <- c("source", "target", "value")
    links$source <- sapply(links$source, FUN = function(x) {return(as.character(nodes[x+1]))}) #x+1 since js starts at 0
    links$target <- sapply(links$target, FUN = function(x) {return(nodes[x+1])}) #x+1 since js starts at 0
    #now we finally have the data in the form we need
    sankeyPlot <- rCharts$new()
    sankeyPlot$setLib('.')
    sankeyPlot$setTemplate(script = "layouts/chart.html")
    sankeyPlot$set(
        data = links,
        nodeWidth = 15,
        nodePadding = 10,
        layout = 32,
        width = 960,
        height = 500,
        units = "TWh",
        title = "Sankey Diagram"
    )
    sankeyPlot
    
    #### Example 2 Riverplot ####
    library(riverplot)
    nodes <- c( LETTERS[1:3] )
    edges <- list( A= list( C= 10 ), B= list( C= 10 ) )
    r <- makeRiver( nodes, edges, node_xpos= c( 1,1,2 ),
                    node_labels= c( A= "Node A", B= "Node B", C= "Node C" ),
                    node_styles= list( A= list( col= "yellow" )) )
    plot( r )
    # equivalent form:
    nodes <- data.frame( ID= LETTERS[1:3],
                         x= c( 1, 1, 2 ),
                         col= c( "yellow", NA, NA ),
                         labels= c( "Node A", "Node B", "Node C" ),
                         stringsAsFactors= FALSE )
    r <- makeRiver( nodes, edges )
    plot( r )
    
    edgedf <- data.frame(N1 = c('A','B','A'), N2 = c('C','C','B') , Value = c(20,10,5))
    plot(makeRiver(nodes, edgedf, node_xpos = 0:2, node_ypos = c(0,1,-1)))
    
}

