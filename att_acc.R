#
# slmcfall
# PURPOSE: accumulate upstream attributes
#

# LIBRARIES
library(sp)
library(rgdal)
library(rgeos)

# FUNCTIONS
functions_path <- "C:\\Users\\sean.mcfall\\Documents\\WRD\\attribute-accum\\functions.R"
source(functions_path)

  ###          ###
 # Begin script #
###          ###

#
# 1. Data Prep
#

# load shapefile
path <- "C:\\Users\\sean.mcfall\\Documents\\WRD\\AEA\\archydro_example\\acc_script\\huc12_upperBear_join"
shpfile <- loadShpfile(path)

# convert to data frame
catch.df <- as.data.frame(shpfile)

# MAJSTAT STUFF
# create list of majstat polygons to iterate over
majstat.df <- catch.df[c("MAJORITY")]
# remove repeated majstat groups
majstat.list <- unique(as.list(majstat.df)[[2]])
# remove null majstat groups
majstat.list <- majstat.list[majstat.list != 0]

aqi.list <- list()

for (majstat in majstat.list){

  # get one majority stats group
  catch.df.filter <- catch.df[catch.df$MAJORITY==majstat,]
  nrows.filter <- nrow(catch.df.filter)
  
  
  if (nrows.filter > 1) {
    # capture hydroid's, our iterator
    hydroids <- getHydroIDs(catch.df.filter,"HUC12")
    
    # create smaller df to pull from
    bare.df <- createHUCdf(catch.df.filter, "HUC12", "ToHUC")
    
    #
    # 2. Tree Creation
    #
    
    # create list of all hydroids and their adjacent hydroids
    megalist <- createAdjList(hydroids)
    
    upstreamList <- list()
    for (item in names(megalist)) {
    
      finalList <- c()
      nextList  <- c()
      adjNodes  <- megalist[item][[1]]
      
      nextList <- c(nextList, adjNodes)
  
      while (length(nextList) != 0) {
        for (node in nextList) {
          #browser()
          neighborNodes <- megalist[toString(node)][[1]]
          # remove node from nextList
          finalList <- c(finalList, nextList[1])
          nextList <- nextList[-1]
          
          if (length(neighborNodes) != 0) {
            nextList <- c(nextList,neighborNodes)
            
          }
        }
      }
      
      idname <- item
      appList <- list(finalList)
      names(appList) <- idname
      upstreamList <- c(upstreamList, appList)
    }
    
    names(upstreamList) <- names(megalist)
    
    # add in original nodes for each node in upstreamList
    allstreamList <- list()
    index <- 1
    for (node in upstreamList) {
      origNode <- (names(upstreamList)[index])
      node <- list(c(node, origNode))
      allstreamList <- c(allstreamList,node)
      index <- index + 1
    }
    
    #
    # 3. Connect Nodes to Data Frames
    #
    
    df.list <- list()
    for (item in allstreamList) {
      df.list <- c(df.list, list((catch.df.filter[catch.df.filter$HUC12 %in% item,])))
    }
    
    # assign correct names
    names(df.list) <- names(megalist)
    
    #
    # 4. Calculate Accumulative AQI
    #
    
    # find longest data frame in list of data frames
    # this is the node that all nodes feed into, ostensibly
    df.max <- findLongestDf(df.list)
    
    #endNode <- findLongestDf(df.list)
    endNode <- df.max
    
    # !!! SUM is stream length in 30m cells!
    # ...which means any diagonally connected cells aren't 1*30m but sqrt(2)*30m
    # intermediate calculation for attribute accumulation
    endNode$StreamLength <- endNode[c('SUM')]
    endNode$AQI <- endNode[c('MEAN')]
    endNode$AQILength <- with(endNode, StreamLength * AQI)
    
    accAQI <- sum(endNode$AQILength) / sum(endNode$StreamLength)
    
    print(accAQI)
  
  }  # number of rows if statment
}  # majstat for loop
