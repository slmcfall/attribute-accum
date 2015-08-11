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
majstat.list <- unique(as.list(majstat.df)[[1]])

aqi.list <- list()

#might need to remove zeroes, as they may be nulls

# get one majority stats group
catch.df.filter <- catch.df[catch.df$MAJORITY==1,]

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
      neighborNodes <- megalist[toString(node)][[1]]
      # remove node from nextList
      finalList <- c(finalList, nextList[1])
      nextList <- nextList[-1]
      if (length(neighborNodes) != 0) {
        nextList <- c(nextList,neighborNodes)
      }
    }
    print (length(nextList))
  }
  
  idname <- item
  appList <- list(finalList)
  names(appList) <- idname
  upstreamList <- c(upstreamList, appList)
}

names(upstreamList) <- names(megalist)

#
# 3. Connect Nodes to Data Frames
#

df.list <- list()
for (item in upstreamList) {
  df.list <- c(df.list, list((catch.df.filter[catch.df.filter$HUC12 %in% item,])))
}

# assign correct names
names(df.list) <- names(megalist)

#
# 4. Calculate Accumulative AQI
#

# find longest data frame in list of data frames
# this is the node that all nodes feed into, ostensibly
endNode <- findLongestDf(df.list)
#endNode <- df.list$`160101020303`


# !!! SUM is stream length in 30m cells!
# ...which means any diagonally connected cells aren't 1*30m but sqrt(2)*30m
# intermediate calculation for attribute accumulation
endNode$StreamLength <- endNode[c('SUM')]
endNode$AQI <- endNode[c('MEAN')]
endNode$AQILength <- with(endNode, StreamLength * AQI)

accAQI <- sum(endNode$AQILength) / sum(endNode$StreamLength)

print(accAQI)


############################# SNIPPETS

# how to capture a single value
# should definitely make this a function
y <- catch.df[catch.df$NextDownID==3,]

y["HydroID"][[1]]  # capture one column's values


id <- 27
nextdownids <- bare.df[bare.df$NextDownID==id,]
ndiList <- as.list(nextdownids)

# dealing with nodes that have no neighbors
for (thing in megalist) {
  if (length(thing) != 0) {
    print (thing)
  }
}


catch.df[catch.df$HydroID %in% node5[[1]],]

# creates vector in column format 
hello.df <- ifelse(catch.df$HydroID>5, TRUE, FALSE)

# calculate new columns
df.27 <- df.list['27'][[1]]

df.27$mulField <- 2

df.27$resField <- with(df.27, Shape_Leng * mulField)



node3 <- megalist["3"]
node27 <- megalist["27"]

#############################









