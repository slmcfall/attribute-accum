#
# slmcfall
# PURPOSE: accumulate upstream attributes
#

library(sp)
library(rgdal)
library(rgeos)

# load in catchment example
catch_path <- "C:\\Users\\sean.mcfall\\Documents\\WRD\\AEA\\archydro_example\\acc_script\\catchment"
catch_name <- basename(catch_path)
catch_dir <- dirname(catch_path)

catch <- readOGR(dsn = catch_dir, layer = catch_name)

# convert to data frame
catch.df <- as.data.frame(catch)

# capture hydroid's, our iterator
hydroids <- catch.df$HydroID
nextids <- catch.df$NextDownID

# make them into a dataframe
bare.df <- catch.df[,c("HydroID","NextDownID")]

# get a units upstream neighbors
connectNode <- function(id) {
  
  idname <- toString(id)
  nextdownids <- as.vector(bare.df[bare.df$NextDownID==id,]$HydroID)
  linkedlist <- list(nextdownids)
  names(linkedlist) <- idname
  
  return(linkedlist)
}

#
# create list of all nodes and the nodes corresponding neighbors
#
megalist <- list()
for (id in hydroids) {
  node <- connectNode(id)
  megalist <- c(megalist,node)
}

node3 <- megalist["3"]
node27 <- megalist["27"]

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
  }
  
  idname <- item
  appList <- list(finalList)
  names(appList) <- idname
  upstreamList <- c(upstreamList, appList)
}

names(upstreamList) <- names(megalist)
print (upstreamList)

#
# create data frames list with accumulative nodes of each node
#

df.list <- list()
for (item in upstreamList) {
  df.list <- c(df.list, list((catch.df[catch.df$HydroID %in% item,])))
}
# assign correct names
names(df.list) <- names(megalist)

# calculate new columns
df.27 <- df.list['27'][[1]]

df.27$mulField <- 2

df.27$resField <- with(df.27, Shape_Leng * mulField)







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

#############################









