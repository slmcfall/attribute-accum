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
# 0. Data Parameters, Main call
#

input.path  <- "C:\\Users\\sean.mcfall\\Documents\\California\\accumulation\\NASA\\region17"
output.path <- "C:\\Users\\sean.mcfall\\Documents\\California\\accumulation\\NASA\\region17_output"
HUC.col     <- "Hydroseq"
ToHUC.col   <- "DnHydroseq"
StrLen.col  <- "STRLEN"
StrOrd.col  <- "STRORD"
AQI.col     <- "AQI"


#
# 1. Data Prep
#

print ("1 | Loading and setting up data...")

# load shapefile
# path <- "C:\\Users\\sean.mcfall\\Documents\\WRD\\AEA\\archydro_example\\huc12_lowerBear"
shpfile <- loadShpfile(input.path)

# convert to data frame
catch.df <- as.data.frame(shpfile)

#
# 2. Tree Creation
#

print ("2 | Finding, connecting, compiling all upstreams nodes...")
print ("NOTE: this step takes the longest")

# capture hydroid's, our iterator
hydroids <- getHydroIDs(catch.df, HUC_name = HUC.col)

# create smaller df to pull from
bare.df <- createHUCdf(catch.df, HUC_name = HUC.col, ToHuc_name = ToHUC.col)

# create list of all hydroids and their adjacent hydroids
megalist <- createAdjList(hydroids, bare.df, hydro_id = HUC.col, down_id = ToHUC.col)

upstreamList <- list()
for (item in names(megalist)) {
  
  print (length(upstreamList))
  print (item)
  
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
  # debugging, where the while loop gets stuck
  #print (length(upstreamList))
  #print (item)
  
  # appending to list
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

names(allstreamList) <- names(megalist)

#
# 3. Connect Nodes to Data Frames
#

print ("3 | Attaching connected nodes to appropriate data frames...")

df.list <- list()
for (item in allstreamList) {
  print (length(df.list))
  df.list <- c(df.list, list((catch.df[catch.df[c(HUC.col)][[1]] %in% item,])))
}

# assign correct names
names(df.list) <- names(megalist)

#
# 4. Calculate Accumulative AQI
#

print ("4 | Calculating accumulation stats...")

accAQI.vec <- c()
for (node in df.list) {
  
  #node$lclWtAQI <- with(node)
  
  numerator <- with(node, fs2014 * AreaSqKM_1)
  denominator <- with(node, AreaSqKM_1)
  #denominator <- node[c(StrLen.col)]
  
  accAQI <- sum(numerator) / sum(denominator)
  accAQI.vec <- c(accAQI.vec, accAQI)
}

catch.df$fs2014_acc <- accAQI.vec

# append to shapefile dataframe
shpfile@data$fs2014_acc <- catch.df$fs2014_acc

#
# 5. Output
#

print ("5 | Writing new shapfile with accumulation stats...")

# writeOGR
output.path <- output_path
output.name <- basename(output.path)
output.dir <- dirname(output.path)

writeOGR(shpfile, dsn = output.dir, layer = output.name, driver = "ESRI Shapefile", overwrite_layer = TRUE)



