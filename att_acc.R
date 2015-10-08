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

input.path  <- "C:\\Users\\sean.mcfall\\Documents\\WRD\\umpqua\\huc12_umpqua"
output.path <- "C:\\Users\\sean.mcfall\\Documents\\WRD\\umpqua\\huc12_umpqua_SD"
HUC.col     <- "HUC12"
ToHUC.col   <- "ToHUC"
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

# column creation

# unique identifier
catch.df$ID <- 1:nrow(catch.df)
# stream length
if (is.null(catch.df$StrLen)) {
  catch.df$StrLen <- catch.df[c(StrLen.col)]
}
# AQI
if (is.null(catch.df$AQI)) {
  catch.df$AQI <- catch.df[c(AQI.col)]
}
# stream order
if (is.null(catch.df$StrOrd)) {
  catch.df$StrOrd <- catch.df[c(StrOrd.col)]
}
# HUC
if (is.null(catch.df$UNIT)) {
  catch.df$UNIT <- catch.df[c(HUC.col)]
}
# ToHUC
if (is.null(catch.df$ToUNIT)) {
  catch.df$ToUNIT <- catch.df[c(ToHUC.col)]
}

#bare.df$Zonation_1 <- as.character(bare.df$Zonation_1)
#bare.df$Zonation_I <- as.character(bare.df$Zonation_I)

#bare.df$Zonation_1[bare.df$Zonation_1 == "-1"] <- "TERMINUS"

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
megalist <- createAdjList(hydroids, bare.df)

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
  # debugging, where the while loop gets stuck
  print (length(upstreamList))
  print (item)
  
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
  
  numerator <- with(node, StrLen * AQI)
  denominator <- node[c(StrLen.col)]
  
  accAQI <- sum(numerator) / sum(denominator)
  accAQI.vec <- c(accAQI.vec, accAQI)
}

catch.df$accAQI <- accAQI.vec
catch.df$AQIdiff <- with(catch.df, AQI - accAQI)

# add accAQI column to spatial polygons data frame
shpfile@data$accAQIno<- catch.df$accAQI

shpfile@data$AQIdiff<- catch.df$AQIdiff

#
# standard deviation experiment
#

AQI.mean           <- mean(as.vector(catch.df$AQI), na.rm = TRUE)
AQI.one.and.qtr.sd <- AQI.mean + 1.05*sd(as.vector(catch.df$AQI), na.rm = TRUE)
AQI.qtr.sd         <- AQI.mean + 0.25*sd(as.vector(catch.df$AQI), na.rm = TRUE)
AQI.neg.qtr.sd     <- AQI.mean - 0.25*sd(as.vector(catch.df$AQI), na.rm = TRUE)
AQI.three.qtr.sd   <- AQI.mean + 0.65*sd(as.vector(catch.df$AQI), na.rm = TRUE)

accAQI.mean           <- mean(as.vector(catch.df$accAQIno), na.rm = TRUE)
accAQI.one.and.qtr.sd <- accAQI.mean + 1.05*sd(as.vector(catch.df$accAQIno), na.rm = TRUE)
accAQI.qtr.sd         <- accAQI.mean + 0.25*sd(as.vector(catch.df$accAQIno), na.rm = TRUE)
accAQI.neg.qtr.sd     <- accAQI.mean - 0.25*sd(as.vector(catch.df$accAQIno), na.rm = TRUE)
accAQI.three.qtr.sd   <- accAQI.mean + 0.65*sd(as.vector(catch.df$accAQIno), na.rm = TRUE)

getAccAQIValue <- function(accAQI_val) {
  if (is.nan(accAQI_val)) {
    return (0)
  } else if (accAQI_val > accAQI.one.and.qtr.sd) {
    return(4)
  } else if (accAQI_val > accAQI.three.qtr.sd) {
    return(3)
  } else if (accAQI_val > accAQI.qtr.sd){
    return(2)
  } else if ((accAQI_val > accAQI.neg.qtr.sd)&(accAQI_val < accAQI.qtr.sd)) {
    return(1)
  } else {
    return(0)
  }
}

getAQIValue <- function(AQI_val) {
  if (AQI_val > AQI.one.and.qtr.sd) {
    return(4)
  } else if (AQI_val > AQI.three.qtr.sd) {
    return(3)
  } else if (AQI_val > AQI.qtr.sd){
    return(2)
  } else if ((AQI_val > AQI.neg.qtr.sd)&(AQI_val < AQI.qtr.sd)) {
    return(1)
  } else {
    return(0)
  }
}

# local AQI
AQI.vec <- c()
for (node in catch.df$AQI) {
  AQI.vec <- c(AQI.vec, (getAQIValue(node)))
}
catch.df$AQIsd <- AQI.vec
catch.df$AQIsd <- as.character(catch.df$AQIsd)

# accumulative AQI
accAQIval.vec <- c()
for (node in catch.df$accAQI) {
  accAQIval.vec <- c(accAQIval.vec, (getAccAQIValue(node)))
}
catch.df$accAQIsd <- accAQIval.vec
catch.df$accAQIsd <- as.character(catch.df$accAQIsd)

# combine the two
catch.df$SD <- with(catch.df, paste(AQIsd, accAQIsd, sep = ""  ))

SD.vec <- as.vector(catch.df$SD)

# spread A -1 -.5 mean .5 1
spread.A <- catch.df$SD
barplot(table(spread.A), main = "spread I")

# spread B -.25|.25 .75 1.25
spread.B <- catch.df$SD
barplot(table(spread.B))

# append to shapefile dataframe
shpfile@data$SD <- catch.df$SD
#
# 4. Calculate Accumulative AQI
#

print ("5 | Writing new shapfile with accumulation stats...")

# writeOGR
output.path <- output_path
output.name <- basename(output.path)
output.dir <- dirname(output.path)

writeOGR(shpfile, dsn = output.dir, layer = output.name, driver = "ESRI Shapefile", overwrite_layer = TRUE)



