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

input.path  <- "C:\\Users\\sean.mcfall\\Documents\\WRD\\columbiaBasin\\huc12_edit_cB_join"
output.path <- "C:\\Users\\sean.mcfall\\Documents\\WRD\\attribute-accum\\columbiaBasin_accAQI_main"
HUC.col     <- "HUC12"
ToHUC.col   <- "ToHUC"
StrLen.col  <- "STRLEN"
StrOrd.col  <- "STRORD"
AQI.col     <- "AQI"

main(input.path, output.path, HUC.col, ToHUC.col, StrLen.col, StrOrd.col, AQI.col)

main <- function(input_path, output_path, HUC_col, ToHUC_col, StrLen_col, StrOrd_col, AQI_col) {
  
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
    catch.df$StrLen <- catch.df[c(StrLen_col)]
  }
  # AQI
  if (is.null(catch.df$AQI)) {
    catch.df$AQI <- catch.df[c(AQI_col)]
  }
  # stream order
  if (is.null(catch.df$StrOrd)) {
    catch.df$StrOrd <- catch.df[c(StrOrd_col)]
  }
  # HUC
  if (is.null(catch.df$UNIT)) {
    catch.df$UNIT <- catch.df[c(HUC_col)]
  }
  # ToHUC
  if (is.null(catch.df$ToUNIT)) {
    catch.df$ToUNIT <- catch.df[c(ToHUC_col)]
  }
  
  
  #
  # 2. Tree Creation
  #
  
  print ("2 | Finding, connecting, compiling all upstreams nodes...")
  print ("NOTE: this step takes the longest")
  
  # capture hydroid's, our iterator
  hydroids <- getHydroIDs(catch.df, HUC_name = HUC_col)
  
  # create smaller df to pull from
  bare.df <- createHUCdf(catch.df, HUC_name = HUC_col, ToHuc_name = ToHUC_col)
  
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
    #print (item)
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
    df.list <- c(df.list, list((catch.df[catch.df[c(HUC_col)][[1]] %in% item,])))
  }
  
  # assign correct names
  names(df.list) <- names(megalist)
  
  #
  # 4. Calculate Accumulative AQI
  #
  
  print ("4 | Calculating accumulation stats...")
  
  accAQI.vec <- c()
  for (node in df.list) {
    
    numerator <- with(node, StrLen * AQI )#  * StrOrd)
    denominator <- node[c(StrLen_col)]
    
    accAQI <- sum(numerator) / sum(denominator)
    accAQI.vec <- c(accAQI.vec, accAQI)
  }
  
  catch.df$accAQInoStrOrd <- accAQI.vec
  
  # add accAQI column to spatial polygons data frame
  shpfile@data$accAQIno<- catch.df$accAQInoStrOrd
  
  #
  # 4. Calculate Accumulative AQI
  #
  
  print ("5 | Writing new shapfile with accumulation stats...")
  
  # writeOGR
  output.path <- output_path
  output.name <- basename(output.path)
  output.dir <- dirname(output.path)
  
  writeOGR(shpfile, dsn = output.dir, layer = output.name, driver = "ESRI Shapefile", overwrite_layer = TRUE)
}


