



loadShpfile <- function(shapefile_path) {
  
  catch_path <- shapefile_path
  catch_name <- basename(catch_path)
  catch_dir <- dirname(catch_path)
  
  catch <- readOGR(dsn = catch_dir, layer = catch_name)
  
  return(catch)
}


getHydroIDs <- function(shpfile_df, HUC_name) {
  hydroIDs <- shpfile_df[,c(HUC_name)]
  return(hydroIDs)
}

createHUCdf <- function(shpfile_df, HUC_name, ToHuc_name) {
  bare.df <- shpfile_df[,c(HUC_name,ToHuc_name)]
  return(bare.df)
}

createAdjList <- function(hydroIDs_list, bare_df) {
  adj.list <- list()
  for (id in hydroIDs_list) {
    node <- connectNode(id, bare_df)
    adj.list <- c(adj.list,node)
  }
  return(adj.list)
}

connectNode <- function(id, bare_df) {
  
  idname <- toString(id)
  nextdownids <- as.vector(bare_df[bare_df$DnHydroseq==id,]$Hydroseq)
  linkedlist <- list(nextdownids)
  names(linkedlist) <- idname
  
  return(linkedlist)
}

findLongestDf <- function(df_list) {
  first.length <- length(df_list[[1]][[1]])
  df.max <- 0
  for (df in df_list) {
    length.df <- (length(df[[1]]))
    if (length.df > first.length) {
      first.length <- length.df
      df.max <- df
    }
  }
  return (df.max)
}

findLongestDf <- function(df_list) {
  # get first data frame in list of data frames
  first.df <- df_list[[1]]
  # get the number of rows in the first data frame
  first.length <- length(first.df[[1]])
  # set the highest number df as the first
  df.max <- first.df
  # determine the df with the most rows
  for (df in df_list) {
    length.df <- (length(df[[1]]))
    if (length.df > first.length) {
      first.length <- length.df
      df.max <- df
    }
  }
  return (df.max)
}


input.path  <- "C:\\Users\\sean.mcfall\\Documents\\WRD\\AEA\\archydro_example\\huc12_lowerBear"
output.path <- "C:\\Users\\sean.mcfall\\Documents\\WRD\\attribute-accum\\johnDay_accAQI"
HUC.col     <- "HUC12"
ToHUC.col   <- "ToHUC"
StrLen.col  <- "StrLen"
StrOrd.col  <- "StrOrd"
AQI.col     <- "AQI_1"

#main2(input.path, output.path, HUC.col, ToHUC.col, StrLen.col, StrOrd.col, AQI.col)

main2 <- function(input_path, output_path, HUC_col, ToHUC_col, StrLen_col, StrOrd_col, AQI_col) {
  
  #
  # 1. Data Prep
  #
  print ("Data prep...")
  
  # load shapefile
  path <- input_path
  shpfile <- loadShpfile(path)
  
  # convert to data frame
  catch.df <- as.data.frame(shpfile)
  
  # unique identifier
  catch.df$ID <- 1:nrow(catch.df)
  # streamLeng
  catch.df$StrLen <- catch.df[c(StrLen_col)]
  # AQI
  catch.df$AQI <- catch.df[c(AQI_col)]
  # stream order
  catch.df$StrOrd <- catch.df[c(StrOrd_col)]
  
  #
  # 2. Tree Creation
  #
  
  # capture hydroid's, our iterator
  hydroids <- getHydroIDs(catch.df,HUC_col)
  
  # create smaller df to pull from
  bare.df <- createHUCdf(catch.df, HUC_col, ToHUC_col)
  
  # create list of all hydroids and their adjacent hydroids
  megalist <- createAdjList(hydroids, HUC_col, ToHUC_col)
  
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
  
  names(allstreamList) <- names(megalist)
  
  #
  # 3. Connect Nodes to Data Frames
  #
  
  df.list <- list()
  for (item in allstreamList) {
    df.list <- c(df.list, list((catch.df[catch.df[c(HUC_col)][[1]] %in% item,])))
  }
  
  # assign correct names
  names(df.list) <- names(megalist)
  
  #
  # 4. Calculate Accumulative AQI
  #
  
  accAQI.vec <- c()
  for (node in df.list) {
    
    numerator <- with(node, StrLen * AQI * StrOrd)
    denominator <- node[c(StrLen_col)]
    
    accAQI <- sum(numerator) / sum(denominator)
    accAQI.vec <- c(accAQI.vec, accAQI)
  }
  
  catch.df$accAQI <- accAQI.vec
  
  # add accAQI column to spatial polygons data frame
  shpfile@data$accAQI <- catch.df$accAQI
  
  # writeOGR
  output.path <- output_path
  output.name <- basename(output.path)
  output.dir <- dirname(output.path)
  
  writeOGR(shpfile, dsn = output.dir, layer = output.name, driver = "ESRI Shapefile", overwrite_layer = TRUE)
  
  }




