

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

createAdjList <- function(hydroIDs_list) {
  adj.list <- list()
  for (id in hydroIDs_list) {
    node <- connectNode(id)
    adj.list <- c(adj.list,node)
  }
  return(adj.list)
}

connectNode <- function(id) {
  
  idname <- toString(id)
  nextdownids <- as.vector(bare.df[bare.df$ToHUC==id,]$HUC12)
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

main <- function(shpfile_path, HUC_col, ToHuc_col, Maj_col, StrmLen_col, AQI_col) {
  
  #
  # 1. Data Prep
  #
  
  # load shapefile
  path <- shpfile_path
  shpfile <- loadShpfile(path)
  
  # convert to data frame
  catch.df <- as.data.frame(shpfile)
  
  # MAJSTAT STUFF
  # create list of majstat polygons to iterate over
  majstat.df <- catch.df[c(Maj_col)]
  # get majstat list
  majstat.list <- as.list(majstat.df)[[1]]
  # remove repeated values
  majstat.unique <- unique(majstat.list)
  # remove null majstat groups
  majstat.unique <- majstat.unique[majstat.unique != 0]
  
  accAQI.vec <- c()
  majstat.vec <- c()
  
  for (majstat in majstat.unique){
    
    # get one majority stats group
    catch.df.filter <- catch.df[catch.df[c(Maj_col)]==majstat,]
    nrows.filter <- nrow(catch.df.filter)
    
    if (nrows.filter > 1) {
      # add majstat category to vector list for final data frame
      majstat.vec <- c(majstat.vec, majstat)
      
      # capture hydroid's, our iterator
      hydroids <- getHydroIDs(catch.df.filter,HUC_col)
      
      # create smaller df to pull from
      bare.df <- createHUCdf(catch.df.filter, HUC_col, ToHuc_col)
      
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
      
      #end.node <- findLongestDf(df.list)
      end.node <- df.max
      
      # !!! SUM is stream length in 30m cells!
      # ...which means any diagonally connected cells aren't 1*30m but sqrt(2)*30m
      # intermediate calculation for attribute accumulation
      end.node$StreamLength <- end.node[c(StrmLen_col)]
      end.node$AQI <- end.node[c(AQI_col)]
      end.node$AQILength <- with(end.node, StreamLength * AQI)
      
      accAQI <- sum(end.node$AQILength) / sum(end.node$StreamLength)
      
      accAQI.vec <- c(accAQI.vec,accAQI)
      
    }  # number of rows if statment
  }  # majstat for loop
  
  accAQI.df <- data.frame(majstat.vec,accAQI.vec)
  names(accAQI.df) <- c(Maj_col,"accAQI")
  
  catch.df2 <- catch.df
  
  final.df <- merge(catch.df2, accAQI.df, by=Maj_col)
  
  return(final.df)
}




