

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