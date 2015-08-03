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

# iterate over hydroids to find applicable upstream chunks

# how to capture a single value
# should definitely make this a function
y <- catch.df[catch.df$NextDownID==3,]

y["HydroID"][[1]]  # capture one column's values

for (id in hydroids) {
      
  hydroidNext <- c(id)
  hydroidUsed <- c(id)
      
  # get top of stack
  top <- hydroidNext[[1]]

  # get hydroids that are upstream
  upstream <- bare.df[bare.df$NextDownID==top,]
  if (id==3) {
    
    # capture and append upstream id's
    c <- 1
    while (c < length(upstream)) {
      # capture upstream id
      upstreamAppend <- upstream["HydroID"][[c]]
      # append upstream id
      hydroidNext <- c(hydroidNext, upstreamAppend)
      # remove any repeated values
      hydroidNext <- unique(hydroidNext)
      c = c + 1
    
    # append used hydroid to Used list
    hydroidUsed <- c(hydroidUsed, top)
      
    # append used hydroid from Next list
    hydroidNext <- hydroidNext[-1]
    
    print (hydroidNext)
    }}
}









