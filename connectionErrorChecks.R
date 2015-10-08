
# pull in data
path <- "C:\\Users\\sean.mcfall\\Documents\\WRD\\california\\huc12_point"
base <- basename(path)
dir <- dirname(path)

cBas <- readOGR(dsn = dir, layer = base)

# format as data frame
cBas.data <- as.data.frame(cBas@data)

##  HUC12 == ToHUC
# convert applicable columns to char type
cBas.data$HUC12 <- as.character(cBas.data$HUC12)  # as.numeric was truncating for w/e reason
cBas.data$ToHUC <- as.character(cBas.data$ToHUC)

cBas.data$HUCequals <- with(cBas.data, HUC12 == ToHUC)

##  length of ToHUC, should be 12 digits 
cBas.data$HUClength <- with(cBas.data, nchar(ToHUC))

## loops, if HUCs feed into each other
# takes the equal step a step further...
cBas.data$HUCinverse <- with(cBas.data, paste(ToHUC,HUC12, sep =""))
cBas.data$HUCnormal <- with(cBas.data, paste(HUC12,ToHUC, sep =""))

cBas.data$HUCinveql <- with(cBas.data, HUCnormal == HUCinverse)

## check if name has non numeric characters in it
cBas.data$HUCNumeric <- with(cBas.data, as.numeric(ToHUC))
cBas.data$HucIsNumeric <- is.numeric(cBas.data$HUCNumeric)
