##################################################################
#### MASTER SCRIPT for running the RGB flight height analysis ####
#### Author: Grant Humphries
#### Date: December 9, 2020
#### Email: grant.humphries@hidefsurveying.co.uk
#### Github: https://github.com/HiDef-Aerial-Surveying/RBG_Flight_Height_Analysis
##################################################################


# Source script -----------------------------------------------------------
source("Code/helper.scripts/Bootstrap_method_helpers_KK.R")


# Load libraries ----------------------------------------------------------
## Uses the load_libs function which will install a package if it's not already

lib_list <- c("readxl","foreach","gridExtra","ggpubr","rayshader","viridis","raster",
              "gstat","rgdal","sp","spatialEco","HTSSIP","hablar",
              "data.table","tidyverse","boot","GPArotation","psych","cowplot")


Load_Libs(lib_list)

# Hard coded variables ----------------------------------------------------

###################################
# List of species to be used in analysis
SPP <- c("kittiwake", "herring gull", "gannet", "lesser black-backed gull", "great black-backed gull") # need to be in lower case

boundary.file <- "Galloper_Extension_Joined_4kmbuffer_WGS84"

###################################
## TYPE IN THE WORKSPACE WHERE YOUR BIRDS AT HEIGHT SPREADSHEETS ARE STORED
SSpath <- "Code/data.to.process/Zone90"
###################################
## TYPE IN THE WORKSPACE WHERE THE OBSERVATION SPREADSHEETS ARE STORED
OBpath <- "Code/observation.data/"
###################################
## TYPE IN THE FULL PATH TO THE BOUNDARY SHAPEFILE
boundary.shapefile <- paste0("Code/boundary.shapefile/",boundary.file,".shp")

###################################

WGS84 <- sf::st_crs(4326)$proj4string

# Load reflection database -----------------------------------------------

load(file = "Code/outputs/Dat.reflect")


# Get worksheets into data frame ------------------------------------------
mlist <- list.files(path=SSpath,full.names = T)

# could change this to be SppDatComplete <-  foreach(p = 1:length(mlist), .combine = 'rbind)
for(p in 1:length(mlist)) {

# might need to put this in a loop but for now it's outside of a loop
mmlist <- list.files(path = mlist[p], full.names = T)

## Combines the data in the 
datout <- data.table::rbindlist(lapply(mmlist,function(x){
  tt <- readxl::read_xlsx(x)
  tt <- tt %>% dplyr::select(-`Identification Date`)
  return(tt)}),use.names=TRUE,fill=TRUE)

names(datout)[29] <- "Reflection"
unique(datout$Reflection)


# Get bird lengths --------------------------------------------------------


## Gets the birds lengths and puts them into a dataframe for inputting into the flight height calcs

MeasureDat <- datout %>% dplyr::filter(Species %in% SPP,) # This needs to be saved so we can compare proportion of measured birds that are estimated
SppDat <- datout %>% dplyr::filter(Species %in% SPP,)
names(SppDat)[names(SppDat) == "Behaviour.x"] <- "Behaviour"
SppDat <- get.lengths(SppDat)
SppDat <- SppDat[SppDat$lengthvals$CV < 10,]


# Calculate flight heights ------------------------------------------------
## Running this should calculate flight heights for the species' of interest
## This process can take a while depending on how many birds need to be done
MeasureDat$month <- months(MeasureDat$`Survey Date`)
MeasureDat$year <- substring(MeasureDat$`Survey Date`, 1, 4)
SppDat$month <- months(SppDat$`Survey Date`)
monthdat <- months(SppDat$`Survey Date`)


# This is the line that estimates flight heights. 
fhdata <- get.fhs(SppDat,Dat.reflect,bootsize = 50) # this isn't working at the moment

# Saving ----------------------------------------------------------------

# Join SppDat with fhdata - this makes absolutely sure the right rows are matched together
names(fhdata) <- gsub("\\."," ", names(fhdata))
SppDat <- inner_join(SppDat, fhdata, by = c("Species", "month", "Reel Ref", "Frame Ref","Marker Number"))


########
# save measured data
writexl::write_xlsx(MeasureDat, paste0("Code/outputs/MeasureDat/MeasureDat",unique(MeasureDat$month),"_",unique(MeasureDat$year),".xlsx"))
# save estimated data
writexl::write_xlsx(SppDat, paste0("Code/outputs/SppDat/SppDat",unique(SppDat$month),"_",unique(SppDat$year),".xlsx"))
# save fhdata
write.table(fhdata, "Code/outputs/fhdata/fhdatacomplete.txt", row.names =  FALSE, col.names = FALSE, append = TRUE)

}
