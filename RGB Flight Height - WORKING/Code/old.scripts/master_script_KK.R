##################################################################
#### MASTER SCRIPT for running the RGB flight height analysis ####
#### Author: Grant Humphries
#### Date: December 9, 2020
#### Email: grant.humphries@hidefsurveying.co.uk
#### Github: https://github.com/HiDef-Aerial-Surveying/RBG_Flight_Height_Analysis
##################################################################


# Source script -----------------------------------------------------------
source("Code/helper.scripts/Bootstrap_method_helpers_KK.R")
source("Code/config.R")

# Load libraries ----------------------------------------------------------
## Uses the load_libs function which will install a package if it's not already

lib_list <- c("readxl","foreach","gridExtra","ggpubr","rayshader","viridis","raster",
              "gstat","rgdal","sp","spatialEco","HTSSIP","hablar",
              "data.table","tidyverse","boot","GPArotation","psych","cowplot")


Load_Libs(lib_list)

# Hard coded variables ----------------------------------------------------

###################################
## TYPE IN THE WORKSPACE WHERE YOUR BIRDS AT HEIGHT SPREADSHEETS ARE STORED
SSpath <- "Code/data.to.process"
###################################
## TYPE IN THE WORKSPACE WHERE THE OBSERVATION SPREADSHEETS ARE STORED
OBpath <- "Code/observation.data/"
###################################
## TYPE IN THE FULL PATH TO THE BOUNDARY SHAPEFILE
boundary.shapefile <- paste0("Code/boundary.shapefile/",boundary.file,".shp")

###################################

#### Load reflection database ####
load(file = "Code/outputs/Dat.reflect")

# Get worksheets into data frame ------------------------------------------
mlist <- list.files(path=SSpath,full.names = T)

# could change this to be SppDatComplete <-  foreach(p = 1:length(mlist), .combine = 'rbind)
for(p in 1:length(mlist)) {
  
mmlist <- list.files(path = mlist[p], full.names = T)


## Combines the data in the 
datout <- data.table::rbindlist(lapply(mmlist,function(x){
  tt <- readxl::read_xlsx(x)
  tt <- tt %>% dplyr::select(-`Identification Date`)
  return(tt)}),use.names=TRUE,fill=TRUE)

names(datout)[29] <- "Reflection"
unique(datout$Reflection)

# Get observation data sheets to get Lat / Lons ---------------------------

  olist <- list.files(path=OBpath,full.names = T)

  ## Combines the data in the
  obsout <- data.table::rbindlist(lapply(olist,function(x){
    tt <- readxl::read_xlsx(x)
    return(tt)}),use.names=TRUE,fill=TRUE)

  #datout <- datout[,-67] # removes duplicate "Reflection" column so next line of code will work

  datout$Species <- tolower(datout$Species)
  obsout$Species <- tolower(obsout$Species)

# Get Lat Lons from observation spreadsheet ------------------------------------------
MeasureDat <- obsout %>% dplyr::filter(Species %in% SPP,) # This needs to be saved so we can compare proportion of measured birds that are estimated
  # Unfortunately, because the data weren't clipped to only include birds within the survey area, for this project we need to use the obsout, rather than datout dataset. 
SppDat_x <- datout %>% dplyr::filter(Species %in% SPP,)
ObsDat <- obsout %>% dplyr::filter(Species %in% SPP,)

# account for duplicate reel and frame id in the case that there is more than one bird of the same species in a frame
SppDat_x$Camera <- ifelse(SppDat_x$Camera > 4, SppDat_x$Camera-4, SppDat_x$Camera)

ObsDat %>% group_by(Camera, `Reel Ref`, `Frame Ref`, Species) %>% mutate(id = row_number()) %>% ungroup() -> ObsDat

SppDat_x %>% group_by(Camera, `Reel Name`, Frame, Species) %>% mutate(id = row_number()) %>% 
  rename(`Reel Ref` = `Reel Name`, `Frame Ref` = Frame, `Survey Date` = Date) %>% ungroup() -> SppDat_x

# join the two together
SppDat <- inner_join(SppDat_x, ObsDat,
                          by = c("Survey Date", "Camera", "Reel Ref", "Frame Ref", "Species", "id")) # need to include date here because sometimes there was more than one survey date.

##################################################

#1) Write a script for spatial clip BEFORE FH measurements. (lines 66 - 112)
# Output spreadsheets needs to emulate input spreadsheet (columns)

#2) Summarise the number of birds / species in each new (clipped) spreadsheet

#3) Feed those data back to FH peeps, Ruth to assess approximate time.

#4) From completed data, assess differences between PCH clipped and unclipped

########################################################################

# Get bird lengths --------------------------------------------------------

## Gets the birds lengths and puts them into a dataframe for inputting into the flight height calcs
names(SppDat)[names(SppDat) == "Behaviour.x"] <- "Behaviour"
SppDat <- get.lengths(SppDat)
MeasureDat <- SppDat # This needs to be saved so we can compare proportion of measured birds that are estimated
SppDat <- SppDat[SppDat$lengthvals$CV < 10,]


# Calculate flight heights ------------------------------------------------
## Running this should calculate flight heights for the species' of interest
## This process can take a while depending on how many birds need to be done
MeasureDat$month <- months(MeasureDat$`Survey Date`)
MeasureDat$year <- substring(MeasureDat$`Survey Date`, 1, 4)
SppDat$month <- months(SppDat$`Survey Date`)
monthdat <- months(SppDat$`Survey Date`)

# This is the line that estimates flight heights. 
fhdata <- get.fhs(SppDat,Dat.reflect,bootsize = bootsize)


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
