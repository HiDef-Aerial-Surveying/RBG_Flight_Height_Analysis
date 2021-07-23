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
source("Code/Grants_Random_crap.R")

# Load libraries ----------------------------------------------------------
## Uses the load_libs function which will install a package if it's not already

lib_list <- c("readxl","foreach","gridExtra","ggpubr","rayshader","viridis","raster",
              "gstat","rgdal","sp","spatialEco","HTSSIP","hablar",
              "data.table","tidyverse","boot","GPArotation","psych","cowplot")


Load_Libs(lib_list)

calibration <- readxl::read_excel("GPS Search.xlsm", sheet = "Calibration")

# Hard coded variables ----------------------------------------------------

###################################
## TYPE IN THE WORKSPACE WHERE YOUR BIRDS AT HEIGHT SPREADSHEETS ARE STORED
SSpath <- "Code/data.to.process"
###################################
## TYPE IN THE WORKSPACE WHERE THE OBSERVATION SPREADSHEETS ARE STORED
OBpath <- "Code/observation.data/"
###################################
## TYPE IN THE FULL PATH TO THE BOUNDARY SHAPEFILE
#boundary.shapefile <- paste0("Code/boundary.shapefile/",boundary.file,".shp")

###################################

#### Load reflection database ####
#load(file = "Code/outputs/Dat.reflect.imputed")
load(file = "Code/outputs/Dat.reflect")

# Get worksheets into data frame ------------------------------------------
mlist <- list.files(path=SSpath,full.names = T)
olist <- list.files(path=OBpath,full.names = T)


# could change this to be SppDatComplete <-  foreach(p = 1:length(mlist), .combine = 'rbind)
for(p in 1:length(mlist)) {
 
mmlist <- list.files(path = mlist[p], full.names = T)
oolist <- list.files(path = olist[p], full.names = T)

survnm <- strsplit(strsplit(olist[p], "/")[[1]][3], "_")[[1]][3]


if(identical(strsplit(mlist[p], "/")[[1]][3], strsplit(olist[p], "/")[[1]][3]) == FALSE) {
  stop(paste0(strsplit(mlist[p], "/")[[1]][3], " = data & ", strsplit(olist[p], "/")[[1]][3], " = obs.. The obs and data sheets aren't from the same survey! Check data matches between folders."))
}


## Combines the data in the data to process folder
datout <- data.table::rbindlist(lapply(mmlist,function(x){
  tt <- readxl::read_xlsx(x)
  tt$Camera1 <- as.numeric(gsub("C", "", str_split(str_split(x, "/")[[1]][4], "_")[[1]][5]))
  tt <- tt %>% dplyr::select(-`Identification Date`)
  return(tt)}),use.names=TRUE,fill=TRUE)


# Get observation data sheets to get Lat / Lons ---------------------------

   ## Combines the data in the observation folder
  obsout <- data.table::rbindlist(lapply(oolist,function(x){
    tt <- readxl::read_xlsx(x)
    return(tt)}),use.names=TRUE,fill=TRUE)

  #datout <- datout[,-67] # removes duplicate "Reflection" column so next line of code will work

# Split obs data into two seagreen areas
  obsout <- obsout[!is.na(obsout$Latitude),]
  obsout$lon <- obsout$Longitude
  obsout$lat <- obsout$Latitude
  obsout$surv <- survnm
  coordinates(obsout) <- ~Longitude+Latitude
  projection(obsout) <- sf::st_crs(4326)$proj4string

  obs.sub.sea2 <- obsout[Sea2,]
  obs.sub.sea3 <- obsout[Sea3,]

  obsout2 <- obs.sub.sea2@data
  obsout3 <- obs.sub.sea3@data
  
  obsout2$loc <- "SG2"
  obsout3$loc <- "SG3"
  

  datout$Species <- tolower(datout$Species)
  obsout2$Species <- tolower(obsout2$Species)
  obsout3$Species <- tolower(obsout3$Species)

# Get Lat Lons from observation spreadsheet ------------------------------------------
  
  # Unfortunately, because the data weren't clipped to only include birds within the survey area, for this project we need to use the obsout, rather than datout dataset. 
  SppDat_x <- datout %>% dplyr::filter(Species %in% SPP,)
  ObsDat2 <- obsout2 %>% dplyr::filter(Species %in% SPP,)
  ObsDat3 <- obsout3 %>% dplyr::filter(Species %in% SPP,)
  
   # account for duplicate reel and frame id in the case that there is more than one bird of the same species in a frame
  SppDat_x$`Plane Height` <- round(SppDat_x$`Plane Height`/5)*5
  SppDat_x$Camera <- ifelse(SppDat_x$Camera > 4, SppDat_x$Camera-4, SppDat_x$Camera)
  
  ObsDat2 %>% group_by(Camera, `Reel Ref`, `Frame Ref`, Species) %>% mutate(id = row_number()) %>% ungroup() -> ObsDat2
  ObsDat3 %>% group_by(Camera, `Reel Ref`, `Frame Ref`, Species) %>% mutate(id = row_number()) %>% ungroup() -> ObsDat3
  
  
  
  SppDat_x %>% group_by(Camera, `Reel Name`, Frame, Species) %>% mutate(id = row_number()) %>% 
    rename(`Reel Ref` = `Reel Name`, `Frame Ref` = Frame, `Survey Date` = Date) %>% ungroup() -> SppDat_x
  
  # join the two together
  SppDat.sg2 <- inner_join(SppDat_x, ObsDat2,
                           by = c("Survey Date", "Camera", "Reel Ref", "Frame Ref", "Species", "id")) # need to include date here because sometimes there was more than one survey date.
  
  # join the two together
  SppDat.sg3 <- inner_join(SppDat_x, ObsDat3,
                           by = c("Survey Date", "Camera", "Reel Ref", "Frame Ref", "Species", "id")) # need to include date here because sometimes there was more than one survey date.
  
  MeasureDat2 <- SppDat.sg2 %>% dplyr::filter(Species %in% SPP,) # This needs to be saved so we can compare proportion of measured birds that are estimated
  MeasureDat3 <- SppDat.sg3 %>% dplyr::filter(Species %in% SPP,) # This needs to be saved so we can compare proportion of measured birds that are estimated
  MeasureDat2$month <- months(MeasureDat2$`Survey Date`)
  MeasureDat2$year <- substring(MeasureDat2$`Survey Date`, 1, 4)

  MeasureDat3$month <- months(MeasureDat3$`Survey Date`)
  MeasureDat3$year <- substring(MeasureDat3$`Survey Date`, 1, 4)
 
########################################################################

# Fix calibration ---------------------------------------------------------
# Don't move this up the script as the Camera column is used for other things before this so might break the code
  
  # SG2
calibration <- as.data.frame(calibration)

for(i in 1:dim(SppDat.sg2)[1]){
  SppDat.sg2$CalibrationFixed[i] <- calibration[pmatch(SppDat.sg2$`Plane Height`[i], calibration[,1], duplicates.ok = TRUE), SppDat.sg2$Camera1[i]+1]
}

  SppDat.sg2$calprop <- SppDat.sg2$CalibrationFixed/SppDat.sg2$Calibration # proportion to multiply to fix lengths 
  
  
  #SG£
  for(i in 1:dim(SppDat.sg3)[1]){
    SppDat.sg3$CalibrationFixed[i] <- calibration[pmatch(SppDat.sg3$`Plane Height`[i], calibration[,1], duplicates.ok = TRUE), SppDat.sg3$Camera1[i]+1]
  }
  
  SppDat.sg3$calprop <- SppDat.sg3$CalibrationFixed/SppDat.sg3$Calibration # proportion to multiply to fix lengths 

  # Get bird lengths --------------------------------------------------------

datasets <- list(SppDat.sg2, SppDat.sg3)

lapply(datasets, function(x){

## Gets the birds lengths and puts them into a dataframe for inputting into the flight height calcs
names(x)[names(x) == "Behaviour.x"] <- "Behaviour"
x <- get.lengths(x)
#MeasureDat <- SppDat # This needs to be saved so we can compare proportion of measured birds that are estimated
x <- x[x$lengthvals$CV < 10,]


# Calculate flight heights ------------------------------------------------
## Running this should calculate flight heights for the species' of interest
## This process can take a while depending on how many birds need to be done
x$month <- months(x$`Survey Date`)
  monthdat <- months(x$`Survey Date`)

# This is the line that estimates flight heights.
fhdata <- get.fhs(x,New.Dat.Reflect,bootsize = bootsize)


# Saving ----------------------------------------------------------------

# Join SppDat with fhdata - this makes absolutely sure the right rows are matched together
names(fhdata) <- gsub("\\."," ", names(fhdata))
x <- inner_join(x, fhdata, by = c("Species", "month", "Reel Ref", "Frame Ref","Marker Number"))

file.split <- str_split(obsout$Survey, "_")[[1]]
file_names <- paste(file.split[1], file.split[2], file.split[3], file.split[5],unique(x$loc), sep = "_")

# ########

})

# save measured data
file.split <- str_split(obsout$Survey, "_")[[1]]
files_names <- paste(file.split[1], file.split[2], file.split[3], file.split[5], sep = "_")

writexl::write_xlsx(MeasureDat2, paste0("Code/outputs/MeasureDat/",files_names,"_", unique(MeasureDat2$loc),".xlsx"))
writexl::write_xlsx(MeasureDat3, paste0("Code/outputs/MeasureDat/",files_names,"_", unique(MeasureDat3$loc),".xlsx"))

}

