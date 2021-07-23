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
'%!in%' <- function(x,y)!('%in%'(x,y))

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
SSpath <- "Code/data.to.process.split" # the data are already split so we don't need to split them again in this code
###################################
## TYPE IN THE WORKSPACE WHERE THE OBSERVATION SPREADSHEETS ARE STORED
#OBpath <- "Code/observation.data/"
###################################
## TYPE IN THE FULL PATH TO THE BOUNDARY SHAPEFILE
#boundary.shapefile <- paste0("Code/boundary.shapefile/",boundary.file,".shp")

###################################

#### Load reflection database ####
load(file = "Code/outputs/Dat.reflect.imputed")

# Get worksheets into data frame ------------------------------------------
mlist <- list.files(path=SSpath,full.names = T)
#olist <- list.files(path=OBpath,full.names = T)

seagreens <- c("SG2", "SG3")

# could change this to be SppDatComplete <-  foreach(p = 1:length(mlist), .combine = 'rbind)
for(p in 1:length(mlist)) {
 
mmlist <- list.files(path = mlist[p], full.names = T)

file.split <- str_split(mmlist[1], "/")[[1]][3]

for(sg in 1:2){
  
  sglist <- list.files(path = mmlist[sg], full.names = T)

survnm <- strsplit(strsplit(sglist, "/")[[1]][3], "_")[[1]][3]

## Combines the data in the 
datout <- data.table::rbindlist(lapply(sglist,function(x){
  tt <- readxl::read_xlsx(x)
  tt$Camera <- as.numeric(gsub("C", "", str_split(str_split(x, "/")[[1]][5], "_")[[1]][5]))
  # tt <- tt %>% dplyr::select(-`Identification Date`)
  if(dim(tt)[1] > 0){
  return(tt)}}),use.names=TRUE,fill=TRUE)


# Split obs data into two seagreen areas
datout <- datout[!is.na(datout$lat),]
datout$surv <- survnm

  datout$loc <- seagreens[sg]
  
  datout$Species <- tolower(datout$Species)
  
  SppDat_x <- datout %>% dplyr::filter(Species %in% SPP,)

  SppDat_x <- SppDat_x %>% rename(`Reel Ref` = `Reel Name`, `Frame Ref` = Frame, `Survey Date` = Date)
  
  MeasureDat <- SppDat_x
  
  MeasureDat$month <- months(MeasureDat$`Survey Date`)
  MeasureDat$year <- substring(MeasureDat$`Survey Date`, 1, 4)
 
########################################################################


# Fix calibration ---------------------------------------------------------

  calibration <- as.data.frame(calibration)
  
  SppDat_x$`Plane Height` <- round(SppDat_x$`Plane Height`/5)*5
  
  for(i in 1:dim(SppDat_x)[1]){
    SppDat_x$CalibrationFixed[i] <- calibration[pmatch(SppDat_x$`Plane Height`[i], calibration[,1], duplicates.ok = TRUE), SppDat_x$Camera[i]+1]
  }
  
  SppDat_x$calprop <- SppDat_x$CalibrationFixed/SppDat_x$Calibration # proportion to multiply to fix lengths 
  
  
# Get bird lengths --------------------------------------------------------

## Gets the birds lengths and puts them into a dataframe for inputting into the flight height calcs
  SppDat_x <- get.lengths(SppDat_x)

  SppDat_x <- SppDat_x[SppDat_x$lengthvals$CV < 10,]


# Calculate flight heights ------------------------------------------------
## Running this should calculate flight heights for the species' of interest
## This process can take a while depending on how many birds need to be done
  SppDat_x$month <- months(SppDat_x$`Survey Date`)
  monthdat <- months(SppDat_x$`Survey Date`)

# This is the line that estimates flight heights.
fhdata <- get.fhs(SppDat_x,New.Dat.Reflect,bootsize = bootsize)


# Saving ----------------------------------------------------------------

# Join SppDat with fhdata - this makes absolutely sure the right rows are matched together
names(fhdata) <- gsub("\\."," ", names(fhdata))
SppDat_x <- inner_join(SppDat_x, fhdata, by = c("Species", "month", "Reel Ref", "Frame Ref","Marker Number"))

file_names <- paste(file.split, seagreens[sg], sep = "_")

########

# save estimated data
writexl::write_xlsx(SppDat_x, paste0("Code/outputs/SppDat/",file_names,".xlsx"))

writexl::write_xlsx(MeasureDat, paste0("Code/outputs/MeasureDat/",file_names,".xlsx"))

}
}

