##################################################################
#### MASTER SCRIPT for running the RGB flight height analysis ####
#### Author: Grant Humphries + Kat Keogan
#### Date: June 2021
#### Email: grant.humphries@hidefsurveying.co.uk
#### Github: https://github.com/HiDef-Aerial-Surveying/RBG_Flight_Height_Analysis
##################################################################


# Source script -----------------------------------------------------------
source("Code/helper.scripts/Bootstrap_method_helpers_KK.R")
source("Code/config.R")
#devtools::install_github("https://github.com/HiDef-Aerial-Surveying/HiDEF.git")
library(HiDef)
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
# List of species to be used in analysis
SPP <- tolower(c("kittiwake", "herring gull", "gannet", "lesser black-backed gull", "great black-backed gull")) # need to be in lower case

###################################
## TYPE IN THE WORKSPACE WHERE YOUR BIRDS AT HEIGHT SPREADSHEETS ARE STORED
## For Gallopers they are stored in the 800 folder, inside the appropriate Zone number.
SSfol <- "R:\\Projects\\HP00000 - Projects\\HP00099 - Innogy Galloper Post-construction Monitoring\\800 - Completed Flight Height Sheets" # change this path for each Galloper

Zone <- "Zone74" # and also change this for each Galloper

SSpath <- paste(SSfol, Zone, sep = "\\") # make sure the zone is correct, with no spaces
###################################
## TYPE IN THE WORKSPACE WHERE THE OBSERVATION SPREADSHEETS ARE STORED
# This will only be needed where data have not been split into three areas, as will already have lat/lon added
OBpath <- "Code/observation.data/"
###################################
## CHOOSE WHICH BOUNDARY SHAPEFILE YOU WANT (IN CONFIG.R)
boundary.shapefile <- boundary.shapefile.gpcm

###################################

# Load reflection database -----------------------------------------------
# 09/06/21 - this is the reflection database used for Seagreen FH modelling. No changes made yet based on birds potentially being incorrectly measured.
load(file = "Code/outputs/Dat.reflect")


# Get worksheets into data frame ------------------------------------------
mlist <- list.files(path = SSpath, full.names = TRUE)

# Loop through each folder to create database -----------------------------

for(p in 1:length(mlist)) {

# might need to put this in a loop but for now it's outside of a loop
mmlist <- list.files(path = mlist[p], full.names = T)

file.split <- str_split(mmlist[1], "/")[[1]][2] # extract name of folder so we can save using this name

## Combines the data in the folder into one dataframe
datout <- data.table::rbindlist(lapply(mmlist,function(x){
  tt <- readxl::read_xlsx(x)
  tt$Camera <- as.numeric(substring(grep(unlist(strsplit(x,"_")),
               pattern="C\\d",value = T), 2))
  tt <- tt %>% dplyr::select(-`Identification Date`)
  if(dim(tt)[1] > 0){
    return(tt)}}),use.names=TRUE,fill=TRUE)

# Get bird lengths --------------------------------------------------------

  datout <- datout[!is.na(datout$Latitude),]
  datout$Species <- tolower(datout$Species)
  
  if("Behaviour.x" %in% colnames(datout)){
  datout <- datout %>% rename("Behaviour" = "Behaviour.x")
  }

  SppDat_x <- datout %>% dplyr::filter(Species %in% SPP)

  #SppDat_x <- SppDat_x %>% rename(`Reel Ref` = `Reel Name`, `Frame Ref` = Frame, `Survey Date` = Date)

  MeasureDat <- SppDat_x # We save this here because we'll need to know how many birds were measured versus how many FH was estimated for.

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
  #SppDat_x[,65:71] <- na_if(SppDat_x[,65:71], "NA")
  
## Gets the birds lengths and puts them into a dataframe for inputting into the flight height calcs

  SppDat_x <- get.lengths(SppDat_x)
  
  SppDat_x <- SppDat_x[SppDat_x$lengthvals$CV < 10,]


# Calculate flight heights ------------------------------------------------
## Running this should calculate flight heights for the species' of interest
## This process can take a while depending on how many birds need to be done
  SppDat_x$month <- months(SppDat_x$`Survey Date`)
  monthdat <- months(SppDat_x$`Survey Date`)
   
  if(nrow(SppDat_x) > 0){
# This is the line that estimates flight heights. 
  fhdata <- get.fhs(SppDat_x,Dat.reflect,bootsize = bootsize)

# Saving ----------------------------------------------------------------

# Join SppDat with fhdata - this makes absolutely sure the right rows are matched together
  names(fhdata) <- gsub("\\."," ", names(fhdata))
  SppDat_x <- inner_join(SppDat_x, fhdata, by = c("Species", "month", "Reel Ref", "Frame Ref","Marker Number"))
  }

  ########
  dir.create(paste0(SSfol, "/", Zone, "_FHest"), showWarnings = FALSE)
  dir.create(paste0(SSfol, "/", Zone, "_Measure"), showWarnings = FALSE)
  # save estimated data
  writexl::write_xlsx(SppDat_x, paste0(SSfol, "/", Zone, "_FHest/",file.split,".xlsx"))
  
  writexl::write_xlsx(MeasureDat, paste0(SSfol, "/", Zone, "_Measure/",file.split,".xlsx"))

}
