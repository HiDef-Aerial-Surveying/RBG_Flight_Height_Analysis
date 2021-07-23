##################################################################
#### MASTER SCRIPT for clipping data to windfarm footprint ####
#### Author: Grant Humphries
#### Date: December 9, 2020
#### Email: grant.humphries@hidefsurveying.co.uk
#### Github: https://github.com/HiDef-Aerial-Surveying/RBG_Flight_Height_Analysis
##################################################################


# Source script -----------------------------------------------------------
devtools::install_github("https://github.com/HiDef-Aerial-Surveying/HiDEF.git")
library(HiDef)

# Load libraries ----------------------------------------------------------
## Uses the load_libs function which will install a package if it's not already

lib_list <- c("readxl","raster","sf",
              "sp","dplyr", "stringr",
              "writexl")


Load_Libs(lib_list)

# Hard coded variables ----------------------------------------------------

###################################
## TYPE IN THE WORKSPACE WHERE YOUR BIRDS AT HEIGHT SPREADSHEETS ARE STORED
SSpath <- "R:\\Projects\\HP00000 - Projects\\HP00102 - SSE North Irish Sea (Braymore Point)\\500 - Data Processing\\2021 - Month 02 - Survey 01\\Zone110_M02_S01_D01_21"
###################################
## TYPE IN THE WORKSPACE WHERE THE OBSERVATION SPREADSHEETS ARE STORED
# It's important that only the observation sheet(s) you need are in this folder, if anything else is there this won't work.

OBpath <- "R:\\Projects\\HP00000 - Projects\\HP00102 - SSE North Irish Sea (Braymore Point)\\500 - Data Processing\\2021 - Month 02 - Survey 01\\Output\\Clipping Obs" # made the unzipped for clipping folder to put observation sheets
###################################
## TYPE IN THE FULL PATH TO THE BOUNDARY SHAPEFILE
boundary.shapefile <- raster::shapefile("R:\\Projects\\HP00000 - Projects\\HP00102 - SSE North Irish Sea (Braymore Point)\\100 - Project Control\\Survey Design\\Updated shapefiles\\BRA_RevisedSurveyArea_UTM30N_SSE_20200811.shp")

boundary.shapefile <- spTransform(boundary.shapefile, sf::st_crs(4326)$proj4string)

###################################

# Get observation data sheets to get Lat / Lons ---------------------------

olist <- list.files(path=OBpath,full.names = T)
# ospl <- strsplit(olist, "/")
# osplmore <- lapply(ospl, function(x) strsplit(x[2], "_"))

## Combines the data in the
obsout <- data.table::rbindlist(lapply(olist,function(x){
  tt <- readxl::read_xlsx(x)
  return(tt)}),use.names=TRUE,fill=TRUE)

# turning observation sheet into a spatial object
obsout <- obsout[!is.na(obsout$Latitude),]
obsout$lon <- obsout$Longitude
obsout$lat <- obsout$Latitude
obsout <- obsout %>% filter(str_detect(Behaviour, "^Flying"))
coordinates(obsout) <- ~Longitude+Latitude
projection(obsout) <- sf::st_crs(4326)$proj4string


# clipping to shapefile
obs.clipped <- obsout[boundary.shapefile,]


# turn observation sheet back into data frame
obsout <- obs.clipped@data
obsout$Species <- tolower(obsout$Species)


obsout %>% group_by(Camera, `Reel Ref`, `Frame Ref`, Species) %>% mutate(id = row_number()) %>% rename(`Reel Name` = `Reel Ref`, Frame = `Frame Ref`, Date = `Survey Date`) %>% ungroup() -> obsout


# This next part is just the usual combine the observation sheet with the data sheet
# Get worksheets into data frame ------------------------------------------
mlist <- list.files(path=SSpath,full.names = T, pattern = "\\.xlsx$")

pathname <- unlist(strsplit(mlist, "/"))
dir.create(paste0(pathname[1], "_clipped"))

 for(p in 1:length(mlist)) {

  
  ## Combines the data in the 
    tt <- readxl::read_xlsx(mlist[p])
    datout <- tt %>% dplyr::select(-`Identification Date`)
    
  datout$Species <- tolower(datout$Species)
  
  # Get Lat Lons from observation spreadsheet ------------------------------------------
  # Unfortunately, because the data weren't clipped to only include birds within the survey area, for this project we need to use the obsout, rather than datout dataset. 
  # SppDat_x <- datout %>% dplyr::filter(Species %in% SPP,)
  # ObsDat2 <- obsout2 %>% dplyr::filter(Species %in% SPP,)

  # account for duplicate reel and frame id in the case that there is more than one bird of the same species in a frame
  datout$Camera <- ifelse(datout$Camera > 4, datout$Camera-4, datout$Camera)
  
  datout %>% group_by(Camera, `Reel Name`, Frame, Species) %>% mutate(id = row_number()) %>% 
    ungroup() -> datout

  
  # join the two together
  # Only choose the columns in obsout that you're matching by, and also the lat and lon
  SppDat <- inner_join(datout, obsout[, c("Date", "Camera", "Reel Name", "Frame", "Species", "id", "lat", "lon")]) # need to include date here because sometimes there was more than one survey date.

  
  ##################################################

  pathnames <- unlist(strsplit(mlist[p], "/"))
  folname <- str_remove(pathnames[2], ".xlsx")
  
    write_xlsx(SppDat, paste(paste0(pathnames[1], "_clipped"), paste0(folname, "_clipped.xlsx"), sep = "/"))
  
 }
