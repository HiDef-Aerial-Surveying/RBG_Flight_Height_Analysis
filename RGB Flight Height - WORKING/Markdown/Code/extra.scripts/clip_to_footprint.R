##################################################################
#### MASTER SCRIPT for clipping data to windfarm footprint ####
#### Author: Grant Humphries
#### Date: December 9, 2020
#### Email: grant.humphries@hidefsurveying.co.uk
#### Github: https://github.com/HiDef-Aerial-Surveying/RBG_Flight_Height_Analysis
##################################################################

### This script is specifically for seagreen, as data need to be clipped to two separate areas. 

# Source script -----------------------------------------------------------
source("Code/helper.scripts/Bootstrap_method_helpers_KK.R")
source("Code/config.R")
source("Code/Grants_Random_crap.R") # this has shapefile info for seagreen specifically


# Load libraries ----------------------------------------------------------
## Uses the load_libs function which will install a package if it's not already

lib_list <- c("readxl","foreach","gridExtra","ggpubr","rayshader","viridis","raster",
              "gstat","rgdal","sp","spatialEco","HTSSIP","hablar",
              "data.table","tidyverse","boot","GPArotation","psych","cowplot", "openxlsx")


Load_Libs(lib_list)

# Hard coded variables ----------------------------------------------------

###################################
## TYPE IN THE WORKSPACE WHERE YOUR BIRDS AT HEIGHT SPREADSHEETS ARE STORED
SSpath <- "R:\\Projects\\HP00000 - Projects\\HP00104 - Seagreen Phase 2 and 3\\500 - Data Processing\\2019 - Month 07 - Survey 01\\Zone85_M07_S01_D02_19"
###################################
## TYPE IN THE WORKSPACE WHERE THE OBSERVATION SPREADSHEETS ARE STORED
OBpath <- "R:\\Projects\\HP00000 - Projects\\HP00104 - Seagreen Phase 2 and 3\\600 - Survey Reports\\Issued Docs\\Unzipped for clipping\\2019 - Month 07 - Survey 01" # made the unzipped for clipping folder to put observation sheets
###################################
## TYPE IN THE FULL PATH TO THE BOUNDARY SHAPEFILE
#boundary.shapefile <- paste0("Code/boundary.shapefile/",boundary.file,".shp") 

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
# Sea2 is the name of the reduced area shapefile (found in config.R)
obs.sub.sea2 <- obsout[Sea2,]
obs.sub.sea3 <- obsout[Sea3,]

# turn observation sheet back into data frame
obsout2 <- obs.sub.sea2@data
obsout3 <- obs.sub.sea3@data

#obsout2kit <- obsout2 %>% filter(Species == "Kittiwake")

# This next part is just the usual combine the observation sheet with the data sheet
# Get worksheets into data frame ------------------------------------------
mlist <- list.files(path=SSpath,full.names = T)
# 

 for(p in 1:length(mlist)) {

  
  ## Combines the data in the 
    tt <- readxl::read_xlsx(mlist[p])
    datout <- tt %>% dplyr::select(-`Identification Date`)
    
  datout$Species <- tolower(datout$Species)
  obsout2$Species <- tolower(obsout2$Species)
  obsout3$Species <- tolower(obsout3$Species)
  
  # Get Lat Lons from observation spreadsheet ------------------------------------------
  # Unfortunately, because the data weren't clipped to only include birds within the survey area, for this project we need to use the obsout, rather than datout dataset. 
  SppDat_x <- datout %>% dplyr::filter(Species %in% SPP,)
  ObsDat2 <- obsout2 %>% dplyr::filter(Species %in% SPP,)
  ObsDat3 <- obsout3 %>% dplyr::filter(Species %in% SPP,)
  
  # account for duplicate reel and frame id in the case that there is more than one bird of the same species in a frame
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
  
  ##################################################
  
  
  names(SppDat.sg2)[1:length(names(datout))] <- names(datout)
  names(SppDat.sg3)[1:length(names(datout))] <- names(datout)
  
  SppDat.sg2 <- SppDat.sg2[,c(1:length(names(datout)),length(SppDat.sg2)-1,length(SppDat.sg2))]
  SppDat.sg3 <- SppDat.sg3[,c(1:length(names(datout)),length(SppDat.sg3)-1,length(SppDat.sg3))]
  
  # SppDat.sg2 <- SppDat.sg2[, -which(names(SppDat.sg2) %in% c("...11", "...28","...31"))]
  # SppDat.sg3 <- SppDat.sg3[, -which(names(SppDat.sg3) %in% c("...11", "...28","...31"))]
  
  pathnames <- unlist(strsplit(mlist[p], "/"))
  folname <- str_remove(pathnames[2], ".xlsx")
  
  #path <- str_remove(pathnames[1], "Data")
  path <- unlist(strsplit(pathnames[1], "Zone85"))[1]
  
  write.xlsx(SppDat.sg2, paste0(path, "SG2/", folname, "_SG2.xlsx"))
  write.xlsx(SppDat.sg3, paste0(path, "SG3/", folname, "_SG3.xlsx"))

  # counts_spec_full <- SppDat_x %>% group_by(Species) %>% count()
  # names(counts_spec_full) <- c("Species", "Full")
  # 
  # counts_spec_SG2 <- SppDat.sg2 %>% group_by(Species) %>% count()
  # names(counts_spec_SG2) <- c("Species", "SG2")
  #                             
  # counts_spec_SG3 <- SppDat.sg3%>% group_by(Species) %>% count()
  # names(counts_spec_SG3) <- c("Species", "SG3")
  # 
  # kk <- full_join(counts_spec_full, counts_spec_SG2)
  # spec_comp <- full_join(kk, counts_spec_SG3)
  # 
  # spec_comp$sheet <- folname
  
  #write.table(spec_comp, "R:\\Projects\\HP00000 - Projects\\HP00104 - Seagreen Phase 2 and 3\\900 - Bird flight height\\clipped_species_comparison.txt", append = TRUE, row.names = FALSE, col.names = FALSE)
  
 }
