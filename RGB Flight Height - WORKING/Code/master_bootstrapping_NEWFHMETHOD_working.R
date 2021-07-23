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
source("Code/helper.scripts/Bootstrap_NEWFHMETHOD_HELPERS.R")
#devtools::install_github("https://github.com/HiDef-Aerial-Surveying/HiDEF.git")
library(HiDef)
'%!in%' <- function(x,y)!('%in%'(x,y))

# Load libraries ----------------------------------------------------------
## Uses the load_libs function which will install a package if it's not already

  lib_list <- c("readxl","foreach","gridExtra","ggpubr","rayshader","viridis","raster",
                "gstat","rgdal","sp","spatialEco","HTSSIP","hablar",
                "data.table","tidyverse","boot","GPArotation","psych","cowplot","R.matlab")
  
  
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
  #SSfol <- "R:\\Projects\\HP00000 - Projects\\HP00100 - Innogy Galloper Wind Farm Extension Baseline Surveys\\800 - Completed Flight Height Sheets" # change this path for each Galloper
  #SSfol <- "R:\\Projects\\HP00000 - Projects\\HP00101 - Innogy Greater Gabbard Wind Farm Extension Baseline Surveys\\800 - Completed Flight Height Sheets" # change this path for each Galloper
  
  Zone <- "Zone74" # Galloper PCM
  #Zone <- "Zone80" # Galloper extension
  #Zone <- "Zone90" # Gabbard extension
  
  SSpath <- paste(SSfol, Zone, sep = "\\") # make sure the zone is correct, with no spaces
###################################
## TYPE IN THE WORKSPACE WHERE THE OBSERVATION SPREADSHEETS ARE STORED
# This will only be needed where data have not been split into three areas, as will already have lat/lon added
#OBpath <- "Code/observation.data/"
###################################
## CHOOSE WHICH BOUNDARY SHAPEFILE YOU WANT (IN CONFIG.R)
  boundary.shapefile <- boundary.shapefile.gpcm

###################################

# Load reflection database -----------------------------------------------
# 09/06/21 - this is the reflection database used for Seagreen FH modelling. No changes made yet based on birds potentially being incorrectly measured.
  load(file = "Code/outputs/Dat.reflect.NEWFH")

# Account for calibration in reflect data lengths -------------------------

  for(mult in 1:length(Dat.reflect$LENS)){
    Dat.reflect$LENS[[mult]] <- (Dat.reflect$LENS[[mult]]*Dat.reflect$CalibrationFixed[mult])*Dat.reflect$calprop[mult]
  }

# Get worksheets into data frame ------------------------------------------
  mlist <- list.files(path = SSpath, full.names = TRUE)

# Loop through each folder to create database -----------------------------

  for(p in 1:length(mlist)) {
    
    # might need to put this in a loop but for now it's outside of a loop
    mmlist <- list.files(path = mlist[p], full.names = T, pattern = "*.RDS")
    
    file.split <- str_split(mmlist[1], "/")[[1]][2] # extract name of folder so we can save using this name
    
    # Read in matlab files ----------------------------------------------------
    
    datout <- data.table::rbindlist(lapply(mmlist,function(x){
      tt <- readRDS(x)
      # tt$Camera <- as.numeric(substring(grep(unlist(strsplit(x,"_")),
      #                                        pattern="C\\d",value = T), 2))
      tt$month <- as.numeric(substring(grep(unlist(strsplit(x,"_")),
                                            pattern="M",value = T)[1], 2,3))
      tt$year <- as.numeric(unlist(strsplit(mlist[p],"_"))[!grepl(unlist(strsplit(mlist[p],"_")),
                                                           pattern = "\\D") == TRUE]) # choose string with only numbers, which will give us the year
      tt <- tt %>% dplyr::select(-`Identification Date`)
      if(dim(tt)[1] > 0){
        return(tt)}}),use.names=TRUE,fill=TRUE)
    
  
    # Get bird lengths --------------------------------------------------------
    
    #datout <- datout[!is.na(datout$Latitude),]
    datout$Species <- tolower(datout$Species)
    
    # if("Behaviour.x" %in% colnames(datout)){
    #   datout <- datout %>% rename("Behaviour" = "Behaviour.x")
    # }
    # 
    SppDat_x <- datout %>% dplyr::filter(Species %in% SPP)
    
    MeasureDat <- SppDat_x # We save this here because we'll need to know how many birds were measured versus how many FH was estimated for.
    

    
    ########################################################################
    
    # Fix calibration ---------------------------------------------------------
    
    if(nrow(SppDat_x) > 0){
    calibration <- as.data.frame(calibration)
    
    SppDat_x$`Plane Height` <- round(SppDat_x$`Plane Height`/5)*5
    
    for(i in 1:dim(SppDat_x)[1]){
      SppDat_x$CalibrationFixed[i] <- calibration[pmatch(SppDat_x$`Plane Height`[i], calibration[,1], duplicates.ok = TRUE), SppDat_x$Camera[i]+1]
    }
    
    SppDat_x$calprop <- SppDat_x$CalibrationFixed/SppDat_x$Calibration # proportion to multiply to fix lengths

# Get lengths -------------------------------------------------------------

    SppDat_temp <- foreach(i=1:nrow(SppDat_x),.combine="rbind")%do%{
      testdat <- SppDat_x[i,]
      if(!is.null(testdat$Frame.Data[[1]])){
        cat(paste0(i,'\n'))
        
        FrameDat <- testdat$Frame.Data[[1]]$value  # This is the data from the original reviewer
        
        ### Uses the get.all.lengths function to calculate new lengths from the .mat file
        ### using the new method.
        LEN.TAB <- foreach(Frame=1:length(FrameDat),.combine="rbind")%do%{
          LPs <- get.line.points(FrameDat,Frame)
          LENS <- get.all.lengths(FrameDat,Frame,LPs)
          
          Hx <- FrameDat[[Frame]][[1]][,,1]$Head[,,1]$x
          Hy <- FrameDat[[Frame]][[1]][,,1]$Head[,,1]$y
          Tx <- FrameDat[[Frame]][[1]][,,1]$Tail[,,1]$x
          Ty <- FrameDat[[Frame]][[1]][,,1]$Tail[,,1]$y
          lengthdots <- euc.dist(c(Hx,Hy),c(Tx,Ty))
          
          return(tibble(LENS = list(unlist(LENS)),meanLEN = mean(unlist(LENS),na.rm=TRUE),Frame=Frame,man.length=lengthdots))
        }
        
        # This figures out which frame has the maximum mean length 
        maxLENS <- LEN.TAB[which.max(LEN.TAB$meanLEN),]
        testdat <- cbind(testdat, maxLENS)
        
        ### This returns all the metrics
        return(tibble(testdat))   
      }
    }
    
    SppDat_x <- SppDat_temp
    
    #####################################################################
    
    
      # This is the line that estimates flight heights. 
      fhdata <- get.fhs.mat(SppDat_x,Dat.reflect,bootsize = bootsize)
      
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
