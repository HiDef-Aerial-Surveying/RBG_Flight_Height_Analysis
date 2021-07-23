##################################################################
#### MASTER SCRIPT for running the RGB flight height analysis ####
#### ADDITIONAL CODE TO GET DATA INTO FORMAT FOR COLLISION RISK MODELLING
#### Author: Grant Humphries + Kat Keogan
#### Date: June 2021
#### Email: grant.humphries@hidefsurveying.co.uk
#### Github: https://github.com/HiDef-Aerial-Surveying/RBG_Flight_Height_Analysis
##################################################################


# Source script -----------------------------------------------------------
source("helper.scripts/Bootstrap_method_helpers_KK.R")
source("config.R")
source("helper.scripts/Bootstrap_NEWFHMETHOD_HELPERS.R")
#devtools::install_github("https://github.com/HiDef-Aerial-Surveying/HiDEF.git")
library(HiDef)
'%!in%' <- function(x,y)!('%in%'(x,y))

# Load libraries ----------------------------------------------------------
## Uses the load_libs function which will install a package if it's not already

lib_list <- c("readxl","foreach","gridExtra","ggpubr","rayshader","viridis","raster",
              "gstat","rgdal","sp","spatialEco","HTSSIP","hablar",
              "data.table","tidyverse","boot","GPArotation","psych","cowplot","R.matlab")


Load_Libs(lib_list)

# calibration <- readxl::read_excel("GPS Search.xlsm", sheet = "Calibration")

# Hard coded variables ----------------------------------------------------

###################################
# List of species to be used in analysis
#SPP <- tolower(c("kittiwake", "herring gull", "gannet", "lesser black-backed gull", "great black-backed gull")) # need to be in lower case

SPP <- "kittiwake"

###################################
## TYPE IN THE WORKSPACE WHERE YOUR BIRDS AT HEIGHT SPREADSHEETS ARE STORED
## Stored in the 800 folder
SSpath <- "R:\\Projects\\HP00000 - Projects\\HP00102 - SSE North Irish Sea (Braymore Point)\\800 - Bird Flight Height\\Completed Flight Height Data (New Method 2021)" # change this path for each project

# This is where the flight height in format for sCRM will go
OUTpath <- "R:\\Consultancy\\HC00002 - Resources\\Trial Collision risk models July 2021\\Flight height boot format"

###################################
## TYPE IN THE WORKSPACE WHERE THE OBSERVATION SPREADSHEETS ARE STORED
# This will only be needed where data have not been split into three areas, as will already have lat/lon added
#OBpath <- "Code/observation.data/"
###################################
## CHOOSE WHICH BOUNDARY SHAPEFILE YOU WANT (IN CONFIG.R)
boundary.shapefile <- boundary.shapefile.braymore

###################################

# Load reflection database -----------------------------------------------
# 09/06/21 - this is the reflection database used for Seagreen FH modelling. No changes made yet based on birds potentially being incorrectly measured.
load(file = "outputs/Dat.reflect.NEWFH")


# Get worksheets into data frame ------------------------------------------
mlist <- list.files(path = SSpath, full.names = TRUE)

# Loop through each folder to create database -----------------------------

# for(p in 1:length(mlist)) {
#   
#   # might need to put this in a loop but for now it's outside of a loop
#   mmlist <- list.files(path = mlist[p], full.names = T, pattern = "*.RDS")
#   
#   file.split <- str_split(mmlist[1], "/")[[1]][2] # extract name of folder so we can save using this name
#   
  # Read in matlab files ----------------------------------------------------
  
  datout <- data.table::rbindlist(lapply(mlist,function(x){
    tt <- readRDS(x)
    tt$month <- as.numeric(substring(grep(unlist(strsplit(x,"/")),
                                          pattern=".Month ",value = T), 14, 15))
    tt$year <- as.numeric(substring(grep(unlist(strsplit(x,"/")),
                                          pattern=".Month ",value = T), 1, 4))
    # tt <- tt %>% dplyr::select(-`Identification Date`)
    if(dim(tt)[1] > 0){
      return(tt)}}),use.names=TRUE,fill=TRUE)
  

  # Get bird lengths --------------------------------------------------------
  

  datout$Species <- tolower(datout$Species)

  SppDat_x <- datout %>% dplyr::filter(Species %in% SPP)
  

# Account for calibration in reflect data lengths -------------------------

for(mult in 1:length(Dat.reflect$LENS)){
  Dat.reflect$LENS[[mult]] <- Dat.reflect$LENS[[mult]]*Dat.reflect$CalibrationFixed[mult]
}


# Run flight height -------------------------------------------------------

  
    fullfhdata <- foreach(bootx = 1:200,.combine = 'cbind') %do% {
    # This is the line that estimates flight heights. 
    fhdata <- get.fhs.mat(SppDat_x,Dat.reflect,bootsize = bootsize)
    
    names(fhdata) <- gsub("\\."," ", names(fhdata))
    
    # This numbers the bootstrap outputs
    newlw <- paste0("lwheight", bootx)
    newmd <- paste0("mdheight", bootx)
    newhi <- paste0("hiheight", bootx)
    
    
    names(fhdata)[names(fhdata) == "lwheight"] <- paste0("lwheight", bootx)
    names(fhdata)[names(fhdata) == "mdheight"] <- paste0("mdheight", bootx)
    names(fhdata)[names(fhdata) == "hiheight"] <- paste0("hiheight", bootx)
    
    return(fhdata)
    }
    
  # remove duplicated columns for species/month/year etc. 
    fullfhdata_trim <- fullfhdata[which(duplicated(colnames(fullfhdata)) == FALSE)]
    save(fullfhdata_trim, file = "Code/outputs/fullfhdata")
    
    SppDat_x <- SppDat_x %>% rename("Reel Ref" = "Reel Name", "Frame Ref" = "Frame Number")
    
    # Saving ----------------------------------------------------------------
    
    # Join SppDat with fhdata - this makes absolutely sure the right rows are matched together
    
    SppDat_x <- inner_join(SppDat_x, fullfhdata_trim, by = c("Species", "month", "Reel Ref", "Frame Ref","Marker Number"))
  # }
    
    # select only mean height
    meanFHdata <- fullfhdata_trim %>% group_by(Species, month, year) %>% select(starts_with("mdheight"))
    
    # round so the values can be binned into 1m bins for CRM
    meanFHdata[4:ncol(meanFHdata)] <- round(meanFHdata[4:ncol(meanFHdata)], 0)
    
    # create template bootstrap dataframe
    # boot_full <- data.frame(Species = c(rep("gannet", 500),
    #                                               rep("kittiwake", 500),
    #                                               rep("great black-backed gull", 500),
    #                                               rep("herring gull", 500),
    #                                               rep("lesser black-backed gull", 500)),
    #                         value = rep(seq(1,500,1), 5))
    

# Create data frame in sCRM format ----------------------------------------

    
    # create template bootstrap dataframe
    boot_full <- data.frame(Species = rep("kittiwake", 500),
                            value = seq(1,500,1))

    # count the number of birds in each bin 
    FH_pop_dist_boot <- meanFHdata %>% 
                        group_by(Species) %>% 
                        gather(boot, value, 4:203) %>% # puts all boot iterations and values into columns rather than rows
                        group_by(Species, boot) %>% 
                        count(value) %>% # counts how many times a flight height is found ("value" now represents the 1m bands)
                        spread(boot, n, fill = 0) %>% # turns the dataframe back into its original format with a column for each bootstrap iteration
                        full_join(boot_full) %>% # we need a row for every height band from 1-500. This joins with the above data to make that happen
                        mutate(across(everything(), ~replace_na(.x, 0))) %>% # replace NAs with 0s across all rows and columns as they are count data now.
                        arrange(Species, value) # order by species and then ascending flight height bands from 1-500
    
    # Convert counts to proportions for the sCRM
    
    totalbirds <- unique(apply(FH_pop_dist_boot[,3:dim(FH_pop_dist_boot)[2]], 2, sum))
    
    FH_pop_dist_boot[,3:dim(FH_pop_dist_boot)[2]] <- FH_pop_dist_boot[,3:dim(FH_pop_dist_boot)[2]]/totalbirds
    
    # rename columns so they match the sCRM bootstrap flight format
    colnames(FH_pop_dist_boot) <- c("Species", "Height_m", paste0("bootId_", rep(1:200)))
    
    
    # save as .csv by species
    sapply(SPP, function(x){
    
    dat_spec <- FH_pop_dist_boot %>% filter(Species == x)
    dat_specx <- dat_spec[,-1]
    write.csv(dat_specx, paste0(OUTpath, "/FHD_bootstrapData_", x, ".csv"), row.names = FALSE)
    
    # Calculate proportion at collision risk height (40m - 320m)
    pchMEAN <- mean(apply(FH_pop_dist_boot[40:320,3:dim(FH_pop_dist_boot)[2]], 2, sum))
    pchSD <- sd(apply(FH_pop_dist_boot[40:320,3:dim(FH_pop_dist_boot)[2]], 2, sum))
    
    write.table(cbind(pchMEAN, pchSD), paste0(OUTpath, "/FHD_PCH_", x, ".txt"))
    
    })


  