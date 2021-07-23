##################################
#### Script to build reflection database for Flight height analysis
#### This script accompanies all other flight height scripts
#### Grant Humphries
###################################

# Source script -----------------------------------------------------------
  source("Code/helper.scripts/Bootstrap_method_helpers_KK.R")
  source("Code/config.R")
  source("Code/helper.scripts/Bootstrap_NEWFHMETHOD_HELPERS.R")
  library(HiDef)

# Load libraries ----------------------------------------------------------
## Uses the load_libs function which will install a package if it's not already

  lib_list <- c("readxl","foreach","gridExtra","ggpubr","rayshader","viridis","raster",
                "gstat","rgdal","sp","spatialEco","HTSSIP","hablar",
                "data.table","tidyverse","boot","GPArotation","psych","cowplot", "R.matlab")
  
  
  Load_Libs(lib_list)
  
  calibration <- readxl::read_excel("GPS Search.xlsm", sheet = "Calibration")

  # Hard coded variables ----------------------------------------------------

###################################
# ## TYPE IN THE WORKSPACE WHERE YOUR BIRDS AT HEIGHT SPREADSHEETS ARE STORED
# SSpath <- "Code/data.to.process"
# ###################################
# ## TYPE IN THE WORKSPACE WHERE THE OBSERVATION SPREADSHEETS ARE STORED
# OBpath <- "Code/observation.data/"
# ###################################
# ## TYPE IN THE FULL PATH TO THE BOUNDARY SHAPEFILE
# boundary.shapefile <- paste0("Code/boundary.shapefile/",boundary.file,".shp")

###################################

# Build reflection database -----------------------------------------------

  reflist_mat <- list.files(path="O:\\Reflection Database\\Reflection_database_02072021",full.names = T, pattern = "*\\.mat$")
  reflistxl <- list.files(path="O:\\Reflection Database\\Reflection_database_02072021",full.names = T, pattern = "*\\.xlsx$")
  ref_filter <- read_xlsx("O:\\Reflection Database\\All_mean_reflect_final_clean.xlsx")
  

# Read matlab files in Reflect database -----------------------------------

  refout_mat <- foreach(j=reflist_mat,.combine="rbind")%do%{
    X <- R.matlab::readMat(j)
    
    ### This makes sure to grab only birds that have been measured
    outputlist <- lapply(X$Birds, function(x){
      if(length(x[[1]][,,1]$possible) == 1){
        if(x[[1]][,,1]$possible == 1){
          return(x[[1]])
        }
      }
    })
    
  
    outputlist <- outputlist[!sapply(outputlist,is.null)]
    
    ### This will format each row in the output list to create 
    ### a nested tibble that can be queried
    if(length(outputlist) > 0){
    
    dataout <- foreach(x=1:length(outputlist),.combine='rbind')%do%{
      
      data.spot <- outputlist[[x]][,,1]
      df <- tibble(File.Name = j,
                   Frame = data.spot$Frame.Number,
                   'Marker Number' = data.spot$Marker.Number,
                   'Reel Name' = data.spot$Reel.Number,
                   Start.Frame = data.spot$frame.range[,,1]$start.bird.frame,
                   End.Frame = data.spot$frame.range[,,1]$end.bird.frame,
                   #Person = data.spot$Person,
                   Frame.Data = list(enframe(data.spot$Frame))
      )
    }
    return(dataout)
    }
  }
  
  

# Read excel files in -----------------------------------------------------

  refout <- data.table::rbindlist(lapply(reflistxl,function(x) {
    aa <- readxl::read_xlsx(x)#,sheet="Data")
    aa$Camera <- as.numeric(gsub("C", "", str_split(str_split(x, "/")[[1]][2], "_")[[1]][5]))
    aa <- aa[,-c(2,7,39)] # need to remove some columns because they aren't the same format between sheets so throws up an error. 
    aa$Survey <- str_split(x, "/")[[1]][3]
    aa
  }),
  use.names=TRUE,fill=TRUE
  )
  
  refout$Species <- tolower(refout$Species)
  
  
# Fixing the incorrect calibrations
  calibration <- as.data.frame(calibration)
  
  refs <- refout # to fix a hangover from earlier deleted code
  
  refs$`Plane Height` <- round(refs$`Plane Height`/5)*5
  refs$CalibrationFixed <- NA
  for(i in 1:dim(refs)[1]){
  refs$CalibrationFixed[i] <- calibration[pmatch(refs$`Plane Height`[i], calibration[,1], duplicates.ok = TRUE), refs$Camera[i]+1]
  }
  
  refs$calprop <- refs$CalibrationFixed/refs$Calibration # proportion to multiply to fix lengths
  
  

# Join matlab and non-matlab dfs ------------------------------------------

  reflect_data <- left_join(refout_mat, refs, by = c("Frame", "Marker Number", "Reel Name"))


# Join Ruth's filtered data -----------------------------------------------

  ref_filter <- rename(ref_filter, c("Marker Number" = "Marker", "Reel Name" = "reel"))
  
  reflect_data <- left_join(reflect_data, ref_filter, by = c("Species", "Marker Number", "Reel Name", "Frame"))
  reflect_data <- rename(reflect_data, "Frame Number" = "Frame")
  
  reflect_data <- reflect_data[-which(reflect_data$'Keep?' == 0), ]
  

### This builds the reflection data
  
  Dat.reflect <- foreach(i=1:nrow(reflect_data),.combine="rbind")%do%{

    testdat <- reflect_data[i,]
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
  

save(Dat.reflect, file = "Code/outputs/Dat.reflect.NEWFH")






