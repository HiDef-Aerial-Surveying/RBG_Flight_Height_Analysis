##########################
### Script to build flight height database
### This reads .mat files and extracts relevant information
### Used before flight height is estimated
### Kat Keogan (Grant Humphries) 06/07/21
##########################

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

SSfol <- "O:\\HP00102 - SSE North Irish Sea (Braymore Point)"
ENDfol <- "R:\\Projects\\HP00000 - Projects\\HP00102 - SSE North Irish Sea (Braymore Point)\\800 - Bird Flight Height\\Completed Flight Height Data (New Method 2021)"

## CHOOSE WHICH BOUNDARY SHAPEFILE YOU WANT (IN CONFIG.R)
boundary.shapefile <- boundary.shapefile.braymore

# List month folders ------------------------------------------------------

month.list <- list.files(SSfol)
mon.temp <- list.files(SSfol, pattern = "\\.xlsx$") # there's an excel file in this folder we don't want so this will remove it.
month.list <- month.list[which(month.list != mon.temp)]

for(month in 1:length(month.list)){
  
  cat(paste0(month.list[month], '\n'))
  
  data_mat <- list.files(paste(SSfol, month.list[month], "Data", sep = "/"), pattern = "\\.mat$", full.names = TRUE)
  data_xl <- list.files(paste(SSfol, month.list[month], "Data", sep = "/"), pattern = "\\.xlsx$", full.names = TRUE)
  
# Read matlab files -------------------------------------------------------
  
  dataout_mat <- foreach(j=data_mat,.combine="rbind")%do%{
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
    
    ### This will format each row in the output list to create 
    ### a nested tibble that can be queried
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
    }
  }


# Read in excel files -----------------------------------------------------

  datout <- data.table::rbindlist(lapply(data_xl,function(x) {
    aa <- readxl::read_xlsx(x)#,sheet="Data")
    aa$Camera <- as.numeric(gsub("C", "", str_split(str_split(x, "/")[[1]][4], "_")[[1]][5]))
    aa <- aa[,-c(2,7)] # need to remove some columns because they aren't the same format between sheets so throws up an error. 
    aa$Survey <- str_split(x, "/")[[1]][3]
    aa
  }),
  use.names=TRUE,fill=TRUE
  )
  
  datout$Species <- tolower(datout$Species)

# Fixing the incorrect calibrations
  
  calibration <- as.data.frame(calibration)
  
  datout$`Plane Height` <- round(datout$`Plane Height`/5)*5
  datout$CalibrationFixed <- NA
  for(i in 1:dim(datout)[1]){
    datout$CalibrationFixed[i] <- calibration[pmatch(datout$`Plane Height`[i], calibration[,1], duplicates.ok = TRUE), datout$Camera[i]+1]
  }
  
  datout$calprop <- datout$CalibrationFixed/datout$Calibration # proportion to multiply to fix lengths
  
  
# Join matlab and non-matlab dfs ------------------------------------------
  
  if(!is.null(dataout_mat)){
  
  measured_data <- left_join(dataout_mat, datout, by = c("Frame", "Marker Number", "Reel Name"))
  measured_data <- rename(measured_data, "Frame Number" = "Frame")


### This builds the lengths database
  
  lengths_data <- foreach(i=1:nrow(measured_data),.combine="rbind")%do%{
    
    testdat <- measured_data[i,]
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
  

# Save data ---------------------------------------------------------------

  endfilename <- paste0(month.list[month], ".RDS")
  
  saveRDS(lengths_data, paste(ENDfol, endfilename, sep = "/"))
  
  
}
}



