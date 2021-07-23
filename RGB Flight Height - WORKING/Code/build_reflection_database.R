##################################
#### Script to build reflection database for Flight height analysis
#### This script accompanies all other flight height scripts
#### Grant Humphries
###################################

# Source script -----------------------------------------------------------
source("Code/Bootstrap_method_helpers_KK.R")
source("Code/config.R")
library(HiDef)

# Load libraries ----------------------------------------------------------
## Uses the load_libs function which will install a package if it's not already

lib_list <- c("readxl","foreach","gridExtra","ggpubr","rayshader","viridis","raster",
              "gstat","rgdal","sp","spatialEco","HTSSIP","hablar",
              "data.table","tidyverse","boot","GPArotation","psych","cowplot")


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

reflist <- list.files(path="Reflection_Database/Data_to_build_20perc_database/",full.names = T)
refout <- data.table::rbindlist(lapply(reflist,function(x) {
  aa <- readxl::read_xlsx(x)#,sheet="Data")
  aa$Camera <- as.numeric(gsub("C", "", str_split(str_split(x, "/")[[1]][3], "_")[[1]][5]))
  aa <- aa[,-c(2,7,39)] # need to remove some columns because they aren't the same format between sheets so throws up an error. 
  aa$Survey <- str_split(x, "/")[[1]][3]
  aa
}),
use.names=TRUE,fill=TRUE
)
refout$Species <- tolower(refout$Species)
names(refout)[27] <- "Reflection"
unique(refout$Reflection)
### Get only the birds that have reflection... 
### Reflection chosen in two columns so separate and then combine, keeping only unique rows so we don't double up
refoutY <- refout[refout$Reflection=="Y",]
refoutR <- refout[grepl("R|reflection|Reflection|Refl|refl|Ref|ref", refout$Comments...38),]
refoutAll <- rbind(refoutY,refoutR) %>% unique

## Check sample size for each species
refoutAll %>% count(Species)
reflectcount <- refoutAll %>% count(Species)

#save(reflectcount, file = "Code/outputs/reflectcount")

## Check species names!! If  
unique(refoutAll$Species)
## Filter the reflection data by the species of interest
# refs <- refout[refout$Species == SPP] # original line that doesn't include two species
refs <- dplyr::filter(refoutAll, Species %in% SPP)


# Fixing the incorrect calibrations
calibration <- as.data.frame(calibration)

refs$`Plane Height` <- round(refs$`Plane Height`/5)*5
refs$CalibrationFixed <- NA
for(i in 1:dim(refs)[1]){
refs$CalibrationFixed[i] <- calibration[pmatch(refs$`Plane Height`[i], calibration[,1], duplicates.ok = TRUE), refs$Camera[i]+1]
}

refs$calprop <- refs$CalibrationFixed/refs$Calibration # proportion to multiply to fix lengths

### This builds the reflection data

Dat.reflect <- get.refl.lengths(refs)
Dat.reflect <- Dat.reflect[Dat.reflect$CV < 10, ]
save(Dat.reflect, file = "Code/outputs/Dat.reflect")

############################################
### Impute data and add to Dat.reflect

############################################
real.data <- read.csv("Data/bird_lengths.csv")
load("Data/Dat.reflect")

Dat.reflect$Imputed <- NA

to.impute <- c("lesser black-backed gull",
               "common gull",
               "little gull",
               "arctic tern",
               "common tern",
               "sandwich tern",
               "manx shearwater",
               "great skua")

spp.to.compare <- c('kittiwake',"gannet","herring gull")

newly.imputed <- foreach(j=to.impute,.combine='rbind')%do%{
  imputed.data <- impute.species(real.data,Dat.reflect,j,spp.to.compare)
  return(imputed.data)
}


New.Dat.Reflect <- rbind(Dat.reflect,newly.imputed)

save(New.Dat.Reflect,file="Data/Dat.reflect.imputed")





newly.imputed %>% nest(data=c(-Species)) %>%
  mutate(
    meansize = map_dbl(data,~mean(.x$fin.length))
  )





