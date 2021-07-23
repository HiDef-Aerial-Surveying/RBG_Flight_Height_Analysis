#############################
### config file for the markdown document
### 18.06.21
#############################

## SHAPEFILE

boundary.file.galx <- "GALext_Potential_Extension_Area_180524_WGS84.shp" # galloper extension
boundary.file.ggbx <- "GGX_PotentialDevelopmentAreasForTCESubmission_SSE_20180528.shp" # greater gabbard extension
boundary.file.gpcm <- "Galloper&Cable_4kmbuffer.shp" # Galloper PCM (Not split)

## TYPE IN THE FULL PATH TO THE BOUNDARY SHAPEFILE
# Galloper extension
boundary.shapefile.galx <- paste0("R:\\Projects\\HP00000 - Projects\\HP00099 - Innogy Galloper Post-construction Monitoring\\100 - Project Control\\Survey Design\\Shapefiles\\",boundary.file.galx) # galloper extension
# Gabbard extension
boundary.shapefile.ggbx <- paste0("R:\\Projects\\HB00000 - Bids\\HB00232 - Innogy Galloper Post-construction Monitoring\\GIS\\Shapefiles\\GGX Dev Area\\",boundary.file.ggbx)
# Galloper PCM
boundary.shapefile.gpcm <- paste0("R:\\Projects\\HB00000 - Bids\\HB00232 - Innogy Galloper Post-construction Monitoring\\GIS\\Shapefiles\\",boundary.file.gpcm)

##########################################################
### ENTER CORRECT FILES/OBJECTS HERE ###
## DATA SOURCES
# Choose the correct folders here

fh.dat <- "R:\\Projects\\HP00000 - Projects\\HP00099 - Innogy Galloper Post-construction Monitoring\\800 - Completed Flight Height Sheets\\Zone74_FHest"

m.dat <- "R:\\Projects\\HP00000 - Projects\\HP00099 - Innogy Galloper Post-construction Monitoring\\800 - Completed Flight Height Sheets\\Zone74_Measure"

# Add correct boundary file here:
boundary <- boundary.shapefile.gpcm

#########################################################

WGS84 <- sf::st_crs(4326)$proj4string

###################################
## TYPE IN THE LOW AND HIGH POINT OF THE TURBINE BLADES
turbine.low <- 32
turbine.high <- 252
turbine.max <- 322

##################################
# DATES FOR SPECIFYING YEARS 1 AND 2
Date1 <- as.Date("2019-03-01")
Date2 <- as.Date("2020-02-28")
