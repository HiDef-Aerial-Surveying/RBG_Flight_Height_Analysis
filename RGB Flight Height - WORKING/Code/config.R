## TYPE IN SPECIES NAME HERE
# SPP <- "Gannet"
# SPP <- tolower(c("Arctic skua", "Arctic tern", "common gull", "common tern", "fulmar", "gannet", "great black-backed gull", "great skua", "herring gull", "kittiwake", "lesser black-backed gull", "little gull", "Manx shearwater", "pomarine skua", "red-throated diver", "sandwich tern", "sooty shearwater"))

SPP <- tolower("kittiwake")

## SHAPEFILE

boundary.file.galx <- "GALext_Potential_Extension_Area_180524_WGS84.shp" # galloper extension
boundary.file.ggbx <- "GGX_PotentialDevelopmentAreasForTCESubmission_SSE_20180528.shp" # greater gabbard extension
boundary.file.gpcm <- "Galloper&Cable_4kmbuffer.shp" # Galloper PCM (Not split)

boundary.file.braymore <- "BRA_RevisedSurveyArea_WGS84_SSE_20200811.shp"

## TYPE IN THE FULL PATH TO THE BOUNDARY SHAPEFILE
# Galloper extension
boundary.shapefile.galx <- paste0("R:\\Projects\\HP00000 - Projects\\HP00099 - Innogy Galloper Post-construction Monitoring\\100 - Project Control\\Survey Design\\Shapefiles\\",boundary.file.galx) # galloper extension
# Gabbard extension
boundary.shapefile.ggbx <- paste0("R:\\Projects\\HB00000 - Bids\\HB00232 - Innogy Galloper Post-construction Monitoring\\GIS\\Shapefiles\\GGX Dev Area\\",boundary.file.ggbx)
# Galloper PCM
boundary.shapefile.gpcm <- paste0("R:\\Projects\\HB00000 - Bids\\HB00232 - Innogy Galloper Post-construction Monitoring\\GIS\\Shapefiles\\",boundary.file.gpcm)

# Braymore Point
boundary.shapefile.braymore <- paste0("R:\\Projects\\HP00000 - Projects\\HP00102 - SSE North Irish Sea (Braymore Point)\\500 - Data Processing\\2021 - Month 02 - Survey 01\\z\\Density Estimates\\Shapefile\\", boundary.file.braymore)

#########################################################

WGS84 <- sf::st_crs(4326)$proj4string

###################################
## TYPE IN THE LOW AND HIGH POINT OF THE TURBINE BLADES
turbine.low <- 32
turbine.high <- 252
turbine.max <- 322

##################################
## TYPE IN NUMBER OF BOOTSTRAPS
# The real analysis need 1000 bootstraps. 
# Don't run on VPN or it'll take aaaaages
bootsize <- 1000

##################################
# DATES FOR SPECIFYING YEARS 1 AND 2
Date1 <- as.Date("2019-03-01")
Date2 <- as.Date("2020-02-28")
