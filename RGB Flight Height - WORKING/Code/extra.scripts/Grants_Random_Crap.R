library(sp)
library(raster)

Sea2 <- raster::shapefile("Code/boundary.shapefile/SEA2_Ph2SiteBoundary_SSE_20181011.shp")
Sea2 <- spTransform(Sea2,sf::st_crs(4326)$proj4string)

Sea3 <- raster::shapefile("Code/boundary.shapefile/SEA3_Ph3SiteBoundary_SSE_20181011.shp")
Sea3 <- spTransform(Sea3,sf::st_crs(4326)$proj4string)

Seag <- raster::shapefile("Code/boundary.shapefile/Seagreen_Phase_2_and_3.shp")
Seag <- spTransform(Seag,sf::st_crs(4326)$proj4string)

# x <- readxl::read_xlsx("Code/outputs/SppDat/SppDatMay_2019.xlsx")
# x <- x[!is.na(x$Latitude),]
# x.kitt <- x %>% dplyr::filter(Species == "kittiwake")
# x.kitt$lon <- x.kitt$Longitude  ### <- IMPORTANT
# x.kitt$lat <- x.kitt$Latitude   ### <- IMPORTANT
# coordinates(x.kitt) <- ~Longitude+Latitude
# projection(x.kitt) <- sf::st_crs(4326)$proj4string
# 
# 
# x.kitt.sub.Sea2 <- x.kitt[Sea2,]### Spatial Clip
# x.kitt.sub.Sea2$Region <- "Seagreen2"
# x.kitt.sub.Sea3 <- x.kitt[Sea3,]
# x.kitt.sub.Sea3$Region <- "Seagreen3"

# x.kitt.sub <- x.kitt[Seag,]    ## spatial clip
# 
# x.kitt.all.data <- rbind(x.kitt.sub.Sea2@data,x.kitt.sub.Sea3@data)
# 
# ###################################################################################
# 
# Thresh <- 25
# prop.cr <- length(x.kitt[x.kitt$mdheight > Thresh,])/nrow(x.kitt) * 100
# prop.cr.sub <- length(x.kitt.all.data[x.kitt.all.data$mdheight > Thresh,])/nrow(x.kitt.all.data) * 100
# 
# nrow(x.kitt)
# nrow(x.kitt.all.data)
# 
# ggplot()+
#   geom_point(data=x.kitt.all.data,aes(x = lon,y=lat),pch=21)+
#   ggthemes::theme_clean()
# 
# 
# 
# 
# 
# 
# 
