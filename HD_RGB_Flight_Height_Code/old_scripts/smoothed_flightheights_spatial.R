###################################
### Merge measured bird spreadsheets with observations 
### to get the appropriate lat/lon values

source("Scripts/Helpers.R")
Load_Libs(c("readxl","foreach",
            "data.table","rayshader","raster",
            "viridis","sp","rgdal","gstat",
            "spatialEco","hablar","tidyverse","HTSSIP"))

spp <- "Kittiwake"
obs <- readxl::read_xlsx("data/Zone99_M06_surveys_Merge_for_LatLons/Observations/Zone99_M06_S01_D01_19_Observations_MSS_21082019.xlsx")
obs$Camera <- as.character(obs$Camera)
flist <- list.files(path="data/Zone99_M06_surveys_Merge_for_LatLons/",pattern="*.xlsx",full.names=T)
dat <- rbindlist(lapply(flist,read_xlsx),fill=TRUE)
dat$Camera <- plyr::revalue(as.character(dat$Camera),c("5" = "1","8" = "4", "7" = "3"))





t1 <- obs %>% dplyr::filter(Species == 'Kittiwake')
dat <- dat %>% dplyr::filter(Species == 'Kittiwake')

dat$Latitude <- t1$Latitude
dat$Longitude <- t1$Longitude


shapearea <- dat
coordinates(shapearea) <- ~Longitude+Latitude
proj4string(shapearea) <- sf::st_crs(4326)$proj4string
  #readOGR(dsn="Data/Shape",
             #        layer="GG_merged_dissolve")
#shapearea <- spTransform(shapearea,sf::st_crs(4326)$proj4string)

e <- extent(shapearea)
# coerce to a SpatialPolygons object
p <- as(e, 'SpatialPolygons')  
proj4string(p) <- sf::st_crs(4326)$proj4string


data_mod <- KI.frame %>% dplyr::filter(category=="Mean")


rr <- produce.raster(data_mod,p,
                     month=NULL,grd.density=50000,
                     idp=3.0, crs=sf::st_crs(4326)$proj4string)

pointdat <- data.frame(rr$dmodt)
pointdatshp <- rr$dmodt
rr <- rr$rr

outdf <- create.df.to.plot(rr,threshold = 0)


g <- ggplot() +  
  geom_tile(data=outdf, aes(x=x, y=y, fill=value), alpha=0.8) +
  geom_point(data=data_mod,aes(x=Lon,y=Lat),pch=19)+
  scale_fill_viridis() +
  labs(fill = "Height (m)",x="Longitude",y="Latitude")+
  coord_equal() +
  ggthemes::theme_map()+
  ggtitle("b")+
  theme(legend.position="right",
        #legend.title = element_blank(),
        axis.title = element_text(),
        panel.background = element_blank(),
        panel.border = element_rect(colour="black",fill="NA")
  )

g

ggsave(plot=g,filename="Figures/smoothed_height_kittiwake.png",device = "png",width = 6,height=4)













spp <- "Gannet"
dat <- rbindlist(lapply(flist,read_xlsx),fill=TRUE)
dat$Camera <- plyr::revalue(as.character(dat$Camera),c("5" = "1","8" = "4", "7" = "3"))

t1 <- obs %>% dplyr::filter(Species == spp)
dat <- dat %>% dplyr::filter(Species == spp)

dat$Latitude <- t1$Latitude
dat$Longitude <- t1$Longitude


shapearea <- dat
coordinates(shapearea) <- ~Longitude+Latitude
proj4string(shapearea) <- sf::st_crs(4326)$proj4string
#readOGR(dsn="Data/Shape",
#        layer="GG_merged_dissolve")
#shapearea <- spTransform(shapearea,sf::st_crs(4326)$proj4string)

e <- extent(shapearea)
# coerce to a SpatialPolygons object
p <- as(e, 'SpatialPolygons')  
proj4string(p) <- sf::st_crs(4326)$proj4string
reflectdfGX <- data.frame(
  min.reflection.ud=46.3,
  mean.reflection.ud=57.6,
  max.reflection.ud=69.9,
  min.reflection.lr=51.8,
  mean.reflection.lr=67.3,
  max.reflection.lr=81.5
)
GX.data <- spp.data(dat,reflectdfGX)

GX.mins <- return.heights(GX.data,grep(names(GX.data),pattern="minheights*"))
GX.mins$category <- "Minimum"
GX.means <- return.heights(GX.data,grep(names(GX.data),pattern="meanheights*"))
GX.means$category <- "Mean"
GX.maxs <- return.heights(GX.data,grep(names(GX.data),pattern="maxheights*"))
GX.maxs$category <- "Maximum"

GX.frame <- do.call('rbind',list(GX.mins,GX.means,GX.maxs))

GX.frame$Lon <- rep(GX.data$Lon,3)
GX.frame$Lat <- rep(GX.data$Lat,3)


data_modGX <- GX.frame %>% dplyr::filter(category=="Mean")


rrGX <- produce.raster(data_modGX,p,
                     month=NULL,grd.density=50000,
                     idp=3.0, crs=sf::st_crs(4326)$proj4string)

pointdatGX <- data.frame(rrGX$dmodt)
pointdatshpGX <- rrGX$dmodt
rrGX <- rrGX$rr

outdfGX <- create.df.to.plot(rrGX,threshold = 0)


g2 <- ggplot() +  
  geom_tile(data=outdfGX, aes(x=x, y=y, fill=value), alpha=0.8) +
  geom_point(data=data_modGX,aes(x=Lon,y=Lat),pch=19)+
  scale_fill_viridis() +
  labs(fill = "Height (m)",x="Longitude",y="Latitude")+
  coord_equal() +
  ggthemes::theme_map()+
  ggtitle("a")+
  theme(legend.position="right",
        #legend.title = element_blank(),
        axis.title = element_text(),
        panel.background = element_blank(),
        panel.border = element_rect(colour="black",fill="NA")
  )

g2


library(ggpubr)


g3 <- ggarrange(g2,g,ncol=1,nrow=2)

g3


ggsave(plot=g3,filename="Figures/smoothed_heights.png",device = "png",width = 6,height=8)








#plot_gg(g, width = 5, height = 5, raytrace = FALSE, preview = TRUE)

#plot_gg(g, width = 5, height = 5, multicore = TRUE, scale = 250, 
        zoom = 0.7, theta = 10, phi = 30, windowsize = c(800, 800),shadow_intensity = 0,raytrace = FALSE,)
#Sys.sleep(0.2)
#render_snapshot(filename = "Figures/3d_height_Kittiwake.png",vignette = T)


