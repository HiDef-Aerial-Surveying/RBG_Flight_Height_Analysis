######################################
### 3d figure of flight heights
####

source("Scripts/Helpers.R")
Load_Libs(c("readxl","foreach",
            "data.table","rayshader","raster",
            "viridis","sp","rgdal","gstat",
            "spatialEco","hablar","tidyverse","HTSSIP",
            "gridExtra","ggpubr","GPArotation","plotly","psych"))


obs <- readxl::read_xlsx("data/Zone99_M06_surveys_Merge_for_LatLons/Observations/Zone99_M06_S01_D01_19_Observations_MSS_21082019.xlsx")
obs$Camera <- as.character(obs$Camera)
flist <- list.files(path="data/Zone99_M06_surveys_Merge_for_LatLons/",pattern="*.xlsx",full.names=T)
dat <- rbindlist(lapply(flist,read_xlsx),fill=TRUE)
dat$Camera <- plyr::revalue(as.character(dat$Camera),c("5" = "1","8" = "4", "7" = "3"))
t1 <- obs %>% dplyr::filter(Species == 'Kittiwake')
dat <- dat %>% dplyr::filter(Species == 'Kittiwake')
dat$Latitude <- t1$Latitude
dat$Longitude <- t1$Longitude
names(dat)[29] <- "Reflection"


reflist <- list.files(path="Data/Measured_by_Dayle/",full.names = T)
refout <- data.table::rbindlist(lapply(reflist,function(x) readxl::read_xlsx(x,sheet="Data")),
                                use.names=TRUE,fill=TRUE)
names(refout)[29] <- "Reflection"
unique(refout$Reflection)

refout <- refout[refout$Reflection=="Y",]
KIrefs <- refout[refout$Species == "Kittiwake",]

#### CHECK THESE BECAUSE THERE WAS A PROBLEM WITH CALIBRATION...
KI.reflect <- get.refl.lengths(KIrefs)

KI <- dat %>% dplyr::filter(Species == "Kittiwake",Reflection %in% c(NA, "No"))

KI <- get.lengths(KI)  

KI.fhdata <- get.fhs(KI,KI.reflect)

KI$low.height <- KI.fhdata$lwheight
KI$mid.height <- KI.fhdata$mdheight
KI$high.height <- KI.fhdata$hiheight


shapearea <- dat
coordinates(shapearea) <- ~Longitude+Latitude
proj4string(shapearea) <- sf::st_crs(4326)$proj4string
e <- extent(shapearea)
# coerce to a SpatialPolygons object
p <- as(e, 'SpatialPolygons')  
proj4string(p) <- sf::st_crs(4326)$proj4string


data_mod <- KI
names(data_mod)[68:69] <- c("Lat","Lon")

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
  theme(legend.position="right",
        #legend.title = element_blank(),
        axis.title = element_text(),
        panel.background = element_blank(),
        panel.border = element_rect(colour="black",fill="NA")
  )

g


plot_gg(g,multicore=TRUE,width=6,height=4,scale=310) 

render_movie("Figures/Flight_Heights_animated.mp4",fps=60,zoom=0.9)

n_frames <- 180
thetas <- transition_values(from = -45, to = -135, steps = n_frames)
zscale <- 50
save_3d_gif(g, file = "Figures/Flight_heights_animated.gif", duration = 6,
               solid = TRUE, shadow = TRUE, zscale = zscale,
               theta = thetas, phi = 45)

save_3d_gif()



