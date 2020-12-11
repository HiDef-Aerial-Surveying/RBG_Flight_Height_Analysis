source("Scripts/Helpers.R")
Load_Libs(c("readxl","foreach",
            "data.table","rayshader","raster",
            "viridis","sp","rgdal","gstat",
            "spatialEco","hablar","tidyverse","HTSSIP"))

Turbine.inputs <- readxl::read_xlsx("Data/Input_data.xlsx",sheet="Blades")
rotor.range <- Turbine.inputs$Value
Species.inputs <- readxl::read_xlsx("Data/Input_data.xlsx",sheet="Species")

spp <- "Lesser black-backed Gull"

flist <- list.files(path="Data/All_GWF/",full.names=T)

shapearea <- readOGR(dsn="Data/Shape",
                     layer="GG_merged_dissolve")
shapearea <- spTransform(shapearea,CRS("+init=epsg:4326"))

e <- extent(shapearea)
# coerce to a SpatialPolygons object
p <- as(e, 'SpatialPolygons')  
proj4string(p) <- CRS("+init=epsg:4326")



dat <- merge.files(flist)

data_mod <- calc.flight.height(dat,spp)



rr <- produce.raster(data_mod,p,
                     month=NULL,grd.density=50000,
                     idp=3.0, crs=CRS("+init=epsg:4326"))

pointdat <- data.frame(rr$dmodt)
pointdatshp <- rr$dmodt
rr <- rr$rr

outdf <- create.df.to.plot(rr,threshold = 0)



GWF4kout <- readOGR(dsn="Data/Shape",
                     layer="Galloper_merge")
GWF4kout <- spTransform(GWF4kout,CRS("+init=epsg:4326"))
GWF4koutdf <- fortify(GWF4kout)




g <- ggplot() +  
  geom_tile(data=outdf, aes(x=x, y=y, fill=value), alpha=0.8) +
  #geom_polygon(aes(x = long, y = lat, group = group), data = GWF4koutdf,fill=NA,  
  #             col = "black") +
  #geom_point(data=pointdat,aes(x=Lon,y=Lat,color=Percent_Overlap),pch=21)+
  scale_fill_viridis() +
  coord_equal() +
  ggthemes::theme_map()+
  theme(legend.position="right",
        legend.title = element_blank(),
        panel.background = element_rect(fill = "lightblue")
  )

plot_gg(g, width = 5, height = 5, raytrace = FALSE, preview = TRUE)

plot_gg(g, width = 5, height = 5, multicore = TRUE, scale = 250, 
        zoom = 0.7, theta = 10, phi = 30, windowsize = c(800, 800))
Sys.sleep(0.2)
render_snapshot()









# Weighted KDE using cadmium and raster object to define grid
rt <- rr
rt[] <- rep(1,raster::ncell(rt))
dens.kde <- sp.kde(x = pointdatshp, bw = 0.05,     #y = pointdatshp$Percent_Overlap <- for weights?
                      newdata = rt, standardize = TRUE )
dens.rr <- raster::mask(dens.kde, shapearea)



test <- dens.rr * rr




outdf2 <- create.df.to.plot(test,threshold = 0)



g <- ggplot() +  
  geom_tile(data=outdf2, aes(x=x, y=y, fill=value), alpha=0.8) +
  geom_polygon(aes(x = long, y = lat, group = group), data = GWF4koutdf,fill=NA,  
               col = "black") +
  #geom_point(data=pointdat,aes(x=Lon,y=Lat,color=Percent_Overlap),pch=21)+
  scale_fill_viridis() +
  coord_equal() +
  ggthemes::theme_map()+
  theme(legend.position="right",
        legend.title = element_blank(),
        panel.background = element_rect(fill = "lightblue")
  )

plot_gg(g, width = 5, height = 5, raytrace = FALSE, preview = TRUE)

plot_gg(g, width = 5, height = 5, multicore = TRUE, scale = 250, 
        zoom = 0.7, theta = 10, phi = 30, windowsize = c(800, 800))
