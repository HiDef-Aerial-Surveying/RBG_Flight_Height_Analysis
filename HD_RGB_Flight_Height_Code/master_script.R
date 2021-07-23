##################################################################
#### MASTER SCRIPT for running the RGB flight height analysis ####
#### Author: Grant Humphries
#### Date: December 9, 2020
#### Email: grant.humphries@hidefsurveying.co.uk
#### Github: https://github.com/HiDef-Aerial-Surveying/RBG_Flight_Height_Analysis
##################################################################


# Source script -----------------------------------------------------------
source("HD_RGB_Flight_Height_Code/helper.scripts/Bootstrap_method_helpers.R")


# Load libraries ----------------------------------------------------------
## Uses the load_libs function which will install a package if it's not already

lib_list <- c("readxl","foreach","gridExtra","ggpubr","rayshader","viridis","raster",
              "gstat","rgdal","sp","spatialEco","HTSSIP","hablar",
              "data.table","tidyverse","boot","GPArotation","psych","cowplot")


Load_Libs(lib_list)

# Hard coded variables ----------------------------------------------------

###################################
## TYPE IN SPECIES NAME HERE
SPP <- "Kittiwake"
###################################
## TYPE IN THE WORKSPACE WHERE YOUR BIRDS AT HEIGHT SPREADSHEETS ARE STORED
SSpath <- "HD_RGB_Flight_Height_Code/data.to.process/"
###################################
## TYPE IN THE WORKSPACE WHERE THE OBSERVATION SPREADSHEETS ARE STORED
OBpath <- "HD_RGB_Flight_Height_Code/observation.data/"
###################################
## TYPE IN THE FULL PATH TO THE BOUNDARY SHAPEFILE
boundary.shapefile <- "HD_RGB_Flight_Height_Code/boundary.shapefile/Seagreen_Phase_2_and_3_12km_buffer.shp"
###################################
## TYPE IN THE LOW AND HIGH POINT OF THE TURBINE BLADES
turbine.low <- 25
turbine.high <- 150
###################################


WGS84 <- sf::st_crs(4326)$proj4string



# Build reflection database -----------------------------------------------

reflist <- list.files(path="Reflection_Database/Data_to_build_20perc_database/",full.names = T)
refout <- data.table::rbindlist(lapply(reflist,function(x) readxl::read_xlsx(x,sheet="Data")),
                                use.names=TRUE,fill=TRUE)
names(refout)[29] <- "Reflection"
unique(refout$Reflection)
### Get only the birds that have reflection... 
refout <- refout[refout$Reflection=="Y",]
## Check sample size for each species
refout %>% count(Species)

## Check species names!! If  
unique(refout$Species)
## Filter the reflection data by the species of interest
refs <- refout[refout$Species == SPP,]

### This builds the reflection data
Dat.reflect <- get.refl.lengths(refs)


# Get worksheets into data frame ------------------------------------------
mlist <- list.files(path=SSpath,full.names = T)

## Combines the data in the 
datout <- data.table::rbindlist(lapply(mlist,function(x){
  tt <- readxl::read_xlsx(x,sheet="Data")
  tt <- tt %>% dplyr::select(-`Identification Date`)
  return(tt)}),use.names=TRUE,fill=TRUE)

names(datout)[29] <- "Reflection"
unique(datout$Reflection)



# Get observation data sheets to get Lat / Lons ---------------------------

olist <- list.files(path=OBpath,full.names = T)

## Combines the data in the 
obsout <- data.table::rbindlist(lapply(olist,function(x){
  tt <- readxl::read_xlsx(x)
  return(tt)}),use.names=TRUE,fill=TRUE)




# Get Lat Lons ------------------------------------------------------------

#Problem filtering datout due to duplicate reflection columns. Worth knowing for future...
#names(datout)
#datout[,67] <- as.factor(datout[,67])
#summary(datout[,29])
#summary(datout[,67])
datout <- datout[,-67]

SppDat <- datout %>% dplyr::filter(Species == SPP,)
ObsDat <- obsout %>% dplyr::filter(Species == SPP,)

### Until there's a better way to match, we just have to assume a 1:1 match
SppDat$Latitude <- ObsDat$Latitude
SppDat$Longitude <- ObsDat$Longitude



# Get bird lengths --------------------------------------------------------

## Gets the birds lengths and puts them into a dataframe for inputting into the flight height calcs
## This is where you might want to check how many 
SppDat <- SppDat %>% dplyr::filter(Reflection %in% c(NA, "No"))
SppDat <- get.lengths(SppDat)



# Calculate flight heights ------------------------------------------------
## Running this should calculate flight heights for the species of interest
## This process can take a while depending on how many birds need to be done

fhdata <- get.fhs(SppDat,Dat.reflect,bootsize = 1000)





# Plotting ----------------------------------------------------------------

### Generates the FH distribution plot
Dat.plot <- plot.fhs(SPP,fhdata,turbine.low,turbine.high)
Dat.plot


SppDat$low.height <- fhdata$lwheight
SppDat$mid.height <- fhdata$mdheight
SppDat$high.height <- fhdata$hiheight

monthdat <- months(SppDat$Date)



###############################
### If you have altered this code to do multiple species, you could merge them into
### a single data frame here and then create the box plot
### For example: If you ran this and created SppDat.GX for Gannet and SppDat.KI for Kittiwake
### 
### all.frame <- rbind(SppDat.GX,SppDat.KI)

all.frame <- SppDat %>% dplyr::select(Species,low.height,mid.height,high.height)
all.frame$month <- monthdat

## Gathers the frame into LONG format for plotting
all.frame <- gather(all.frame,category,out,low.height:high.height)
all.frame$category <- plyr::revalue(all.frame$category,c("low.height"="Low",
                                                         "high.height"="High",
                                                         'mid.height'="Mean"))

all.frame$category <- factor(all.frame$category, levels=c("Low","Mean","High"))



# Calculate birds at PCH --------------------------------------------------
#### PCH estimate ####

all.frame %>% nest(data=c(out)) %>%
  mutate(
    pch = map_dbl(data,~length(which(.x$out > turbine.low & .x$out < turbine.high))/length(.x$out))
  ) %>% group_by(Species,category) %>% summarise(PCH = mean(pch))




all.frame %>% nest(data=c(out,month)) %>%
  mutate(
    pch = map_dbl(data,~length(which(.x$out > turbine.low & .x$out < turbine.high))/length(.x$out))
  )

all.frame %>% group_by(species,category) %>% summarise(mean(out))





Bplot <- ggplot(all.frame) +
  geom_boxplot(aes(x=category,y=out,fill=Species),width=0.5,position= position_dodge(width=1))+
  stat_summary(aes(x=category,y=out,group=Species),
               fun=mean, geom="point", shape=18, size=3.5, color="black",position= position_dodge(width=1))+
  scale_fill_manual(name="Species",values=c("grey30","grey60"))+
  ggthemes::theme_gdocs()+
  ylab("Height (m)")+xlab("")+
  theme(
    panel.border = element_blank()
  )
Bplot




# The 3D plotting method --------------------------------------------------


shapearea <- raster::shapefile(boundary.shapefile)
shapearea <- spTransform(shapearea,WGS84)
e <- extent(shapearea)
# coerce to a SpatialPolygons object
p <- as(e, 'SpatialPolygons')  
proj4string(p) <- sf::st_crs(4326)$proj4string

data_mod <- SppDat
names(data_mod)[which(names(data_mod)=="Latitude")] <- "Lat"
names(data_mod)[which(names(data_mod)=="Longitude")] <- "Lon"


rr <- produce.raster(data_mod,p,
                     month=NULL,grd.density=50000,
                     idp=3.0, crs=sf::st_crs(4326)$proj4string)

pointdat <- data.frame(rr$dmodt)
pointdatshp <- rr$dmodt
rr <- rr$rr



rR <- raster::mask(rr,shapearea)

outdf <- create.df.to.plot(rR,threshold = 0)
shapefileDF <- fortify(shapearea)



g <- ggplot() +  
  geom_tile(data=outdf, aes(x=x, y=y, fill=value), alpha=0.8) +
  geom_polygon(aes(x = long, y = lat, group = group), data = shapefileDF,fill=NA,  
               col = "black") +
  #geom_point(data=data_mod,aes(x=Lon,y=Lat),pch=19)+
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


plot_gg(g,multicore=TRUE,width=6,height=4,scale=280) 
render_snapshot()



