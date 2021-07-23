##################################################################
#### MASTER SCRIPT for plotting the RGB flight height analysis ####
#### Author: Grant Humphries
#### Date: December 9, 2020
#### Email: grant.humphries@hidefsurveying.co.uk
#### Github: https://github.com/HiDef-Aerial-Surveying/RBG_Flight_Height_Analysis
##################################################################


# Source script -----------------------------------------------------------
source("Code/helper.scripts/Bootstrap_method_helpers_KK.R")
source("Code/config.R")
'%!in%' <- function(x,y)!('%in%'(x,y))

# Load libraries ----------------------------------------------------------
## Uses the load_libs function which will install a package if it's not already

lib_list <- c("readxl","foreach","gridExtra","ggpubr","rayshader","viridis","raster",
              "gstat","rgdal","sp","spatialEco","HTSSIP","hablar",
              "data.table","tidyverse","boot","GPArotation","psych","cowplot")


Load_Libs(lib_list)


# Import saved bootstrap and measured data --------------------------------
# There are two data frames here to compare the proportion of measured birds with estimated birds

# bootstrapped flight heights
reflist <- list.files("Code/outputs/SppDat", full.names = TRUE)

SppDat <- data.table::rbindlist(lapply(reflist,function(x) readxl::read_xlsx(x,sheet="Sheet1")),
                      use.names=TRUE,fill=TRUE)

SppDat <- SppDat[order(SppDat$`Survey Date`),]

# This creates column to split between years 1 and 2 if needed. 
SppDat$yr <- "Year 1"

SppDat$yr[as.Date(SppDat$`Survey Date`) %!in% Date1:Date2] <- "Year 2"

# all data
measurelist <- list.files("Code/outputs/MeasureDat", full.names = TRUE)

MeasureDat <- rbindlist(lapply(measurelist, function(x) readxl::read_xlsx(x,sheet = "Sheet1")),
                        use.names = TRUE, fill = TRUE)

MeasureDat <- MeasureDat[order(MeasureDat$`Survey Date`),]

# This creates column to split between years 1 and 2 if needed. 
MeasureDat$yr <- "Year 1"

MeasureDat$yr[as.Date(MeasureDat$`Survey Date`) %!in% Date1:Date2] <- "Year 2"

load(file = "Code/outputs/reflectcount")


# Order data --------------------------------------------------------------

# Table - measured versus estimated + reflection --------------------------

MeasureDat %>% nest(-Species, -month, -year, -yr) -> aa

SppDat %>% nest(-Species, -month, -year, -yr) -> bb

cc <- full_join(aa, bb, by = c("Species", "month", "year", "yr"))
cc <- inner_join(cc, reflectcount, by = "Species") # add the number of reflected birds

cc$data.x <- unlist(lapply(cc$data.x, nrow))

for (i in 1:dim(cc)[1]){
cc$data.y1[i] <- unlist(ifelse(is.null(cc$data.y[[i]]), 0, nrow(cc$data.y[[i]])))
}

cc$data.y <- cc$data.y1
cc$percm <- round(cc$data.y/cc$data.x*100, 1)

cc <- cc[,c(1,2,3,5,6,9,7,4)]
cc$enough <- ifelse(cc[,7] > 24, "Yes", "No")

cc <- as.data.table(cc)
cc$month <- paste(cc$month, cc$year, sep = " ")
cc <- subset(cc, select = -year)

names(cc) <- c("Species", "month", "measured", "estimated", "percm", "n", "year", "enough")



#cc$month <- factor(cc$month, levels = c("March", "April", "May", "June", "July", "August", "September", "October", "November", "December", "January", "February"))

## AUTOMATE this part if needed
# Still need to order the months to be in the right order.
cc %>% filter(Species == "kittiwake")


# Calculate birds at PCH --------------------------------------------------
#### PCH estimate ####

all.frame <- SppDat %>% dplyr::select(Species,lwheight,mdheight,hiheight,month, year, yr)

## Gathers the frame into LONG format for plotting
all.frame <- gather(all.frame,category,out,lwheight:hiheight)
all.frame$category <- plyr::revalue(all.frame$category,c("lwheight"="Low",
                                                         "mdheight"="Mean",
                                                         'hiheight'="High"))

all.frame$category <- factor(all.frame$category, levels=c("Low","Mean","High"))

all.frame$yr <- factor(all.frame$yr, levels = c("Year 1", "Year 2"))

all.frame$month <- factor(all.frame$month, levels = c("March", "April", "May", "June", "July", "August", "September", "October", "November", "December", "January", "February"))


# Calculations for PCH table -----------------------------

std <- function(x) sd(x)/sqrt(length(x))

all.frame %>% nest(data=c(out)) %>%
  mutate(
    ss = map_dbl(data, ~length(.x$out)),
    mn = map_dbl(data, ~mean(.x$out)),
    CIs = map_dbl(data, ~std(.x$out)*1.96), 
    mdn = map_dbl(data, ~median(.x$out)),
    IQR25 = map_dbl(data, ~quantile(.x$out, 0.25)),
    IQR75 = map_dbl(data, ~quantile(.x$out, 0.75)),
    pchS = map_dbl(data,~length(which(.x$out > turbine.low & .x$out < turbine.high))/length(.x$out)),
    pchL = map_dbl(data,~length(which(.x$out > turbine.low & .x$out < turbine.max))/length(.x$out)),
  ) %>% group_by(Species,yr,month,year,category) %>% 
  summarise(ss = ss, 
            mn = round(mn, 1), 
            CI = paste("(", round(mn-CIs, digits = 1),"-", round(mn+CIs, digits = 1),")", sep = ""), 
            mdn = round(mdn, 1),
            IQR = paste(round(IQR25, digits = 1), "-", round(IQR75, digits = 1), sep = ""),
            PCHsmall = round(mean(pchS)*100, 1),
            PCHlarge = round(mean(pchL)*100)) -> pchTable




# Boxplots ---------------------------------


Bplot <- all.frame %>% filter(Species == SPP[i]) %>%
 ggplot() +
  geom_boxplot(aes(x=category,y=out, fill = month),width=0.5,position= position_dodge(width=1))+
  geom_hline(yintercept=turbine.low,linetype="dashed",size=1)+
  geom_hline(yintercept=turbine.high,linetype="dashed",size=1)+
  geom_hline(yintercept=turbine.max,linetype="dashed",size=1)+
  stat_summary(aes(x=category,y=out,group=Species),
               fun=mean, geom="point", shape=18, size=3.5, color="black",position= position_dodge(width=1))+
  scale_fill_manual(values= c("grey60", "grey60", "grey60"))+
  ggthemes::theme_gdocs()+
  ylab("Height (m)")+xlab("")+
  facet_wrap(month ~ ., ncol = 3) +
  theme(
    panel.border = element_blank(),
    legend.position = "none",
    strip.text.x = element_text(size = 12, face = "bold", colour = "black")
  )
Bplot


# Just presenting mean, to look at general trend
# Figure out way to label each box

Mplot <- all.frame %>% filter(Species == SPP[i], category == "Mean") %>%
  ggplot() +
  geom_boxplot(aes(x=month,y=out),fill = "grey60", width=0.5,position= position_dodge(width=1))+
  geom_hline(yintercept=turbine.low,linetype="dashed",size=1)+
  geom_hline(yintercept=turbine.high,linetype="dashed",size=1)+
  geom_hline(yintercept=turbine.max,linetype="dashed",size=1)+
  stat_summary(aes(x=month,y=out),
               fun=mean, geom="point", shape=18, size=3.5, color="black",position= position_dodge(width=1))+
  ggthemes::theme_gdocs()+
  ylab("Height (m)")+xlab("")+
  theme(
    panel.border = element_blank(),
    legend.position = "none",
    strip.text.x = element_text(size = 12, face = "bold", colour = "black"))
Mplot


# Code for blue plots -----------------------------------------------------

#Dat.plot <- plot.fhs(SPP,fhdata,turbine.low,turbine.high)
SppDatSPP <- SppDat[which(SppDat$Species == SPP[i]),]

Dat.plot <- plot.fhs(SPP[i],SppDatSPP,turbine.low,turbine.high,turbine.max)
Dat.plot

# The 3D plotting method --------------------------------------------------

SppDatSPP <- dplyr::filter(SppDat, Species == "gannet", month == "May")

shapearea <- raster::shapefile(boundary.shapefile)
shapearea <- spTransform(shapearea,WGS84)
e <- extent(shapearea)
# coerce to a SpatialPolygons object
p <- as(e, 'SpatialPolygons')  
proj4string(p) <- sf::st_crs(4326)$proj4string

data_mod <- SppDatSPP
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



ggplot() +  
  geom_tile(data=outdf, aes(x=x, y=y, fill=value), alpha=0.8) +
  geom_polygon(aes(x = long, y = lat, group = group), data = shapefileDF,fill=NA,  
               col = "black") +
  #geom_point(data=data_mod,aes(x=Lon,y=Lat),pch=19)+
  scale_fill_viridis() +
  labs(fill = "Height (m)",x="Longitude",y="Latitude")+
  coord_equal() +
  ggthemes::theme_map()+
  ggtitle("May") +
  theme(legend.position="right",
        #legend.title = element_blank(),
        plot.title = element_text(size = 12, face = "bold", colour = "black"),
        axis.title = element_text(),
        panel.background = element_blank(),
        panel.border = element_rect(colour="black",fill="NA")
  )

grid.arrange(gk7, gk8, ncol = 1)
grid.arrange(gg7, gg8, ncol = 1)
#ggpubr
#inner margins 

plot_gg(gk8,multicore=TRUE,width=6,height=4,scale=280) 
render_snapshot()



