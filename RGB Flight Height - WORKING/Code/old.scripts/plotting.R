
# Lowering MSFA flight height to 0-10 -------------------------------------
SppDat$msfa <- ifelse(grepl("MSFA|msfa|Mixed|Assoc", SppDat$AddComs), TRUE, FALSE)

SppDat$mid.height <- ifelse(SppDat$msfa, 0, fhdata$mdheight)
SppDat$low.height <- ifelse(SppDat$msfa, 0, fhdata$lwheight)
SppDat$high.height <- ifelse(SppDat$msfa, 0, fhdata$hiheight)


# Removing outliers for birds not in MSFA ---------------------------------

SppDat$meanmeasure <- unlist(lapply(SppDat$lengthvals$values, mean, na.rm = TRUE))

test <- SppDat %>% 
  nest(data=c(-Species, -month))
unlist(lapply(test$data, nrow)) # to get the sample sizes for each species/month

'%!in%' <- function(x,y)!('%in%'(x,y))
#test$newdata <- NA
new.dat <- c()
for(i in 1:nrow(test)){
  
  datf <- as.data.frame(test$data[i])
  
  tt <- as.tibble(datf[which(datf$meanmeasure %!in% c(boxplot.stats(datf$meanmeasure[which(datf$msfa == FALSE)])$out)),])
  tt1 <- as.tibble(cbind(test$Species[i], test$month[i], tt))
  new.dat <- rbind(new.dat, tt1)
  
}

names(new.dat)[1:2] <- c("Species", "month")
new.dat$month <- factor(new.dat$month, levels = c("July", "August"))
fhdata1 <- select(new.dat, c("low.height", "mid.height", "high.height", "Species", "month"))
names(fhdata1) <- c("lwheight", "mdheight", "hiheight", "species", "month")

#SppDat %>% group_by(Species, month) %>% filter()

# Creating datasets for different species ---------------------------------

#SppDat.KI <- SppDat
#SppDat.GX <- SppDat

###############################
### If you have altered this code to do multiple species, you could merge them into
### a single data frame here and then create the box plot
### For example: If you ran this and created SppDat.GX for Gannet and SppDat.KI for Kittiwake
### 
#SppDat <- rbind(SppDat.GX,SppDat.KI)

all.frame <- new.dat %>% dplyr::select(Species,low.height,mid.height,high.height,month)
#all.frame$month <- monthdat

## Gathers the frame into LONG format for plotting
all.frame <- gather(all.frame,category,out,low.height:high.height)
all.frame$category <- plyr::revalue(all.frame$category,c("low.height"="Low",
                                                         "high.height"="High",
                                                         'mid.height'="Mean"))

all.frame$category <- factor(all.frame$category, levels=c("Low","Mean","High"))


#ss = map(data, ~nrow(.x$data))

# Calculate birds at PCH --------------------------------------------------
#### PCH estimate ####
# Calculations for Table 3 in Seagreen report -----------------------------

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
    ) %>% group_by(Species,month,category) %>% 
  summarise(ss = ss, 
            mn = mn, 
            CI = paste("(", round(mn-CIs, digits = 1),"-", round(mn+CIs, digits = 1),")", sep = ""), 
            mdn = mdn,
            IQR = paste(round(IQR25, digits = 1), "-", round(IQR75, digits = 1), sep = ""),
            PCHsmall = mean(pchS)*100, 
            PCHlarge = mean(pchL)*100)



# Code for making Table 4 in report ---------------------------------------
SppDat1 %>% nest(-Species, -month) -> aa
new.dat %>% nest(-Species, -month) -> bb
measured <- unlist(lapply(aa$data, nrow))
estimated <- unlist(lapply(bb$data, nrow))

percm <- estimated/measured*100

cbind(aa[,1], aa[,2], measured, estimated, percm)

#all.frame %>% nest(data=c(out,month)) %>%
#  mutate(
#    pch = map_dbl(data,~length(which(.x$out > turbine.low & .x$out < turbine.high))/length(.x$out))
#  )

#all.frame %>% group_by(Species,category) %>% summarise(mean(out))


all.frame.GX <- all.frame[which(all.frame$Species == "Gannet"),]
all.frame.KI <- all.frame[which(all.frame$Species == "Kittiwake"),]

Bplot <- ggplot(all.frame.GX) +
  geom_boxplot(aes(x=category,y=out, fill = month),width=0.5,position= position_dodge(width=1))+
  geom_hline(yintercept=turbine.low,linetype="dashed",size=1)+
  geom_hline(yintercept=turbine.high,linetype="dashed",size=1)+
  geom_hline(yintercept=turbine.max,linetype="dashed",size=1)+
  stat_summary(aes(x=category,y=out,group=Species),
               fun=mean, geom="point", shape=18, size=3.5, color="black",position= position_dodge(width=1))+
  scale_fill_manual(values= c("grey60", "grey60"))+
  ggthemes::theme_gdocs()+
  ylab("Height (m)")+xlab("")+
  facet_wrap(month ~ ., ncol = 1) +
  theme(
    panel.border = element_blank(),
    legend.position = "none",
    strip.text.x = element_text(size = 12, face = "bold", colour = "black")
  )
Bplot


# Code for blue plots -----------------------------------------------------

#Dat.plot <- plot.fhs(SPP,fhdata,turbine.low,turbine.high)
fhdata.GX <- fhdata1[which(fhdata1$species == "Gannet"),]
fhdata.KI <- fhdata1[which(fhdata1$species == "Kittiwake"),]

SPP <- "Gannet"
specs <- paste(SPP, ": low = ", turbine.low, ", high =" , turbine.high, sep = " ")

Dat.plot <- plot.fhs(SPP,fhdata.GX,turbine.low,turbine.high,turbine.max)
Dat.plot


# anonymising data --------------------------------------------------------

fhdata.anon <- fhdata.GX
fhdata.anon <- fhdata.anon[which(fhdata.anon$month == "August"),]
anon <- rnorm(fhdata.anon$mdheight, mean = 0, sd = 15)
fhdata.anon[,c(1:3)] <- fhdata.anon[,c(1:3)] + anon
fhdata.anon[fhdata.anon < 0] <- 0
fhdata.anon <- sample_n(fhdata.anon, dim(fhdata.anon)[1]*0.75)

Dat.plot.anon <- plot.fhs(SPP,fhdata.anon,turbine.low,turbine.high,turbine.max)
Dat.plot.anon

# The 3D plotting method --------------------------------------------------

SppDatG7 <- dplyr::filter(new.dat, Species == "Gannet", month == "July")
SppDatG8 <- dplyr::filter(new.dat, Species == "Gannet", month == "August")
SppDatK7 <- dplyr::filter(new.dat, Species == "Kittiwake", month == "July")
SppDatK8 <- dplyr::filter(new.dat, Species == "Kittiwake", month == "August")

shapearea <- raster::shapefile(boundary.shapefile)
shapearea <- spTransform(shapearea,WGS84)
e <- extent(shapearea)
# coerce to a SpatialPolygons object
p <- as(e, 'SpatialPolygons')  
proj4string(p) <- sf::st_crs(4326)$proj4string

data_mod <- SppDatK7
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



gk7 <- ggplot() +  
  geom_tile(data=outdf, aes(x=x, y=y, fill=value), alpha=0.8) +
  geom_polygon(aes(x = long, y = lat, group = group), data = shapefileDF,fill=NA,  
               col = "black") +
  #geom_point(data=data_mod,aes(x=Lon,y=Lat),pch=19)+
  scale_fill_viridis() +
  labs(fill = "Height (m)",x="Longitude",y="Latitude")+
  coord_equal() +
  ggthemes::theme_map()+
  ggtitle("July") +
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



