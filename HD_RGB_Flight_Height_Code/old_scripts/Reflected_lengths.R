#################################
### Code for figure 2 of flight height manuscript
require(foreach)
library(gridExtra)
library(ggpubr)
library(rayshader)
library(viridis)

calc_cv <- function(x,sokal.correction=F) {
  cv1 <- (sd(x,na.rm=T) / mean(x,na.rm=T))
  if(sokal.correction==TRUE){
    cv1 <- (1+(1/(4*length(x)))) * cv1
  }
  return(cv1*100)
}

spp.data <- function(datf){
  Tom <- datf %>% dplyr::filter(!is.na(Location),!is.na(`Plane Height`))
  Tom <- data.frame(Tom)
  
  out <- foreach(i=1:7,.combine='cbind')%do%{
    
    colnams <- sapply(c("R","G","B"),function(x) paste0("Frame.",i,".lengths.in.",x)) 
    colinds <- sapply(colnams,function(x) grep(names(Tom),pattern=x))
    
    oo <- foreach(j=1:nrow(Tom),.combine='rbind')%do%{
      R <- as.numeric(unlist(strsplit(Tom[j,colinds["R"]],"      ")))
      G <- as.numeric(unlist(strsplit(Tom[j,colinds["G"]],"      "))) 
      B <- as.numeric(unlist(strsplit(Tom[j,colinds["B"]],"      ")))
      Rmean <- mean(R,na.rm=T)
      Gmean <- mean(G,na.rm=T)
      Bmean <- mean(B,na.rm=T)
      return(c(Rmean,Gmean,Bmean))
    }
    
    lenmean <- rowMeans(oo,na.rm=T)
    
  }
  
  out <- data.frame(out)
  newout <- cbind(Tom[,1:15],out)
  newout$FrameX <- Tom$Frame.X
  newout$FrameY <- Tom$Frame.Y
  newout <- newout %>% dplyr::filter(!is.na(result.1),result.1<150)
  newout$cv <- sapply(1:nrow(newout),function(x) calc_cv(as.vector(unlist(newout[x,16:22])),sokal.correction=T))
  newout <- newout %>% dplyr::filter(cv < 10)
  newout$birdlen <- sapply(1:nrow(newout),function(x) max(newout[x,16:22],na.rm=T))
  
  cc <- data.frame(out=newout$birdlen)
  qt <- t(data.frame(qts = quantile(cc$out,c(0.025,0.05,0.95,0.975))))
  qt <- data.frame(qt)
  names(qt) <- c("llc","lc","uc","uuc")
  
  
  return(list(dfr=newout,cc=cc,qt=qt))
  
}


flist <- list.files(path="Data/Measured_by_Dayle/",pattern="*.xlsx",full.names = T)
newout <- data.table::rbindlist(lapply(flist,function(x) readxl::read_xlsx(x,sheet="Data")),
                      use.names=TRUE,fill=TRUE)
names(newout)[29] <- "Reflection"
unique(newout$Reflection)

filtered <- newout %>% dplyr::filter(Reflection == "Y")
unique(filtered$Species)

#saveRDS(filtered,"Data/Reflected_Birds_23_July_2020.rds")
#filtered <- readRDS("Data/Reflected_Birds_23_July_2020.rds")

GX <- filtered %>% dplyr::filter(Species == "Gannet")
KI <- filtered %>% dplyr::filter(Species == "Kittiwake")
HG <- filtered %>% dplyr::filter(Species == "Herring Gull")
LB <- filtered %>% dplyr::filter(Species == "Lesser black-backed Gull")
CG <- filtered %>% dplyr::filter(Species == "Common Gull")
FM <- filtered %>% dplyr::filter(Species == "Fulmar")



GX.data <- spp.data(GX)
KI.data <- spp.data(KI)

#############################################################################
# Relationship between direction of flight and length in reflected --------
#### Get mean lengths from reflection ####
mean(GX.data$dfr$birdlen)
mean(KI.data$dfr$birdlen)
#########################

GX.data$dfr %>% group_by(Behaviour) %>%
  summarise(mean(birdlen),n(),max(birdlen),min(birdlen))

GX.data$dfr %>% group_by(Camera) %>%
  summarise(mean(birdlen),n(),max(birdlen),min(birdlen))


KI.data$dfr %>% group_by(Behaviour) %>%
  summarise(mean(birdlen),n(),max(birdlen),min(birdlen))

KI.data$dfr %>% group_by(Camera) %>%
  summarise(mean(birdlen),n(),max(birdlen),min(birdlen))

KD.du <- KI.data$dfr %>% dplyr::filter(Behaviour %in% c("Flying D", "Flying U"))
KD.lr <- KI.data$dfr %>% dplyr::filter(Behaviour %in% c("Flying L", "Flying R"))
KD.dlru <- KI.data$dfr %>% dplyr::filter(Behaviour %in% c("Flying DL","Flying DR",
                                                          "Flying UL","Flying UR"))
ks.test(KD.du$birdlen,KD.lr$birdlen,alternative="greater")
ks.test(KD.du$birdlen,KD.dlru$birdlen,alternative="greater")
ks.test(KD.lr$birdlen,KD.dlru$birdlen,alternative="less")


GX.du <- GX.data$dfr %>% dplyr::filter(Behaviour %in% c("Flying D", "Flying U"))
GX.lr <- GX.data$dfr %>% dplyr::filter(Behaviour %in% c("Flying L", "Flying R"))
GX.dlru <- GX.data$dfr %>% dplyr::filter(Behaviour %in% c("Flying DL","Flying DR",
                                                          "Flying UL","Flying UR"))

ks.test(GX.du$birdlen,GX.lr$birdlen,alternative="greater")
ks.test(GX.du$birdlen,GX.dlru$birdlen,alternative="greater")
ks.test(GX.lr$birdlen,GX.dlru$birdlen,alternative="less")

################################################################################################

# Figure 2 - KI and GX density histograms NOT SPLIT -----------------------


GX.data$dfr$dir <- plyr::revalue(GX.data$dfr$Behaviour, c("Flying D" = "Perpendicular", "Flying U" = "Perpendicular",
                                           "Flying L" = "Parallel", "Flying R" = "Parallel",
                                           "Flying DL" = "Parallel","Flying DR" = "Parallel",
                                           "Flying UL" = "Parallel","Flying UR" = "Parallel")) 

KI.data$dfr$dir <- plyr::revalue(KI.data$dfr$Behaviour, c("Flying D" = "Perpendicular", "Flying U" = "Perpendicular",
                                                          "Flying L" = "Parallel", "Flying R" = "Parallel",
                                                          "Flying DL" = "Parallel","Flying DR" = "Parallel",
                                                          "Flying UL" = "Parallel","Flying UR" = "Parallel")) 

KI.data$dfr <- KI.data$dfr %>% dplyr::filter(Behaviour != "Flying (Direction Unknown)")

#### Get bird length ranges for reflection ####
KI.data$dfr %>% group_by(dir) %>% 
  summarise(mean(birdlen),quantile(birdlen,0.025),quantile(birdlen,0.975))
GX.data$dfr %>% group_by(dir) %>% 
  summarise(mean(birdlen),quantile(birdlen,0.025),quantile(birdlen,0.975))


GXplot2 <- ggplot(GX.data$dfr) +
  geom_density(aes(x = birdlen,fill=dir),alpha=0.6) +
  scale_x_continuous(limits=c(30,95),expand=c(0,0))+
  scale_y_continuous(expand=c(0,0),limits=c(0,0.06))+
  scale_fill_brewer(palette = "Greys")+
  xlab("Length (cm)")+
  ggthemes::theme_gdocs()+
  annotate("text",x=80,y=0.05,label="p << 0.001")+
  theme(
    panel.border=element_blank(),
    plot.background = element_blank(),
    legend.position = "bottom"
  )+ labs(fill = "Direction of flight")+
  ggtitle("a")

KIplot2 <- ggplot(KI.data$dfr) +
  geom_density(aes(x = birdlen,fill=dir),alpha=0.6) +
  scale_x_continuous(limits=c(20,50),expand=c(0,0))+
  scale_y_continuous(expand=c(0,0),limits=c(0,0.2))+
  scale_fill_brewer(palette = "Greys")+
  xlab("Length (cm)")+
  annotate("text",x=40,y=0.15,label="p = 0.241")+
  ggthemes::theme_gdocs()+
  theme(
    panel.border=element_blank(),
    plot.background = element_blank(),
    legend.position = "bottom"
  )+ labs(fill = "Direction of flight")+
  ggtitle('b')


DistPlot <- ggarrange(GXplot2,KIplot2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom")

ggsave(plot = DistPlot,filename="Figures/Ref.Length.Dir_Flight.png",device = "png",
       width = 9,height=5)




######################################################
### Testing for patterns in the X / Y 


GX.xyl <- ggplot(GX.data$dfr) +
  geom_point(aes(x = FrameX,y=FrameY,color=birdlen)) +
  #scale_x_continuous(limits=c(20,50),expand=c(0,0))+
  scale_y_continuous(breaks=seq(300,1600,by=300),limits=c(300,1600))+
  xlab("X coordinate")+
  ylab("Y coordinate")+
  scale_color_distiller(palette="Greys")+
  ggthemes::theme_gdocs()+
  theme(
    panel.border=element_blank(),
    plot.background = element_blank()
  ) + labs(color="Length (cm)")+ggtitle("a")

GX.xyl
KI.xyl <- ggplot(KI.data$dfr) +
  geom_point(aes(x = FrameX,y=FrameY,color=birdlen)) +
  #scale_x_continuous(limits=c(20,50),expand=c(0,0))+
  scale_y_continuous(breaks=seq(300,1600,by=300),limits=c(300,1600))+
  xlab("X coordinate")+
  ylab("Y coordinate")+
  scale_color_distiller(palette="Greys")+
  ggthemes::theme_gdocs()+
  theme(
    panel.border=element_blank(),
    plot.background = element_blank()
  ) + labs(color="Length (cm)")+ggtitle("b")

KI.xyl

#plot_gg(g, width = 5, height = 5, multicore = TRUE, scale = 150, 
#        zoom = 0.7, theta = 10, phi = 30, windowsize = c(800, 800),shadow_intensity = 0.9)

XYLplot <- ggarrange(GX.xyl,KI.xyl,
                       ncol=2, nrow=1)

ggsave(plot = XYLplot,filename="Figures/BL_by_XY.png",device = "png",
       width = 10,height=4)



# Density plots with percentiles ------------------------------------------

KIdat <- KI.data$dfr
KIdat.perp <- KIdat %>% dplyr::filter(dir == "Perpendicular")
KIdat.para <- KIdat %>% dplyr::filter(dir == "Parallel")

KIqt.perp <- t(data.frame(qts = quantile(KIdat.perp$birdlen,c(0.025,0.05,0.95,0.975))))
KIqt.perp <- data.frame(KIqt.perp)
names(KIqt.perp) <- c("llc","lc","uc","uuc")

KIqt.para <- t(data.frame(qts = quantile(KIdat.para$birdlen,c(0.025,0.05,0.95,0.975))))
KIqt.para <- data.frame(KIqt.para)
names(KIqt.para) <- c("llc","lc","uc","uuc")



GXdat <- GX.data$dfr
GXdat.perp <- GXdat %>% dplyr::filter(dir == "Perpendicular")
GXdat.para <- GXdat %>% dplyr::filter(dir == "Parallel")

GXqt.perp <- t(data.frame(qts = quantile(GXdat.perp$birdlen,c(0.025,0.05,0.95,0.975))))
GXqt.perp <- data.frame(GXqt.perp)
names(GXqt.perp) <- c("llc","lc","uc","uuc")

GXqt.para <- t(data.frame(qts = quantile(GXdat.para$birdlen,c(0.025,0.05,0.95,0.975))))
GXqt.para <- data.frame(GXqt.para)
names(GXqt.para) <- c("llc","lc","uc","uuc")



GXplot3a <- ggplot(GXdat.perp) +
  geom_density(aes(x = birdlen),alpha=0.6,fill=NA) +
  scale_x_continuous(limits=c(30,95),expand=c(0,0))+
  scale_y_continuous(expand=c(0,0),limits=c(0,0.06))+
  geom_vline(data=GXqt.perp,aes(xintercept=llc),linetype='dashed')+
  geom_vline(data=GXqt.perp,aes(xintercept=uuc),linetype='dashed')+
  xlab("Length (cm)")+
  ggthemes::theme_gdocs()+
  theme(
    panel.border=element_blank(),
    plot.background = element_blank()
  )+
  ggtitle("a")


GXplot3b <- ggplot(GXdat.para) +
  geom_density(aes(x = birdlen),alpha=0.6,fill=NA) +
  scale_x_continuous(limits=c(30,95),expand=c(0,0))+
  scale_y_continuous(expand=c(0,0),limits=c(0,0.06))+
  geom_vline(data=GXqt.para,aes(xintercept=llc),linetype='dashed')+
  geom_vline(data=GXqt.para,aes(xintercept=uuc),linetype='dashed')+
  xlab("Length (cm)")+
  ggthemes::theme_gdocs()+
  theme(
    panel.border=element_blank(),
    plot.background = element_blank()
  )+
  ggtitle("b")



KIplot3a <- ggplot(KIdat.perp) +
  geom_density(aes(x = birdlen),alpha=0.6,fill=NA) +
  scale_x_continuous(limits=c(20,50),expand=c(0,0))+
  scale_y_continuous(expand=c(0,0),limits=c(0,0.2))+
  geom_vline(data=KIqt.perp,aes(xintercept=llc),linetype='dashed')+
  geom_vline(data=KIqt.perp,aes(xintercept=uuc),linetype='dashed')+
  xlab("Length (cm)")+
  ggthemes::theme_gdocs()+
  theme(
    panel.border=element_blank(),
    plot.background = element_blank()
  )+
  ggtitle("c")


KIplot3b <- ggplot(KIdat.para) +
  geom_density(aes(x = birdlen),alpha=0.6,fill=NA) +
  scale_x_continuous(limits=c(20,50),expand=c(0,0))+
  scale_y_continuous(expand=c(0,0),limits=c(0,0.2))+
  geom_vline(data=KIqt.para,aes(xintercept=llc),linetype='dashed')+
  geom_vline(data=KIqt.para,aes(xintercept=uuc),linetype='dashed')+
  xlab("Length (cm)")+
  ggthemes::theme_gdocs()+
  theme(
    panel.border=element_blank(),
    plot.background = element_blank()
  )+
  ggtitle("d")

DistPlot2 <- ggarrange(GXplot3a,KIplot3a,
                       GXplot3b,KIplot3b,
                      ncol=2, nrow=2)

ggsave(plot = DistPlot2,filename="Figures/Ref.Length.Dir_Flight_all.png",device = "png",
       width = 6,height=6)

