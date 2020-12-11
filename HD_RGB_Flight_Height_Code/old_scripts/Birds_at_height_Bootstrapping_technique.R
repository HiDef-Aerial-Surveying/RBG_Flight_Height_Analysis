###################################
### Galloper flight heights for Manuscript
##################

source("Scripts/Bootstrap_method_helpers.R")

require(foreach)
library(gridExtra)
library(ggpubr)
library(rayshader)
library(viridis)
library(data.table)
library(tidyverse)
library(ggpubr)
library(boot)
library(GPArotation)
library(plotly)
library(psych)
#library(cowplot)
library(magick)



reflist <- list.files(path="Data/Measured_by_Dayle/",full.names = T)
refout <- data.table::rbindlist(lapply(reflist,function(x) readxl::read_xlsx(x,sheet="Data")),
                                use.names=TRUE,fill=TRUE)
names(refout)[29] <- "Reflection"
unique(refout$Reflection)

refout <- refout[refout$Reflection=="Y",]
KIrefs <- refout[refout$Species == "Kittiwake",]
GXrefs <- refout[refout$Species == "Gannet",]


#### CHECK THESE BECAUSE THERE WAS A PROBLEM WITH CALIBRATION...
KI.reflect <- get.refl.lengths(KIrefs)
GX.reflect <- get.refl.lengths(GXrefs)


mlist <- list.files(path="Data/Measured_by_Dayle/",pattern="Zone99*",full.names = T)
newout <- data.table::rbindlist(lapply(mlist,function(x) readxl::read_xlsx(x,sheet="Data")),
                                use.names=TRUE,fill=TRUE)
names(newout)[29] <- "Reflection"
unique(newout$Reflection)

#################################################################################
GX <- newout %>% dplyr::filter(Species == "Gannet",Reflection %in% c(NA, "No"))
KI <- newout %>% dplyr::filter(Species == "Kittiwake",Reflection %in% c(NA, "No"))

GX <- get.lengths(GX)  
KI <- get.lengths(KI)  


######## Write function to get PCH and sd of PCH



GX.fhdata <- get.fhs(GX,GX.reflect)
KI.fhdata <- get.fhs(KI,KI.reflect)





GXplot <- plot.fhs('a',GX.fhdata)
KIplot <- plot.fhs('b',KI.fhdata)


G <- ggarrange(GXplot,KIplot,nrow=1)
G

ggsave("Figures/FH_distribution_Bootstrap_method.png",plot=G,width=10,height=6,type = "cairo-png")




############################################################


GX$low.height <- GX.fhdata$lwheight
GX$mid.height <- GX.fhdata$mdheight
GX$high.height <- GX.fhdata$hiheight

KI$low.height <- KI.fhdata$lwheight
KI$mid.height <- KI.fhdata$mdheight
KI$high.height <- KI.fhdata$hiheight


GX$species <- "Gannet"
KI$species <- "Kittiwake"
all.frame <- rbind(GX,KI)

monthdat <- months(all.frame$Date)

all.frame <- all.frame %>% dplyr:select(species,low.height,mid.height,high.height)
all.frame$month <- monthdat

#all.frame <- all.frame[monthdat == "June",]

all.frame <- gather(all.frame,category,out,low.height:high.height)
all.frame$category <- plyr::revalue(all.frame$category,c("low.height"="Low",
                                                         "high.height"="High",
                                                         'mid.height'="Mean"))

all.frame$category <- factor(all.frame$category, levels=c("Low","Mean","High"))

#### PCH estimate ####

all.frame %>% nest(data=c(out)) %>%
  mutate(
    pch = map_dbl(data,~length(which(.x$out > 25))/length(.x$out))
  ) %>% group_by(species,category) %>% summarise(mean(pch),sd(pch))


all.frame %>% nest(data=c(out,month)) %>%
  mutate(
    pch = map_dbl(data,~length(which(.x$out > 25))/length(.x$out))
  )

all.frame %>% group_by(species,category) %>% summarise(mean(out))





Bplot <- ggplot(all.frame) +
  geom_boxplot(aes(x=category,y=out,fill=species),width=0.5,position= position_dodge(width=1))+
  stat_summary(aes(x=category,y=out,group=species),
               fun=mean, geom="point", shape=18, size=3.5, color="black",position= position_dodge(width=1))+
  scale_fill_manual(name="Species",values=c("grey30","grey60"))+
  ggthemes::theme_gdocs()+
  ylab("Height (m)")+xlab("")+
  theme(
    panel.border = element_blank()
  )
Bplot

ggsave("Figures/Boxplot.png",plot=Bplot,width=6,height=6,type = "cairo-png")



all.frame %>% group_by(category,species) %>% summarise(mean(out,na.rm=T))


dataf.GX <- prep.proportions(all.frame[all.frame$species=='Gannet',])
dataf.KI <- prep.proportions(all.frame[all.frame$species=='Kittiwake',])

#####################################################################
#### Generates a plot showing proportion of birds in each flight ####

GX.pl <- ggplot(dataf.GX,aes(x=height,y=proportion,linetype=heightv,fill=heightv))+
  geom_point(aes(pch=heightv),size=3)+
  geom_smooth(method="loess",color='black',span=1)+
  #scale_x_continuous(limits=c(0,150))+
  #scale_y_continuous(limits=c(-0.0005,0.05),expand=c(0,0))+
  ggthemes::theme_gdocs()+
  theme(
    panel.border=element_blank(),
    plot.background = element_blank(),
    legend.position="right"
  )+labs(linetype="",pch="",fill="",x="Flight height (m)",y="Proportion of total")+
  ggtitle('a')

KI.pl <- ggplot(dataf.KI,aes(x=height,y=proportion,linetype=heightv,fill=heightv))+
  geom_point(aes(pch=heightv),size=3)+
  geom_smooth(method="loess",color='black',span=1)+
  #scale_x_continuous(limits=c(0,150))+
  #scale_y_continuous(limits=c(-0.0005,0.05),expand=c(0,0))+
  ggthemes::theme_gdocs()+
  theme(
    panel.border=element_blank(),
    plot.background = element_blank(),
    legend.position="right"
  )+labs(linetype="",pch="",fill="",x="Flight height (m)",y="Proportion of total")+
  ggtitle('b')


G <- ggarrange(GX.pl,KI.pl,nrow=1,common.legend=TRUE,legend="right")

ggsave(plot=G,filename="Figures/Proportion_birds_in_bands_3.jpeg",device="jpeg",
       width=12,height=5)



################################################################################
#### percent measured ####
GX <- newout %>% dplyr::filter(Species == "Gannet",
                               Reflection %in% c(NA, "No"),
                               Behaviour != "Sitting",
                               Behaviour != "Taking off")
KI <- newout %>% dplyr::filter(Species == "Kittiwake",
                               Reflection %in% c(NA, "No"),
                               Behaviour != "Sitting",
                               Behaviour != "Taking off")


### 4 gannets not done in July surveys
nrow(KI[is.na(KI$`Frame 1 lengths in R`),])/nrow(KI[!is.na(KI$`Frame 1 lengths in R`),])

(nrow(GX[is.na(GX$`Frame 1 lengths in R`),]) - 4)/nrow(GX[!is.na(GX$`Frame 1 lengths in R`),])


