##################################################
### Bird body length sensitivity calculations ####
##################################################

library(foreach)
library(ggthemes)
library(Cairo)
library(tidyverse)
library(ggpubr)

flight.height <- function(plane.height,reflected.length,measured.length){
  fh <- plane.height * (1 - (reflected.length/measured.length))
  return(fh)
}
expected.length <- function(reflected.length,flight.height,plane.height){
  ml <- reflected.length / (1-(flight.height/plane.height))
  return(ml)
}

ph <- c(500,520,540,560)
rl <- c(37,44.5,78.2)
Bird <- c("KI","GB","GX")
fh <- c(10,20,30,50)

Dfrm <- expand.grid(ph,rl,fh)
names(Dfrm) <- c('ph','rl','fh')
Dfrm$Bird <- NA
Dfrm$Bird[Dfrm$rl == 37.0] <- "KI"
Dfrm$Bird[Dfrm$rl == 44.5] <- "GB"
Dfrm$Bird[Dfrm$rl == 78.2] <- "GX"


Dfrm$EL <- expected.length(Dfrm$rl,Dfrm$fh,Dfrm$ph)
Dfrm$diff <- Dfrm$EL - Dfrm$rl

Dfrm$rl <- as.factor(Dfrm$rl)
Dfrm$ph <- as.factor(Dfrm$ph)


GX <- as_tibble(Dfrm) %>% dplyr::filter(Bird=='GX')
GXp <- ggplot(GX,aes(x=fh,y=diff))+
  geom_line(aes(linetype=ph))+
  geom_point(aes(shape=ph),size=3)+
  scale_shape_discrete(name="Plane height")+
  scale_linetype_discrete(name="Plane height")+
  scale_y_continuous(breaks=seq(0,20,1))+
  ggtitle("a")+
  ylab("Expected difference from base (78.2 cm)")+
  xlab("Flight height (m)")+
  theme_classic2()+
  theme(legend.position="bottom",
        legend.direction = "horizontal",
        legend.box.spacing = unit(1,"cm"),
        panel.border = element_blank(),
        panel.grid.major = element_line(colour="grey80"))
GXp

Kitt <- as_tibble(Dfrm) %>% dplyr::filter(Bird=='KI')
KIp <- ggplot(Kitt,aes(x=fh,y=diff))+
  geom_line(aes(linetype=ph))+
  geom_point(aes(shape=ph),size=3)+
  scale_shape_discrete(name="Plane height")+
  scale_linetype_discrete(name="Plane height")+
  scale_y_continuous(breaks=seq(0,8,0.5))+
  ggtitle("b")+
  ylab("Expected difference from base (37.0 cm)")+
  xlab("Flight height (m)")+
  theme_classic2()+
  theme(legend.position="bottom",
        legend.direction = "horizontal",
        legend.box.spacing = unit(1,"cm"),
        panel.border = element_blank(),
        panel.grid.major = element_line(colour="grey80"))

KIp


#GBBG <- tbl_df(Dfrm) %>% dplyr::filter(Bird=='GB')
#GBp <- ggplot(GBBG,aes(x=fh,y=diff))+
#  geom_line(aes(color=ph))+
#  geom_point(aes(fill=ph),pch=21,size=3)+
#  scale_fill_brewer(name="Plane height",palette = "Set1")+
#  scale_color_brewer(name="Plane height",palette = "Set1")+
#  scale_y_continuous(breaks=seq(0,12,0.5))+
#  ggtitle("GBB Gull")+
#  ylab("Expected difference from base (44.5 cm)")+
#  xlab("Flight height (m)")+
#  theme_gdocs()




G <- ggarrange(GXp,KIp,nrow=1,common.legend=TRUE,legend = "bottom",legend.grob = get_legend(KIp))

G

ggsave(plot=G,filename="Figures/Length_Height_Sensitivity.png",type = "cairo-png",
       h=6,w=10)





Kitt <- tbl_df(Dfrm) %>% dplyr::filter(Bird=='KI')
KIp <- ggplot(Kitt,aes(x=fh,y=EL))+
  geom_line(aes(color=ph))+
  geom_point(aes(fill=ph),pch=21,size=3)+
  scale_fill_brewer(name="Plane height",palette = "Set1")+
  scale_color_brewer(name="Plane height",palette = "Set1")+
  scale_y_continuous(breaks=seq(32,40,0.5))+
  ggtitle("Kittiwake (base 32.0cm)")+
  ylab("Expected bird length (cm)")+
  xlab("Flight height (m)")+
  theme_gdocs()




GBBG <- tbl_df(Dfrm) %>% dplyr::filter(Bird=='GB')
GBp <- ggplot(GBBG,aes(x=fh,y=EL))+
  geom_line(aes(color=ph))+
  geom_point(aes(fill=ph),pch=21,size=3)+
  scale_fill_brewer(name="Plane height",palette = "Set1")+
  scale_color_brewer(name="Plane height",palette = "Set1")+
  scale_y_continuous(breaks=seq(45,57,0.5))+
  ggtitle("GBB Gull (base 44.5cm)")+
  ylab("Expected bird length (cm)")+
  xlab("Flight height (m)")+
  theme_gdocs()


GX <- tbl_df(Dfrm) %>% dplyr::filter(Bird=='GX')
GXp <- ggplot(GX,aes(x=fh,y=EL))+
  geom_line(aes(color=ph))+
  geom_point(aes(fill=ph),pch=21,size=3)+
  scale_fill_brewer(name="Plane height",palette = "Set1")+
  scale_color_brewer(name="Plane height",palette = "Set1")+
  scale_y_continuous(breaks=seq(80,100,1))+
  ggtitle("Gannet (base 78.2cm)")+
  ylab("Expected bird length (cm)")+
  xlab("Flight height (m)")+
  theme_gdocs()



ggsave(plot=KIp,filename="D:/PROJECTS_CURRENT/Flight_Height/Kittiwake_length.png",type = "cairo-png",
       h=6,w=6)

ggsave(plot=GBp,filename="D:/PROJECTS_CURRENT/Flight_Height/GBBG_length.png",type = "cairo-png",
       h=6,w=6)

ggsave(plot=GXp,filename="D:/PROJECTS_CURRENT/Flight_Height/Gannet_length.png",type = "cairo-png",
       h=6,w=6)

#Expected (measured) length = reflection length / ((platform height/plane height) - 1)