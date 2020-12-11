###################################
### Galloper flight heights for Manuscript
##################

source("Scripts/Helpers.R")
require(foreach)
library(gridExtra)
library(ggpubr)
library(rayshader)
library(viridis)
library(data.table)
library(tidyverse)
library(ggpubr)

bird.range.thresh <- function(value,threshold=1){
  if(value > threshold){
    return(1)
  }else if(value == 100){
    return(1)
  }else {
    return(0)
  }
}

calc_cv <- function(x,sokal.correction=F) {
  cv1 <- (sd(x,na.rm=T) / mean(x,na.rm=T))
  if(sokal.correction==TRUE){
    cv1 <- (1+(1/(4*length(x)))) * cv1
  }
  return(cv1*100)
}

spp.data <- function(datf,reflectdf){
  
  min.reflection.ud <- reflectdf$min.reflection.ud
  mean.reflection.ud <- reflectdf$mean.reflection.ud
  max.reflection.ud <- reflectdf$max.reflection.ud
  min.reflection.lr <- reflectdf$min.reflection.lr
  mean.reflection.lr <- reflectdf$mean.reflection.lr
  max.reflection.lr <- reflectdf$max.reflection.lr
  #aircraft.height <- reflectdf$aircraft.height
  
  Tom <- datf %>% dplyr::filter(!is.na(Location),!is.na(`Plane Height`))
  Tom$dir <- plyr::revalue(Tom$Behaviour, c("Flying D" = "Perpendicular", "Flying U" = "Perpendicular",
                                            "Flying L" = "Parallel", "Flying R" = "Parallel",
                                            "Flying DL" = "Parallel","Flying DR" = "Parallel",
                                            "Flying UL" = "Parallel","Flying UR" = "Parallel")) 
  
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
      aircraft.height <- Tom$Plane.Height[j]
      
      if(Tom$dir[j] == "Parallel"){
        min.heights <- sapply(c(R,G,B),function(x) flight.height(aircraft.height,max.reflection.lr,x))
        mean.heights <- sapply(c(R,G,B),function(x) flight.height(aircraft.height,mean.reflection.lr,x))
        max.heights <- sapply(c(R,G,B),function(x) flight.height(aircraft.height,min.reflection.lr,x))
      }else if(Tom$dir[j] == "Perpendicular"){
        min.heights <- sapply(c(R,G,B),function(x) flight.height(aircraft.height,max.reflection.ud,x))
        mean.heights <- sapply(c(R,G,B),function(x) flight.height(aircraft.height,mean.reflection.ud,x))
        max.heights <- sapply(c(R,G,B),function(x) flight.height(aircraft.height,min.reflection.ud,x))
      }
      
      dataf <- tibble(vals=list(c(R,G,B)),maxheights=list(max.heights),meanheights=list(mean.heights),
                      minheights=list(min.heights),rgbs=list(c(Rmean,Gmean,Bmean)))
      return(dataf)
    }
    
    oo$lenmean <- sapply(1:nrow(oo),function(x) mean(oo$rgbs[x][[1]],na.rm=T))
    names(oo) <- paste0(names(oo),"_Frame.",i)
    return(oo)  
  }
  
  newout <- cbind(Tom[,1:15],out)
  newout$FrameX <- Tom$Frame.X
  newout$FrameY <- Tom$Frame.Y
  
  lenmnInds <- grep(names(newout),pattern="lenmean_*")
  newout <- newout %>% dplyr::filter(!is.na(lenmean_Frame.1),lenmean_Frame.1<150)
  newout$cv <- sapply(1:nrow(newout),function(x) calc_cv(as.vector(unlist(newout[x,lenmnInds])),sokal.correction=T))
  newout <- newout %>% dplyr::filter(cv < 10)
  newout$birdlen <- sapply(1:nrow(newout),function(x) max(newout[x,lenmnInds],na.rm=T))
  
  return(newout)
}


mlist <- list.files(path="Data/Measured_by_Dayle/",pattern="Zone99*",full.names = T)
newout <- data.table::rbindlist(lapply(mlist,function(x) readxl::read_xlsx(x,sheet="Data")),
                                use.names=TRUE,fill=TRUE)
names(newout)[29] <- "Reflection"
unique(newout$Reflection)


GX <- newout %>% dplyr::filter(Species == "Gannet")
KI <- newout %>% dplyr::filter(Species == "Kittiwake")


reflectdfKI <- data.frame(
  min.reflection.ud=28.1,
  mean.reflection.ud=33.7,
  max.reflection.ud=40.3,
  min.reflection.lr=28.5,
  mean.reflection.lr=35.0,
  max.reflection.lr=41.5
)

reflectdfGX <- data.frame(
  min.reflection.ud=46.3,
  mean.reflection.ud=57.6,
  max.reflection.ud=69.9,
  min.reflection.lr=51.8,
  mean.reflection.lr=67.3,
  max.reflection.lr=81.5
)


KI.data <- spp.data(KI,reflectdfKI)
GX.data <- spp.data(GX,reflectdfGX)


###########################################################
# This function will return the flight heights of the birds
# after the spp.data() function has been run
# This will return a dataframe with a column of all
# calculated flight heights for the species in the site
############################################################
return.heights <- function(dataf,indices){
  out <- foreach(i=1:nrow(dataf),.combine='c')%do%{
    tbird <- dataf[i,indices]
    test <- sapply(1:ncol(tbird),
                       function(x){
                         vs <- median(unlist(tbird[,x]),na.rm=T)
                       })
    #test <- unlist(tbird)
    #test <- test[!is.na(test)]
    test <- max(test,na.rm=T)
    # Any negative values get turned to 0
    #test[test<0] <- 0
    if(test < 0)test <- 0
    return(test)
  }
  return(data.frame(out))
}




GX.mins <- return.heights(GX.data,grep(names(GX.data),pattern="minheights*"))
GX.mins$category <- "Minimum"
GX.means <- return.heights(GX.data,grep(names(GX.data),pattern="meanheights*"))
GX.means$category <- "Mean"
GX.maxs <- return.heights(GX.data,grep(names(GX.data),pattern="maxheights*"))
GX.maxs$category <- "Maximum"

GX.frame <- do.call('rbind',list(GX.mins,GX.means,GX.maxs))

KI.mins <- return.heights(KI.data,grep(names(KI.data),pattern="minheights*"))
KI.mins$category <- "Minimum"
KI.means <- return.heights(KI.data,grep(names(KI.data),pattern="meanheights*"))
KI.means$category <- "Mean"
KI.maxs <- return.heights(KI.data,grep(names(KI.data),pattern="maxheights*"))
KI.maxs$category <- "Maximum"
KI.frame <- do.call('rbind',list(KI.mins,KI.means,KI.maxs))

#gx.frame <- GX.frame[GX.frame$out!=0,]

GX.frame$species <- "Gannet"
KI.frame$species <- "Kittiwake"
all.frame <- rbind(GX.frame,KI.frame)


all.frame %>% group_by(category,species) %>% summarise(mean(out))





Bplot <- ggplot(all.frame) +
  geom_boxplot(aes(x=category,y=out,fill=species),width=0.5,position= position_dodge(width=1))+
  stat_summary(aes(x=category,y=out,fill=species),
               fun=mean, geom="point", shape=21, size=2.5, color="black",position= position_dodge(width=1))+
  scale_fill_manual(name="Species",values=c("grey30","grey60"))+
  ggthemes::theme_gdocs()+
  ylab("Height (m)")+xlab("")+
  theme(
    panel.border = element_blank()
  )

ggsave("Figures/Boxplot.png",plot=Bplot,width=6,height=6,type = "cairo-png")



#################################################
#### Create overlap plot ####
### This is a plot of the height ranges of individual birds
###
overlap.plot <- function(min.rotor,max.rotor,threshold,dataf,titlename){
  dataf$thresh <- as.character(sapply(dataf$overlap,function(x)bird.range.thresh(x,threshold)))
  ggplot(dataf)+
    geom_errorbar(aes(ymin=Minimum,ymax=Maximum,x=birdid,color=thresh),width=0,size=3)+
    geom_point(aes(x=birdid,y=Mean),color="black")+
    scale_color_manual(name="Overlap with rotor area",values=c("darkseagreen2","lightblue2"),labels=c("Yes","No"))+
    geom_hline(yintercept=min.rotor,linetype="dashed",size=1.25)+
    geom_hline(yintercept=max.rotor,linetype="dashed",size=1.25)+
    geom_hline(yintercept = 0)+
    #scale_y_continuous(expand=c(0,0))+
    theme_classic2()+
    ylab("Height (m)")+
    xlab("")+
    ggtitle(titlename)+
    theme(legend.position="bottom",
          legend.direction = "horizontal",
          legend.box.spacing = unit(1,"cm"),
          panel.border = element_blank(),
          axis.text.x = element_blank(),
          panel.grid.major = element_line(colour="grey80")
    )
  
}
#############################

GX.test <- GX.frame
GX.test$birdid <- rep(c(1:nrow(GX.frame[GX.frame$category=="Minimum",])),3)
GX.test <- spread(GX.test,category,out)
rotor.range <- c(25,150)
GX.test$overlap<-sapply(1:nrow(GX.test),
       function(x){
         HTSSIP:::perc_overlap(GX.test$Minimum[x],GX.test$Maximum[x],
                               rotor.range[1],rotor.range[2])
       })

GX.test$overlap[is.nan(GX.test$overlap)] <- 0
GX.test <- GX.test %>% arrange(desc(overlap))
GX.test$birdid <- c(1:nrow(GX.test))


KI.test <- KI.frame
KI.test$birdid <- rep(c(1:nrow(KI.frame[KI.frame$category=="Minimum",])),3)
KI.test <- spread(KI.test,category,out)
rotor.range <- c(25,150)
KI.test$overlap<-sapply(1:nrow(KI.test),
                        function(x){
                          HTSSIP:::perc_overlap(KI.test$Minimum[x],KI.test$Maximum[x],
                                                rotor.range[1],rotor.range[2])
                        })

KI.test$overlap[is.nan(KI.test$overlap)] <- 0
KI.test <- KI.test %>% arrange(desc(overlap))
KI.test$birdid <- c(1:nrow(KI.test))



GX1 <- overlap.plot(min.rotor = rotor.range[1],max.rotor = rotor.range[2],
             threshold=0,dataf=GX.test,titlename="a")

KI1 <- overlap.plot(min.rotor = rotor.range[1],max.rotor = rotor.range[2],
                    threshold=0,dataf=KI.test,titlename="b")



G <- ggarrange(GX1,KI1,nrow=1,common.legend=TRUE,legend = "bottom",legend.grob = get_legend(GX1))
G

ggsave("Figures/Overlaps.png",plot=G,width=10,height=6,type = "cairo-png")





GX2 <- overlap.plot(min.rotor = rotor.range[1],max.rotor = rotor.range[2],
             threshold=1,dataf=GX.test,titlename="Exceptionally unlikely")
GX3 <- overlap.plot(min.rotor = rotor.range[1],max.rotor = rotor.range[2],
             threshold=10,dataf=GX.test,titlename="Very unlikely")
GX4 <- overlap.plot(min.rotor = rotor.range[1],max.rotor = rotor.range[2],
             threshold=33.3,dataf=GX.test,titlename="Unlikely")
GX5 <- overlap.plot(min.rotor = rotor.range[1],max.rotor = rotor.range[2],
             threshold=66.6,dataf=GX.test,titlename="Likely")
GX6 <- overlap.plot(min.rotor = rotor.range[1],max.rotor = rotor.range[2],
             threshold=90,dataf=GX.test,titlename="Very likely")
GX7 <- overlap.plot(min.rotor = rotor.range[1],max.rotor = rotor.range[2],
             threshold=99,dataf=GX.test,titlename="Virtually certain")
GX8 <- overlap.plot(min.rotor = rotor.range[1],max.rotor = rotor.range[2],
             threshold=100,dataf=GX.test,titlename="Certain")
  



KI2 <- overlap.plot(min.rotor = rotor.range[1],max.rotor = rotor.range[2],
                    threshold=1,dataf=KI.test,titlename="Exceptionally unlikely")
KI3 <- overlap.plot(min.rotor = rotor.range[1],max.rotor = rotor.range[2],
                    threshold=10,dataf=KI.test,titlename="Very unlikely")
KI4 <- overlap.plot(min.rotor = rotor.range[1],max.rotor = rotor.range[2],
                    threshold=33.3,dataf=KI.test,titlename="Unlikely")
KI5 <- overlap.plot(min.rotor = rotor.range[1],max.rotor = rotor.range[2],
                    threshold=66.6,dataf=KI.test,titlename="Likely")
KI6 <- overlap.plot(min.rotor = rotor.range[1],max.rotor = rotor.range[2],
                    threshold=90,dataf=KI.test,titlename="Very likely")
KI7 <- overlap.plot(min.rotor = rotor.range[1],max.rotor = rotor.range[2],
                    threshold=99,dataf=KI.test,titlename="Virtually certain")
KI8 <- overlap.plot(min.rotor = rotor.range[1],max.rotor = rotor.range[2],
                    threshold=100,dataf=KI.test,titlename="Certain")




ggsave(plot=G,filename="Figures/OverlapFigure.jpeg",device="jpeg",
       width=12,height=20)




prep.proportions <- function(x){
  cats <- c("Minimum","Mean","Maximum")
  ot <- foreach(i=cats,.combine='rbind')%do%{
    y <- x[x$category == i,]
    brks <- seq(0,max(y$out),by=10)  
    brktab <- data.frame(table(cut(y$out,breaks=brks,include.lowest = T,right=F)))
    brktab$height <- c((1:nrow(brktab))*10)
    brktab$proportion <- brktab$Freq/sum(brktab$Freq)
    brktab$heightv <- i
    return(brktab)
  }
  return(ot)
}
dataf.GX <- prep.proportions(GX.frame)
dataf.KI <- prep.proportions(KI.frame)

#####################################################################
#### Generates a plot showing proportion of birds in each flight ####
#### band (1m bands)
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


library(ggpubr)
G <- ggarrange(GX.pl,KI.pl,nrow=1,common.legend=TRUE,legend="right")

ggsave(plot=G,filename="Figures/Proportion_birds_in_bands_2.jpeg",device="jpeg",
       width=12,height=5)
