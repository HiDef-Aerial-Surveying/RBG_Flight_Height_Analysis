###############################################
### Helpers for the bootstrapping method
#############################################

source("Code/config.R")

### Library loading function - if not installed, then install it...
Load_Libs <- function(LIBS){
  for(l in LIBS){
    if(require(l,character.only=TRUE,warn.conflicts = FALSE)){
      print(paste(l, "Loaded"))
    }else{
      install.packages(l,repos='http://cran.us.r-project.org')
      require(l,character.only=TRUE,warn.conflicts = FALSE)
    }
  }
}


## This uses the sokal correction to calculate the CV
calc_cv <- function(x,sokal.correction=F) {
  cv1 <- (sd(x,na.rm=T) / mean(x,na.rm=T))
  if(sokal.correction==TRUE){
    cv1 <- (1+(1/(4*length(x)))) * cv1
  }
  return(cv1*100)
}

## Just in case, a function for standard error
se <- function(x) sqrt(var(x)/length(x))



#Flight height formula
flight.height <- function(aircraft.height, reflected.size, measured.size){
  aircraft.height*(1-(reflected.size/measured.size))
}


#Function for counting if value meets a threshold criteria
bird.range.thresh <- function(value,threshold=1){
  if(value > threshold){
    return(1)
  }else if(value == 100){
    return(1)
  }else {
    return(0)
  }
}




### This merge files function merges files using a foreach loop
## This gives more nuanced control to the merging operation to replace field names
## where needed. This was done due to the problems with the Plane Height / Flight height
## column name issue in older data sheets
merge.files <- function(flist){
  n <- length(flist)
  dat <- foreach(k=flist,.combine='rbind')%do%{
    v <- readxl::read_xlsx(k)
    names(v)[grep("Flight height",names(v))] <- "Plane Height"
    names(v)[grep("Plane height",names(v))] <- "Plane Height"
    if(length(grep("Plane Height",names(v)))>0){
      v <- v %>%
        dplyr::select(Date,Camera,`Reel Name`,Frame,
                      `Marker Number`, Species,
                      `Plane Height`, `Frame 1`:`Frame 8`,
                      Latitude,Longitude) 
      return(v)
    }
  }
  return(dat)
}



### This function is used in other functions; it uses the row and column indices
### of the location of the red, green and blue channel measurements
# get.mean <- function(rowin,colinds,DatF){
#   ## rowin = row of the dataframe to analyze - integer
#   ## colinds = a dataframe that gives the column index for the red, green or blue column for the frame of interest - data frame
#   ## DatF = the dataframe being analyzed - data frame
#   R <- as.numeric(unlist(strsplit(DatF[rowin,colinds["R"]],"      ")))
#   G <- as.numeric(unlist(strsplit(DatF[rowin,colinds["G"]],"      "))) 
#   B <- as.numeric(unlist(strsplit(DatF[rowin,colinds["B"]],"      ")))
#   Rmean <- mean(R,na.rm=T)
#   Gmean <- mean(G,na.rm=T)
#   Bmean <- mean(B,na.rm=T)
#   outmean <- mean(c(Rmean,Gmean,Bmean),na.rm=T)
#   return(list(outmean=outmean,values=c(R,G,B)))
# }

### This function is used in other functions; it uses the row and column indices
### of the location of the red, green and blue channel measurements
### THIS ALTERED FUNCTION FIXES INCORRECT CALIBRATIONS
get.mean <- function(rowin,colinds,DatF){
  ## rowin = row of the dataframe to analyze - integer
  ## colinds = a dataframe that gives the column index for the red, green or blue column for the frame of interest - data frame
  ## DatF = the dataframe being analyzed - data frame
  
    # ifelse statement necessary here because the code doesn't like when a frame has NAs
  if(is.na(DatF[rowin,colinds["R"]]) == FALSE){
  R <- as.numeric(unlist(strsplit(DatF[rowin,colinds["R"]],"      ")))
  G <- as.numeric(unlist(strsplit(DatF[rowin,colinds["G"]],"      "))) 
  B <- as.numeric(unlist(strsplit(DatF[rowin,colinds["B"]],"      ")))
  }else{
  R <- NA
  G <- NA
  B <- NA
  }
  
  
  # incorrect calibrations fixed here
  R <- R*DatF$calprop[rowin]
  G <- G*DatF$calprop[rowin]
  B <- B*DatF$calprop[rowin]
  
  Rmean <- mean(R,na.rm=T)
  Gmean <- mean(G,na.rm=T)
  Bmean <- mean(B,na.rm=T)
  outmean <- mean(c(Rmean,Gmean,Bmean),na.rm=T)
  return(list(outmean=outmean,values=c(R,G,B)))
 
}


## This function will create the data frame with lengths of birds at reflection height
## The output is fed directly into the flight height calculations
get.refl.lengths <- function(DatF){
  DatF <- as.data.frame(DatF)
  DatF$dir <- plyr::revalue(DatF$Behaviour, c("Flying D" = "Perpendicular", "Flying U" = "Perpendicular",
                                              "Flying L" = "Parallel", "Flying R" = "Parallel",
                                              "Flying DL" = "Parallel","Flying DR" = "Parallel",
                                              "Flying UL" = "Parallel","Flying UR" = "Parallel")) 
  print('running...')
  refl.lengths <- foreach(j=1:nrow(DatF),.combine='rbind',.errorhandling = 'remove') %do%{
    
    mns <- foreach(i=1:8,.combine='c') %do%{
      colnams <- sapply(c("R","G","B"),function(x) paste0("Frame.",i,".lengths.in.",x)) 
      colinds <- sapply(colnams,function(x) grep(names(DatF),pattern=x))
      datout <- get.mean(j,colinds,DatF)
      return(datout$outmean)
    }
    fin.length <- mns[which.max(mns)]
    ### Temporary to remove unusual high values.
    CV <- calc_cv(mns, sokal.correction = TRUE)
    if(fin.length < 200){
    return(data.frame(fin.length=fin.length,Dir=DatF$dir[j], CV = CV, Species = DatF$Species[j]))
    }
    
    
  }
  print('complete!')
  return(refl.lengths)
}

## This script is slightly similar to the one above but was used in troubleshooting to extract the full distribution of measured lengths, rather than just the mean.
## Script to extract full distribution of measured lengths in the reflection database
# Do not use this when estimating reflection lengths. 
get.refl.length.dists <- function(DatF){
  DatF <- as.data.frame(DatF)
  DatF$dir <- plyr::revalue(DatF$Behaviour, c("Flying D" = "Perpendicular", "Flying U" = "Perpendicular",
                                              "Flying L" = "Parallel", "Flying R" = "Parallel",
                                              "Flying DL" = "Parallel","Flying DR" = "Parallel",
                                              "Flying UL" = "Parallel","Flying UR" = "Parallel")) 
  print('running...')
  refl.lengths <- foreach(j=1:nrow(DatF),.combine='rbind',.errorhandling = 'remove') %do%{
    
    mns <- foreach(i=1:8,.combine='rbind') %do%{
      colnams <- sapply(c("R","G","B"),function(x) paste0("Frame.",i,".lengths.in.",x)) 
      colinds <- sapply(colnams,function(x) grep(names(DatF),pattern=x))
      datout <- get.mean(j,colinds,DatF)
      return(tibble(values = list(datout$values), means = datout$outmean))
    }
    
    
    fin.length <- mns$means[which.max(mns$means)]
    
    alldists <- unlist(mns$values)
    
    if(fin.length < 200){
    x <- tibble(dists = list(alldists), fin.length = fin.length, Species = DatF$Species[j], zone = DatF$Location[j], camera = DatF$Camera[j], reel = DatF$`Reel Name`[j], Frame = DatF$Frame[j], Marker = DatF$`Marker Number`[j], dir = DatF$dir[j], Survey = DatF$Survey[j], age = DatF$`Approximate Age`[j])
              return(x)
    }
  
    
  }
  print('complete!')
  return(refl.lengths)
}


## Function to extract the lengths of birds AT HEIGHT for processing
get.lengths <- function(datf){
  DF<- datf %>% dplyr::filter(!is.na(Location),!is.na(`Plane Height`),!is.na(`Frame 1 lengths in R`))
  
  if(nrow(DF) == 0){
    print(paste0("no data for ", datf$`Survey Date`))
    return(DF)
  }else{
    
  DF$dir <- plyr::revalue(DF$Behaviour, c("Flying D" = "Perpendicular", "Flying U" = "Perpendicular",
                                          "Flying L" = "Parallel", "Flying R" = "Parallel",
                                          "Flying DL" = "Parallel","Flying DR" = "Parallel",
                                          "Flying UL" = "Parallel","Flying UR" = "Parallel")) 
  DF <- as.data.frame(DF)
  
  lengths <- foreach(j=1:nrow(DF),.combine='rbind') %do%{
    
    frame.count <- max(parse_number(names(DF)[grep(" lengths in ", names(DF))])) # this accounts for some months not having all 8 frames
    
    mns <- foreach(i=1:frame.count,.combine='rbind', .errorhandling = "pass") %do%{
      colnams <- sapply(c("R","G","B"),function(x) paste0("Frame ",i," lengths in ",x)) 
      colinds <- sapply(colnams,function(x) grep(names(DF),pattern=x))
      datout <- get.mean(j,colinds,DF)
      return(tibble(outmean=datout$outmean,values=list(datout$values)))
          }
    fin.length <- mns$outmean[which.max(mns$outmean)]
    fin.CV <- calc_cv(mns$outmean, sokal.correction = TRUE)
    if(length(fin.length)==0){
      fin.values <- NA
    }
    fin.values <- unlist(mns$values[which.max(mns$outmean)])
    
    return(tibble(values=list(fin.values), CV=fin.CV))
  }
  
  DF <- tibble(DF)
  DF$lengthvals <- lengths
  return(DF) 
  }
}



### This takes the output from get.lengths (DatH) and
### get.refl.lengths (DatR), and calculates the flight heights
### using the bootstrapping technique

get.fhs <- function(DatH,DatR, bootsize){
  Spplist <- SPP[SPP %in% unique(DatH$Species)] # we can only choose species in the main SPP list that are present in the dataframe for each month
  specs <- foreach(k=1:length(Spplist),.combine='rbind') %do% {
   print(Spplist[k])
    DatR1 <- DatR[DatR$Species == Spplist[k],]
    
  refl.par <- DatR1[DatR1$Dir == "Parallel",]
  refl.per <- DatR1[DatR1$Dir == "Perpendicular",]
  
  ## Bootstrap parallel facing birds
  refl.par.boot <- foreach(i=1:bootsize,.combine='c') %do% {
    sampl <- sample(refl.par$fin.length,size=length(refl.par$fin.length),replace=T)
    return(mean(sampl))
  }
  
  mnrfl.par <- mean(refl.par.boot)
  rngrefl.par <- range(refl.par.boot)
  
  ## Bootstrap perpendicular facing birds
  refl.per.boot <- foreach(i=1:bootsize,.combine='c') %do% {
    sampl <- sample(refl.per$fin.length,size=length(refl.per$fin.length),replace=T)
    return(mean(sampl))
  }
  mnrfl.per <- mean(refl.per.boot)
  rngrefl.per <- range(refl.per.boot) 
  
  
  refl.all.boot <- foreach(i=1:bootsize,.combine='c') %do% {
    sampl <- sample(DatR$fin.length,size=length(DatR$fin.length),replace=T)
    return(mean(sampl))
  }
  mnrfl.all <- mean(refl.all.boot)
  rngrefl.all <- range(refl.all.boot) 
  
    DatH1 <- DatH[which(DatH$Species == Spplist[k]),]
  fhdata <- foreach(j = 1:nrow(DatH1),.combine='rbind',.verbose = TRUE) %do% {
    
    print(j)
    if(length(DatH1$lengthvals[[1]][[j]])>0){
      
      behav <- DatH1$dir[j]
      if(behav == "Perpendicular"){
        mnrfl <- mnrfl.per
        rngrefl <- rngrefl.per
      }else if(behav == "Parallel"){
        mnrfl <- mnrfl.par
        rngrefl <- rngrefl.par
      }else{
        mnrfl <- mnrfl.all
        rngrefl <- rngrefl.all
      }
      
      
      
      output <- foreach(i=DatH1$lengthvals[[1]][[j]],.combine='rbind') %do%{
        airc <- DatH$`Plane Height`[j]
        bhmn <- sapply(mnrfl,function(x) flight.height(airc,x,i))
        bhdn <- sapply(rngrefl[2],function(x) flight.height(airc,x,i))
        bhup <- sapply(rngrefl[1],function(x) flight.height(airc,x,i))
        
        x <- tibble(bhdn,bhmn,bhup)
        return(x)
      }
      
      output[output < 0] <- 0
      
      print("Running bootstrap...")
      booto <- foreach(i=1:bootsize,.combine='rbind') %do% {
        low <- sample(output$bhdn,size=nrow(output),replace=T)
        mid <- sample(output$bhmn,size=nrow(output),replace=T)
        high <- sample(output$bhup,size=nrow(output),replace=T)
        lw <- as.numeric(quantile(low,0.025, na.rm = TRUE))
        md <- mean(mid,na.rm=T)
        hi <- as.numeric(quantile(high,0.975, na.rm = TRUE))
        
        return(data.frame(lw,md,hi))
      }
      
      
      mdheight <- mean(booto$md)
      lwheight <-  mean(booto$lw,na.rm=T)#
      hiheight <-  mean(booto$hi,na.rm=T)#
    }else{
      mdheight <- NA
      lwheight <- NA
      hiheight <- NA
    }
    
    
    
    return(data.frame(lwheight,mdheight,hiheight, Species = DatH1$Species[j], month = months(DatH1$`Survey Date`[j]), year = substring(DatH1$`Survey Date`[j], 1, 4), "Reel Ref" = DatH1$`Reel Ref`[j], "Frame Ref" = DatH1$`Frame Ref`[j], "Marker Number" = DatH1$`Marker Number`[j]))
  }
  
  return(fhdata)
  }
  
  return(specs)
  
  
}

## This will generate the generic flight height plot (no wind turbine png)
## turbine.low = the lowest point of the turbine blades above sea level
## turbine.high = the heighest point of the turbine blades above sea level

plot.fhs <- function(fhdata,turbine.low,turbine.high,turbine.max){
  fhdata <- fhdata[order(fhdata$mdheight,fhdata$hiheight),]
  Spplist <- unique(fhdata$Species)
  Monthlist <- unique(fhdata$month)
  
  fhdata <- 
    
    foreach(i = 1:length(Spplist), .combine = 'rbind') %do% {
    fhdata1 <- fhdata[which(fhdata$Species == Spplist[i]),]  
      fhdata3 <- foreach(j = 1:length(Monthlist), .combine = 'rbind') %do% {
        fhdata2 <- fhdata1[which(fhdata1$month == Monthlist[j]),]
  fhdata2$birdid <- 1:nrow(fhdata2)
  return(fhdata2)
      }
      return(fhdata3)
  }
  
  
  G <- ggplot(fhdata)+
    geom_errorbar(aes(ymin=lwheight,ymax=hiheight,x=birdid),width=0,size=2,colour='lightblue')+
    geom_point(aes(x=birdid,y=mdheight),color="black")+
    geom_hline(yintercept=turbine.low,linetype="dashed",size=1.25)+
    geom_hline(yintercept=turbine.high,linetype="dashed",size=1.25)+
    geom_hline(yintercept=turbine.max,linetype="dashed",size=1.25)+
    geom_hline(yintercept = 0)+
    #scale_y_continuous(limits = c(0,180))+
    facet_wrap(month ~ ., ncol = 3, scales = "free_x") +
    theme_classic2()+
    ylab("Height (m)")+
    xlab("")+
    #ggtitle(titlename)+
    theme(legend.position="bottom",
          legend.direction = "horizontal",
          legend.box.spacing = unit(1,"cm"),
          panel.border = element_blank(),
          axis.text.x = element_blank(),
          panel.grid.major = element_line(colour="grey80")
    )
  
  return(G)
}




### This function is for creating the raster that is used in the 
### rayshader 3d plot

produce.raster <- function(data_mod,shapearea,
                           month=NULL,grd.density=50000,
                           idp=3.0, crs=sf::st_crs(4326)$proj4string){
  if(!is.null(month)){
    dmodt <- data_mod[data_mod$MONTH == month,]
  }else{
    dmodt <- data_mod
  }
  
  #dmodt <- data_mod %>% dplyr::select(Lat,Lon,Percent_Overlap)
  
  #dmodt$Percent_Overlap[dmodt$Percent_Overlap < 0] <- 0
  
  coordinates(dmodt) <- c('Lon','Lat')
  proj4string(dmodt) <- crs
  
  # Create an empty grid where n is the total number of cells
  grd              <- as.data.frame(spsample(shapearea, "regular", n=grd.density))
  names(grd)       <- c("X", "Y")
  coordinates(grd) <- c("X", "Y")
  gridded(grd)     <- TRUE  # Create SpatialPixel object
  fullgrid(grd)    <- TRUE  # Create SpatialGrid object
  
  # Add P's projection information to the empty grid
  raster::projection(grd) <- proj4string(dmodt)
  
  # Interpolate the grid cells using a power value of 2 (idp=2.0)
  ## Uses mid.height column to do the IDW
  P.idw <- gstat::idw(mdheight ~ 1, dmodt, newdata=grd, idp=idp)  #Percent_Overlap
  r       <- raster::raster(P.idw)
  rr <- raster::mask(r, shapearea)
  
  return(list(rr=rr,dmodt=dmodt))
}



create.df.to.plot <- function(rr,threshold){
  test_spdf <- as(rr, "SpatialPixelsDataFrame")
  test_df <- as.data.frame(test_spdf)
  colnames(test_df) <- c("value", "x", "y")
  test_df$value[test_df$value < 10] <- 0
  return(test_df)
}






### This function is for putting birds in 10 m flight height bands
### and then plotting the distribtion of those
### NOT important for reporting - this was for the manuscript

prep.proportions <- function(x){
  cats <- c("Low","Mean","High")
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


#################################################
## Additional functions for doing imputation
#### NOTE!! For this function, you have to ensure that the LARGER OF THE TWO SPECIES
#### is in the 2nd argument. I.E. the species code for the LARGER bird = code2
get.prop <- function(dat,sp1,sp2,datas=c("real","hidef")){
  ## dat = the dataframe read from bird_lengths.csv
  ## sp1 = one species to compare
  ## sp2 = the other species to compare
  ## datas = if the data are measured (real), or from hidef cameras reflect db (hidef)
  if(datas == "real"){
    spd <- c(sp1,sp2)
    
    sp1l <- dat$max[tolower(dat$species) == sp1]
    sp2l <- dat$max[tolower(dat$species) == sp2]
    larger <- which.max(c(sp1l,sp2l))
    smaller <- which.min(c(sp1l,sp2l))
    
    outmax <- dat$max[tolower(dat$species) == spd[smaller]] / dat$max[tolower(dat$species) == spd[larger]]
    outmin <- dat$min[tolower(dat$species) == spd[smaller]] / dat$min[tolower(dat$species) == spd[larger]]
  }else if(datas == "hidef"){
    ### GANNET CORRECTION! (To be investigated)
    if(sp1 == "gannet"){
      testtt <- dat[dat$Species == sp2,]
      testtt <- testtt[testtt$fin.length > 60,]  ## 25th percentile
      sp1.Quants <- quantile(testtt$fin.length,c(0.0275,0.975))
    }else{
      sp1.Quants <- quantile(dat$fin.length[dat$Species == sp1],
                             c(0.0275,0.975))
    }
    if(sp2 == "gannet"){
      testtt <- dat[dat$Species == sp2,]
      testtt <- testtt[testtt$fin.length > 60,]  ## 25th percentile
      sp2.Quants <- quantile(testtt$fin.length,c(0.0275,0.975))
    }else{
      sp2.Quants <- quantile(dat$fin.length[dat$Species == sp2],
                             c(0.0275,0.975))
    }
    
    sp.Quants <- list(sp1.Quants,sp2.Quants)
    
    larger <- which.max(c(sp1.Quants[2],sp2.Quants[2]))
    smaller <- which.min(c(sp1.Quants[2],sp2.Quants[2]))
    
    outmax <- sp.Quants[[smaller]][2] / sp.Quants[[larger]][2]
    outmin <- sp.Quants[[smaller]][1] / sp.Quants[[larger]][1]
    
    
  }
  return(list(outmax,outmin))
}


impute.species <- function(real.data,
                           Dat.reflect,
                           to.impute,
                           spp.to.compare=c('kittiwake',"gannet","herring gull")){
  
  xx <- to.impute
  combos <- combn(spp.to.compare,2)
  ### Create an estimate of the camera offset / error
  out <- foreach(i=1:ncol(combos),.combine='c')%do%{
    sp1 <- combos[1,i]
    sp2 <- combos[2,i]
    
    hdprop <- get.prop(Dat.reflect,sp1,sp2,datas="hidef")
    rlprop <- get.prop(real.data,sp1,sp2,datas="real")
    
    mxdif <- hdprop[[1]] - rlprop[[1]]
    mndif <- hdprop[[2]] - rlprop[[2]]
    return(c(mxdif,mndif))
  }
  
  ErrorVal <- mean(out)
  
  ### Next we calculate the proportional sizes of known HD birds (e.g., KI, HG, GX)
  ### to the sizes of the unknown bird.
  
  mnvars <- foreach(i=1:length(spp.to.compare),.combine='rbind')%do%{
    
    known.p <- get.prop(real.data,spp.to.compare[i],xx,datas="real")  
    max.p <- known.p[[2]]
    min.p <- known.p[[1]]
    spd <- c(spp.to.compare[i],xx)
    sp1l <- real.data$max[tolower(real.data$species) == spd[1]]
    sp2l <- real.data$max[tolower(real.data$species) == spd[2]]
    larger <- which.max(c(sp1l,sp2l))
    smaller <- which.min(c(sp1l,sp2l))
    ### Using the proportional sizes of the known v unknown birds, we can calculate 
    ### the new size for the unknown bird that we would see in our cameras
    ### Calculation is done by multiplying if size is larger for the comparative species
    ### For example, if KI = 46 and SPP = 43 in real data, you would multiply. 
    if(larger == 1){
      min.dat <- min.p *  Dat.reflect$fin.length[Dat.reflect$Species==spd[1]]
      max.dat <- max.p *  Dat.reflect$fin.length[Dat.reflect$Species==spd[1]] 
      all.dat <- c(min.dat,max.dat)
    }else if(larger == 2){
      min.dat <- Dat.reflect$fin.length[Dat.reflect$Species==spd[1]]/min.p
      max.dat <- Dat.reflect$fin.length[Dat.reflect$Species==spd[1]]/max.p
      all.dat <- c(min.dat,max.dat)
    }
    mndat <- mean(all.dat) - (mean(all.dat)*ErrorVal)
    vadat <- var(all.dat) - (var(all.dat)*ErrorVal)
    
    ### Get the average difference between parallel and perpendicular birds
    
    dd <- Dat.reflect %>% dplyr::filter(Species == spp.to.compare[i]) %>%
      nest(data=c(-Dir)) %>% mutate(
        mean.len = map_dbl(data,~mean(.$fin.length))
      ) %>% dplyr::filter(Dir %in% c('Parallel',"Perpendicular")) %>%
      dplyr::select(Dir,mean.len)
    
    dd <- data.frame(dd)
    row.names(dd) <- dd$Dir
    par.perp <- dd["Parallel",]$mean.len - dd["Perpendicular",]$mean.len
    
    
    return(data.frame(mean=mndat,var=vadat,parperdiff=par.perp,species=spp.to.compare[i]))
  }  
  ### Compute a normal distribution
  
  imputed.data.parallel <- rnorm(250,mean(mnvars$mean),sd = sqrt(mean(mnvars$var)))
  imputed.data.perpend <- imputed.data.parallel - mean(mnvars$parperdiff)
  
  
  outframe <- data.frame(fin.length=c(imputed.data.parallel,imputed.data.perpend),
                         Dir = c(rep("Parallel",250),rep("Perpendicular",250)),
                         CV = NA,
                         Species = xx,
                         Imputed = "Y"
  )
  
  return(outframe)  
}


