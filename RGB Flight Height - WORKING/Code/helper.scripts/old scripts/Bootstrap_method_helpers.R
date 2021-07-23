###############################################
### Helpers for the bootstrapping method
#############################################


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
get.mean <- function(rowin,colinds,DatF){
  ## rowin = row of the dataframe to analyze - integer
  ## colinds = a dataframe that gives the column index for the red, green or blue column for the frame of interest - data frame
  ## DatF = the dataframe being analyzed - data frame
  R <- as.numeric(unlist(strsplit(DatF[rowin,colinds["R"]],"      ")))
  G <- as.numeric(unlist(strsplit(DatF[rowin,colinds["G"]],"      "))) 
  B <- as.numeric(unlist(strsplit(DatF[rowin,colinds["B"]],"      ")))
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
    
    mns <- foreach(i=1:7,.combine='c') %do%{
      colnams <- sapply(c("R","G","B"),function(x) paste0("Frame.",i,".lengths.in.",x)) 
      colinds <- sapply(colnams,function(x) grep(names(DatF),pattern=x))
      datout <- get.mean(j,colinds,DatF)
      return(datout$outmean)
    }
    fin.length <- mns[which.max(mns)]
    ### Temporary to remove unusual high values.
    if(fin.length < 200){
      return(data.frame(fin.length=fin.length,Dir=DatF$dir[j]))
    }
    
    
  }
  
  return(refl.lengths)
}




## Function to extract the lengths of birds AT HEIGHT for processing
get.lengths <- function(datf){
  DF<- datf %>% dplyr::filter(!is.na(Location),!is.na(`Plane Height`),!is.na(`Frame 1 lengths in R`))
  DF$dir <- plyr::revalue(DF$Behaviour, c("Flying D" = "Perpendicular", "Flying U" = "Perpendicular",
                                          "Flying L" = "Parallel", "Flying R" = "Parallel",
                                          "Flying DL" = "Parallel","Flying DR" = "Parallel",
                                          "Flying UL" = "Parallel","Flying UR" = "Parallel")) 
  DF <- as.data.frame(DF)
  
  lengths <- foreach(j=1:nrow(DF),.combine='rbind') %do%{
    
    mns <- foreach(i=1:7,.combine='rbind') %do%{
      colnams <- sapply(c("R","G","B"),function(x) paste0("Frame.",i,".lengths.in.",x)) 
      colinds <- sapply(colnams,function(x) grep(names(DF),pattern=x))
      datout <- get.mean(j,colinds,DF)
      return(tibble(outmean=datout$outmean,values=list(datout$values)))
    }
    fin.length <- mns$outmean[which.max(mns$outmean)]
    if(length(fin.length)==0){
      fin.values <- NA
    }
    fin.values <- unlist(mns$values[which.max(mns$outmean)])
    
    return(tibble(values=list(fin.values)))
  }
  
  DF <- tibble(DF)
  DF$lengthvals <- lengths
  return(DF) 
}  




### This takes the output from get.lengths (DatH) and
### get.refl.lengths (DatR), and calculates the flight heights
### using the bootstrapping technique

get.fhs <- function(DatH,DatR, bootsize){
  
  refl.par <- DatR[DatR$Dir == "Parallel",]
  refl.per <- DatR[DatR$Dir == "Perpendicular",]
  
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
  
  
  fhdata <- foreach(j = 1:nrow(DatH),.combine='rbind',.verbose = TRUE) %do% {
    
    print(j)
    if(length(DatH$lengthvals[[1]][[j]])>0){
      
      behav <- DatH$Behaviour[j]
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
      
      
      
      output <- foreach(i=DatH$lengthvals[[1]][[j]],.combine='rbind') %do%{
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
        lw <- mean(low,na.rm=T)
        md <- mean(mid,na.rm=T)
        hi <- mean(high,na.rm=T)
        
        return(data.frame(lw,md,hi))
      }
      
      
      mdheight <- mean(booto$md)
      lwheight <- as.numeric(quantile(booto$lw,0.025))
      hiheight <- as.numeric(quantile(booto$hi,0.975))
    }else{
      mdheight <- NA
      lwheight <- NA
      hiheight <- NA
    }
    
    
    
    return(data.frame(lwheight,mdheight,hiheight))
  }
  
  
  return(fhdata)
  
  
}








### This will generate the generic flight height plot (no wind turbine png)
## turbine.low = the lowest point of the turbine blades above sea level
## turbine.high = the heighest point of the turbine blades above sea level

plot.fhs <- function(titlename,fhdata,turbine.low,turbine.high){
  fhdata <- fhdata[order(fhdata$mdheight,fhdata$hiheight),]
  fhdata$birdid <- 1:nrow(fhdata)
  
  G <- ggplot(fhdata)+
    geom_errorbar(aes(ymin=lwheight,ymax=hiheight,x=birdid),width=0,size=2,colour='lightblue')+
    geom_point(aes(x=birdid,y=mdheight),color="black")+
    geom_hline(yintercept=turbine.low,linetype="dashed",size=1.25)+
    geom_hline(yintercept=turbine.high,linetype="dashed",size=1.25)+
    geom_hline(yintercept = 0)+
    #scale_y_continuous(limits = c(0,180))+
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
  P.idw <- gstat::idw(mid.height ~ 1, dmodt, newdata=grd, idp=idp)  #Percent_Overlap
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
