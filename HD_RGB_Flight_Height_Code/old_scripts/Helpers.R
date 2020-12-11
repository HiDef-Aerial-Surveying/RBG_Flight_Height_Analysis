#############################################
## Helper functions for 3D rayshader plots ##
#############################################



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



calc_cv <- function(x,sokal.correction=F) {
  cv1 <- (sd(x,na.rm=T) / mean(x,na.rm=T))
  if(sokal.correction==TRUE){
    cv1 <- (1+(1/(4*length(x)))) * cv1
  }
  return(cv1*100)
}


se <- function(x) sqrt(var(x)/length(x))



#Flight height formula
# flight.height <- function(aircraft.height, reflected.size, measured.size,cam.angle){
#   
#   aircraft.distance <- aircraft.height / cos(cam.angle*pi/180)
#   ## This equation is the height of a bird diagonally to the surface
#   hyp <- aircraft.distance*(1-(reflected.size/measured.size))
#   ## Needs to be corrected using Trig to calculate the adjacent height
#   bird.height <- cos(cam.angle * pi/180) * hyp
#   return(bird.height)
# }

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




calc.flight.height <- function(dat,spp){
  dat <- data.frame(dat)
  dat$Species <- tolower(dat$Species)
  names(dat) <- c("Date","Camera","Reel Name", "Frame","Marker Number","Species", "Plane Height",
                  "Frame 1", "Frame 2","Frame 3", "Frame 4", "Frame 5","Frame 6","Frame 7", "Frame 8",
                  "Lat","Lon")
  flist <-c("Frame 1","Frame 2","Frame 3","Frame 4","Frame 5","Frame 6","Frame 7","Frame 8")
  dat[,flist][dat[,flist]=="N/A"] <- NA
  dat <- dat %>% hablar::convert(hablar::num(`Plane Height`, `Frame 1`,`Frame 2`,
                                             `Frame 3`,`Frame 4`,
                                             `Frame 5`,`Frame 6`,
                                             `Frame 7`,`Frame 8`))
  
  sp_code <- Species.inputs$Species_Code[Species.inputs$Species == spp]
  
  data_mod <- dat %>%
    dplyr::filter(Species==tolower(spp)) %>%
    # dplyr::select(Date,Camera,`Reel Name`,Frame,`Marker Number`,`Frame X`,`Frame Y`,`Plane Height`:`Frame 8`) %>%
    mutate(
      rsums = pmap_dbl(list(`Frame 1`,`Frame 2`,
                            `Frame 3`,`Frame 4`,
                            `Frame 5`,`Frame 6`,
                            `Frame 7`,`Frame 8`),
                       function(...) sum(c(...), na.rm = TRUE))
    ) %>%
    dplyr::filter(rsums != 0) %>%
    mutate(variance = pmap_dbl(list(`Frame 1`,`Frame 2`,
                                    `Frame 3`,`Frame 4`,
                                    `Frame 5`,`Frame 6`,
                                    `Frame 7`,`Frame 8`),
                               function(...) var(c(...), na.rm = TRUE)),
           CV = pmap_dbl(list(`Frame 1`,`Frame 2`,
                              `Frame 3`,`Frame 4`,
                              `Frame 5`,`Frame 6`,
                              `Frame 7`,`Frame 8`),
                         function(...) calc_cv(c(...))),
           mean = pmap_dbl(list(`Frame 1`,`Frame 2`,
                                `Frame 3`,`Frame 4`,
                                `Frame 5`,`Frame 6`,
                                `Frame 7`,`Frame 8`),
                           function(...) mean(c(...), na.rm = TRUE)),               
           max = pmap_dbl(list(`Frame 1`,`Frame 2`,
                               `Frame 3`,`Frame 4`,
                               `Frame 5`,`Frame 6`,
                               `Frame 7`,`Frame 8`),
                          function(...) max(c(...), na.rm = TRUE))
    ) %>%
    ## Filter out the CV values that are too high
    dplyr::filter(CV < 100) %>%
    ### Now we calculate the mean - 2 * SD of the Max bird length to eliminate values
    #dplyr::filter(max > (mean(max) - 2*sd(max))) %>%
    mutate(
      ## Apply the flight height formula to calculate the minimum height and maximum height that
      ## the bird could be flying at
      Min_bird_height = pmap_dbl(list(`Plane Height`,
                                      Species.inputs$Max_length[Species.inputs$Species_Code == sp_code],
                                      max),
                                 flight.height),
      Max_bird_height = pmap_dbl(list(`Plane Height`,
                                      Species.inputs$Min_length[Species.inputs$Species_Code == sp_code],
                                      max),
                                 flight.height)
      
    ) %>% 
    mutate(
      Flight_Height_Range = Max_bird_height - Min_bird_height,
      ## We calculate the percent overlap of the bird's range in the rotor range
      Percent_Overlap = pmap_dbl(list(Min_bird_height,Max_bird_height,
                                      rotor.range[1],rotor.range[2]),
                                 HTSSIP:::perc_overlap)
    ) %>%
    # Summarize proportion of birds in and out of collision height --------
  mutate(
    Thresh_100 = pmap_dbl(list(Percent_Overlap,100),bird.range.thresh),
    Thresh_99 = pmap_dbl(list(Percent_Overlap,99),bird.range.thresh),
    Thresh_90 = pmap_dbl(list(Percent_Overlap,90),bird.range.thresh),
    Thresh_66.6 = pmap_dbl(list(Percent_Overlap,66.6),bird.range.thresh),
    Thresh_33.3 = pmap_dbl(list(Percent_Overlap,33.3),bird.range.thresh),
    Thresh_10 = pmap_dbl(list(Percent_Overlap,10),bird.range.thresh),
    Thresh_1 = pmap_dbl(list(Percent_Overlap,1),bird.range.thresh),
    Thresh_0 = pmap_dbl(list(Percent_Overlap,0),bird.range.thresh)
  )
  
  data_mod$MONTH <- month(data_mod$Date)
  data_mod$YEAR <- year(data_mod$Date)
  return(data_mod)
}


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
  P.idw <- gstat::idw(out ~ 1, dmodt, newdata=grd, idp=idp)  #Percent_Overlap
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
  newout$Lat <- Tom$Latitude
  newout$Lon <- Tom$Longitude
  
  lenmnInds <- grep(names(newout),pattern="lenmean_*")
  newout <- newout %>% dplyr::filter(!is.na(lenmean_Frame.1),lenmean_Frame.1<150)
  newout$cv <- sapply(1:nrow(newout),function(x) calc_cv(as.vector(unlist(newout[x,lenmnInds])),sokal.correction=T))
  newout <- newout %>% dplyr::filter(cv < 10)
  newout$birdlen <- sapply(1:nrow(newout),function(x) max(newout[x,lenmnInds],na.rm=T))
  
  return(newout)
}

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


