###############################################
### Helpers for the bootstrapping method
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



spp.data <- function(datf,reflectdf){
  
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



get.mean <- function(rowin,colinds,DatF){
  R <- as.numeric(unlist(strsplit(DatF[rowin,colinds["R"]],"      ")))
  G <- as.numeric(unlist(strsplit(DatF[rowin,colinds["G"]],"      "))) 
  B <- as.numeric(unlist(strsplit(DatF[rowin,colinds["B"]],"      ")))
  Rmean <- mean(R,na.rm=T)
  Gmean <- mean(G,na.rm=T)
  Bmean <- mean(B,na.rm=T)
  outmean <- mean(c(Rmean,Gmean,Bmean),na.rm=T)
  return(list(outmean=outmean,values=c(R,G,B)))
}

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







sapply(1:nrow(GX),function(x) mean(GX$lengthvals$values[x][[1]]))






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





get.fhs <- function(DatH,DatR){
  
  refl.par <- DatR[DatR$Dir == "Parallel",]
  refl.per <- DatR[DatR$Dir == "Perpendicular",]
  
  
  refl.par.boot <- foreach(i=1:1000,.combine='c') %do% {
    sampl <- sample(refl.par$fin.length,size=length(refl.par$fin.length),replace=T)
    return(mean(sampl))
  }
  
  mnrfl.par <- mean(refl.par.boot)
  rngrefl.par <- range(refl.par.boot)
  
  refl.per.boot <- foreach(i=1:1000,.combine='c') %do% {
    sampl <- sample(refl.per$fin.length,size=length(refl.per$fin.length),replace=T)
    return(mean(sampl))
  }
  mnrfl.per <- mean(refl.per.boot)
  rngrefl.per <- range(refl.per.boot) 
  
  
  refl.all.boot <- foreach(i=1:1000,.combine='c') %do% {
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
      booto <- foreach(i=1:1000,.combine='rbind') %do% {
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









plot.fhs <- function(titlename,fhdata){
  fhdata <- fhdata[order(fhdata$mdheight,fhdata$hiheight),]
  fhdata$birdid <- 1:nrow(fhdata)
  
  G <- ggplot(fhdata)+
    geom_errorbar(aes(ymin=lwheight,ymax=hiheight,x=birdid),width=0,size=2,colour='lightblue')+
    geom_point(aes(x=birdid,y=mdheight),color="black")+
    geom_hline(yintercept=25,linetype="dashed",size=1.25)+
    geom_hline(yintercept=150,linetype="dashed",size=1.25)+
    geom_hline(yintercept = 0)+
    scale_y_continuous(limits = c(0,180))+
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
  P.idw <- gstat::idw(mid.height ~ 1, dmodt, newdata=grd, idp=idp)  #Percent_Overlap
  r       <- raster::raster(P.idw)
  rr <- raster::mask(r, shapearea)
  
  return(list(rr=rr,dmodt=dmodt))
}












save_3d_gif <- function(hillshade, heightmap, file, duration = 5, ...) {
  require(rayshader)
  require(magick)
  require(rgl)
  require(gifski)
  require(rlang)
  
  # capture dot arguments and extract variables with length > 1 for gif frames
  dots <- rlang::list2(...)
  var_exception_list <- c("windowsize")
  dot_var_lengths <- purrr::map_int(dots, length)
  gif_var_names <- names(dots)[dot_var_lengths > 1 & 
                                 !(names(dots) %in% var_exception_list)]
  # split off dot variables to use on gif frames
  gif_dots <- dots[gif_var_names]
  static_dots <- dots[!(names(dots) %in% gif_var_names)]
  gif_var_lengths <- purrr::map_int(gif_dots, length)
  # build expressions for gif variables that include index 'i' (to use in the for loop)
  gif_expr_list <- purrr::map(names(gif_dots), ~rlang::expr(gif_dots[[!!.x]][i]))
  gif_exprs <- exprs(!!!gif_expr_list)
  names(gif_exprs) <- names(gif_dots)
  message(paste("gif variables found:", paste(names(gif_dots), collapse = ", ")))
  
  # TODO - can we recycle short vectors?
  if (length(unique(gif_var_lengths)) > 1) 
    stop("all gif input vectors must be the same length")
  n_frames <- unique(gif_var_lengths)
  
  # generate temp .png images
  temp_dir <- tempdir()
  img_frames <- file.path(temp_dir, paste0("frame-", seq_len(n_frames), ".png"))
  on.exit(unlink(img_frames))
  message(paste("Generating", n_frames, "temporary .png images..."))
  for (i in seq_len(n_frames)) {
    message(paste(" - image", i, "of", n_frames))
    rgl::clear3d()
    hillshade %>%
      plot_3d_tidy_eval(heightmap, !!!append(gif_exprs, static_dots))
    rgl::snapshot3d(img_frames[i])
  }
  
  # build gif
  message("Generating .gif...")
  magick::image_write_gif(magick::image_read(img_frames), 
                          path = file, delay = duration/n_frames)
  message("Done!")
  invisible(file)
}


plot_3d_tidy_eval <- function(hillshade, ...) {
  dots <- rlang::enquos(...)
  plot_3d_call <- rlang::expr(plot_3d(hillshade, !!!dots))
  rlang::eval_tidy(plot_3d_call)
}


#' Create a numeric vector of transition values.
#' @description This function helps generate a sequence 
#' of numeric values to transition "from" a start point
#' "to" some end point. The transition can be "one_way" 
#' (meaning it ends at the "to" point) or "two_way" (meaning
#' we return back to end at the "from" point).
#'
#' @param from starting point for transition values
#' @param to ending point (for one-way transitions) or turn-around point 
#'           (for two-way transitions)
#' @param steps the number of steps to take in the transation (i.e. the length
#'              of the returned vector)
#' @param one_way logical value to determine if we should stop at the "to" value
#'                (TRUE) or turn around and return to the "from" value (FALSE)
#' @param type string defining the transition type - currently suppoerts "cos"
#'             (for a cosine curve) and "lin" (for linear steps)
#'
#' @return a numeric vector of transition values
#' 
transition_values <- function(from, to, steps = 10, 
                              one_way = FALSE, type = "cos") {
  if (!(type %in% c("cos", "lin")))
    stop("type must be one of: 'cos', 'lin'")
  
  range <- c(from, to)
  middle <- mean(range)
  half_width <- diff(range)/2
  
  # define scaling vector starting at 1 (between 1 to -1)
  if (type == "cos") {
    scaling <- cos(seq(0, 2*pi / ifelse(one_way, 2, 1), length.out = steps))
  } else if (type == "lin") {
    if (one_way) {
      xout <- seq(1, -1, length.out = steps)
    } else {
      xout <- c(seq(1, -1, length.out = floor(steps/2)), 
                seq(-1, 1, length.out = ceiling(steps/2)))
    }
    scaling <- approx(x = c(-1, 1), y = c(-1, 1), xout = xout)$y 
  }
  
  middle - half_width * scaling
}
