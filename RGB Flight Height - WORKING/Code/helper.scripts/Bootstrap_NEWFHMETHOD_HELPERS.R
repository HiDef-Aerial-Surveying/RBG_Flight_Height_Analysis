#############################################
### Helpful functions ###
##


plot.rgb <- function(DAT){
  require(grid)
  col <- rgb(DAT$R,DAT$G,DAT$B,maxColorValue = 255)
  dim(col) <- dim(DAT$R)
  return(grid.raster(col,interpolate=FALSE))
}


euc.dist <- function(x1, x2) sqrt(sum((x1 - x2) ^ 2))



### This function will return the points the selected by the user
### As well as the calculated start and end points for the tail
### and head in a frame
get.line.points <- function(FrameDat,Frame){
  Obj <- FrameDat[[Frame]][[1]][,,1]
  H <- c(Obj$Head[,,1]$x,Obj$Head[,,1]$y)
  Ta <- c(Obj$Tail[,,1]$x,Obj$Tail[,,1]$y)
  ## Flips the values if needed to line up with Matlab code
  if (abs(H[2]) > abs(Ta[2])){
    temp <- H;
    H = Ta;
    Ta = temp;  
  }
  add_p = 0.25 ## 25% added to length of the line
  # The starting points of the line (furthest from bird)
  H_p1 = round(H + add_p*(H-Ta))  
  T_p1 = round(Ta + add_p*(Ta-H)) 
  # The end points of the line (closest to the bird)
  H_p2 = round(H_p1 + (T_p1-H_p1)/3)
  T_p2 = round(T_p1 + (H_p1-T_p1)/3)
  
  return(list(H=H,Ta=Ta,H_st=H_p1,H_en=H_p2,T_st=T_p1,T_en=T_p2))
}







get.indices <- function(L.MATRIX,p1,p2){
  mat.orig <- L.MATRIX
  L.MATRIX[L.MATRIX == 0] <- NA
  newMat <- round(BBmisc::normalize(c(L.MATRIX),method = "range",range = c(0,1)),3)
  newMat <- matrix(newMat,nrow=nrow(mat.orig),ncol=ncol(mat.orig))
  indvals <- which(newMat >= 0.2 & newMat <= 0.4,arr.ind = TRUE)
  if(length(indvals)==0){
    indvals <- which(newMat >= 0.15 & newMat <= 0.5,arr.ind = TRUE)
  }
  
  ### Some of the indices will be the same, so make sure to only get the distinct values
  INDICES <- data.frame(indvals)
  
  ### This calculates the indices in reference to the image by using the extreme
  ### head and tail points calculated by the getlinepoints function.
  newINDICES <- foreach(i=1:nrow(INDICES),.combine='rbind') %do% {
    newY <- min(p2[2],p1[2]) + (INDICES[i,"col"] - 1)
    newX <- min(p2[1],p1[1]) + (INDICES[i,"row"] - 1)
    return(data.frame(row=newY,col=newX))
  }
  return(newINDICES)
}




#### Using the Head.indices and Tail.indices returned from the
#### get.indices function, get channel specific possible lengths
#### Using the euclidean distance function and every unique 
#### pair of coordinates
get.channel.lens <- function(Head.Indices,Tail.Indices,manual.length,perc.thresh=0.1){
  LENSOUT <- foreach(i=1:nrow(Head.Indices),.combine='c')%do%{
    aa <- foreach(j=1:nrow(Tail.Indices),.combine='c')%do%{
      lenval <- euc.dist(Head.Indices[i,],Tail.Indices[j,])
      return(lenval)
    }
    return(aa)
  }
  upper <- manual.length + (manual.length * perc.thresh)
  lower <- manual.length - (manual.length * perc.thresh)
  LENSOUT <- LENSOUT[LENSOUT < upper & LENSOUT > lower]
  
  return(LENSOUT)
}





### Gets the list of all calculated lengths (in pixels) from a specific frame
### Uses FrameDat, Frame and LPs (LPs returned from get.line.points function)
get.all.lengths <- function(FrameDat, Frame, LPs){
  ### Using the custom "get.indices" function, we calculate the actual locations
  ### of the points where colour change > 75th percentile.
  ### This is done for the R,G and B matrices
  
  Hx <- FrameDat[[Frame]][[1]][,,1]$Head[,,1]$x
  Hy <- FrameDat[[Frame]][[1]][,,1]$Head[,,1]$y
  
  Tx <- FrameDat[[Frame]][[1]][,,1]$Tail[,,1]$x
  Ty <- FrameDat[[Frame]][[1]][,,1]$Tail[,,1]$y
  
  manual.length <- euc.dist(c(Hx,Hy),c(Tx,Ty))
  
  headR.MATRIX <- FrameDat[[Frame]][[1]][,,1]$Head.Mat[,,1]$R
  headG.MATRIX <- FrameDat[[Frame]][[1]][,,1]$Head.Mat[,,1]$G
  headB.MATRIX <- FrameDat[[Frame]][[1]][,,1]$Head.Mat[,,1]$B
  tailR.MATRIX <- FrameDat[[Frame]][[1]][,,1]$Tail.Mat[,,1]$R
  tailG.MATRIX <- FrameDat[[Frame]][[1]][,,1]$Tail.Mat[,,1]$G
  tailB.MATRIX <- FrameDat[[Frame]][[1]][,,1]$Tail.Mat[,,1]$B
  
  headR.INDICES <- get.indices(headR.MATRIX,LPs$H_st,LPs$H_en)
  headG.INDICES <- get.indices(headG.MATRIX,LPs$H_st,LPs$H_en)
  headB.INDICES <- get.indices(headB.MATRIX,LPs$H_st,LPs$H_en)
  
  tailR.INDICES <- get.indices(tailR.MATRIX,LPs$T_st,LPs$T_en)
  tailG.INDICES <- get.indices(tailG.MATRIX,LPs$T_st,LPs$T_en)
  tailB.INDICES <- get.indices(tailB.MATRIX,LPs$T_st,LPs$T_en)
  
  Rlens <- get.channel.lens(headR.INDICES,tailR.INDICES,manual.length)
  Glens <- get.channel.lens(headG.INDICES,tailG.INDICES,manual.length)
  Blens <- get.channel.lens(headB.INDICES,tailB.INDICES,manual.length)
  
  return(list(R=Rlens,G=Glens,B=Blens))
  
}




### This takes the output from get.lengths (DatH) and
### get.refl.lengths (DatR), and calculates the flight heights
### using the bootstrapping technique

get.fhs.mat <- function(DatH,DatR, bootsize){
  Spplist <- SPP[SPP %in% unique(DatH$Species)] # we can only choose species in the main SPP list that are present in the dataframe for each month
  
  DatR$Dir <- plyr::revalue(DatR$Behaviour, c("Flying D" = "Perpendicular", "Flying U" = "Perpendicular",
                                              "Flying L" = "Parallel", "Flying R" = "Parallel",
                                              "Flying DL" = "Parallel","Flying DR" = "Parallel",
                                              "Flying UL" = "Parallel","Flying UR" = "Parallel")) 
  
  DatH$dir <- plyr::revalue(DatH$Behaviour, c("Flying D" = "Perpendicular", "Flying U" = "Perpendicular",
                                              "Flying L" = "Parallel", "Flying R" = "Parallel",
                                              "Flying DL" = "Parallel","Flying DR" = "Parallel",
                                              "Flying UL" = "Parallel","Flying UR" = "Parallel")) 
  
  specs <- foreach(k=1:length(Spplist),.combine='rbind') %do% {
    print(Spplist[k])
    DatR1 <- DatR[DatR$Species == Spplist[k],]
    
    refl.par <- DatR1[DatR1$Dir == "Parallel",]
    refl.per <- DatR1[DatR1$Dir == "Perpendicular",]
    
    ## Bootstrap parallel facing birds
    refl.par.boot <- foreach(i=1:bootsize,.combine='rbind') %do% {
      sampl <- sample(refl.par$LENS,size=length(refl.par$LENS),replace=T)
      sample.mean <- unlist(lapply(sampl, mean)) # added this line to get the mean of each distribution
      return(tibble(mnsmp = mean(sample.mean, na.rm = TRUE))) 
    }
    
    mnrfl.par <- mean(refl.par.boot$mnsmp, na.rm = TRUE)
    rngrefl.par <- range(unlist(refl.par.boot$mnsmp), na.rm = TRUE)

    ## Bootstrap perpendicular facing birds
    refl.per.boot <- foreach(i=1:bootsize,.combine='rbind') %do% {
      sampl <- sample(refl.per$LENS,size=length(refl.per$LENS),replace=T)
      sample.mean <- unlist(lapply(sampl, mean)) # added this line to get the mean of each distribution
      return(tibble(mnsmp = mean(sample.mean, na.rm = TRUE))) 
    }
    
    mnrfl.per <- mean(refl.per.boot$mnsmp, na.rm = TRUE)
    rngrefl.per <- range(unlist(refl.per.boot$mnsmp), na.rm = TRUE) 

    
    refl.all.boot <- foreach(i=1:bootsize,.combine='rbind') %do% {
      sampl <- sample(DatR$LENS,size=length(DatR$LENS),replace=T)
      sample.mean <- unlist(lapply(sampl, mean)) # added this line to get the mean of each distribution
      return(tibble(mnsmp = mean(sample.mean, na.rm = TRUE))) 
    }
    
    mnrfl.all <- mean(refl.all.boot$mnsmp, na.rm = TRUE)
    rngrefl.all <- range(unlist(refl.all.boot$mnsmp), na.rm = TRUE)
    
    DatH1 <- DatH[which(DatH$Species == Spplist[k]),]
    fhdata <- foreach(j = 1:nrow(DatH1),.combine='rbind',.verbose = TRUE) %do% {
      
      print(j)
      if(length(DatH1$LENS[[j]])>0){
        
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
        
        DatH1$LENS[[j]] <- (DatH1$LENS[[j]]*DatH1$CalibrationFixed[j])*DatH1$calprop[j]
        
        output <- foreach(i=DatH1$LENS[[j]],.combine='rbind') %do%{
          airc <- DatH1$`Plane Height`[j]
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
      
      
      
      return(data.frame(lwheight,mdheight,hiheight, Species = DatH1$Species[j], month = DatH1$month[j], year = DatH1$year[j], "Reel Ref" = DatH1$`Reel Ref`[j], "Frame Ref" = DatH1$`Frame Ref`[j], "Marker Number" = DatH1$`Marker Number`[j]))
    }
    
    return(fhdata)
  }
  
  return(specs)
  
  
}




### This takes the output from get.lengths (DatH) and
### get.refl.lengths (DatR), and calculates the flight heights
### using the bootstrapping technique
# 
# get.fhs.mat <- function(DatH,DatR, bootsize){
#   Spplist <- SPP[SPP %in% unique(DatH$Species)] # we can only choose species in the main SPP list that are present in the dataframe for each month
#   
#     DatH$dir <- plyr::revalue(DatH$Behaviour, c("Flying D" = "Perpendicular", "Flying U" = "Perpendicular",
#                                               "Flying L" = "Parallel", "Flying R" = "Parallel",
#                                               "Flying DL" = "Parallel","Flying DR" = "Parallel",
#                                               "Flying UL" = "Parallel","Flying UR" = "Parallel")) 
#   
#   specs <- foreach(k=1:length(Spplist),.combine='rbind') %do% {
#     print(Spplist[k])
#     DatR1 <- DatR[DatR$Species == Spplist[k],]
#     
#     refl.par <- DatR1[DatR1$Dir == "Parallel",]
#     refl.per <- DatR1[DatR1$Dir == "Perpendicular",]
#     
#     ## Bootstrap parallel facing birds
#     refl.par.boot <- foreach(i=1:bootsize,.combine='c') %do% {
#       sampl <- sample(refl.par$fin.length,size=length(refl.par$fin.length),replace=T)
#       return(mean(sample))
#     }
#     
#     mnrfl.par <- mean(refl.par.boot)
#     rngrefl.par <- range(refl.par.boot)
#     
#     ## Bootstrap perpendicular facing birds
#     refl.per.boot <- foreach(i=1:bootsize,.combine='c') %do% {
#       sampl <- sample(refl.per$fin.length,size=length(refl.per$fin.length),replace=T)
#       return(mean(sampl))
#     }
#     mnrfl.per <- mean(refl.per.boot)
#     rngrefl.per <- range(refl.per.boot) 
#     
#     
#     refl.all.boot <- foreach(i=1:bootsize,.combine='c') %do% {
#       sampl <- sample(DatR$fin.length,size=length(DatR$fin.length),replace=T)
#       return(mean(sampl))
#     }
#     mnrfl.all <- mean(refl.all.boot)
#     rngrefl.all <- range(refl.all.boot) 
#     
#     DatH1 <- DatH[which(DatH$Species == Spplist[k]),]
#     fhdata <- foreach(j = 1:nrow(DatH1),.combine='rbind',.verbose = TRUE) %do% {
#       
#       print(j)
#       if(length(DatH1$LENS[[j]])>0){
#         
#         behav <- DatH1$dir[j]
#         if(behav == "Perpendicular"){
#           mnrfl <- mnrfl.per
#           rngrefl <- rngrefl.per
#         }else if(behav == "Parallel"){
#           mnrfl <- mnrfl.par
#           rngrefl <- rngrefl.par
#         }else{
#           mnrfl <- mnrfl.all
#           rngrefl <- rngrefl.all
#         }
#         
#         DatH1$LENS[[j]] <- DatH1$LENS[[j]]*DatH1$CalibrationFixed[j]
#         
#         output <- foreach(i=DatH1$LENS[[j]],.combine='rbind') %do%{
#           airc <- DatH$`Plane Height`[j]
#           bhmn <- sapply(mnrfl,function(x) flight.height(airc,x,i))
#           bhdn <- sapply(rngrefl[2],function(x) flight.height(airc,x,i))
#           bhup <- sapply(rngrefl[1],function(x) flight.height(airc,x,i))
#           
#           x <- tibble(bhdn,bhmn,bhup)
#           return(x)
#         }
#         
#         output[output < 0] <- 0
#         
#         print("Running bootstrap...")
#         booto <- foreach(i=1:bootsize,.combine='rbind') %do% {
#           low <- sample(output$bhdn,size=nrow(output),replace=T)
#           mid <- sample(output$bhmn,size=nrow(output),replace=T)
#           high <- sample(output$bhup,size=nrow(output),replace=T)
#           lw <- as.numeric(quantile(low,0.025, na.rm = TRUE))
#           md <- mean(mid,na.rm=T)
#           hi <- as.numeric(quantile(high,0.975, na.rm = TRUE))
#           
#           return(data.frame(lw,md,hi))
#         }
#         
#         
#         mdheight <- mean(booto$md)
#         lwheight <-  mean(booto$lw,na.rm=T)#
#         hiheight <-  mean(booto$hi,na.rm=T)#
#       }else{
#         mdheight <- NA
#         lwheight <- NA
#         hiheight <- NA
#       }
#       
#       
#       
#       return(data.frame(lwheight,mdheight,hiheight, Species = DatH1$Species[j], month = months(DatH1$`Survey Date`[j]), year = substring(DatH1$`Survey Date`[j], 1, 4), "Reel Ref" = DatH1$`Reel Ref`[j], "Frame Ref" = DatH1$`Frame Ref`[j], "Marker Number" = DatH1$`Marker Number`[j]))
#     }
#     
#     return(fhdata)
#   }
#   
#   return(specs)
#   
#   
# }
