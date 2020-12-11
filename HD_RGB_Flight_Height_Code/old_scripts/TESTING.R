library(foreach)
X <- data.frame(readxl::read_xlsx("Data/Sample_data.xlsx"))

calc_cv <- function(x,sokal.correction=F) {
  cv1 <- (sd(x,na.rm=T) / mean(x,na.rm=T))
  if(sokal.correction==TRUE){
    cv1 <- (1+(1/(4*length(x)))) * cv1
  }
  return(cv1*100)
  }

out <- foreach(i=2:ncol(X),.combine='c') %do%{
  vals <- as.numeric(strsplit(X[,i]," ")[[1]])
  return(vals)
}



cc <- data.frame(out=out)
qt <- data.frame(lc = quantile(cc$out,c(0.025)),uc = quantile(cc$out,c(0.975)))

ggplot(cc) +
  geom_density(aes(y = out),fill='blue',alpha=0.4) +
  geom_hline(data=qt,aes(yintercept=lc),linetype='dashed')+
  geom_hline(data=qt,aes(yintercept=uc),linetype='dashed')+
  geom_hline(data=qt,aes(yintercept=mean(out)))+
  geom_rug(aes(y = out, x = 0), position = position_jitter(height = 0))+
  scale_y_continuous(limits=c(25,50),expand=c(0,0))+
  scale_x_continuous(expand=c(0,0),limits=c(0,0.2))+
  ylab("Length (cm)")+ 
  ggthemes::theme_gdocs()


FrameNames <- unique(unlist(strsplit(names(X)[2:ncol(X)],"R|G|B")))

calc_cv(out2,sokal.correction = TRUE)

out2 <- foreach(i=FrameNames,.combine='cbind') %do%{
  dat <- X[,grep(names(X),pattern=i)]
  vals <- as.vector(sapply(1:ncol(dat),function(x)as.numeric(strsplit(dat[,x]," ")[[1]])))
  mval <- mean(vals)
  return(mval)
}




datf <- data.frame(bird=0)
for(i in 1:3){
  for(j in c("R","G","B")){
    cnmR <- paste0("F",i,j)  
    vals <- paste(round(runif(1,34,37),4),
                  round(runif(1,34,37),4),
                  round(runif(1,34,37),4),
                  round(runif(1,34,37),4))
    datf2 <- data.frame(vals)
    names(datf2) <- cnmR
    datf <- cbind(datf,datf2)
  }
}



flight.height <- function(aircraft.height, reflected.size, measured.size){
  aircraft.height*(1-(reflected.size/measured.size))
}


max(out2)
min(out2)

datout <- foreach(i=2:ncol(datf),.combine='c') %do%{
  vals <- as.numeric(strsplit(X[,i]," ")[[1]])
  return(vals)
}


maxheights <- sapply(datout,function(x) flight.height(545,x,max(out2)))

minheights <- sapply(datout,function(x) flight.height(545,x,min(out2)))


cc <- data.frame(out=maxheights)
cc$out[cc$out < 0] <- 0
qt <- data.frame(lc = quantile(cc$out,c(0.025)),uc = quantile(cc$out,c(0.975)))

cc2 <- data.frame(out2=minheights)
#cc2$out2[cc2$out2 < 0] <- 0
qt2 <- data.frame(lc2 = quantile(cc2$out2,c(0.025)),uc2 = quantile(cc2$out2,c(0.975)))

ggplot(cc) +
  geom_density(aes(y = out),fill='blue',alpha=0.4) +
  geom_hline(data=qt,aes(yintercept=lc),linetype='dashed')+
  geom_hline(data=qt,aes(yintercept=uc),linetype='dashed')+
  geom_hline(data=qt,aes(yintercept=mean(cc$out)))+
  #geom_rug(aes(y = out, x = 0), position = position_jitter(height = 0))+
  
  geom_density(data=cc2,aes(y = out2),fill='orange',alpha=0.4) +
  geom_hline(data=qt2,aes(yintercept=lc2),linetype='dashed',color='red')+
  geom_hline(data=qt2,aes(yintercept=uc2),linetype='dashed',color='red')+
  geom_hline(data=qt2,aes(yintercept=median(cc2$out2)))+
  #geom_rug(data=cc2,aes(y = out2, x = 0), position = position_jitter(height = 0))+
  
  
  scale_y_continuous(limits=c(0,180),expand=c(0,0))+
  scale_x_continuous(expand=c(0,0),limits=c(0,0.05))+
  ylab("Height (m)")+ 
  ggthemes::theme_gdocs()






othertest <- data.frame(bird=0)

othertest <- foreach(i=datout,.combine='c')%do%{
  oo <- foreach(j=out,.combine='c')%do%{
    xx <- flight.height(545,i,j)
    return(xx)
  }
  return(oo)
}

ot <- data.frame(out=othertest)
ot$out[ot$out<0] <- 0
ggplot(ot) +
  geom_density(aes(y = out),fill='blue',alpha=0.4) +
  #geom_hline(data=qt,aes(yintercept=lc),linetype='dashed')+
  #geom_hline(data=qt,aes(yintercept=uc),linetype='dashed')+
  geom_hline(data=qt,aes(yintercept=mean(ot$out)))+
  #geom_rug(aes(y = out, x = 0), position = position_jitter(height = 0))+
  scale_y_continuous(limits=c(0,180),expand=c(0,0))+
  scale_x_continuous(expand=c(0,0),limits=c(0,0.05))+
  ylab("Height (m)")+ 
  ggthemes::theme_gdocs()


