fh <- function(airc,refl,atheight){
  return(airc * (1 - (refl/atheight)))
}

airc <- 550

bird1.frame1 <- rnorm(30,35,3)
bird1.frame2 <- rnorm(30,33,3)
bird1.frame3 <- rnorm(30,37,3)

bird.at.ref <- rnorm(200,36,5)

mean(bird1.frame1)
mean(bird1.frame2)
mean(bird1.frame3)


output <- foreach(i=bird1.frame3,.combine='c') %do%{
  bhs <- sapply(bird.at.ref,function(x) fh(airc,x,i))
}

booto <- foreach(i=1:1000,.combine='c') %do% {
  sampl <- sample(output,size=length(output),replace=T)
  return(mean(sampl))
}

mean(booto)

quantile(booto,c(0.025,0.975))



bird1.frame3



final.length = 37.85849 (+CIs)

mean 34.6, CIs(+-)


birda <- c(34.5, 37.86, 39.1)
refl <-  c(33.1, 34.6, 37.2)




min.fh <- fh(airc,33.1,34.5)
max.fh <- fh(airc,37.2,39.1)
mean.fh <- fh(airc,34.6,37.86)
max.fh
mean.fh
min.fh





bird2 <- rnorm(40,26,14)


birds.at.reflection <- c(32.5, 34.7, 36.4...)



95th birds.at.reflection max = 38.5,   min 32.4,   




bird1.frame3 <- rnorm(30,37,3)
bird1.frame3.hts <- sapply(bird1.frame3,function(x) fh(airc,34.6,x))

bird <- bird1.frame3.hts
plot(density(bird1.frame3.hts))
rotor.min <- 50

chitest <- function(bird,rotor.min){
  testf <- data.frame(above=length(which(bird>rotor.min)),below=length(which(bird<rotor.min)))
  return(chisq.test(testf))
}

chitest(bird1.frame3.hts,rotor.min)





chitest(bird2,rotor.min)


b1 <- data.frame(dat=bird1,bird="bird1")
b2 <- data.frame(dat=bird2,bird="bird2")
b <- rbind(b1,b2)


