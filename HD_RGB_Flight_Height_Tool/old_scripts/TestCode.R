## Bootstrap test for skewed data to get 95% or 2SD values
require(readxl)


X <- readxl::read_xlsx("./reflected_data/HiDef GANNET reflection height data ALL projects - CLEANED.xlsx")

out <- foreach(k=1:nrow(X),.combine='c') %do% {
  y <- unlist(X[k,3:10])
  return(y)
}
values <- as.numeric(out)
values <- values[!is.na(values)]




nboot <- 1000

bootstrapper <- function(values,nboot){
  resamples <- lapply(1:nboot, function(i) sample(values,size=ceiling(length(values)*0.5),replace=T))
  r.median <- sapply(resamples,median)
  r.mean <- sapply(resamples,mean)
  return(list(data=resamples,medians=r.median,means=r.mean))
}

output <- bootstrapper(values,nboot)
mean(values)
mean(output$means)

quantile(values,c(0.025,0.975))
quantile(output$means,c(0.025,0.975))






