######################################################
### Make estimates of the error in the camera data ###
######################################################

X <- readxl::read_xlsx("Data/Error.xlsx")

flight.height <- function(plane.height,reflected.length,measured.length){
  fh <- plane.height * (1 - (reflected.length/measured.length))
  return(fh)
}

sd(height)/sqrt(length(height))

X$heightcalc <- sapply(1:nrow(X),function(x) flight.height(X$`GPS Height`[x],X$Measurement_16[x],X$Measurement_172[x]))
mean(X$heightcalc)
sd(X$heightcalc)
