
############################################
real.data <- read.csv("Data/bird_lengths.csv")
load("Data/Dat.reflect")

to.impute <- c("lesser black-backed gull")

spp.to.compare <- c('kittiwake',"gannet","herring gull")

imputed.data <- impute.species(real.data,Dat.reflect,to.impute,spp.to.compare)

tibble(imputed.data)










#############################################################################
### A sense check on the data using lesser black-backed gull...
### Imputed the data for LBBG

compared <- Dat.reflect$fin.length[Dat.reflect$Species=="lesser black-backed gull"]
## Both of the following tests were not significant (means were the same)
ks.test(imputed.data,compared)
t.test(imputed.data,compared)

  


