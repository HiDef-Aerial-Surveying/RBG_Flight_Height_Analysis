library(foreach)
library(tidyverse)
wkd <- "C:/Users/Bird3/Desktop/AB 25 months - Flight Height sheets/New folder"


flist <- list.files(path=wkd,full.names=TRUE)


tframe <- readxl::read_xlsx(flist[1])
tframe <- tframe %>% dplyr::select(Date,Camera,`Reel Name`,Frame,`Marker Number`, Species, `Plane Height`, `Frame 1`:`Frame 8`)
nn <- names(tframe)




dat <- foreach(k=flist,.combine='rbind')%do%{
#  incProgress(1/n, detail = paste("Merging", k))
 # Sys.sleep(0.02)
#for(k in flist){
  
    v <- readxl::read_xlsx(k)
  names(v)[grep("Flight height",names(v))] <- "Plane Height"
  names(v)[grep("Plane height",names(v))] <- "Plane Height"
  if(length(grep("Plane Height",names(v)))>0){
    v <- v %>%
      dplyr::select(Date,Camera,`Reel Name`,Frame,`Marker Number`, Species, `Plane Height`, `Frame 1`:`Frame 8`)
    return(v)
  }

}

dat <- data.frame(dat)
names(dat) <- c("Date","Camera","Reel Name", "Frame","Marker Number","Species", "Plane Height",
                "Frame 1", "Frame 2","Frame 3", "Frame 4", "Frame 5","Frame 6","Frame 7", "Frame 8")



flist <-c("Frame 1","Frame 2","Frame 3","Frame 4","Frame 5","Frame 6","Frame 7","Frame 8")
dat[,flist][dat[,flist]=="N/A"] <- NA
dat <- dat %>% hablar::convert(hablar::num(`Plane Height`,`Frame 1`,`Frame 2`,
                                           `Frame 3`,`Frame 4`,
                                           `Frame 5`,`Frame 6`,
                                           `Frame 7`,`Frame 8`))
spp <- "Kittiwake"
data_mod <- dat %>%
  dplyr::filter(Species==spp) %>%
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
                                    Species.inputs$Max_length[Species.inputs$Species_Code == 'KI'],
                                    max),
                               flight.height),
    Max_bird_height = pmap_dbl(list(`Plane Height`,
                                    Species.inputs$Min_length[Species.inputs$Species_Code == 'KI'],
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
