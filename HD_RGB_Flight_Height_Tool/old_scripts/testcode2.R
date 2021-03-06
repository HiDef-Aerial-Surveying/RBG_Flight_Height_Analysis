

dfils <- list.files(path="C:/Users/Bird3/Desktop/AB 25 months - Flight Height sheets/New folder/",pattern="*.xlsx",full.names=T)
for(i in dfils){
  x <- readxl::read_excel(i,sheet="Data")
  print(i)
  print(ncol(x))
}


dat <- readxl::read_xlsx("C:/Users/Bird3/Desktop/AB 25 months - Flight Height sheets/New folder/Zone65_M01_S01_D01_C5_20 Complete MW.xlsx", sheet="Data", range = cell_cols("A:G"))


flist <-c("Frame 1","Frame 2","Frame 3","Frame 4","Frame 5","Frame 6","Frame 7","Frame 8")
dat[,flist][dat[,flist]=="N/A"] <- NA
dat <- dat %>% hablar::convert(hablar::num(`Frame 1`,`Frame 2`,
                                           `Frame 3`,`Frame 4`,
                                           `Frame 5`,`Frame 6`,
                                           `Frame 7`,`Frame 8`))


data_mod <- dat %>%
  dplyr::filter(Species==spp) %>%
  dplyr::select(Date,Camera,`Reel Name`,Frame,`Marker Number`,`Frame X`,`Frame Y`,`Plane Height`:`Frame 8`) %>%
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




T100.inPCH <- sum(data_mod$Thresh_100)
T99.inPCH <- sum(data_mod$Thresh_99)
T90.inPCH <- sum(data_mod$Thresh_90)
T66.6.inPCH <- sum(data_mod$Thresh_66.6)
T33.3.inPCH <- sum(data_mod$Thresh_33.3)
T10.inPCH <- sum(data_mod$Thresh_10)
T1.inPCH <- sum(data_mod$Thresh_1)
T0.inPCH <- sum(data_mod$Thresh_0)


T100.outPCH <- nrow(data_mod) - sum(data_mod$Thresh_100)
T99.outPCH <- nrow(data_mod) - sum(data_mod$Thresh_99)
T90.outPCH <- nrow(data_mod) - sum(data_mod$Thresh_90)
T66.6.outPCH <- nrow(data_mod) - sum(data_mod$Thresh_66.6)
T33.3.outPCH <- nrow(data_mod) - sum(data_mod$Thresh_33.3)
T10.outPCH <- nrow(data_mod) - sum(data_mod$Thresh_10)
T1.outPCH <- nrow(data_mod) - sum(data_mod$Thresh_1)
T0.outPCH <- nrow(data_mod) - sum(data_mod$Thresh_0)


proportion_thresh_100 <- T100.inPCH/nrow(data_mod)
proportion_thresh_99 <- T99.inPCH/nrow(data_mod)
proportion_thresh_90 <- T90.inPCH/nrow(data_mod)
proportion_thresh_66.6 <- T66.6.inPCH/nrow(data_mod)
proportion_thresh_33.3 <- T33.3.inPCH/nrow(data_mod)
proportion_thresh_10 <- T10.inPCH/nrow(data_mod)
proportion_thresh_1 <- T1.inPCH/nrow(data_mod)
proportion_thresh_0 <- T0.inPCH/nrow(data_mod)




out_df <- data.frame(
  "IPCC description" = c("Threshold value", "Number inside collision height",
                         "Number outside collision height", "Total number of birds",
                         "Proportion at collision height"),
  "Certain" = c(1,T100.inPCH,T100.outPCH,T100.inPCH+T100.outPCH,proportion_thresh_100),
  "Virtually certain" = c(0.99,T99.inPCH,T99.outPCH,T99.inPCH+T99.outPCH,proportion_thresh_99),
  "Very likely" = c(0.9,T90.inPCH,T90.outPCH,T90.inPCH+T90.outPCH,proportion_thresh_90),
  "Likely" = c(0.666,T66.6.inPCH,T66.6.outPCH,T66.6.inPCH+T66.6.outPCH,proportion_thresh_66.6),
  "Unlikely" = c(0.333,T33.3.inPCH,T33.3.outPCH,T33.3.inPCH+T33.3.outPCH,proportion_thresh_33.3),
  "Very unlikely" = c(0.1,T10.inPCH,T10.outPCH,T10.inPCH+T10.outPCH,proportion_thresh_10),
  "Exceptionally unlikely" = c(0.01,T1.inPCH,T1.outPCH,T1.inPCH+T1.outPCH,proportion_thresh_1),
  "Impossible" = c(0,T0.inPCH,T0.outPCH,T0.inPCH+T0.outPCH,proportion_thresh_0)
)
names(out_df) <- c("IPCC description","Certain", "Virtually certain", "Very likely",
                   "Likely","Unlikely","Very unlikely","Exceptionally unlikely",
                   "Impossible")