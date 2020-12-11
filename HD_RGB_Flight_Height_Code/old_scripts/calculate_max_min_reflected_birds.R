library(tidyverse)

calc_cv <- function(x) { (sd(x,na.rm=T) / mean(x,na.rm=T))*100 }
X <- read.csv("data/all_merged_old.csv")

unique(X$Species)

spp <- "Kittiwake"
reflfilter <- c("Y",'Yes')

X1 <- X %>% 
  dplyr::filter(Species == spp,
                Reflection. %in% reflfilter) %>%
  dplyr::select(Frame.1:Frame.8)


X1 <- X1 %>% hablar::convert(hablar::num( Frame.1,Frame.2,
                                          Frame.3,Frame.4,
                                          Frame.5,Frame.6,
                                          Frame.7,Frame.8))
X2 <- X1 %>%
  mutate(
    means = pmap_dbl(.,function(...) mean(c(...),na.rm=T)),
    sds = pmap_dbl(.,function(...) sd(c(...),na.rm=T))
  ) %>%
  mutate(
    CVs = pmap_dbl(list(Frame.1,Frame.2,
                        Frame.3,Frame.4,
                        Frame.5,Frame.6,
                        Frame.7,Frame.8),
                   function(...) calc_cv(c(...)))
  ) %>%
  dplyr::filter(!is.na(CVs),CVs<10) %>%
  mutate(
    maxval = pmap_dbl(list(Frame.1,Frame.2,
                           Frame.3,Frame.4,
                           Frame.5,Frame.6,
                           Frame.7,Frame.8),
                      function(...) max(c(...),na.rm=T))
  )

nrow(X2)


## Note - LBBG October 5 2020 - 11 were removed when Paul re-ided birds in August so there are only 19 LBBGs

quantile(X2$means,c(0.025,0.975))
quantile(X2.Lbbg$means,c(0.025,0.975))

