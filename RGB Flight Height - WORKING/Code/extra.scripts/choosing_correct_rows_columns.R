SppList <- c("kittiwake", "gannet", "lesser black-backed gull", "herring gull", "great black-backed gull")


all.data <- list.files("outputs/SppDat")

lapply(all.data, function(x){
  tt <- readxl::read_xlsx(paste0("outputs/SppDat/",x))
  tt <- tt %>% filter(Species %in% SppList,)
  
  if(length(intersect(names(tt), "Resolution")) > 0){
  
  tt <- tt %>% select("Location",
                      "Survey Date",
                      "Camera", 
                      starts_with("Resolution"),
                      "Reel Ref",
                      starts_with("Time"),
                      "Frame Ref",
                      "Marker Number",
                      starts_with("Broad Category"),
                      starts_with("Species Group"),
                      "Group Confidence",
                      "Species",
                      "Species Confidence",
                      "Behaviour",
                      "Approximate Age",
                      "lon",
                      "lat",
                      "lwheight",
                      "mdheight",
                      "hiheight")
  }else{
    tt <- tt %>% select("Location",
                        "Survey Date",
                        "Camera", 
                        "Resolution.x",
                        "Reel Ref",
                        "Time.x",
                        "Frame Ref",
                        "Marker Number",
                        "Broad Category.x",
                        "Species Group.x",
                        "Group Confidence",
                        "Species",
                        "Species Confidence",
                        "Behaviour",
                        "Approximate Age",
                        "lon",
                        "lat",
                        "lwheight",
                        "mdheight",
                        "hiheight")
  }
  colnames(tt) <- c("Location",
                    "Survey Date",
                    "Camera", 
                    "Resolution",
                    "Reel Ref",
                    "Time",
                    "Frame Ref",
                    "Marker Number",
                    "Broad Category",
                    "Species Group",
                    "Group Confidence",
                    "Species",
                    "Species Confidence",
                    "Behaviour",
                    "Approximate Age",
                    "Longitude",
                    "Latitude",
                    "Lower Height (m) (2.5% CI)",
                    "Mean Height (m)",
                    "Upper Height (m) (97.5% CI)")
  
  tt$`Survey Date` <- format(tt$`Survey Date`, format = "%d/%m/%Y")
  tt$Time <- strftime(tt$Time, format = "%H:%M:%S")
  writexl::write_xlsx(tt, paste0("outputs/Zone85_FlightHeightData/", x))
})
