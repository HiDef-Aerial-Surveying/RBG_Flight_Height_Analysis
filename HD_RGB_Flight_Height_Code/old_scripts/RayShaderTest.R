
library(rayshader)
library(rgdal)
library(gstat) 
library(sp)
library(ggplot2)
library(viridis)

Test <- data.frame(
  y = round(runif(100,45,72),4),
  x = round(runif(100,100,130),4),
  z = c(runif(25,10,15),runif(25,5,15),runif(50,15,20))
)

coordinates(Test) <- c('x','y')
proj4string(Test) <- CRS("+init=epsg:4326")

# Create an empty grid where n is the total number of cells
grd              <- as.data.frame(spsample(Test, "regular", n=50000))
names(grd)       <- c("X", "Y")
coordinates(grd) <- c("X", "Y")
gridded(grd)     <- TRUE  # Create SpatialPixel object
fullgrid(grd)    <- TRUE  # Create SpatialGrid object

# Add P's projection information to the empty grid
raster::projection(grd) <- proj4string(Test)

# Interpolate the grid cells using a power value of 2 (idp=2.0)
P.idw <- gstat::idw(z ~ 1, Test, newdata=grd, idp=2.0)

# Convert to raster object then clip to Texas
r       <- raster::raster(P.idw)


elmat = raster_to_matrix(r)

##('imhof1','imhof2','imhof3','imhof4','desert', 'bw', and 'unicorn').

elmat %>%
  sphere_shade(texture = "unicorn",colorintensity = 6) %>%
  add_shadow(ray_shade(elmat, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0) %>%
  plot_3d(elmat, zscale = 0.25, fov = 0, theta = 135, 
          zoom = 0.75, phi = 45, windowsize = c(1000, 800))
Sys.sleep(0.2)
render_snapshot()



test_spdf <- as(r, "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")

g <- ggplot() +  
geom_tile(data=test_df, aes(x=x, y=y, fill=value), alpha=0.8) + 
scale_fill_viridis() +
coord_equal() +
  ggthemes::theme_map()+
theme(legend.position="none")


par(mfrow = c(1, 2))

plot_gg(g, width = 5, height = 5, raytrace = FALSE, preview = TRUE)
plot_gg(g, width = 5, height = 5, multicore = TRUE, scale = 250, 
        zoom = 0.7, theta = 10, phi = 30, windowsize = c(800, 800))
Sys.sleep(0.2)
render_snapshot(clear = TRUE)

render_depth(focus = 0.68, focallength = 200,clear=TRUE)
render_depth(focus = 1.2, focallength = 100,clear=TRUE)










Test <- data.frame(
  y = round(runif(100,45,72),4),
  x = round(runif(100,100,130),4),
  z = c(runif(25,10,15),runif(25,5,15),runif(50,15,20)),
  Species = c(rep("Guillemot",50),rep("Gannet",50))
)

corrections <- data.frame(Species=c("Guillemot","Gannet"),
                          corr = c(1.45,1.67))

#############################
Test$corrected <- NA
for(i in unique(Test$Species)){
  Test$corrected[Test$Species == i] <- Test$z[Test$Species == i] * corrections$corr[corrections$Species==i]
}

##########################
Test %>% 
  left_join(corrections,by="Species") %>%
  mutate(
    corrected = z*corr
  )



x <- c(rnorm(100,25,5))

hist(x)



