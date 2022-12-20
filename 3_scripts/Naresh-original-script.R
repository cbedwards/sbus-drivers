## Naresh code


rm(list = ls())
library(sp)
library(raster)
library(tidyverse)


#This "r" is at coarser resolution, about 7 km, and we want to count the land cover types that fall within each of this file's pixel !!!
mygrid <- raster("~/Google Drive File Stream/My Drive/ModelingGreenUPPhenoMisMatch/ExtractingLULC/MasterLivnehGrid.tif")
plot(mygrid)
mygrid
#us <- shapefile("tl_2017_us_state/tl_2017_us_state.shp")
#plot(us, axes=T)
#plot(r, extent=extent(us))
#plot(us, add=T)


#This us is the land cover type from NASA, this will be for Canada and USA
USandCAN <- raster("LCType.tif")
USandCAN

plot(USandCAN )
plot(mygrid)

res(mygrid)/res(USandCAN)

# a test on a small dataset
# --------------------------------
#g <- crop(mygrid, extent(-100,-63,22.9375,52.87694))
#g <- crop(mygrid, extent(-100,-99.9,40,40.1))
#plot(g)
#p <- rasterToPolygons(g)
p <- rasterToPolygons(mygrid)
plot(p, add=T)
df <- raster::extract(USandCAN, p); df
length(df)
for(i in 1:length(df)){
  print(table(df[[i]]))
}

library(gtools)
grand <- data.frame()
for(i in 1:length(df)){
  d <- data.frame(table(df[[i]]))
  d2 <- as.data.frame(t(d[,2]))
  colnames(d2) <- d[,1]
  class(d2)
  grand <- smartbind(grand, d2)
}
grand
coordinates(p)
grand <- cbind(coordinates(p), grand); grand
colnames(grand)[1:2] <- c("long", "lat"); grand




# actual cataset
# ------------------
p <- rasterToPolygons(mygrid)
plot(mygrid)

plot(p, add=T, col="gray")
plot(canadaLULC, add=T)
plot(usaLULC, add=T)

# extract one country at a time
# since you have made a extracting grid without erasing the unnecessary cells (it looks rectangular), the code goes over all the pixes in the rectangle
# one solution, create a grid just to cover your study area
# or, after data extraction, erase all cells withotu any value for any of the class
# my guess is that the data merging might fail with the second approach, so, use the first solution

df <- raster::extract(usaLULC, p); df
length(df)
for(i in 1:length(df)){
  print(table(df[[i]]))
}

library(gtools)
grand <- data.frame()
for(i in 1:length(df)){
  d <- data.frame(table(df[[i]])); e
  d2 <- as.data.frame(t(d[,2]))
  colnames(d2) <- d[,1]
  class(d2)
  grand <- smartbind(grand, d2)
}
grand
coordinates(p)
grand <- cbind(coordinates(p), grand); grand
colnames(grand)[1:2] <- c("long", "lat"); grand
