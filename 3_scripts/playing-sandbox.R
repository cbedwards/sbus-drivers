## sandbox
library(here)
library(sp)
library(raster)
library(tidyverse)
mygrid <- raster(here("1_raw_data/tmax/annualTMAX_1979.tif"))
plot(mygrid)
mygrid
# p <- rasterToPolygons(mygrid)
# plot(p, add=T)

boston = cbind(-71.06, 42.36)
davis = cbind(-121.74, 38.54)
locs = rbind(boston, davis)
points(x = boston[1], y = boston[2])
df <- raster::extract(mygrid, locs);
df
length(df)


## goals:
##  for a given spatial extent, find the yearly (or other time frame) means for the given metric
##  return this as a dataframe that can be saved or analyzed.

## important first step: how does extract work with polygons?

x1 = rbind(c(-80, 40), c(-80, 35), c(-85, 35), c(-85, 40))
x2 = rbind(c(-82, 38), c(-82, 33), c(-87, 33), c(-87, 38))
a = spPolygons(x1, x2)
b = spPolygons(list(x1, x2))
plot(mygrid)
# plot(a, add = T)
plot(b, add = T, col = 'coral')

df <- raster::extract(mygrid, b)[[1]]

## quick check of numerics:
sum(df)/length(df)
mean(df)


## Function(s) to define region of interest:
##  option 1: polygon from range map
##  option 2: x-km squares centered on points (50x50km at a minimum, since that's equivalent to naba circles)
##  option 3: polygon MINUS data coverage. Here I think we actually just use 1 and 2, and create a weighted difference.
##  Option 4: the collection of points corresponding to survey locations (probably just return each year)

library(data.table, code)
library(tidyverse)
get_survey_points = function(species){
  raw = as.data.frame(fread(here("2_data_wrangling/cleaned-data/cleaned-data-aggregated.csv")))
  dat = raw[raw$code == code, ]
  dat.mat = dat %>%
    select(lon, lat) %>%
    round(4) %>%
    unique() %>%
    as.matrix()
  ## might decide we want to return something with more details as well. Leaving structure in place for this.
  return(list(mat = dat.mat, details = NULL))
}

make_survey_poly = function(){
  return()
}


## Function to take spatial extent, metric, and temporal resolution, and return a dataframe with a col for the average metric
## across the space (using raster::extract), the temporal identifier, and the number of cells involved. For option 4,
## return each point separately for each year.

get_drivers = function(space, # points or polygon(s)
                       driver, #tmax, tmin, precip
                       temporal.res = ){ #currently only annual

  return(df)
}


pts.cur = get_survey_points("PIERAP")

df <- raster::extract(mygrid, pts.cur$mat);
