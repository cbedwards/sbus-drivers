## NOTE: it appears that each of our annual tifs are actually identical? Waiting for Naresh answer.

## sandbox
library(here)
library(sp)
library(raster)
library(tidyverse)
library(data.table)
library(geosphere)

mygrid <- raster(here("1_raw_data/precip/annual/annualPCP_1990.tif"))
plot(mygrid)
boston = cbind(-71.06, 42.36)
davis = cbind(-121.74, 38.54)
locs = rbind(boston, davis)
points(x = boston[1], y = boston[2])
df <- raster::extract(mygrid, locs);
df

mygrid <- raster(here("1_raw_data/precip/annual/annualPCP_2020.tif"))
plot(mygrid)
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

## For option 3:
get_survey_points = function(code){
  raw = as.data.frame(fread(here("2_data_wrangling/cleaned-data/cleaned-data-aggregated.csv")))
  dat = raw[raw$code == code, ]
  dat.mat = dat %>%
    select(lon, lat) %>%
    round(4) %>%
    unique() %>%
    as.matrix() %>%
    na.omit()
  ## might decide we want to return something with more details as well. Leaving structure in place for this.
  return(list(mat = dat.mat, details = NULL))
}

## should use get_survey_points and a distance in km, and return a joint polygon of all covered space.
## Probably easiest to use a square or a hex rather than an actual circle (blech, circles)
make_survey_poly = function(code,
                            width = 50){ #width of cube in kilometers
  pts = get_survey_points(code)$mat
  #convert to the diagonal distance from center to corner of cube, in meters
  dist.m = width*1000/2*sqrt(2)
  # dist.m = 50*1000/2*sqrt(2)
  up.right = destPointRhumb(p = pts,
                            b = 45,
                            d = dist.m)
  lwr.right = destPointRhumb(p = pts,
                             b = 45+90,
                             d = dist.m)
  lwr.left = destPointRhumb(p = pts,
                             b = 45+90+90,
                             d = dist.m)
  up.left = destPointRhumb(p = pts,
                            b = 45+90+90+90,
                            d = dist.m)
  #going to do some fiddly apply work to turn these into lists of corners
  all.corners = cbind(up.right, lwr.right, lwr.left, up.left)
  #function to take a row and turn it into a dataframe of lons and lats
  corner_2square = function(x){return((rbind(x[1:2], x[3:4], x[5:6], x[7:8])))}
  square.list = lapply(asplit(all.corners, MARGIN = 1), corner_2square)
  res = spPolygons(square.list)
  return(res)
}

## checking that it behaves decently:

cabbage.poly = make_survey_poly("PIERAP", width = 50)
plot(mygrid)
plot(cabbage.poly, add = T, col = 'coral')



## Function to take spatial extent, metric, and temporal resolution, and return a dataframe with a col for the average metric
## across the space (using raster::extract), the temporal identifier, and the number of cells involved. For option 4,
## return each point separately for each year.

get_drivers = function(space, # points or polygon(s)
                       driver, #tmax, tmin, precip
                       temporal.res){ #currently only annual
  ## Check that arguments are reasonable.
  stopifnot(temporal.res %in% c("annual"))
  stopifnot(driver %in% c("precip", "tmax", "tmin"))
  ## space should either by polygons, or a dataframe or matrix with 2 cols (for lat + lon)
  stopifnot(class(space) == "SpatialPolygons" |
              (class(space)[1] %in% c("matrix", "data.frame") & ncol(space) == 2))
  files.path = here("1_raw_data", driver, temporal.res)
  files.list = list.files(files.path)

  if(class(space)[1] == "SpatialPolygons"){
    res = data.frame(value = rep(-999, length(files.list)),
                     cell.count = rep(-999, length(files.list)),
                     temporal.id = rep("", length(files.list)))
    for(i in 1:length(files.list)){
      # grab temporal identifier from name
      temporal.id = strsplit(files.list[i], "_")[[1]][2]
      temporal.id = gsub("[.]tif", "", temporal.id)
      print(files.list[i])
      mygrid <- raster(paste0(files.path, "/", files.list[i]))
      vals = unlist(raster::extract(mygrid, space))
      res$value[i] = mean(vals, na.rm = TRUE)
      res$cell.count[i] = length(vals)
      res$temporal.id[i] = temporal.id
    }
  }else{
    res.list = list() #gonna do this the slightly slower way
    for(i in 1:length(files.list)){
      # grab temporal identifier from name
      temporal.id = strsplit(files.list[i], "_")[[1]][2]
      temporal.id = gsub("[.]tif", "", temporal.id)
      print(files.list[i])
      mygrid <- raster(paste0(files.path, "/", files.list[i]))
      vals = raster::extract(mygrid, space)
      df.cur = data.frame(lon = space[,1],
                          lat = space[,2],
                          value = vals)
      df.cur$temporal.id = temporal.id
      row.names(df.cur)=NULL
      res.list[[i]] =df.cur
    }
    res = do.call(rbind, res.list)
  }
  return(res)
}


## example: extract time points from
pts.cur = get_survey_points("PIERAP")
df.drivers = get_drivers(space = pts.cur$mat,
                         driver = "tmin",
                         temporal.res = "annual")
df.drivers$year = as.numeric(df.drivers$temporal.id)
ggplot(df.drivers, aes(x = year, y = value)) +
  geom_point()+
  geom_smooth()
## getting -1E9: whats going on? May be missing values?
df.drivers$value[df.drivers$value < -1e8] = NA

ggplot(df.drivers, aes(x = year, y = value)) +
  geom_point()+
  geom_smooth()

drivers.sum = df.drivers %>%
  group_by(year) %>%
  summarize(value = mean(value, na.rm = T))
ggplot(drivers.sum) +
  geom_path(aes(x = year, y = value))

## PROBLEM
temp = df.drivers[df.drivers$lon == df.drivers$lon[1] &
                    df.drivers$lat == df.drivers$lat[1],]
temp = na.omit(temp)

## checking polygon version
df.drivers = get_drivers(space = b,
                         driver = "tmin",
                         temporal.res = "annual")
df.drivers$year = as.numeric(df.drivers$temporal.id)
ggplot(df.drivers) +
  geom_path(aes(x = year, y = value))


## checking that we can get "data-coverage-to-average-climate" pipeline working
BCB.poly = make_survey_poly(code = "EUPHPHA", width = 50)
plot(mygrid)
plot(BCB.poly, add = T, col = 'coral')
df.drivers = get_drivers(space = BCB.poly,
                         driver = "tmin",
                         temporal.res = "annual")
