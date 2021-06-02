library(raster)
library(tidyverse)
library(sf)
library(sp)
library(rgdal)
library(mapview)

# ---------------------------------------------------------------------
# -------- Create a raster from the grid of individual polygons -------
# ---------------------------------------------------------------------

# create CRS to transform grid to
crs <-  "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=NAD83"

# read in grid
hsi_grid <- read_sf('data/CMP_2023/MP2023_S00_G000_C000_U00_V00_SLA_I_00_00_V_grid480.shp') %>%
  st_transform(crs)

# create an empty raster from the extent of the grid at a 480m res, set to the CRS
r <- raster(
  ext = extent(hsi_grid),
  res = c(480,480),
  crs = crs
  )

# make a raster from the hsi grid using the empty raster "r" framework
grid <- fasterize::fasterize(hsi_grid, r, field = "CELLID")


mapview(grid)

# -----------------------------------------------
# ---- Another way of doing the same process ----
# Read in HSI Grid
hsi_grid <- sf::read_sf("data/CMP_2023/MP2023_S00_G000_C000_U00_V00_SLA_I_00_00_V_grid480.shp")

pts <- hsi_grid %>%
  st_centroid() %>%
  st_transform(4326)

pts <- pts %>%
  mutate(lon = st_coordinates(pts)[,1],
         lat = st_coordinates(pts)[,2]) %>%
  st_drop_geometry() %>%
  select(lon, lat, CELLID)

sp::coordinates(pts) <- ~lon+lat

proj4string(pts)=CRS("+init=epsg:4326") # set it to lat-long
# transform to NAD83
pts = spTransform(pts,CRS('+proj=utm +zone=15 +datum=NAD83 +units=m +no_defs'))

# Pts gridded
gridded(pts) = TRUE

# raster from points
r = raster(pts)

projection(r) = CRS('+proj=utm +zone=15 +datum=NAD83 +units=m +no_defs')

mapview(r)
