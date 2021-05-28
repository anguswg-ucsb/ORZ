library(raster)
library(tidyverse)
library(sf)
library(mapview)
crs <-  "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=NAD83"
hsi_grid <- read_sf('data/CMP_2023/MP2023_S00_G000_C000_U00_V00_SLA_I_00_00_V_grid480.shp') %>%
  st_transform(crs)
r <- raster(ext = extent(hsi_grid), res = c(480,480), crs = crs)
grid <- fasterize::fasterize(hsi_grid, r, field = "CELLID")
mapview(grid)

# Read in HSI Grid
hsi_grid <- sf::st_read("data/CMP_2023/MP2023_S00_G000_C000_U00_V00_SLA_I_00_00_V_grid480.shp")
st_crs(hsi_grid) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=NAD83"
crs = "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=NAD83"
crs(hsi_grid)
bb = st_bbox(c(xmin = -94.40728, ymin = 28.66945, xmax= -88.22224, ymax= 30.60358), crs = 4326) %>%
  st_as_sfc() %>%
  st_as_sf() %>%
  st_transform(crs)
# place holder values for grid
r <- raster(ext = extent(bb), resolution = c(480,480), crs = crs)
r[] = 1:ncell(r)
mapview::mapview(grid)

grid <- fasterize::fasterize(hsi_grid, r, field = "CELLID")

values(grid) <- 1:741884

hsi_grid2 <- hsi_grid %>%
  st_centroid() %>%

hsi_grid2 <- as(hsi_grid2, "Spatial")
r2 <- raster::rasterize(hsi_grid2, r, field = "CELLID")
raster::po
741884-173898
567986/2
283993+173898+283993
values(r2) <- 1:283993,
mapview(r2)
st_crs(hsi_grid) <-"+proj=lcc +lat_0=0 +lon_0=-100 +lat_1=48 +lat_2=33 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
crs = "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=NAD83 / UTM zone 15N"

bb = st_bbox(c(xmin = -94.40728, ymin = 28.66945, xmax= -88.22224, ymax= 30.60358), crs = 4326) %>%
  st_as_sfc() %>%
  st_as_sf() %>%
  st_transform(crs)
# place holder values for grid
r <- raster(ext = extent(bb), resolution = c(480,480), crs = crs)
r[] = 1:ncell(r)
mapview::mapview(r)

grid <- fasterize::fasterize(hsi_grid, r)
values(grid) <- 1:741884
mapview(grid)
library(sp)
library(rgdal)

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
r2 <- raster::rasterize(pts, field = "CELLID")
plot(pts)
plot(hsi_grid[1:130000,])
hsi_grid[1:600,]
aoi <- AOI::aoi_get("UCSB") %>%
  st_transform(5070)
crs(aoi)
ext <- raster::extent(hsi_grid)
ext <- raster::extent(c(xmin      = 404710,
                      xmax      =909670 ,
                      ymin      =3199480 ,
                      ymax       = 3374680))

val <- 1:383980
r <- raster(vals = val, ext = ext, resolution = 480, crs = "+proj=longlat +datum=WGS84")
1:04
ext <- raster::extent(c(xmin      = 404710,
                        xmax      = 909670 ,
                        ymin      = 3199480 ,
                        ymax      = 3374680))
# place holder values for grid
val <- 1:383980
r <- raster(vals = val, ext = ext, resolution = c(480,480),
            crs = "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=NAD83")
sf::st_crs(hsi_grid)
sf::st_cr
mapview::mapview(r)
raster::crs(r) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +datum=NAD83"

mapview(r$layer)

173898/2
install.packages("plotKML")
library(plotKML)
r <- vect2rast(hsi_grid)
