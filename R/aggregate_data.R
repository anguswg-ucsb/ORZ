
# Oyster Resource zones data aggregation & cleaning using LDWF API behind online Oyster map
# Lynker
# Angus Watters
# 05/17/2021

library(tidyverse)
library(gridExtra)
library(raster)
library(sf)
library(mapview)
library(lubridate)
library(DBI)
library(RMariaDB)
library(rio)
library(arrow)

# CRMS CSV file, that I convert to a parquet to save space
crms <- readr::read_csv("data/CRMS_Continuous_Hydrographic.csv",
                progress = TRUE) %>%
  janitor::clean_names() %>% # snake_case the column headers
  dplyr::select(1:20, 43:46)  %>% # select relevant columns, removed wind variables
  rename(date = date_mm_dd_yyyy)
crms$date <- mdy(crms$date) # change character data variable to m-d-y Date variable

# convert Stata to SPSS
convert("data/CRMS_Continuous_Hydrographic.csv", "data/CRMS_Continuous_Hydrographic.parquet")

crms <- arrow::read_parquet("data/crms_continuous_hydrographic.parquet")


# API calls to LDWF API
layers <- list()
for (layer in (0L:17L)) {
  layer_name <- paste0("layer_", layer)
  layers[[layer_name]] <-
    jsonlite::fromJSON(
      paste0(
        "http://gis.wlf.la.gov/arcgis/rest/services/Fisheries_Services",
        "/Oyster_Lease_Web_Map/MapServer/",
        as.character(layer),
        "/query?",
        "where=&text=&",
        "objectIds=", paste(0L:400L, collapse = ","),
        "&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=",
        "&spatialRel=esriSpatialRelIntersects&relationParam=",
        "&outFields=&returnGeometry=true&maxAllowableOffset=",
        "&geometryPrecision=&outSR=",
        "&returnIdsOnly=false&returnCountOnly=false&orderByFields=",
        "&groupByFieldsForStatistics=&outStatistics=&returnZ=false",
        "&returnM=false&gdbVersion=&returnDistinctValues=false",
        "&returnTrueCurves=true&resultOffset=&resultRecordCount=&",
        "f=pjson"
      )
    ) %>%
    `[`(-which(names(.) == "spatialReference")) %>%
    tibble::as_tibble()
}
getwd()
# desired shapefiles
cultch <- tidy_layer(layers$layer_10) %>%
  st_cast("POLYGON")
# st_write(cultch, "data/shapefile/cultch_polygon.shp")

aoc <- layers$layer_6[-3,] %>%
  tidy_layer() %>%
  st_cast("POLYGON")
st_write(cultch, "data/shapefile/aoc_polygon.shp")

pub_seedgr <- layers$layer_12 %>%
  tidy_layer() %>%
  st_cast("POLYGON")
st_write(pub_seedgr, "data/shapefile/public_seed_grounds_polygon.shp")
lhd <- layers$layer_2 %>%
  tidy_layer() %>%
  st_cast("POLYGON")
lhd_pending <- layers$layer_3 %>%
  tidy_layer() %>%
  st_cast("POLYGON")
oyst_lease <- layers$layer_14 %>%
  tidy_layer() %>%
  st_cast("POLYGON")

# CPRA projects
cpra_projects <- st_read("data/CPRA Projects.gdb")
mapview(cpra_projects[1:5,])


lst <- list()
for (i in 1:nrow(aoc)) {
  coords <- aoc$features[i,2] %>%
    unlist()
  names <- aoc$features[i,1]
  lon = coords[1:4]
  lat = coords[5:8]

  df <- data.frame(names = names, lon = lon, lat = lat) %>%
    st_as_sf(coords = c("lon", "lat"), crs = "+proj=lcc +lat_1=29.3 +lat_2=30.7 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=1000000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs") %>%
    st_transform(4326)
    # st_cast("MULTIPOINT") %>%
    # st_cast("POLYGON", group_or_split = F)
    # mutate(location = names)

  lst[[i]] = df
}

# bind rows of list to make a sf dataframe
sf <- bind_rows(lst)

# create bounding box to use to filter out extreme points
bb <- sf$geometry[1] %>%
  st_transform(5070) %>%
  st_buffer(400000) %>%
  st_transform(4326) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf()

# filter out extreme points
sf <- st_filter(sf, bb)

# create a polygon from set of points for each location
polys = st_sf(
  aggregate(
    sf$geometry,
    list(sf$NAME),
    function(g){
      st_cast(st_union(g),"POLYGON")
    }
  ))

# view
mapview(polys)



# 1) Cultch Plants/Cultch Plant Waypoints (not sure what the difference is between these two)
# 2) Public seed grounds
# 3) Oyster leases/applications
# 4) AOC permits


# dplyr::copy_to(con, crms, name = "crms_hydrographic_table", temporary=FALSE)
#
# dbplyr::db_copy_to()# Query and collect SQL database
# df <- tbl(con, "crms_hydrographic_table") %>%
  # group_by(station_id, date) %>%
  # replace(is.na(.), 0) %>%
  # mutate(station_id = station_id, across(3:19, mean),
  #           latitude = latitude,
  #           longitude = longitude,
  #           time_zone = time_zone,
  #           sensor_environment = sensor_environment) %>%
  filter(date >= "2015-01-01") %>%
  collect()

stations <- df %>%
  group_by(station_id) %>%
  slice(n = 1)

df[!unique(df$station_id),]
# download from Webpage
# download <- 'https://cims.coastal.louisiana.gov/RequestedDownloads/ZippedFiles/CRMS_Continuous_Hydrographic.zip'
# temp <- tempfile()
# download.file(url = download, temp, mode="wb")
# unzip(temp, 'CRMS_Continuous_Hydrographic.csv')
# dd <- read.table("gbr_Country_en_csv_v2.csv", sep=",",skip=2, header=T)

# Read in CRMS hourly Hydrographic data  https://cims.coastal.louisiana.gov/FullTableExports.aspx#
crms <- readr::read_csv("data/CRMS_Continuous_Hydrographic.csv",
                        n_max = 1000000,
                        progress = TRUE) %>%
  janitor::clean_names() %>% # snake_case the column headers
  dplyr::select(1:20, 43:46)  %>% # select relevant columns, removed wind variables
  rename(date = date_mm_dd_yyyy)
f <- function(x, pos) print(pos)


crms$date <- mdy(crms$date) # change character data variable to m-d-y Date variable

# filter dates after 2015, ~36% of data
df <- filter(df, date >= "2015-01-01")

# group CRMS by stations and date and get daily averages of variables for each location
df <- df %>%
  as.tibble() %>%
  group_by(station_id, date) %>%
  replace(is.na(.), 0) %>%
  summarise(station_id = station_id, across(3:19, mean),
            latitude = latitude,
            longitude = longitude,
            time_zone = time_zone,
            sensor_environment = sensor_environment)

sf <- df %>%
  group_by(station_id, date) %>%
  arrange(date) %>%
  slice(n = 1)

sf <- sf %>%
  ungroup() %>%
  group_by(station_id) %>%
  slice(n =1)

sf <- sf %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
projects <- sf::st_read("data/CPRA Projects.gdb")

mapview(projects[2,])
#first import all files in a single folder as a list
rastlist <- list.files(path = "data/CMP/2017/land_change.gdb/raster",
                       pattern='.tif$',
                       all.files=TRUE,
                       full.names=TRUE)

