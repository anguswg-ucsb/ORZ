# make even clases from raster values
get_classes <- function(rast, lvls){
  int <- seq(
    cellStats(rast, min),
    cellStats(rast, max),
    length.out = lvls
  )
  from    <- int[1:(lvls - 1)]
  to      <- int[2:lvls]
  becomes <- 1:(lvls -1)
  reclass_m <- matrix(c(from, to, becomes),
                      ncol = 3,
                      byrow = FALSE)
  categories <- reclassify(rast, reclass_m, include.lowest = TRUE)
}

# checks geometry and adds character column of name
detect_geometry <- function(string) {
  is_polygon  <- stringr::str_detect(tolower(string), "polygon")
  is_polyline <- stringr::str_detect(tolower(string), "polyline")
  is_point    <- stringr::str_detect(tolower(string), "point")
  ifelse(
    is_polygon,
    "POLYGON",
    ifelse(
      is_polyline,
      "LINESTRING",
      ifelse(
        is_point,
        "POINT",
        NA
      )
    )
  )
}

# Tidy layer geometries from LDWF API
tidy_layer <- function(layer) {
  initial_clean <-
    layer %>%
    dplyr::select(geometryType, features) %>%
    tidyr::unpack(cols = c(tidyselect::everything(), -geometryType)) %>%
    tidyr::unpack(cols = c(tidyselect::everything(), -geometryType)) %>%
    dplyr::mutate(
      geometryType = detect_geometry(geometryType)
    ) %>%
    dplyr::rename(
      Name = tidyselect::contains("name")
    )
  geometry_column <-
    initial_clean %>%
    dplyr::select(tidyselect::last_col()) %>%
    colnames()

  final_clean <-
    initial_clean %>%
    dplyr::rename(
      geometry = tidyselect::contains(geometry_column)
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      geometry = sf::st_sfc(sf::st_multipoint(geometry[,,]))
    ) %>%
    # dplyr::mutate(
    #   geometry = dplyr::case_when(
    #     geometryType == "POLYGON"    ~ sf::st_cast(geometry, "POLYGON"),
    #     geometryType == "LINESTRING" ~ sf::st_cast(geometry, "LINESTRING"),
    #     geometryType == "POINT" ~ sf::st_cast(geometry, "POINT")
    #     # geometryType == "MULTIPOINT" ~ sf::st_cast(geometry, "MULTIPOINT"),
    #   )
    # ) %>%
    dplyr::ungroup() %>%
    sf::st_sf(
      crs = "+proj=lcc +lat_1=29.3 +lat_2=30.7 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=1000000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs"
    ) %>%
    sf::st_transform(4326) %>%
    sf::st_as_sf() %>%
    dplyr::select(-geometryType)
}
# This function downloads and prepares data based on user provided start and end dates
OISST_sub_dl <- function(time_df){
  OISST_dat <- griddap(x = "hawaii_d90f_20ee_c4cb_LonPM180",
                       url = "https://coastwatch.pfeg.noaa.gov/erddap/",
                       time = c(time_df$start, time_df$end),
                       latitude = c(26.5, 30.5),
                       longitude = c(-94, -86),
                       fields = c("salt", "temp"))$data %>%
    mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>%
    dplyr::rename(t = time) %>%
    select(lon, lat, t, salt, temp) %>%
    na.omit()
}
# Date download range by start and end dates per year
# dl_years <- data.frame(date_index = 1:3,
#                        start = as.Date(c( "1990-01-01", "1998-01-01", "2006-01-01")),
#                        end = as.Date(c("1997-12-31", "2005-12-31", "2010-12-01")))
#   OISST_data <- dl_years %>%
#     group_by(date_index) %>%
#     group_modify(~OISST_sub_dl(.x)) %>%
#     ungroup() %>%
#     select(lon, lat, t, salt, temp)

tidy_raster <- function(df, rm_z, param) {
  s <- df %>%
    select(-rm_z) %>%
    group_by(t) %>%
    mutate(row = row_number()) %>%
    pivot_wider(names_from = "t", values_from = param)
  rs <-  rasterFromXYZ(s) %>%
    stack()
}

agg_monthly <- function(stack, metric) {
  #get the date from the names of the layers and extract the month
  indices <- format(as.Date(names(stack), format = "X%Y.%m.%d"), format = "%m")
  indices <- as.numeric(indices)

  #stack layers
  agg <- stackApply(stack, indices, fun = mean)
  agg <- stack(agg)
  # agg <- agg %>%
  #   setNames(c("sal_1", "sal_2", "sal_3", "sal_4","sal_5", "sal_6",
  #              "sal_7", "sal_8", "sal_9", "sal_10","sal_11", "sal_12"))
  #
}
# assigns value "1" if matrix position <= radius and value "NA" if matrix position> radius
make_circ_filter <- function(radius, res){
  circ_filter <- matrix(NA, nrow=1+(2*radius/res), ncol=1+(2*radius/res))
  dimnames(circ_filter)[[1]] <- seq(-radius, radius, by=res)
  dimnames(circ_filter)[[2]] <- seq(-radius, radius, by=res)
  sweeper <- function(mat){
    for(row in 1:nrow(mat)){
      for(col in 1:ncol(mat)){
        dist <- sqrt((as.numeric(dimnames(mat)[[1]])[row])^2 +
                       (as.numeric(dimnames(mat)[[1]])[col])^2)
        if(dist<=radius) {mat[row, col]<-1}
      }
    }
    return(mat)
  }
  out <- sweeper(circ_filter)
  return(out)
}
