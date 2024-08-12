#' Create a rgb false colour image
#'
#' @param band1_path tif to use for red band
#' @param band2_path tif to use for green band
#' @param band3_path tif to use for blue band
#' @param yaml_path odc-metadata.yaml file
#'
#' @return a 3 band tif
#' @export
#'
#' @examples
#' #
fire_landsat_tif <- function(band1_path,band2_path,band3_path,yaml_path,fire_bbox,dest_fold){


  #read yaml file for time and footprint
  yaml <- yaml::read_yaml(yaml_path)

  image_coords <- yaml$geometry$coordinates[[1]] %>%
    purrr::map(.,~paste0(.x,collapse = " "))


  image_poly <-  paste0("POLYGON ((",paste0(unlist(image_coords),collapse=", "),"))") %>%
    data.frame(wkt=.) %>%
    sf::st_as_sf(wkt="wkt",crs=yaml$crs)



  #get the time zone based on footprint centroid and add a local time field
  dat.aus <- rnaturalearth::ne_states(country="Australia") %>%
    dplyr::select(name) %>%
    sf::st_transform(3112)

  dat.tz <- dat.aus%>%
    sf::st_intersection(sf::st_centroid(image_poly %>% sf::st_transform(3112) %>% sf::st_union())) %>%
    dplyr::left_join(dat.timezone.names)


  #create a local date time string to use for file name
  chr_datetimelocal <- format(lubridate::with_tz(as.POSIXct(yaml$properties$datetime,tz="UTC"),
                                                 tz=dat.tz$tz_name),
                              format="%Y%m%d_%H%M%S")



  #compile rgb from individual bands and crop the image
  r1 <- terra::rast(band1_path)
  r2 <- terra::rast(band2_path)
  r3 <- terra::rast(band3_path)

  r_rgb <- c(r1,r2,r3)
  r_rgb <- terra::crop(r_rgb,fire_bbox %>% sf::st_transform(3112) %>% sf::st_buffer(50000) %>% sf::st_transform(sf::st_crs(r_rgb)))


  #write to file
  outtif <- paste0(dest_fold,"\\",substr(basename(band1_path),1,8),chr_datetimelocal,"_","rgb.tif")
  terra::writeRaster(r_rgb,outtif,overwrite=T)


}
