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
fire_landsat_tif <- function(band1_path,band2_path,band3_path,yaml_path){

  yaml <- yaml::read_yaml(yaml_path)
  chr_datetimelocal <- format(lubridate::with_tz(as.POSIXct(yaml$properties$datetime,tz="UTC"),
                                                 tz=paste0("Australia/",tzone_city)),
                              format="%Y%m%d_%H%M%S")



  r1 <- terra::rast(band1_path)
  r2 <- terra::rast(band2_path)
  r3 <- terra::rast(band3_path)

  r_rgb <- c(r3,r2,r1)

  r_rgb <- terra::crop(r_rgb,dat.fire.polygon %>% st_transform(3112) %>% st_buffer(50000) %>% st_transform(st_crs(r_rgb)))

  outtif <- paste0(pth_landsat_tif,chr_datetimelocal,"_", substr(basename(dat.files.ftp$outfile[1]),4,7),"_rgb.tif")
  terra::writeRaster(r_rgb,outtif,overwrite=T)


}
