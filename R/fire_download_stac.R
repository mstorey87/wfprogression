#' Download and save false colour Landsat and Sentinel 2 images.
#'
#' @param fire_bbox Polygon of search area, same polygon used as input to fire_search_stac()
#' @param stac_df Data frame that is the output of fire_search_stac
#' @param dest_folder the output folder for geotifs.
#'
#' @return Writes geotifs to file
#' @export
#'
#' @examples
#' #outdir <- "C:\\temp\\landsat_and_sentinel"
#' #dir.create(outdir)
#' #fire_download_stac(dat.bbox,dat.stac,outdir)
fire_download_stac <- function(fire_bbox,stac_df,dest_folder){


  #stream and crop all the bands
  #these functions only deal with 3 selected bands for now.
  stac_df <- stac_df %>%
    dplyr::mutate(band1_stream=purrr::map(band1,~terra::rast(paste0("/vsicurl/",.x))),
                  band2_stream=purrr::map(band2,~terra::rast(paste0("/vsicurl/",.x))),
                  band3_stream=purrr::map(band3,~terra::rast(paste0("/vsicurl/",.x))),
                  #crop all the images to extent of bounding box
                  crs=purrr::map(band1_stream,terra::crs))

    #loop through one by one (to save memory) and create cropped rgb and write to disk
    for(i in 1:nrow(stac_df)){
      dat <- stac_df[i,]
      b1 <- terra::crop(dat$band1_stream[[1]],sf::st_transform(fire_bbox,dat$crs[[1]]))
      b2 <- terra::crop(dat$band2_stream[[1]],sf::st_transform(fire_bbox,dat$crs[[1]]))
      b3 <- terra::crop(dat$band3_stream[[1]],sf::st_transform(fire_bbox,dat$crs[[1]]))

      #resample resolutions to match
      b3 <- terra::resample(b3,b1)
      b2 <- terra::resample(b2,b1)

      #create rgb and write to file
      rgb <- c(b1,b2,b3)

      rgb <- terra::stretch(rgb)

      out.file.temp <- tempfile(pattern= paste0(dat$datetimelocal_chr,"_",dat$product,"_",dat$tile_dateutc,"utc"),fileext = ".tif")
      terra::writeRaster(rgb,out.file.temp,overwrite=T)

      out.file <- paste0(dest_folder,"/",dat$datetimelocal_chr,"_",dat$product,"_",dat$tile_dateutc,"utc",".tif")
      if(!out.file.temp==out.file) file.copy(out.file.temp,out.file,overwrite = T)

    }




}
