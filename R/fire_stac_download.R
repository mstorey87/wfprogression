#' Download Landsat and Sentinel 2 images.
#'
#' @description
#' Download landsat and sentinel 2  false colour images. Images are clipped to fire_bbox.
#' Requires that data frame output from fire_stac_search()
#'
#'
#' @param fire_bbox sf polygon of search area, same polygon used as input to fire_stac_search()
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
fire_stac_download <- function(fire_bbox,stac_df,dest_folder){



  fn_get_raster <- function(url, max_retries = 5, wait_time = 2) {
    attempt <- 1
    while (attempt <= max_retries) {
      tryCatch({
        r <- terra::rast(url)
        # Check if it's a valid raster by confirming it has dimensions
        if (nrow(r) > 0 && ncol(r) > 0) {
          return(r)
        }
      }, error = function(e) {
        message("Attempt ", attempt, " failed: ", conditionMessage(e))
        Sys.sleep(wait_time)
      })
      attempt <- attempt + 1
    }
    stop("Max retries reached. Could not load raster.")
  }






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


      #ensure extents of bbox and rast overlap, otherwise crop will throw error and stop
      b1_bbox <- sf::st_bbox(dat$band1_stream[[1]]) %>% sf::st_as_sfc() %>% sf::st_as_sf()
      f_bbox <- sf::st_transform(fire_bbox,dat$crs[[1]])

      olap <- sf::st_intersects(b1_bbox,f_bbox)
      if(lengths(olap)==0){
        print(paste0("skipping ",dat$datetimelocal_chr,", image outside bbox"))
        next
      }



      #crop images before downloading
      b1 <- terra::crop(dat$band1_stream[[1]],f_bbox)
      b2 <- terra::crop(dat$band2_stream[[1]],f_bbox)
      b3 <- terra::crop(dat$band3_stream[[1]],f_bbox)

      #resample resolutions to match
      b3 <- terra::resample(b3,b1)
      b2 <- terra::resample(b2,b1)

      #create rgb and write to file
      rgb <- c(b1,b2,b3)

      #stretch to 255 max
      rgb <- terra::stretch(rgb)

      #write a temp file and then copy to output directory.
      #This help it work on shinyapp.io, but is not required if run locally.
      out.file.temp <- tempfile(pattern= paste0(dat$datetimelocal_chr,"_",dat$product,"_",dat$tile_dateutc,"utc_"),fileext = ".tif")
      print(out.file.temp)
      terra::writeRaster(rgb,out.file.temp,overwrite=T)

      out.file <- paste0(dest_folder,"\\",basename(out.file.temp))
      print(out.file)
      if(!out.file.temp==out.file) file.copy(out.file.temp,out.file,overwrite = T)

    }




}
