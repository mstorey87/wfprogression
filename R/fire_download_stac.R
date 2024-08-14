fire_download_stac <- function(stac_df,dest_folder){

    #loop through one by one (to save memory) and create cropped rgb and write to disk
    for(i in 1:nrow(stac_df)){
      dat <- stac_df[i,]
      b1 <- terra::crop(dat$band1_stream[[1]],sf::st_transform(fire_bbox,dat$crs[[1]]))
      b2 <- terra::crop(dat$band2_stream[[1]],sf::st_transform(fire_bbox,dat$crs[[1]]))
      b3 <- terra::crop(dat$band3_stream[[1]],sf::st_transform(fire_bbox,dat$crs[[1]]))

      #resample resolutions to match
      b3 <- terra::resample(b3,b1)
      b2 <- terra::resample(b2,b1)

      #create rgb
      rgb <- c(b1,b2,b3)

      out.file <- paste0(dest_folder,"/",dat$datetimelocal_chr,"_",dat$product,"_",dat$tile_dateutc,"utc",".tif")

      terra::writeRaster(rgb,out.file)



    }




}
