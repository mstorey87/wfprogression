fire_download_himiwari <- function(fire_bbox,df_download,dest_folder){


  #transform bbox to himiwari crs
  #buffer first to capture wider area
  bbox_him <-fire_bbox %>%
    sf::st_transform(3112) %>%
    sf::st_buffer(60000) %>%
    sf::st_transform("+proj=geos +lon_0=140.7 +h=35785863 +x_0=0 +y_0=0 +a=6378137 +b=6356752.3 +units=m +no_defs") %>%
    sf::st_bbox()


  times_unique <- unique(df_download$datetimelocal_chr)


  #loop through and make an rgb or single band depending on time of day
  for(i in 1:length(times_unique)){

    dat.i <- df_download %>%
      dplyr::filter(datetimelocal_chr==times_unique[i]) %>%
      dplyr::arrange(band)

    if(dat.i$daynight[1]=="day" & nrow(dat.i==3)){

      dat.i <- dat.i %>%
        dplyr::mutate(nc=purrr::map(path_download,tidync::tidync),
                      rast=purrr::map(nc,~.x %>%
                                        tidync::activate(.x$variable$name[6]) %>%
                                        tidync::hyper_filter(y = y > bbox_him[2] & y < bbox_him[4],
                                                             x = x > bbox_him[1] & x < bbox_him[3]) %>%

                                        tidync::hyper_tibble() %>%
                                        dplyr::select(x,y,dplyr::everything(),-time)%>%
                                        terra::rast()))

      #combine to rgb, resampling to match cell sizes
      r <- c(dat.i$rast[[3]] ,
             dat.i$rast[[2]] %>% terra::resample(dat.i$rast[[3]]),
             dat.i$rast[[1]]%>% terra::resample(dat.i$rast[[3]]))


      #if not day or not all bands exist, just output band 7
    }else{

      dat.i <- dat.i %>%
        dplyr::filter(band=="B07")

      b7 <- tidync::tidync(dat.i$path_download)
      b7.local <- b7  %>%
        tidync::activate(b7$variable$name[6]) %>%
        tidync::hyper_filter(y = y > bbox_him[2] & y < bbox_him[4],
                             x = x > bbox_him[1] & x < bbox_him[3]) %>%

        tidync::hyper_tibble() %>%
        dplyr::select(x,y,dplyr::everything(),-time)
      r <- terra::rast(b7.local)





    }

    #use terra to stretch and write file
    r <- r %>%  terra::stretch()
    #set Himawari crs
    terra::crs(r) <- "+proj=geos +lon_0=140.7 +h=35785863 +x_0=0 +y_0=0 +a=6378137 +b=6356752.3 +units=m +no_defs"
    outtif <- paste0(dest_folder,"\\",dat.i$datetimelocal_chr[1],"_",
                     dat.i$satellite[1],"_",length(names(r)),"bands.tif")
    terra::writeRaster(r,outtif)

  }

}
