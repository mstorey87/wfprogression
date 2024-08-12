fire_search_himawari <- function(fire_bbox,
                                 start_date,
                                 end_date,
                                 timestep_minutes=10,
                                 dest_folder){


  nci_path <- "https://thredds.nci.org.au/thredds/catalog/ra22/satellite-products/arc/obs/himawari-ahi/fldk/v1-0"


  #get all datetimes to search for images from fire start to fire end, using user specified time step
  #this will be used to create thredds paths
  dates_fires <- seq(as.POSIXct(start_date,tz="UTC"),as.POSIXct(end_date,tz="UTC"),by=paste0(timestep_minutes," mins"))


  #get the time zone based on footprint centroid and add a local time field
  dat.aus <- rnaturalearth::ne_states(country="Australia") %>%
    dplyr::select(name) %>%
    sf::st_transform(3112)
  dat.tz <- dat.aus%>%
    sf::st_intersection(sf::st_centroid(fire_bbox %>% sf::st_transform(3112) %>% sf::st_union())) %>%
    dplyr::left_join(dat.timezone.names)




  #create data frame with paths to himiwari folder for specified dates
  #calculate time fields
  dat.paths <- data.frame(datetime_utc=dates_fires) %>%
    dplyr::mutate(datetimeutc_path=format(datetime_utc,format="/%Y/%m/%d/%H%M"),
                  path_catalog=paste0(nci_path,datetimeutc_path,"/catalog.html"),
                  datetimelocal=lubridate::with_tz(datetime_utc,tz=dat.tz$tz_name),
                  datetimelocal_chr=format(datetimelocal,format="%Y%m%d_%H%M%S"),
                  hour_local=lubridate::hour(datetimelocal),
                  daynight=ifelse(hour_local >= 8 & hour_local <=18,"day","night")) %>%

    #use rvest based function to create a list of all files in each catalog path
    #some will be himiwari 8 and some will be 9
    dplyr::mutate(file_name=purrr::map(path_catalog,~fire_thredds_list(.x,"ABOM_OBS_B07|ABOM_OBS_B04|ABOM_OBS_B01")))

  dat.paths <- dat.paths %>%

    #unnest the results
    tidyr::unnest(cols="file_name") %>%

    #remove NA results i.e. paths with no data (sometime a Himawari capture is missing)

    dplyr::filter(!is.na(file_name)) %>%

    #create download path
    dplyr::mutate(path_download=stringr::str_replace(path_catalog,"catalog","dodsC"),
                  path_download=stringr::str_replace(path_download,"catalog.html",file_name),
                  band=substr(file_name,29,31),
                  satellite=substr(file_name,50,58),
                  path_local=paste0(dest_folder,"\\",datetimelocal_chr,"_H0809_B7.tif")) %>%

    dplyr::select(datetime_utc,datetimelocal,datetimelocal_chr,daynight,path_catalog,
                  file_name,path_local,path_download,band,satellite)


  #transform bbox to himiwari crs
  #buffer first to capture wider area
  bbox_him <-fire_bbox %>%
    sf::st_transform(3112) %>%
    sf::st_buffer(60000) %>%
    sf::st_transform("+proj=geos +lon_0=140.7 +h=35785863 +x_0=0 +y_0=0 +a=6378137 +b=6356752.3 +units=m +no_defs") %>%
    sf::st_bbox()


  times_unique <- unique(dat.paths$datetimelocal_chr)


  #loop through and make an rgb or single band depending on time of day
  for(i in 1:length(times_unique)){

    dat.i <- dat.paths %>%
      dplyr::filter(datetimelocal_chr==times_unique[i]) %>%
      dplyr::arrange(band)

    if(dat.i$daynight[i]=="day" & nrow(dat.i==3)){

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
