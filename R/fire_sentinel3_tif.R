fire_sentinel3_tif <- function(fire_bbox,sen3_folder){

  my.zips<- list.files(sen3_folder,full.names = T,pattern = ".zip") %>%
    tibble::tibble(path=.)




  fire_bbox <- fire_bbox %>%
    sf::st_transform(4326)


  pth_s3_tifs <- paste0(sen3_folder,"\\tif_sentinel3\\")
  dir.create(pth_s3_tifs)

  fn_tmap_sentinel3 <- function(image,polygon,chr_datetime,outpath){

    if(length(names(image))<3){
      image <- c(image,image,image)
      names(image) <- c("b1","b2","b3")
    }
    image <- terra::stretch(image)

    #ensure same crs
    polygon <- sf::st_transform(polygon,st_crs(image))



    tmap_image <- tmap::tm_shape(image,bbox = polygon)+
      tmap::tm_rgb(tmap::tm_mv(names(image)))+
      tmap::tm_shape(polygon)+
      tmap::tm_borders(col="blue")+
      tmap::tm_layout(asp = 0,crs=4326)

    out_image <- paste0(outpath,"\\",chr_datetime,"_plot_sentinel3.jpg")
    tmap::tmap_save(tmap_image,out_image,outer.margins = c(0,0,0,0))
  }


  #get the time zone based on footprint centroid and add a local time field
  dat.aus <- rnaturalearth::ne_states(country="Australia") %>%
    dplyr::select(name) %>%
    sf::st_transform(3112)
  dat.tz <- dat.aus%>%
    sf::st_intersection(sf::st_centroid(fire_bbox %>% sf::st_transform(3112) %>% sf::st_union())) %>%
    dplyr::left_join(dat.timezone.names)


  for(i in 1:nrow(my.zips)){


    #> Loading required package: sp

    sen.zip<- my.zips$path[i]

    temp.fold <- paste0(sen3_folder,"\\tempunzip")


    myfiles <- c("/geodetic_an.nc","/S5_radiance_an.nc","/S6_radiance_an.nc","/S3_radiance_an.nc")

    myfiles <-paste0(stringr::str_replace(basename(sen.zip),".zip",""),myfiles)

    sen.fold <- paste0(temp.fold,"/",stringr::str_replace(basename(sen.zip),".zip",""))

    rastime <- lubridate::with_tz(as.POSIXct(substr(basename(sen.fold),17,31),format="%Y%m%dT%H%M%S",tz="UTC"),tz=dat.tz$tz_name)
    hour_of_day <- lubridate::hour(rastime)

    rastime_chr <- format(rastime,format="%Y%m%d_%H%M%S")



    #
    outname <- paste0(pth_s3_tifs,"\\",rastime_chr,"_sentinel3")



    z <- tryCatch(unzip(sen.zip,files=myfiles,exdir = temp.fold))

    if(is.null(z)){
      file.remove(sen.zip)

    }else{

      coords <- stars::read_ncdf(paste0(sen.fold,"\\geodetic_an.nc"), var = c("latitude_an", "longitude_an"))
      lats <- coords$latitude_an
      lons <- coords$longitude_an


      S6 <- stars::read_ncdf( paste0(sen.fold,"\\S6_radiance_an.nc"), var = "S6_radiance_an")
      S6 <- S6$S6_radiance_an
      S6 <- stars::st_as_stars(S6)
      S6 <- stars::st_as_stars(S6, curvilinear = list(X1 = lons, X2 = lats))
      S6_warp = stars::st_warp(S6, crs = "EPSG:4326",threshold=0.1)

      #do rgb images if hour is day time
      if(hour_of_day > 7 & hour_of_day < 19){
        S5 <- stars::read_ncdf( paste0(sen.fold,"\\S5_radiance_an.nc"), var = "S5_radiance_an")
        S5 <- S5$S5_radiance_an
        S5 <- stars::st_as_stars(S5)
        S5 <- stars::st_as_stars(S5, curvilinear = list(X1 = lons, X2 = lats))


        S3 <- stars::read_ncdf( paste0(sen.fold,"\\S3_radiance_an.nc"), var = "S3_radiance_an")
        S3 <- S3$S3_radiance_an
        S3 <- stars::st_as_stars(S3)
        S3 <- stars::st_as_stars(S3, curvilinear = list(X1 = lons, X2 = lats))

        S5_warp = stars::st_warp(S5, crs = "EPSG:4326",threshold=0.1)
        S3_warp = stars::st_warp(S3, crs = "EPSG:4326",threshold=0.1)#, cellsize = 0.01, threshold = 0.02)
        s3_rgb <- c(S6_warp,S5_warp,S3_warp)
        s3_rgb <- c(S6_warp,S5_warp,S3_warp)
        s3_rgb <- as(s3_rgb,"SpatRaster")
        s3_rgb <- terra::crop(s3_rgb,fire_bbox)
        out_ras <- s3_rgb
      }else{

        S6_warp <- as(S6_warp,"SpatRaster")
        S6_warp <- terra::crop(S6_warp,fire_bbox)
        out_ras <- S6_warp


      }
      terra::writeRaster(out_ras,paste0(outname,".tif"))
      fn_tmap_sentinel3(out_ras,fire_bbox,rastime_chr,sen3_folder)







      unlink(temp.fold,recursive = T)
    }





  }





}
