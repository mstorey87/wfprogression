fire_search_stac <- function(fire_bbox,
                             start_date,
                             end_date,
                             collection_names=c("ga_ls7e_ard_3",
                                                "ga_ls8c_ard_3",
                                                "ga_ls9c_ard_3",
                                                "ga_ls5t_ard_3",
                                                "ga_s2am_ard_3",
                                                "ga_s2bm_ard_3"),
                             download=T,
                             dest_folder=NULL){

  #connect to stac source - digital earth australia
  #see information here: https://knowledge.dea.ga.gov.au/guides/setup/gis/stac/
  stac_source <- rstac::stac(
    "https://explorer.dea.ga.gov.au/stac"
  )


  #query based on location, date and collection name
  stac_query <- rstac::stac_search(
    q = stac_source,
    bbox = sf::st_bbox(sf::st_transform(fire_bbox,4326)),#which crs??
    collections = collection_names,
    datetime = paste0(start_date,"/",end_date),
    limit = 999
  )
  executed_stac_query <- rstac::get_request(stac_query)



  #extract details of each band available from the query results
  #extract items properties including path and url
  dat.x <- purrr::map(executed_stac_query$features,~tibble::tibble(nbart_red=.x$assets$nbart_red$href,
                                                                   nbart_swir_1=.x$assets$nbart_swir_1$href,
                                                                   nbart_swir_2=.x$assets$nbart_swir_2$href,
                                                                   nbart_swir_3=.x$assets$nbart_swir_3$href,
                                                                   nbart_nir=.x$assets$nbart_nir$href,
                                                                   nbart_nir_1=.x$assets$nbart_nir_1$href,
                                                                   nbart_nir_2=.x$assets$nbart_nir_2$href,
                                                                   product=.x$collection,
                                                                   datetime=.x$properties$datetime)) %>%
    dplyr::bind_rows()


  #set bands that will be used for rgb images
  #this depends on satellite type
  dat.x.sentinel2 <- dat.x %>% dplyr::filter(stringr::str_starts(product,"ga_s2")) %>%
    dplyr::mutate(band1=nbart_swir_3,band2=nbart_swir_2,band3=nbart_red)

  dat.x.landsat <- dat.x %>% dplyr::filter(stringr::str_starts(product,"ga_ls"))%>%
    dplyr::mutate(band1=nbart_swir_2,band2=nbart_nir,band3=nbart_red)


  #combine data and calculate https path and date times strings for file names
  dat.aws <- rbind(dat.x.sentinel2,dat.x.landsat) %>%
    dplyr::select(band1,band2,band3,datetime,product) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("band"),
                                ~stringr::str_replace(.x,"s3://dea-public-data","https://dea-public-data.s3.ap-southeast-2.amazonaws.com")) )




  #get the time zone
  my_tz <- fire_get_timezone(fire_bbox)


  dat.aws <- dat.aws %>%
    dplyr::mutate(datetimeutc=as.POSIXct(datetime,format="%Y-%m-%dT%H:%M:%OS",tz="UTC"),
                  datetimelocal=lubridate::with_tz(datetimeutc,tz=my_tz),
                  datetimelocal_chr=format(datetimelocal,format="%Y%m%d%H%M%S"),
                  #tile and date string to use in output file name
                  tile_dateutc=paste0(purrr::map_chr(stringr::str_split(basename(band1),"_"),5),"_",
                                      purrr::map_chr(stringr::str_split(basename(band1),"_"),6))) %>%
    dplyr::select(dplyr::matches("datetime|product|tile"),dplyr::everything())


  #stream and crop all the bands
  dat.aws <- dat.aws %>%
    dplyr::mutate(band1_stream=purrr::map(band1,~terra::rast(paste0("/vsicurl/",.x))),
                  band2_stream=purrr::map(band2,~terra::rast(paste0("/vsicurl/",.x))),
                  band3_stream=purrr::map(band3,~terra::rast(paste0("/vsicurl/",.x))),
                  #crop all the images to extent of bounding box
                  crs=purrr::map(band1_stream,terra::crs))



  if(download==TRUE){
    #loop through one by one (to save memory) and create cropped rgb and write to disk
    for(i in 1:nrow(dat.aws)){
      dat <- dat.aws[i,]
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




  return(dat.aws)

}


