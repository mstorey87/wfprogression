fire_search_stac <- function(fire_bbox,
                             start_date,
                             end_date,
                             collection_names=c("ga_ls7e_ard_3",
                                                "ga_ls8c_ard_3",
                                                "ga_ls9c_ard_3",
                                                "ga_ls5t_ard_3",
                                                "ga_s2am_ard_3",
                                                "ga_s2bm_ard_3")){

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
  dat.x.sentinel2 <- dat.x %>% dplyr::filter(stringr::str_starts(product,"ga_s2"))

  #if there are any sentinel 2 results, give common band names
  if(nrow(dat.x.sentinel2)>0){
    dat.x.sentinel2 <- dat.x.sentinel2 %>%
    dplyr::mutate(band1=nbart_swir_3,band2=nbart_swir_2,band3=nbart_red)

  }



  dat.x.landsat <- dat.x %>% dplyr::filter(stringr::str_starts(product,"ga_ls"))

  #if there are any landsat results, give common band names
  if(nrow(dat.x.landsat)>0){
    dat.x.landsat <- dat.x.landsat %>%
    dplyr::mutate(band1=nbart_swir_2,band2=nbart_nir,band3=nbart_red)

  }



  #combine data and calculate https path and date times strings for file names
  dat.aws <- dplyr::bind_rows(dat.x.landsat,dat.x.sentinel2) %>%
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









  return(dat.aws)

}


