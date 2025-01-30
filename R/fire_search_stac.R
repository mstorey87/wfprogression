#' Search Geoscience Aus. STAC for Landsat and Sentinel 2 data
#'
#' @param fire_bbox A polygon, usually fire bounding box, to search for images
#' @param start_time The first date for which to search for images YYYY-mm-dd
#' @param end_time The last date for which to search for images YYYY-mm-dd
#' @param collection_names Names of which Landsat and Sentinel 2 products to search for. Defaults to all products from Landsat 5 onwards.
#'
#' @return data frame with image paths
#' @export
#'
#' @examples
#' #dat.stac <- fire_search_stac(fire_bbox = dat.bbox,start_time = datestart,end_time = dateend)
fire_search_stac <- function(fire_bbox,
                             start_time,
                             end_time,
                             collection_names=c("ga_ls7e_ard_3",
                                                "ga_ls8c_ard_3",
                                                "ga_ls9c_ard_3",
                                                "ga_ls5t_ard_3",
                                                "ga_s2am_ard_3",
                                                "ga_s2bm_ard_3")){


  checkmate::assert(stringr::str_detect(class(start_time)[1],"POSIXct"),"Error: times must be posixct")
  checkmate::assert(stringr::str_detect(class(end_time)[1],"POSIXct"),"Error: times must be posixct")


  start_time <- lubridate::with_tz(start_time,tz="UTC")
  end_time <- lubridate::with_tz(end_time,tz="UTC")

  start_time <- format(start_time, "%Y-%m-%dT%H:%M:%SZ")
  end_time <- format(end_time, "%Y-%m-%dT%H:%M:%SZ")




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
    datetime = paste0(start_time,"/",end_time),
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


  #test if any images were found
  if(nrow(dat.x)>0){








    #set bands that will be used for rgb images. This only uses 3 bands at the moment to create false colour images.
    #landsat and sentinel 2 bands have different names. So we need to rename bands depending on which satellite it is

    #if there are any sentinel 2 results, give common band names
    dat.x.sentinel2 <- dat.x %>% dplyr::filter(stringr::str_starts(product,"ga_s2"))
    if(nrow(dat.x.sentinel2)>0){
      dat.x.sentinel2 <- dat.x.sentinel2 %>%
        dplyr::mutate(band1=nbart_swir_3,band2=nbart_swir_2,band3=nbart_red)

    }


    #if there are any landsat results, give common band names
    dat.x.landsat <- dat.x %>% dplyr::filter(stringr::str_starts(product,"ga_ls"))
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
    my_tz <- wfprogression::fire_get_timezone(fire_bbox)


    #calculate the time and image tile id fields
    dat.aws <- dat.aws %>%
      dplyr::mutate(datetimeutc=as.POSIXct(datetime,format="%Y-%m-%dT%H:%M:%OS",tz="UTC"),
                    datetimelocal=lubridate::with_tz(datetimeutc,tz=my_tz),
                    datetimelocal_chr=format(datetimelocal,format="%Y%m%d%H%M%S"),
                    #tile and date string to use in output file name
                    tile_dateutc=paste0(purrr::map_chr(stringr::str_split(basename(band1),"_"),5),"_",
                                        purrr::map_chr(stringr::str_split(basename(band1),"_"),6))) %>%
      dplyr::select(dplyr::matches("datetime|product|tile"),dplyr::everything()) %>%
      dplyr::arrange(datetimelocal)









    return(dat.aws)

  }else{

    print("no images")
  }
}

