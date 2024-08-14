fire_search_copernicus <- function(fire_bbox,
                                   start_date,
                                   end_date,
                                   collection_names=c("SENTINEL-3")){


  #connect to stac source - copernicus
  stac_source <- rstac::stac(
    "https://catalogue.dataspace.copernicus.eu/stac"
  )


  #query based on location, date and collection name
  stac_query <- rstac::stac_search(
    q = stac_source,
    bbox = sf::st_bbox(sf::st_transform(fire_bbox,4326)),#which crs??
    collections = collection_names,
    datetime = paste0(start_date,"T00:00:00Z","/",end_date,"T00:00:00Z"),
     limit = 5000
  )

  date_seq <- as.POSIXct(paste0(seq(as.Date(start_date),as.Date(end_date),by="1 day"),"T00:00:00Z"),tz="UTC")
  date_seq_2 <- date_seq+lubridate::days(1)
  dates_chr <- paste0(format(date_seq,format="%Y-%m-%dT%H:%M:%SZ"),"/",format(date_seq_2,format="%Y-%m-%dT%H:%M:%SZ"))


  #create stac query for each date to avoid limit on items return for each query
  stac_queries <- purrr::map(dates_chr,~rstac::stac_search(
    q = stac_source,
    bbox = sf::st_bbox(sf::st_transform(fire_bbox,4326)),#which crs??
    collections = collection_names,
    datetime = .x,
    limit = 1000
  ))


  executed_stac_query <- purrr::map(stac_queries, rstac::get_request)

  #loop through each and   extract items properties including path and url

  res.list <- list()
  for(i in 1:length(executed_stac_query)){
    exec.i <- executed_stac_query[[i]]

    #extract items properties including path and url
    dat.x <- purrr::map(exec.i$features,~tibble::tibble(sen3_path=.x$assets$PRODUCT$href,
                                                        filename=basename(.x$assets$PRODUCT$alternate$s3$href),
                                                 satellite=.x$collection,
                                                 product=.x$properties$productType,
                                                 datetime=.x$properties$datetime)) %>%
      dplyr::bind_rows()

    res.list[[i]] <- dat.x
  }

  #combine the results
  dat.x.all <- do.call(rbind,res.list)


  #get the time zone
  my_tz <- fire_get_timezone(fire_bbox)



  #generate time and date and  fields
  dat.x.all <- dat.x.all %>%
    dplyr::mutate(datetimeutc=as.POSIXct(datetime,format="%Y-%m-%dT%H:%M:%OS",tz="UTC"),
                  datetimelocal=lubridate::with_tz(datetimeutc,tz=my_tz),
                  datetimelocal_chr=format(datetimelocal,format="%Y%m%d%H%M%S"))%>%

    #filter to RBT, which is radiances
    dplyr::filter(product=="SL_1_RBT___")%>%

    #we only need one image for each time. Remove one of the files with identical time
    #sort by processing numbers first.
    dplyr::mutate(processing=substr(filename,87,87),
                  NNN=substr(filename,92,94))%>%
    dplyr::group_by(datetimelocal_chr) %>%
    dplyr::arrange(dplyr::desc(processing),dplyr::desc(NNN)) %>%
    dplyr::filter(dplyr::row_number()==min(dplyr::row_number())) %>%
    dplyr::ungroup() %>%
    dplyr::select(-processing,-NNN)






  return(dat.x.all)



}
