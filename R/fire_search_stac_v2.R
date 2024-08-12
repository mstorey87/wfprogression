fire_search_stac <- function(fire_bbox,
                             start_date,
                             end_date,
                             collection_names=c("ga_ls7e_ard_3",
                                                "ga_ls8c_ard_3",
                                                "ga_ls9c_ard_3",
                                                "ga_ls5t_ard_3",
                                                "ga_s2am_ard_3",
                                                "ga_s2bm_ard_3"),
                             dest_folder){


  items <- s |>
    stac_search(collections = collection_names,
                bbox = as.numeric(bbox_wgs84),
                datetime = glue("{start_date}/{end_date}")) |>

    post_request() |>
    items_fetch(progress = FALSE)

  cat("Found", length(items$features), "items")



  #connect to stac source - digital earth australia
  #see information here: https://knowledge.dea.ga.gov.au/guides/setup/gis/stac/
  stac_source <- rstac::stac(
    "https://explorer.dea.ga.gov.au/stac"
  )

  #get collections that are available (e.g. landsat 8, sentinal 2a)
  # collections_query <- stac_source |>
  #   rstac::collections()
  #
  # available_collections <- rstac::get_request(collections_query)
  # available_collections



  #query based on location, date and collection name
  stac_query <- rstac::stac_search(
    q = stac_source,
    bbox = sf::st_bbox(fire_bbox),
    collections = collection_names,
    datetime = paste0(start_date,"/",end_date)
    # limit = 999
  )
  executed_stac_query <- rstac::get_request(stac_query)
  #rstac::items_assets(executed_stac_query)


  #extract details of each band available from the query results
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




  #get the time zone based on footprint centroid and add a local time field
  dat.aus <- rnaturalearth::ne_states(country="Australia") %>%
    dplyr::select(name) %>%
    sf::st_transform(3112)
  dat.tz <- dat.aus%>%
    sf::st_intersection(sf::st_centroid(fire_bbox %>% sf::st_transform(3112) %>% sf::st_union())) %>%
    dplyr::left_join(dat.timezone.names)


  dat.aws <- dat.aws %>%
    dplyr::mutate(datetimeutc=as.POSIXct(datetime,format="%Y-%m-%dT%H:%M:%OS",tz="UTC"),
                  datetimelocal=lubridate::with_tz(datetimeutc,tz=dat.tz$tz_name),
                  datetimelocal_chr=format(datetimelocal,format="%Y%m%d_%H%M%S"),
                  #tile and date string to use in output file name
                  tile_dateutc=paste0(purrr::map_chr(stringr::str_split(basename(band1),"_"),5),"_",
                                      purrr::map_chr(stringr::str_split(basename(band1),"_"),6)))


  #stream and crop all the bands, converting the
  dat.aws <- dat.aws %>%
    dplyr::mutate(band1_stream=purrr::map(band1,~terra::rast(paste0("/vsicurl/",.x))),
                  band2_stream=purrr::map(band2,~terra::rast(paste0("/vsicurl/",.x))),
                  band3_stream=purrr::map(band3,~terra::rast(paste0("/vsicurl/",.x))),
                  #crop all the images to extent of bounding box
                  crs=purrr::map(band1_stream,terra::crs))


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

    out.file <- paste0(dest_folder,"/",dat$datetimelocal_chr,"local_",dat$product,"_",dat$tile_dateutc,"utc",".tif")

    terra::writeRaster(rgb,out.file)



  }






  return(dat.aws)

}


# dat.aws <- rstac::assets_url(executed_stac_query) %>%
#   data.frame(aws_path=.) %>%
#   dplyr::filter(stringr::str_detect(aws_path,regex_filter)) %>%
#   dplyr::mutate(https_path=stringr::str_replace(aws_path,"s3://dea-public-data","https://dea-public-data.s3.ap-southeast-2.amazonaws.com")) %>%
#   dplyr::mutate(file_name=basename(aws_path),
#                 tile_dateutc=paste0(purrr::map_chr(stringr::str_split(file_name,"_"),5),"_",
#                                  purrr::map_chr(stringr::str_split(file_name,"_"),6))) %>%
#   dplyr::mutate(sat_group=ifelse(stringr::str_starts(file_name,"ga_l"),"landsat","sentinel2"))

#get acquisition time from yaml
#first get time zone for fire


# dat.yaml <- dat.aws %>%
#   dplyr::filter(stringr::str_detect(aws_path,"yaml")) %>%
#   dplyr::mutate(datetimeutc=purrr::map(https_path,~yaml::read_yaml(.x)$wagl$source_datasets$acquisition_datetime )) %>%
#   tidyr::unnest(cols=datetimeutc) %>%
#   dplyr::mutate(datetimeutc=as.POSIXct(datetimeutc,tz="UTC"),
#                 datetimelocal=lubridate::with_tz(datetimeutc,tz=dat.tz$tz_name),
#                 datetimelocal_chr=format(datetimelocal,format="%Y%m%d_%H%M%S")) %>%
#   dplyr::select(tile_dateutc,datetimeutc,datetimelocal,datetimelocal_chr)
#
# #join time back to download paths
# dat.aws <- dat.aws %>%
#   dplyr::left_join(dat.yaml) %>%
#   dplyr::filter(!stringr::str_detect(aws_path,"yaml"))

