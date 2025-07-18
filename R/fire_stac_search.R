#' Search for Landsat and Sentinel-2 imagery from Geoscience Australia's STAC
#'
#' @description
#' Searches the Digital Earth Australia (DEA) STAC API for Landsat and Sentinel-2 images
#' that intersect with a given fire boundary polygon and fall within a specified date range.
#' The function returns a data frame containing cloud storage paths to the bands needed
#' for false-colour composites, which can then be passed to `fire_stac_download()`.
#'
#' The maximum number of returned records is 999, so narrow the spatial or temporal extent
#' if necessary. For details on the DEA STAC service, see:
#' [DEA Knowledge Base](https://knowledge.dea.ga.gov.au/guides/setup/gis/stac/).
#'
#' @param fire_bbox An `sf` polygon (e.g., fire extent) used to spatially filter available imagery.
#' @param start_time POSIXct date-time (local or UTC) for the start of the search window. Will be converted to UTC.
#' @param end_time POSIXct date-time for the end of the search window. Will be converted to UTC.
#' @param collection_names Character vector of product IDs to search for. Defaults to Landsat 5, 7, 8, 9, and Sentinel-2.
#'
#' @return A data frame with image band URLs, product names, acquisition timestamps,
#' local datetimes, and a tile/date identifier for file naming.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(dplyr)
#' # Example bounding box: a square around an area of interest
#' fire_bbox <- st_as_sfc(st_bbox(c(xmin = 149.1, ymin = -35.4, xmax = 149.2, ymax = -35.3)), crs = 4326) %>%
#'   st_as_sf(crs=4326)
#'
#' # Define search time window
#' start_time <- as.POSIXct("2023-01-01", tz = "Australia/Sydney")
#' end_time <- as.POSIXct("2023-01-07", tz = "Australia/Sydney")
#'
#' # Run the search
#' stac_results <- fire_stac_search(fire_bbox, start_time, end_time)
#'
#' head(stac_results)
#'
#' outdir <- "C:/temp/landsat_sentinel"
#' dir.create(outdir)
#' fire_stac_download(fire_bbox, stac_results, outdir)
#' }
fire_stac_search <- function(
    fire_bbox,
    start_time,
    end_time,
    collection_names = c(
      "ga_ls7e_ard_3",
      "ga_ls8c_ard_3",
      "ga_ls9c_ard_3",
      "ga_ls5t_ard_3",
      "ga_s2am_ard_3",
      "ga_s2bm_ard_3"
    )
) {

  checkmate::assert(stringr::str_detect(class(start_time)[1], "POSIXct"), "Error: times must be POSIXct")
  checkmate::assert(stringr::str_detect(class(end_time)[1], "POSIXct"), "Error: times must be POSIXct")

  start_time <- lubridate::with_tz(start_time, tz = "UTC")
  end_time <- lubridate::with_tz(end_time, tz = "UTC")

  start_time <- format(start_time, "%Y-%m-%dT%H:%M:%SZ")
  end_time <- format(end_time, "%Y-%m-%dT%H:%M:%SZ")

  # Connect to DEA STAC API
  stac_source <- rstac::stac(
    "https://explorer.dea.ga.gov.au/stac"
  )

  # Search query
  stac_query <- rstac::stac_search(
    q = stac_source,
    bbox = sf::st_bbox(sf::st_transform(fire_bbox, 4326)),
    collections = collection_names,
    datetime = paste0(start_time, "/", end_time),
    limit = 999
  )
  executed_stac_query <- rstac::get_request(stac_query)

  # Extract assets from features
  dat.x <- purrr::map(executed_stac_query$features, ~ tibble::tibble(
    nbart_red = .x$assets$nbart_red$href,
    nbart_swir_1 = .x$assets$nbart_swir_1$href,
    nbart_swir_2 = .x$assets$nbart_swir_2$href,
    nbart_swir_3 = .x$assets$nbart_swir_3$href,
    nbart_nir = .x$assets$nbart_nir$href,
    nbart_nir_1 = .x$assets$nbart_nir_1$href,
    nbart_nir_2 = .x$assets$nbart_nir_2$href,
    product = .x$collection,
    datetime = .x$properties$datetime
  )) %>%
    dplyr::bind_rows()

  if (nrow(dat.x) > 0) {

    # Sentinel-2 products: select bands for false-colour composite
    dat.x.sentinel2 <- dat.x %>% dplyr::filter(stringr::str_starts(product, "ga_s2"))
    if (nrow(dat.x.sentinel2) > 0) {
      dat.x.sentinel2 <- dat.x.sentinel2 %>%
        dplyr::mutate(band1 = nbart_swir_3, band2 = nbart_swir_2, band3 = nbart_red)
    }

    # Landsat products: select bands for false-colour composite
    dat.x.landsat <- dat.x %>% dplyr::filter(stringr::str_starts(product, "ga_ls"))
    if (nrow(dat.x.landsat) > 0) {
      dat.x.landsat <- dat.x.landsat %>%
        dplyr::mutate(band1 = nbart_swir_2, band2 = nbart_nir, band3 = nbart_red)
    }

    dat.aws <- dplyr::bind_rows(dat.x.landsat, dat.x.sentinel2) %>%
      dplyr::select(band1, band2, band3, datetime, product) %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("band"),
                                  ~ stringr::str_replace(.x, "s3://dea-public-data", "https://dea-public-data.s3.ap-southeast-2.amazonaws.com")
      ))

    my_tz <- wfprogression::fire_get_timezone(fire_bbox)

    dat.aws <- dat.aws %>%
      dplyr::mutate(
        datetimeutc = as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
        datetimelocal = lubridate::with_tz(datetimeutc, tz = my_tz),
        datetimelocal_chr = format(datetimelocal, format = "%Y%m%d%H%M%S"),
        tile_dateutc = paste0(
          purrr::map_chr(stringr::str_split(basename(band1), "_"), 5), "_",
          purrr::map_chr(stringr::str_split(basename(band1), "_"), 6)
        )
      ) %>%
      dplyr::select(dplyr::matches("datetime|product|tile"), dplyr::everything()) %>%
      dplyr::arrange(datetimelocal)

    return(dat.aws)

  } else {

    message("no images")
  }
}

#' #' Search for Landsat and Sentinel 2 data
#' #'
#' #' @description
#' #' Search Geoscience Aus. STAC browser for Landsat and Sentinel 2 images that intersect polygon and match search dates.
#' #' This will return a data frame of images path to input to fire_stac_download() function.
#' #' A max of 999 records will be returned, so location and or date may need to be limited.
#' #' Info on STAC browser is here: https://knowledge.dea.ga.gov.au/guides/setup/gis/stac/
#' #'
#' #' @param fire_bbox A polygon, usually fire bounding box, to search for images
#' #' @param start_time The first time for which to search for images (posixct required). This will be converted to utc.
#' #' @param end_time The last time for which to search for images (posixct required). This will be converted to utc.
#' #' @param collection_names Names of which Landsat and Sentinel 2 products to search for. Defaults to all products from Landsat 5 onwards.
#' #'
#' #' @return data frame with image paths
#' #' @export
#' #'
#' #' @examples
#' #' #dat.stac <- fire_search_stac(fire_bbox = dat.bbox,start_time = datestart,end_time = dateend)
#' fire_stac_search <- function(fire_bbox,
#'                              start_time,
#'                              end_time,
#'                              collection_names=c("ga_ls7e_ard_3",
#'                                                 "ga_ls8c_ard_3",
#'                                                 "ga_ls9c_ard_3",
#'                                                 "ga_ls5t_ard_3",
#'                                                 "ga_s2am_ard_3",
#'                                                 "ga_s2bm_ard_3")){
#'
#'
#'   checkmate::assert(stringr::str_detect(class(start_time)[1],"POSIXct"),"Error: times must be posixct")
#'   checkmate::assert(stringr::str_detect(class(end_time)[1],"POSIXct"),"Error: times must be posixct")
#'
#'
#'   start_time <- lubridate::with_tz(start_time,tz="UTC")
#'   end_time <- lubridate::with_tz(end_time,tz="UTC")
#'
#'   start_time <- format(start_time, "%Y-%m-%dT%H:%M:%SZ")
#'   end_time <- format(end_time, "%Y-%m-%dT%H:%M:%SZ")
#'
#'
#'
#'
#'   #connect to stac source - digital earth australia
#'   #see information here: https://knowledge.dea.ga.gov.au/guides/setup/gis/stac/
#'   stac_source <- rstac::stac(
#'     "https://explorer.dea.ga.gov.au/stac"
#'   )
#'
#'
#'   #query based on location, date and collection name
#'   stac_query <- rstac::stac_search(
#'     q = stac_source,
#'     bbox = sf::st_bbox(sf::st_transform(fire_bbox,4326)),#which crs??
#'     collections = collection_names,
#'     datetime = paste0(start_time,"/",end_time),
#'     limit = 999
#'   )
#'   executed_stac_query <- rstac::get_request(stac_query)
#'
#'
#'
#'   #extract details of each band available from the query results
#'   #extract items properties including path and url
#'   dat.x <- purrr::map(executed_stac_query$features,~tibble::tibble(nbart_red=.x$assets$nbart_red$href,
#'                                                                    nbart_swir_1=.x$assets$nbart_swir_1$href,
#'                                                                    nbart_swir_2=.x$assets$nbart_swir_2$href,
#'                                                                    nbart_swir_3=.x$assets$nbart_swir_3$href,
#'                                                                    nbart_nir=.x$assets$nbart_nir$href,
#'                                                                    nbart_nir_1=.x$assets$nbart_nir_1$href,
#'                                                                    nbart_nir_2=.x$assets$nbart_nir_2$href,
#'                                                                    product=.x$collection,
#'                                                                    datetime=.x$properties$datetime)) %>%
#'     dplyr::bind_rows()
#'
#'
#'   #test if any images were found
#'   if(nrow(dat.x)>0){
#'
#'
#'
#'
#'
#'
#'
#'
#'     #set bands that will be used for rgb images. This only uses 3 bands at the moment to create false colour images.
#'     #landsat and sentinel 2 bands have different names. So we need to rename bands depending on which satellite it is
#'
#'     #if there are any sentinel 2 results, give common band names
#'     dat.x.sentinel2 <- dat.x %>% dplyr::filter(stringr::str_starts(product,"ga_s2"))
#'     if(nrow(dat.x.sentinel2)>0){
#'       dat.x.sentinel2 <- dat.x.sentinel2 %>%
#'         dplyr::mutate(band1=nbart_swir_3,band2=nbart_swir_2,band3=nbart_red)
#'
#'     }
#'
#'
#'     #if there are any landsat results, give common band names
#'     dat.x.landsat <- dat.x %>% dplyr::filter(stringr::str_starts(product,"ga_ls"))
#'     if(nrow(dat.x.landsat)>0){
#'       dat.x.landsat <- dat.x.landsat %>%
#'         dplyr::mutate(band1=nbart_swir_2,band2=nbart_nir,band3=nbart_red)
#'
#'     }
#'
#'
#'
#'     #combine data and calculate https path and date times strings for file names
#'     dat.aws <- dplyr::bind_rows(dat.x.landsat,dat.x.sentinel2) %>%
#'       dplyr::select(band1,band2,band3,datetime,product) %>%
#'       dplyr::mutate(dplyr::across(dplyr::starts_with("band"),
#'                                   ~stringr::str_replace(.x,"s3://dea-public-data","https://dea-public-data.s3.ap-southeast-2.amazonaws.com")) )
#'
#'
#'
#'
#'     #get the time zone
#'     my_tz <- wfprogression::fire_get_timezone(fire_bbox)
#'
#'
#'     #calculate the time and image tile id fields
#'     dat.aws <- dat.aws %>%
#'       dplyr::mutate(datetimeutc=as.POSIXct(datetime,format="%Y-%m-%dT%H:%M:%OS",tz="UTC"),
#'                     datetimelocal=lubridate::with_tz(datetimeutc,tz=my_tz),
#'                     datetimelocal_chr=format(datetimelocal,format="%Y%m%d%H%M%S"),
#'                     #tile and date string to use in output file name
#'                     tile_dateutc=paste0(purrr::map_chr(stringr::str_split(basename(band1),"_"),5),"_",
#'                                         purrr::map_chr(stringr::str_split(basename(band1),"_"),6))) %>%
#'       dplyr::select(dplyr::matches("datetime|product|tile"),dplyr::everything()) %>%
#'       dplyr::arrange(datetimelocal)
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'     return(dat.aws)
#'
#'   }else{
#'
#'     print("no images")
#'   }
#' }
#'
