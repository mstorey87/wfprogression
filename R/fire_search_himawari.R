#' Search BOM repository of Himiwari data via thredds
#'
#' @param fire_bbox A polygon, usually fire bounding box, to search for images
#' @param start_date The first date for which to search for images YYYY-mm-dd
#' @param end_date The last date for which to search for images YYYY-mm-dd
#' @param timestep_minutes minutes timestep in multiples of 10. e.g. search for images every 10 minutes or 60 minutes.
#'
#' @return Data frame with thredds path to Himawari data
#' @export
#'
#' @examples
#' #dat.himiwari <- fire_search_himawari(fire_bbox = dat.bbox,start_date = datestart, end_date = dateend,timestep_minutes = 120)
fire_search_himawari <- function(fire_bbox,
                                 start_date,
                                 end_date,
                                 timestep_minutes=10){


  nci_path <- "https://thredds.nci.org.au/thredds/catalog/ra22/satellite-products/arc/obs/himawari-ahi/fldk/v1-0"


  #get all datetimes to search for images from fire start to fire end, using user specified time step
  #this will be used to create thredds paths
  #subtract 1 day from start date to account for himawari files being utc time
  dates_fires <- seq(as.POSIXct(start_date,tz="UTC")-lubridate::days(1),as.POSIXct(end_date,tz="UTC"),by=paste0(timestep_minutes," mins"))


  #get the time zone
  my_tz <- fire_get_timezone(fire_bbox)




  #create data frame with paths to himiwari folder for specified dates
  #calculate time fields
  dat.paths <- data.frame(datetime_utc=dates_fires) %>%
    dplyr::mutate(datetimeutc_path=format(datetime_utc,format="/%Y/%m/%d/%H%M"),
                  path_catalog=paste0(nci_path,datetimeutc_path,"/catalog.html"),
                  datetimelocal=lubridate::with_tz(datetime_utc,tz=my_tz),
                  datetimelocal_chr=format(datetimelocal,format="%Y%m%d%H%M%S"),
                  date_local=format(datetimelocal,format="%Y-%m-%d"),
                  hour_local=lubridate::hour(datetimelocal),
                  minutes_local=lubridate::minute(datetimelocal),
                  daynight=ifelse(hour_local >= 8 & hour_local <=18,"day","night")) %>%

    #use rvest based function to create a list of all files in each catalog path
    #some will be himiwari 8 and some will be 9
    dplyr::mutate(file_name=purrr::map(path_catalog,~fire_thredds_list(.x,"ABOM_OBS_|ABOM_OBS_|ABOM_OBS_")))

  dat.paths <- dat.paths %>%

    #unnest the results
    tidyr::unnest(cols="file_name") %>%

    #remove NA results i.e. paths with no data (sometime a Himawari capture is missing)

    dplyr::filter(!is.na(file_name)) %>%

    #create fields for time date and download paths
    dplyr::mutate(path_download=stringr::str_replace(path_catalog,"catalog","dodsC"),
                  path_download=stringr::str_replace(path_download,"catalog.html",file_name),
                  band=substr(file_name,29,31),
                  satellite=substr(file_name,50,58)) %>%

    dplyr::select(datetime_utc,datetimelocal,datetimelocal_chr,date_local,
                  hour_local,minutes_local,daynight,path_catalog,
                  file_name,path_download,band,satellite) %>%

    #ensure only dates input by user are included in output
    dplyr::filter(as.Date(date_local) >= as.Date(start_date) & as.Date(date_local) <= as.Date(end_date))




}









