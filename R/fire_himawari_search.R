#' Search for Himiwari data
#'
#' @description
#' Search the BOM repository of Himawari 8 and 9 images that are held on NCI THREDDS: https://geonetwork.nci.org.au/geonetwork/srv/eng/catalog.search#/metadata/f8433_0020_1861_5916.
#' There is a lag of around 2 months for data to appear on this server, so NRT data is also searched for more recent records.
#' The server will be searched and a data frame of paths will be returned. Each row is path to one different band x time combination.
#' Images are available every 10 minutes, but put in a bigger timestep to limit the amount of records returned.
#'
#' @param fire_bbox An sf polygon to search for images
#' @param start_time The first time (posixct) for which to search for images. This will be converted to utc within the function.
#' @param end_time The last time (posixct) for which to search for images in local time. This will be converted to utc within the function.
#' @param timestep_minutes minutes timestep in multiples of 10. e.g. search for images every 10 minutes or 60 minutes.
#'
#' @return Data frame with thredds path to Himawari data
#' @export
#'
#' @examples
#' #dat.himiwari <- fire_search_himawari(fire_bbox = dat.bbox,start_time = datestart, end_time = dateend,timestep_minutes = 120)
fire_himawari_search <- function(fire_bbox,
                                 start_time,
                                 end_time,
                                 timestep_minutes=10){


  checkmate::assert(stringr::str_detect(class(start_time)[1],"POSIXct"),"Error: times must be posixct")
  checkmate::assert(stringr::str_detect(class(end_time)[1],"POSIXct"),"Error: times must be posixct")






  nci_path <- "https://thredds.nci.org.au/thredds/catalog/ra22/satellite-products/arc/obs/himawari-ahi/fldk/v1-0"


  #round input times to nearest x minutes, max of 60 minutes
  timestep_minutes_round <- ifelse(timestep_minutes>60,60,timestep_minutes)

  #get all datetimes to search for images from fire start to fire end, using user specified time step
  #this will be used to create thredds paths
  #ensure utc times are used because these are what the himawari files are written with


  start_time <- lubridate::with_tz(start_time,tz="UTC")
  #round to nearest x minutes (himawari files are every 10 minutes)
  start_time <- lubridate::round_date(start_time,unit = paste0(timestep_minutes_round," minutes"))

  end_time <- lubridate::with_tz(end_time,tz="UTC")
  #round to nearest x minutes (himawari files are every 10 minutes)
  end_time <- lubridate::round_date(end_time,unit = paste0(timestep_minutes_round," minutes"))

  dates_fires <- seq(start_time,end_time,by=paste0(timestep_minutes," mins"))



  #get the time zone
  my_tz <- wfprogression::fire_get_timezone(fire_bbox)




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
                  daynight=ifelse(hour_local >= 8 & hour_local <=18,"day","night"))


  dat.paths <- dat.paths %>%

    #use rvest based function to create a list of all files in each catalog path
    #some will be himiwari 8 and some will be 9
    dplyr::mutate(file_name=purrr::map(path_catalog,~wfprogression::fire_thredds_list(.x,"ABOM_OBS_|ABOM_OBS_|ABOM_OBS_")))

  #if more recent data is required, also search NRT data
  weeks_diff <- as.numeric(difftime(Sys.Date(),end_time,units = "weeks"))
  if(weeks_diff<=3){

    dat.paths.nrt <- dat.paths %>%

      #also see if any records are NRT (near real time), ie recent images
      dplyr::mutate(path_catalog=stringr::str_replace(path_catalog,"/arc/obs/","/nrt/obs/"),
                    file_name=purrr::map(path_catalog,~wfprogression::fire_thredds_list(.x,"ABOM_OBS_|ABOM_OBS_|ABOM_OBS_")))



    #combine results
    dat.paths <- rbind(dat.paths,dat.paths.nrt)




  }




  dat.paths <- dat.paths %>%

    #unnest the results
    tidyr::unnest(cols="file_name") %>%

    #remove NA results i.e. paths with no data (sometime a Himawari capture is missing)

    dplyr::filter(!is.na(file_name))


  #test if no images are found e.g. times prior to himawari existing
  if(nrow(dat.paths)>0){



    dat.paths <- dat.paths %>%

      #create fields for time date and download paths
      dplyr::mutate(path_download=stringr::str_replace(path_catalog,"catalog","dodsC"),
                    path_download=stringr::str_replace(path_download,"catalog.html",file_name),
                    band=substr(file_name,29,31),
                    satellite=substr(file_name,50,58)) %>%

      dplyr::select(datetime_utc,datetimelocal,datetimelocal_chr,date_local,
                    hour_local,minutes_local,daynight,path_catalog,
                    file_name,path_download,band,satellite) %>%

      #ensure only dates input by user are included in output
      dplyr::filter(datetime_utc >= start_time & datetime_utc <= end_time)



    return(dat.paths)


  }else{

    print("No himawari images found")

  }


}









