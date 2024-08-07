#mapkey <- "a5452249ca7c7a4ee2e1e6da787f57cc"
#product <- "VIIRS_SNPP_SP"#"VIIRS_SNPP_NRT",VIIRS_SNPP_SP,VIIRS_NOAA20_NRT,VIIRS_NOAA21_NRT,MODIS_SP,MODIS_NRT
#start_date="2023-12-01"
#end_date="2023-12-31"
#dest_fold <- "C:\\temp_data\\xxx"

#' Search NASA FIRMS hotspots API
#'
#' @param fire_bbox Bounding sf polygon of search area
#' @param mapkey users mapkey requested from https://firms.modaps.eosdis.nasa.gov/api/map_key/
#' @param start_date Search start date in YYYY-mm-dd
#' @param end_date Search end date in YYYY-mm-dd
#' @param dest_fold Folder to temporarily download hotspots files (which are removed in function)
#' @param product_filter Regex character string for filter hotspot products, e.g. 'VIIRS', 'MODIS', 'SNPP', 'NOAA20', 'NOAA21'
#'
#' @return sf point object
#' @export
#'
#' @examples
#' #z <- fire_search_hotspots(x$fire_bbox,mapkey,x$fire_bbox$startdate_search,x$fire_bbox$enddate_search,"C:/temp_data","NRT")
fire_search_hotspots <- function(fire_bbox,mapkey,start_date,end_date,dest_fold,product_filter=NULL){

  #API only allows 10 days per query, queries will need to be in groups of 10 days
  #get the groups of 10 (or less) days
  n_days <- as.numeric(difftime(end_date,start_date,units="days"))+1
  date_seq <- as.character(seq(as.Date(start_date),as.Date(end_date),by="1 day"))
  date_groups <- split(date_seq,ceiling(seq_along(date_seq)/10))

  #create a date string required for the api path n_days/startdate
  date_groups <- paste0(unlist(lapply(date_groups,length)),"/",unlist(lapply(date_groups, `[[`, 1)))

  #a string of the bounding box coordinates for the api path
  box_coords <- paste0(sf::st_bbox(fire_bbox),collapse=",")

  #list of all hotspots products available. Filter them if user inputs a filter string
  products <- c("VIIRS_SNPP_NRT","VIIRS_SNPP_SP","VIIRS_NOAA20_NRT",
                "VIIRS_NOAA21_NRT","MODIS_SP","MODIS_NRT")
  if(!is.null(product_filter)){
    products <- products[stringr::str_detect(products,product_filter)]
  }


  #create all combinations of path and products and dates
  #path_api <- paste0("https://firms.modaps.eosdis.nasa.gov/api/country/csv/",
  #                   mapkey,"/",products  , "/AUS/",n_days,"/",start_date)

  path_api <- paste0("https://firms.modaps.eosdis.nasa.gov/api/area/csv/",
                     mapkey,"/",products  , "/",box_coords,"/")

  path_api <- expand.grid(path_api,date_groups) %>%
    dplyr::mutate(pth=paste0(Var1,Var2)) %>%
    .$pth

  #download both. If no hotspots are found, a file with only headers will be downloaded
  out.files <- paste0(dest_fold,"\\hotspots\\hs_",seq(length(path_api)),"_",
                      purrr::map_chr(stringr::str_split(path_api,"/"),8),".csv")


  dir.create(unique(dirname(out.files)), showWarnings = F)
  download.file(path_api,out.files,mode="wb")

  #load hotspots and combine
  dat.hotspots <- purrr::map(out.files,~readr::read_csv(.x,show_col_types = F) %>%
                               dplyr::mutate(across(everything(),as.character)))
  dat.hotspots <- dplyr::bind_rows(dat.hotspots)

  #remove downloaded files
  file.remove(out.files)

  #if hotspots have been downloaded, return an sf object
  #else the empty data frame will be returned
  if(nrow(dat.hotspots)>0){
    dat.hotspots <- sf::st_as_sf(dat.hotspots,coords=c("longitude","latitude"),crs=4326) %>%
      sf::st_transform(4283)

    #create a satellite name column

    dat.hotspots <- dat.hotspots %>%
      dplyr::mutate(sat_name=ifelse(satellite=="N","SNPP",ifelse(satellite=="N20","NOAA20",ifelse(satellite=="N21","NOAA21",satellite))),
                    sat_name=tolower(paste0(instrument,"_",sat_name)))


    #get the time zone based on fire centroid and add a local time field
    #get the time zone for the fire
    dat.aus <- rnaturalearth::ne_states(country="Australia") %>%
      dplyr::select(name) %>%
      sf::st_transform(3112)

    dat.tz <- dat.aus%>%
      sf::st_intersection(sf::st_centroid(fire_bbox %>% sf::st_transform(3112) %>% sf::st_union())) %>%
      dplyr::left_join(dat.timezone.names)

    dat.hotspots <- dat.hotspots %>%
      dplyr::mutate(acq_time=paste0("0000",acq_time),
                    acq_time=substr(acq_time,nchar(acq_time)-3,nchar(acq_time)),
                    datetimeutc=lubridate::fast_strptime(paste0(acq_date," ",acq_time),format="%Y-%m-%d %H%M",tz="UTC"),
                    timezone=dat.tz$tz_name,
                    datetimelocal=lubridate::with_tz(datetimeutc,tzone=timezone),
                                                            datelocal=format(datetimelocal,format="%Y-%m-%d"))

  }

  return(dat.hotspots)






}
