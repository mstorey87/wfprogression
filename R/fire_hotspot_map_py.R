#' Create geoferenced hotspot and WMS image
#'
#' @param fire_bbox Polygon of fire or area to search for images
#' @param start_date First date from which to search for images
#' @param end_date Final date to search for image
#' @param mapkey A mapkey reuired to download hotspots, https://firms.modaps.eosdis.nasa.gov/api/map_key/
#' @param dest_folder Folder to save output tifs other files used in processing, including hotspots
#' @param add_hotspots T/F to add or exclude hotspots from output images
#'
#' @return Write a geotiff to disk. Also writes hotspots csv
#' @export
#'
#' @examples
#' #fire_hotspot_map(fire_bbox = dat.bbox,start_date = datestart,end_date = dateend,dest_folder = outdir)
fire_hotspot_map_py <- function(fire_bbox,start_time,end_time,mapkey="a5452249ca7c7a4ee2e1e6da787f57cc",dest_folder,add_hotspots=T){


  checkmate::assert(stringr::str_detect(class(start_time)[1],"POSIXct"),"Error: times must be posixct")
  checkmate::assert(stringr::str_detect(class(end_time)[1],"POSIXct"),"Error: times must be posixct")


  mytz=wfprogression::fire_get_timezone(fire_bbox)

  start_time=lubridate::with_tz(start_time,tz=mytz)
  end_time=lubridate::with_tz(end_time,tz=mytz)

  #create a series of GIBS and hotspots maps
  #define the sequence of dates
  start_date=format(start_time,format="%Y-%m-%d")
  end_date=format(end_time,format="%Y-%m-%d")

  date_seq <- seq(as.Date(start_date),as.Date(end_date)+1,by="1 day")#dates are in utc, so include day post to capture local date

  if(add_hotspots==T){

    add_hotspots_test=T

    #download hotspots
    hotspots <- wfprogression::fire_search_hotspots(fire_bbox,mapkey,
                                                    min(date_seq),
                                                    max(date_seq),
                                                    dest_folder)

    if(nrow(hotspots)>0){
      hotspots <- hotspots  %>%
        dplyr::filter(datelocal %in% date_seq,
                      datetimelocal>start_time & datetimelocal<end_time)
    }


    #if no hotspots are found, don't add hotspots to map
    if(nrow(hotspots)==0){
      add_hotspots_test=F

    }

  }else{
    hotspots=NULL
  }


  #define all the WMS layers that will be used. All of these have infrered or thermal bands that highlight active fire.
  #these can be viewed through NASA worldview
  wms_layers <- c("MODIS_Aqua_CorrectedReflectance_Bands721",
                  "MODIS_Terra_CorrectedReflectance_Bands721",
                  "VIIRS_SNPP_CorrectedReflectance_BandsM11-I2-I1",
                  "VIIRS_NOAA20_CorrectedReflectance_BandsM11-I2-I1",
                  "MODIS_Aqua_Brightness_Temp_Band31_Night",
                  "MODIS_Terra_Brightness_Temp_Band31_Night",
                  "VIIRS_NOAA21_Brightness_Temp_BandI5_Night",
                  "VIIRS_NOAA20_Brightness_Temp_BandI5_Night",
                  "VIIRS_SNPP_Brightness_Temp_BandI5_Night")


  #for each day, create and save the wms map image
  for(i in 1:length(date_seq)){

    d <- paste0(date_seq[i],"/",date_seq[i])
    bbox <- sf::st_bbox(fire_bbox)

    for(wms in wms_layers){
      pngout <- paste0(dest_folder,"//",wms,"_",date_seq[i],".png")
      tifout <- paste0(dest_folder,"//",wms,"_",date_seq[i],".tif")
      wfprogression::fire_wms_to_tif_python(wms,d,bbox,size = c(1200,1200),pngout,tifout )
      file.remove(pngout)

      #add hotspots
      if(add_hotspots==T){

        #create unique values for legend and filter hotspots by time and satellite
        #get abbreviation wms layer name that will match sat_name column in hotspots
        wms_lyr_name_abbrev <- stringr::str_split(wms,"_")
        wms_lyr_name_abbrev <- tolower(paste0(wms_lyr_name_abbrev[[1]][1],"_",wms_lyr_name_abbrev[[1]][2]))




        #filter hotspots to day or night depending on wms layer name
        if(stringr::str_detect(wms,"Night")){


          #if night time, filter by acq_date (UTC)
          hotspots2 <- hotspots %>%
            dplyr::filter(daynight=="N",
                          acq_date==date_seq[i])
        }else{
          hotspots2 <- hotspots %>%
            dplyr::filter(daynight=="D",
                          datelocal==date_seq[i])

        }


        #add some useful columns for satellite/sensor name and a label for the map
        hotspots2 <- hotspots2 %>%
          dplyr::filter(sat_name==wms_lyr_name_abbrev) %>%
          dplyr::mutate(label=paste0(sat_name," ",datetimelocal)) %>%
          sf::st_transform(4326)


      r <- terra::rast(tifout)







      #test if circle markers have been added (hotspots)
      #if not don't save map
      m <-  wfprogression::fire_GIBS_map(fire_bbox,date_seq[i],wms,add_hotspots=add_hotspots_test,hotspots)
      m_meths <- m$x$calls %>% as.list()
      circles <- max(stringr::str_detect(purrr::map(1:length(m_meths),~m_meths[[.x]]$method) %>% unlist(),
                                         "addCircleMarkers"))

      if((circles==1 & add_hotspots==T)|add_hotspots==F){
        wfprogression::fire_save_GIBS_map(fire_bbox,m,dest_folder)

      }else if(add_hotspots==T & circles==0){
        print(paste0("no ",wms," hotspots, not saving map"))
        #wfprogression::fire_save_GIBS_map(fire_bbox,m,paste0(dest_folder,"2"))

      }



    }









  }







}
}
