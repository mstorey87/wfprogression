#' Create geoferenced hotspot and WMS image
#'
#' @param fire_bbox Polygon of fire or area to search for images
#' @param start_date First date from which to search for images
#' @param end_date Final date to search for image
#' @param mapkey A mapkey reuired to download hotspots, https://firms.modaps.eosdis.nasa.gov/api/map_key/
#' @param dest_folder Folder to save output tifs other files used in processing, including hotspots
#'
#' @return Write a geotiff to disk. Also writes hotspots csv
#' @export
#'
#' @examples
#' #fire_hotspot_map(fire_bbox = dat.bbox,start_date = datestart,end_date = dateend,dest_folder = outdir)
fire_hotspot_map <- function(fire_bbox,start_date,end_date,mapkey="a5452249ca7c7a4ee2e1e6da787f57cc",dest_folder){

  #create a series of GIBS and hotspots maps
  #define the sequence of dates
  date_seq <- seq(as.Date(start_date),as.Date(end_date),by="1 day")

  #download hotspots
  hotspots <- fire_search_hotspots(fire_bbox,mapkey,
                                   min(date_seq)-1,#dates are in utc, so include day prior to capture local date
                                   max(date_seq),
                                   dest_folder) %>%
    dplyr::filter(datelocal %in% date_seq)




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


    for(wms in wms_layers){


   m <-  fire_GIBS_map(fire_bbox,date_seq[i],wms,T,hotspots)
      fire_save_GIBS_map(fire_bbox,m,dest_folder)

    }









  }







}
