fire_hotspot_map <- function(fire_bbox,start_date,end_date,mapkey,dest_folder){

  #create a series of GIBS and hotspots maps
  date_seq <- seq(as.Date(start_date),as.Date(end_date),by="1 day")

  hotspots <- fire_search_hotspots(fire_bbox,"a5452249ca7c7a4ee2e1e6da787f57cc",
                                   min(date_seq)-1,#dates are in utc, so include day prior to capture local date
                                   max(date_seq),
                                   dest_folder) %>%
    dplyr::filter(datelocal %in% date_seq)




  wms_layers <- c("MODIS_Aqua_CorrectedReflectance_Bands721",
                  "MODIS_Terra_CorrectedReflectance_Bands721",
                  "VIIRS_SNPP_CorrectedReflectance_BandsM11-I2-I1",
                  "VIIRS_NOAA20_CorrectedReflectance_BandsM11-I2-I1",
                  "MODIS_Aqua_Brightness_Temp_Band31_Night",
                  "MODIS_Terra_Brightness_Temp_Band31_Night",
                  "VIIRS_NOAA21_Brightness_Temp_BandI5_Night",
                  "VIIRS_NOAA20_Brightness_Temp_BandI5_Night",
                  "VIIRS_SNPP_Brightness_Temp_BandI5_Night")


  #for each day download hotspots and create map
  for(i in 1:length(date_seq)){


    for(wms in wms_layers){


   m <-  fire_GIBS_map(fire_bbox,date_seq[i],wms,T,hotspots)
      fire_save_GIBS_map(fire_bbox,m,dest_folder)

    }









  }







}
