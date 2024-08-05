# some examples:
# MODIS_Aqua_CorrectedReflectance_TrueColor
# MODIS_Terra_CorrectedReflectance_TrueColor
# MODIS_Aqua_CorrectedReflectance_Bands721
# MODIS_Terra_CorrectedReflectance_Bands721
# VIIRS_SNPP_CorrectedReflectance_BandsM11-I2-I1
# VIIRS_NOAA20_CorrectedReflectance_BandsM11-I2-I1
# VIIRS_NOAA20_CorrectedReflectance_TrueColor
# MODIS_Terra_Aerosol_Optical_Depth_3km
# MODIS_Aqua_Aerosol_Optical_Depth_3km

fire_search_MODIS_VIIRS <- function(fire_bbox,date_string,wms_lyr_name,save_to_file=FALSE,dest_folder=NULL){




  #this helps to make mapshot zoom to the same level as leaflet. From here: https://github.com/r-spatial/mapview/issues/274
  zoomstring <- 'Mozilla/5.0 (compatible; MSIE 10.6; Windows NT 6.1; Trident/5.0; InfoPath.2; SLCC1; .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729; .NET CLR 2.0.50727) 3gpp-gba UNTRUSTED/1.0'


  #transform fire poly to wgs84 to be consistent with WMS
  fire_bbox <- fire_bbox %>%
    sf::st_transform(4326)

  #create an initial leaflet map with the fire polygon. This will be added to later.
  wms_map<-leaflet::leaflet(options = leaflet::leafletOptions(zoomSnap=0,crs = leaflet::leafletCRS("L.CRS.EPSG4326"))) %>%
    leaflet::addPolygons(data = fire_bbox,fillOpacity = 0,color = "red",weight = 2.4)

  wms_map2<-wms_map %>%
      leaflet::addWMSTiles('https://gibs.earthdata.nasa.gov/wms/epsg4326/best/wms.cgi',
                           layers = wms_lyr_name,
                           options = leaflet::WMSTileOptions(time = date_string))


  if(save_to_file==TRUE){
    #save as png. Height and width of output png relative to fire bbox size
    bb <- sf::st_bbox(fire_bbox)
    v1 <- round((bb[4]-bb[2])*2000)
    v2 <- round((bb[3]-bb[1])*2000)

    out.file <- paste0(dest_folder,"\\",wms_lyr_name,"_",date_string,"shot.png")
    mapview::mapshot(wms_map2, file = out.file,remove_url=T,vheight=v1,vwidth=v2, useragent = zoomstring)

    out.tif <-  paste0(dest_folder,"\\",wms_lyr_name,"_",date_string,".tif")
    fn_geo_png(out.file,out.tif,bb)
    file.remove(out.file)
  }





  return(wms_map2)
}
