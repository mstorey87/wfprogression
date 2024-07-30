fire_search_MODIS_VIIRS <- function(fire_bbox,wms_lyr_name,hotspots,dest_folder){

  #extract hotspots depend on which wms layer is chosen
  satnames <- data.frame(wms_name="VIIRS_SNPP_CorrectedReflectance_BandsM11-I2-I1",
                         hotspot_name="SNPP")

  satnames.filter <- satnames %>% filter(wms_name==wms_lyr_name)

  hotspots.filter <- hotspots %>%
    dplyr::filter(satname==satnames.filter$hotspot_name)


  #this helps to make mapshot zoom to the same level as leaflet. From here: https://github.com/r-spatial/mapview/issues/274
  zoomstring <- 'Mozilla/5.0 (compatible; MSIE 10.6; Windows NT 6.1; Trident/5.0; InfoPath.2; SLCC1; .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729; .NET CLR 2.0.50727) 3gpp-gba UNTRUSTED/1.0'


  #transform fire poly to wgs84 to be consistent with WMS
  fire_bbox <- fire_bbox %>%
    sf::st_transform(4326)

  #create an initial leaflet map with the fire polygon. This will be added to later.
  wms_map<-leaflet::leaflet(options = leaflet::leafletOptions(zoomSnap=0,crs = leaflet::leafletCRS("L.CRS.EPSG4326"))) %>%
    leaflet::addPolygons(data = fire_bbox,fillOpacity = 0,color = "red",weight = 2.4)


  #add in all dates to search for images from fire start to fire end
  dates_fires <- as.character(seq(fire_bbox$startdate_search,fire_bbox$enddate_search,by="1 day"))

  res.list <- list()
  for(i in 1:length(dates_fires)){

    wms_map2<-wms_map %>%
      leaflet::addWMSTiles('https://gibs.earthdata.nasa.gov/wms/epsg4326/best/wms.cgi',
                           layers = wms_lyr_name,
                           options = leaflet::WMSTileOptions(time = dates_fires[i]))


    #save as png. Height and width of output png relative to fire bbox size
    bb <- sf::st_bbox(fire_bbox)
    v1 <- round((bb[4]-bb[2])*2000)
    v2 <- round((bb[3]-bb[1])*2000)

    out.file <- paste0(dest_folder,"\\",wms_lyr_name,"_",dates_fires[i],"shot.png")
    mapview::mapshot(wms_map2, file = out.file,remove_url=T,vheight=v1,vwidth=v2, useragent = zoomstring)

    out.tif <-  paste0(dest_folder,"\\",wms_lyr_name,"_",dates_fires[i],".tif")
    fn_geo_png(out.file,out.tif,bb)
    file.remove(out.file)

    #res.list[[i]] <- wms_map2

  }


  return(print("done"))
}
