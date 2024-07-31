fire_map_hotspots <- function(fire_bbox,wms_lyr_name,hotspots,dest_folder){

  #extract hotspots depend on which wms layer is chosen
  satnames <- data.frame(wms_name="VIIRS_SNPP_CorrectedReflectance_BandsM11-I2-I1",
                         hotspot_name="SNPP")

  satnames.filter <- satnames %>% dplyr::filter(wms_name==wms_lyr_name)


  #get the time zone for the fire
  dat.aus <- rnaturalearth::ne_states(country="Australia") %>%
    dplyr::select(name)

  dat.tz <- dat.aus %>%
    sf::st_transform(sf::st_crs(fire_bbox)) %>%
    sf::st_intersection(sf::st_centroid(fire_bbox)) %>%
    dplyr::left_join(dat.timezone.names)




  #filter hotspots by satellite name and fire area
  dates_fires <- format(seq(fire_bbox$startdate_search,fire_bbox$enddate_search,by="1 day"),format="%Y-%m-%d")


  hotspots.filter <- hotspots %>%
    dplyr::filter(satname==satnames.filter$hotspot_name) %>%
    #add column for local time
    dplyr::mutate(datetimelocal=lubridate::with_tz(datetimeutc,tzone=dat.tz$tz_name),
                  date_local=format(datetimelocal,format="%Y-%m-%d")) %>%
    dplyr::filter(date_local %in% dates_fires) %>%

    sf::st_intersection(fire_bbox %>% dplyr::select(-everything()))






  #this helps to make mapshot zoom to the same level as leaflet. From here: https://github.com/r-spatial/mapview/issues/274
  zoomstring <- 'Mozilla/5.0 (compatible; MSIE 10.6; Windows NT 6.1; Trident/5.0; InfoPath.2; SLCC1; .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729; .NET CLR 2.0.50727) 3gpp-gba UNTRUSTED/1.0'


  #transform fire poly to wgs84 to be consistent with WMS
  fire_bbox <- fire_bbox %>%
    sf::st_transform(4326)



  #add in all dates to search for images from fire start to fire end
  times_hotspots <- unique(hotspots.filter$datetimelocal)

  res.list <- list()
  for(i in 1:length(times_hotspots)){

    #create an initial leaflet map with the fire polygon. This will be added to later.
    wms_map<-leaflet::leaflet(options = leaflet::leafletOptions(zoomSnap=0,crs = leaflet::leafletCRS("L.CRS.EPSG4326"))) %>%
      leaflet::addPolygons(data = fire_bbox,fillOpacity = 0,color = "red",weight = 2.4)



    datetime_string <- format(times_hotspots[i],format="%Y-%m-%d %H:%M:%S")

    hotspots.filter.i <- hotspots.filter %>%
      dplyr::filter(datetimelocal==times_hotspots[i])

    #only add wms if daytime hotspots
    hour_of_day <- lubridate::hour(datetime_string)
    if(hour_of_day >= 8 & hour_of_day <= 18){

      wms_map<-wms_map %>%
        leaflet::addWMSTiles('https://gibs.earthdata.nasa.gov/wms/epsg4326/best/wms.cgi',
                    layers = wms_lyr_name,
                    options = leaflet::WMSTileOptions(time = substr(datetime_string,1,10)))
    }


    #add the hotspots
    colpal <- leaflet::colorFactor(palette = c("red"), datetime_string,reverse = F)

    wms_map <- wms_map %>%
      leaflet::addCircleMarkers(data=hotspots.filter.i,
                       radius=3,
                       color=~colpal(datetime_string),
                       stroke = F,
                       fillOpacity = 1) %>%
      leaflet::addLegend('bottomright', pal = colpal, values = datetime_string,  title = paste0(datetime_string," ",hotspots.filter.i$satname[1]),      opacity = 1)




    #save as png. Height and width of output png relative to fire bbox size
    bb <- sf::st_bbox(fire_bbox)
    v1 <- round((bb[4]-bb[2])*2000)
    v2 <- round((bb[3]-bb[1])*2000)

    out.file <- paste0(dest_folder,"\\",wms_lyr_name,"_",dates_fires[i],"shot.png")
    mapview::mapshot(wms_map, file = out.file,remove_url=T,vheight=v1,vwidth=v2, useragent = zoomstring)

    out.tif <-  paste0(dest_folder,"\\",wms_lyr_name,"_",dates_fires[i],".tif")
    fn_geo_png(out.file,out.tif,bb)
    file.remove(out.file)

    #res.list[[i]] <- wms_map2

  }


  return(print("done"))
}
