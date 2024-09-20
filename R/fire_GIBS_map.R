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

#' Create NASA GIBS map with NASA hotspots
#'
#' @param fire_bbox Fire bounding box polygon
#' @param date_string Date for wms image request
#' @param wms_lyr_name wms layer name. See fire_GIBS_products()
#' @param add_hotspots Add VIIRS or MODIS hotspots. TRUE/FALSE
#' @param hotspots hotspots sf object from fire_search_hotspots()
#'
#' @return leaflet map
#' @export
#'
#' @examples
#' # fire_GIBS_map(fire_bbox,date_seq[i],wms,T,hotspots)
fire_GIBS_map <- function(fire_bbox,date_string,wms_lyr_name,add_hotspots=F,hotspots=NULL){


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


  if(add_hotspots==T){

    #create unique values for legend and filter hotspots by time and satellite
    #get abbreviation wms layer name that will match sat_name column in hotspots
    wms_lyr_name_abbrev <- stringr::str_split(wms_lyr_name,"_")
    wms_lyr_name_abbrev <- tolower(paste0(wms_lyr_name_abbrev[[1]][1],"_",wms_lyr_name_abbrev[[1]][2]))




    #filter hotspots to day or night depending on wms layer name
    if(stringr::str_detect(wms_lyr_name,"Night")){


      #if night time, filter by acq_date (UTC)
      hotspots <- hotspots %>%
        dplyr::filter(daynight=="N",
                      acq_date==date_string)
    }else{
      hotspots <- hotspots %>%
        dplyr::filter(daynight=="D",
                      datelocal==date_string)

    }


    hotspots <- hotspots %>%
      dplyr::filter(sat_name==wms_lyr_name_abbrev) %>%
      dplyr::mutate(label=paste0(sat_name," ",datetimelocal)) %>%
      sf::st_transform(4326)



    if(nrow(hotspots)>0){





    #add the hotspots
    colpal <- leaflet::colorFactor(palette ="BrBG", unique(hotspots$label),reverse = T)

    wms_map2 <- wms_map2 %>%
      leaflet::addCircleMarkers(data=hotspots,
                                radius=2,
                                color=~colpal(label),
                                stroke = F,
                                fillOpacity = 1) %>%
      leaflegend::addLegendFactor(
                         pal = colpal,
                         values = unique(hotspots$label),
                           opacity = 1,
                           labelStyle = 'font-size: 24px; font-weight: bold;')

    }else{
      print("no hotspots, base GIBS map only")
    }


  }



  return(wms_map2)
}
