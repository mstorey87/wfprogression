#save as png. Height and width of output png relative to fire bbox size

#this helps to make mapshot zoom to the same level as leaflet. From here: https://github.com/r-spatial/mapview/issues/274



#' Capture wms map screenshot and write geotiff
#'
#' @param fire_bbox Fire bounding box polygon
#' @param wms_GIBS_map Leaflet map from fire_GIBS_map()
#' @param dest_folder Output folder for geotiffs
#'
#' @return write geotiff to disk
#' @export
#'
#' @examples
#' # fire_save_GIBS_map(fire_bbox,m,dest_folder)
fire_save_GIBS_map <- function(fire_bbox,wms_GIBS_map,dest_folder){

  #get bounding box coordinates
   bb <- sf::st_bbox(fire_bbox)

   #calculate a scaling factor for the output image
   v1 <- round((bb[4]-bb[2])*2000)
   v2 <- round((bb[3]-bb[1])*2000)


   #extract datetime of hotspots if they have been added to the map from wms leaflet data
   args_length <- wms_GIBS_map$x$calls %>% length()
   if(args_length==4){

     chr_name <- wms_GIBS_map$x$calls[[4]]$args[[1]] %>%
       stringr::str_replace_all(c("noaa20"="","noaa21"="")) %>%
       stringr::str_split(">") %>%
       purrr::map_chr(3) %>% gsub("[^0-9.]", "", .)


   }else{
     chr_name <- paste0(stringr::str_replace_all(wms_GIBS_map$x$calls[[2]]$args[[4]]$time,"-",""),"_____")


   }


   out_label <- paste0(chr_name," ",wms_GIBS_map$x$calls[[2]]$args[[4]]$layers)

   png_path <- paste0(dest_folder,"\\",out_label,"_temp_png.png")

   #save a png to file
   zoomstring <- 'Mozilla/5.0 (compatible; MSIE 10.6; Windows NT 6.1; Trident/5.0; InfoPath.2; SLCC1; .NET CLR 3.0.4506.2152; .NET CLR 3.5.30729; .NET CLR 2.0.50727) 3gpp-gba UNTRUSTED/1.0'
   mapview::mapshot(wms_GIBS_map, file = png_path,remove_url=T,vheight=v1,vwidth=v2, useragent = zoomstring)



   #read the png and add bounding coordinates
  img <- png::readPNG(png_path)
  x <- raster::brick(img)
  x <- raster::dropLayer(x,4)
  raster::crs(x) <- sf::st_crs(4326)$wkt
  raster::extent(x) <- raster::extent(fire_bbox)

  #strectch
  x <- raster::stretch(x,maxv=254)

  #save as tif
  out.tif <-  paste0(dest_folder,"\\",out_label,".tif")
  raster::writeRaster(x, out.tif, format="GTiff", overwrite=TRUE,datatype="INT1U")

  #remove temporary png
  file.remove(png_path)
}
