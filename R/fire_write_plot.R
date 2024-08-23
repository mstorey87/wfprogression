fire_write_plot <- function(tif_path,polygon,fire_bbox,dest_folder){

  image <- terra::rast(tif_path)
    if(length(names(image))<3){
      image <- c(image,image,image)
      names(image) <- c("b1","b2","b3")
    }
    image <- terra::stretch(image)

    #ensure same crs
    polygon <- sf::st_transform(polygon,sf::st_crs(image))



    tmap_image <- tmap::tm_shape(image,bbox = fire_bbox)+
      tmap::tm_rgb(tmap::tm_mv(names(image)))+
      tmap::tm_shape(polygon)+
      tmap::tm_borders(col="blue")+
      tmap::tm_layout(asp = 0,crs=4326)

    out_image <- paste0(dest_folder,"\\quickview_",tools::file_path_sans_ext(basename(tif_path)),".jpg")
    tmap::tmap_save(tmap_image,out_image,outer.margins = c(0,0,0,0))




}
