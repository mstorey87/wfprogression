#' create gg plot with image and or polygon
#'
#' @param image terra::rast() object
#' @param prog sf polygons
#' @param agg value for terra::aggregate()
#'
#' @return gg plot
#' @export
#'
#' @examples
#' #
fire_gg_map <- function(image=NULL,prog=NULL,agg=5){

  #replace na with zero to ensure terrainr function works
  image <- terra::subst(image,NA,0)

  #aggregate function to make plotting faster
  image <- terra::aggregate(image,agg)


  g <- ggplot2::ggplot()

  if(!is.null(image)){
    g <- g+
        terrainr::geom_spatial_rgb(data = image,mapping = ggplot2::aes(x = x, y = y,r = red,g = green,b = blue),na.rm = T)
  }

  if(!is.null(prog)){
    g <- g+
      ggplot2::geom_sf(data=prog,fill=NA,lwd=0.8,ggplot2::aes(col=firetype))+
      ggplot2::geom_sf(data=prog,fill=NA,lwd=0.8,ggplot2::aes(col=firetype))+
      ggplot2::scale_color_manual(values = c("main"="red","spot"="yellow","backburn"="pink"))

  }




  g <- g+
      ggplot2::coord_sf(lims_method = "geometry_bbox")+
      ggplot2::theme_void()+
      ggspatial::annotation_scale()
  g

  return(g)

}
