#' Create gg plot with and image and/or polygon
#'
#' @description
#' Takes a terra raster and plots it using terrainr package. The terra aggregate function is first used to reduce image size for plotting.
#' If a polygon is also an input, it will be plotted over the image.
#'
#'
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




  g <- ggplot2::ggplot()



  if(!is.null(image)){
    #replace na with zero to ensure terrainr function works
    image <- terra::subst(image,NA,0)

    #aggregate function to make plotting faster
    image <- terra::aggregate(image,agg)


    g <- g+
      terrainr::geom_spatial_rgb(data = image,mapping = ggplot2::aes(x = x, y = y,r = red,g = green,b = blue),na.rm = T)
  }

  if(!is.null(prog)){

    #project prog to match image crs
    if(!is.null(image)){
      prog <- sf::st_transform(prog,sf::st_crs(image))
    }

  }


  if("firetype" %in% names(prog)){

    g <- g+
      ggplot2::geom_sf(data=prog,fill=NA,lwd=0.8,ggplot2::aes(col=firetype))+
      ggplot2::scale_color_manual(values = c("main"="red","spot"="yellow","backburn"="pink"))

  }else{
    g <- g+
      ggplot2::geom_sf(data=prog,fill=NA,lwd=0.8)
  }



  g <- g+
    ggplot2::coord_sf(lims_method = "geometry_bbox")+
    ggplot2::theme_minimal()+
    ggspatial::annotation_scale()
  g

  return(g)

}
