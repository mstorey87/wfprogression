#' Create a bounding box sf polygon
#'
#' @description
#' Takes an sf polygon (e.g. fire boundary), optionally buffers the polygon, create an sf bbox and then convert to sf polygon.
#' If sf_polygon is empty, a box around Australia is the output
#'
#'
#' @param sf_polygon An sf object which is the fire boundary polygon (or search area)
#' @param bbox_buffer_km How many km to buffer the fire polygon by during the creation of a bounding box.
#' @param crs CRS for buffering (epsg code). Must be projected CRS.
#' @return A bounding box sf polygon with the input polygon's CRS
#' @export
#'
#' @examples
#' #fire_bbox_polygon(dat.fire,100,3112)
fire_bbox_polygon <- function(sf_polygon=NULL,
                                 bbox_buffer_km=0,
                              crs=3112){




  #if no fire polygon is input,  use Australia polygon
  if(is.null(sf_polygon)){

    sf_polygon <- sf::st_as_sf(data.frame(geometry= "SRID=4283;POLYGON ((111.3148 -45.36846, 155.6298 -45.36846, 155.6298 -8.916367, 111.3148 -8.916367, 111.3148 -45.36846))"),wkt="geometry")

  }



  #summarise fire polygon to ensure there is only of row
  sf_polygon <- sf_polygon %>%
    sf::st_union() %>%
    sf::st_as_sf()

  sf::st_geometry(sf_polygon) <- "geometry"

  fire_bbox <- sf_polygon %>%
    sf::st_transform(crs) %>%
    sf::st_buffer(bbox_buffer_km*1000) %>%
    sf::st_transform(sf::st_crs(sf_polygon)) %>%
    sf::st_bbox() %>%
    sf::st_as_sfc() %>%
    sf::st_as_sf()


  fire_bbox <- fire_bbox %>%
    cbind(sf::st_drop_geometry(sf_polygon))

  sf::st_geometry(fire_bbox) <- "geometry"


  return(fire_bbox)










}
