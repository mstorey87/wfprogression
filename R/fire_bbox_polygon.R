#' Create a bounding box polygon
#'
#' @param fire_polygon An sf object which is the fire boundary polygon (or search area)
#' @param bbox_buffer_km How many km to buffer the fire polygon by during the creation of a bounding box.
#' @param new_crs New epsg code if transformation is required. Default to 4283 (GDA94)
#' @return A list with one fire polygon and one bounding box polygon
#' @export
#'
#' @examples
#' #x <- fire_bbox_polygon(fire_polygon = dat.fire.polygon,bbox_buffer_km = 20,new_crs = 4283)
fire_bbox_polygon <- function(fire_polygon,
                                 bbox_buffer_km,
                                 new_crs=NULL){


  #transform the fire polygon with new crs if user gives a new crs
  if(!is.null(new_crs)){

    print(paste0("transforming crs from ",sf::st_crs(fire_polygon)$input))
    fire_polygon <- sf::st_transform(fire_polygon,new_crs)
  }


  #summarise fire polygon to ensure there is only of row
  fire_polygon <- fire_polygon %>%
    sf::st_union() %>%
    sf::st_as_sf()

  sf::st_geometry(fire_polygon) <- "geometry"

  fire_bbox <- fire_polygon %>%
    sf::st_transform(3112) %>%
    sf::st_buffer(bbox_buffer_km*1000) %>%
    sf::st_transform(sf::st_crs(fire_polygon)) %>%
    sf::st_bbox() %>%
    sf::st_as_sfc() %>%
    sf::st_as_sf()


  fire_bbox <- fire_bbox %>%
    cbind(sf::st_drop_geometry(fire_polygon))

  sf::st_geometry(fire_bbox) <- "geometry"


  return(fire_bbox)










}
