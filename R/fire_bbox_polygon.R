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
fire_bbox_polygon <- function(search_polygon=NULL,
                                 bbox_buffer_km=0,
                                 new_crs=NULL){


  #transform the fire polygon with new crs if user gives a new crs
  if(!is.null(new_crs)){

    print(paste0("transforming crs from ",sf::st_crs(search_polygon)$input))
    search_polygon <- sf::st_transform(search_polygon,new_crs)
  }


  #if no fire polygon is input,  use australia polygon
  if(is.null(search_polygon)){

    search_polygon <- sf::st_as_sf(data.frame(geometry= "SRID=4283;POLYGON ((111.3148 -45.36846, 155.6298 -45.36846, 155.6298 -8.916367, 111.3148 -8.916367, 111.3148 -45.36846))"),wkt="geometry")

  }



  #summarise fire polygon to ensure there is only of row
  search_polygon <- search_polygon %>%
    sf::st_union() %>%
    sf::st_as_sf()

  sf::st_geometry(search_polygon) <- "geometry"

  fire_bbox <- search_polygon %>%
    sf::st_transform(3112) %>%
    sf::st_buffer(bbox_buffer_km*1000) %>%
    sf::st_transform(sf::st_crs(search_polygon)) %>%
    sf::st_bbox() %>%
    sf::st_as_sfc() %>%
    sf::st_as_sf()


  fire_bbox <- fire_bbox %>%
    cbind(sf::st_drop_geometry(search_polygon))

  sf::st_geometry(fire_bbox) <- "geometry"


  return(fire_bbox)










}
