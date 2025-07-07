#' Create a bounding box sf polygon
#'
#' @description
#' Takes an input sf polygon (e.g. fire boundary) and returns its bounding box as an sf polygon.
#' Optionally buffers the polygon before calculating the bounding box.
#' If `sf_polygon` is NULL, a default box around mainland Australia is returned.
#'
#' @param sf_polygon An sf object representing the fire boundary polygon (or area of interest).
#' @param bbox_buffer_km Distance in kilometres to buffer the polygon before creating the bounding box.
#' @param crs EPSG code for buffering step. Should be a projected CRS (e.g. 3112 for GDA94 / Geoscience Australia Lambert).
#'
#' @return An sf polygon of the bounding box, with the original CRS.
#' @export
#'
#' @examples
#' # Example: Create a bbox around a simple square polygon
#' library(sf)
#' p <- st_as_sf(st_sfc(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))), crs = 4326))
#' fire_bbox_polygon(p, bbox_buffer_km = 10, crs = 3112)
#'
#' # Example: Create default Australia bbox
#' fire_bbox_polygon(NULL, bbox_buffer_km = 0, crs = 3112)
fire_bbox_polygon <- function(sf_polygon = NULL,
                              bbox_buffer_km = 0,
                              crs = 3112) {

  # If no input polygon, use a default box around Australia in EPSG:4283
  if (is.null(sf_polygon)) {
    aus_wkt <- "SRID=4283;POLYGON ((111.3148 -45.36846, 155.6298 -45.36846, 155.6298 -8.916367, 111.3148 -8.916367, 111.3148 -45.36846))"
    sf_polygon <- sf::st_as_sf(data.frame(geometry = aus_wkt), wkt = "geometry")
  }

  # Dissolve multiple parts into one, ensure only one row
  sf_polygon <- sf_polygon %>%
    sf::st_union() %>%
    sf::st_as_sf()

  # Explicitly set geometry column name
  sf::st_geometry(sf_polygon) <- "geometry"

  # Buffer, calculate bbox, convert to polygon, and back-transform to original CRS
  fire_bbox <- sf_polygon %>%
    sf::st_transform(crs) %>%                   # to projected CRS for buffering
    sf::st_buffer(bbox_buffer_km * 1000) %>%    # apply buffer in meters
    sf::st_transform(sf::st_crs(sf_polygon)) %>% # back to original CRS
    sf::st_bbox() %>%                           # create bbox
    sf::st_as_sfc() %>%                         # bbox to sfc polygon
    sf::st_as_sf()                              # sfc to sf

  # Carry over any non-geometry attributes if needed
  fire_bbox <- cbind(fire_bbox, sf::st_drop_geometry(sf_polygon))
  sf::st_geometry(fire_bbox) <- "geometry"

  return(fire_bbox)
}


#' #' Create a bounding box sf polygon
#' #'
#' #' @description
#' #' Takes an sf polygon (e.g. fire boundary), optionally buffers the polygon, create an sf bbox and then convert to sf polygon.
#' #' If sf_polygon is empty, a box around Australia is the output
#' #'
#' #'
#' #' @param sf_polygon An sf object which is the fire boundary polygon (or search area)
#' #' @param bbox_buffer_km How many km to buffer the fire polygon by during the creation of a bounding box.
#' #' @param crs CRS for buffering (epsg code). Must be projected CRS.
#' #' @return A bounding box sf polygon with the input polygon's CRS
#' #' @export
#' #'
#' #' @examples
#' #' #fire_bbox_polygon(dat.fire,100,3112)
#' fire_bbox_polygon <- function(sf_polygon=NULL,
#'                                  bbox_buffer_km=0,
#'                               crs=3112){
#'
#'
#'
#'
#'   #if no fire polygon is input,  use Australia polygon
#'   if(is.null(sf_polygon)){
#'
#'     sf_polygon <- sf::st_as_sf(data.frame(geometry= "SRID=4283;POLYGON ((111.3148 -45.36846, 155.6298 -45.36846, 155.6298 -8.916367, 111.3148 -8.916367, 111.3148 -45.36846))"),wkt="geometry")
#'
#'   }
#'
#'
#'
#'   #summarise fire polygon to ensure there is only of row
#'   sf_polygon <- sf_polygon %>%
#'     sf::st_union() %>%
#'     sf::st_as_sf()
#'
#'   sf::st_geometry(sf_polygon) <- "geometry"
#'
#'   fire_bbox <- sf_polygon %>%
#'     sf::st_transform(crs) %>%
#'     sf::st_buffer(bbox_buffer_km*1000) %>%
#'     sf::st_transform(sf::st_crs(sf_polygon)) %>%
#'     sf::st_bbox() %>%
#'     sf::st_as_sfc() %>%
#'     sf::st_as_sf()
#'
#'
#'   fire_bbox <- fire_bbox %>%
#'     cbind(sf::st_drop_geometry(sf_polygon))
#'
#'   sf::st_geometry(fire_bbox) <- "geometry"
#'
#'
#'   return(fire_bbox)
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' }
