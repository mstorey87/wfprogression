#' Internal function to get time zone via sf intersect
#'
#' @param fire_bbox Fire bounding box polygon
#'
#' @return String of time zone name
#' @export
#'
#' @examples
#' #fire_get_timezone(dat.bbox)
fire_get_timezone <- function(fire_bbox){

  checkmate::assert(curl::has_internet(),"Error: internet connection is required")


  closeAllConnections()
  #get the time zone based on footprint centroid and add a local time field
  dat.aus <- rnaturalearth::ne_states(country="Australia") %>%
    dplyr::select(name) %>%
    sf::st_transform(3112)

  dat.cent <- fire_bbox %>%
    sf::st_union() %>%
    sf::st_centroid() %>%
    sf::st_transform(3112) %>%
    sf::st_as_sf()

  #get nearest state to each point
  dat.tz <-dat.aus[sf::st_nearest_feature(dat.cent,dat.aus),] %>%
    dplyr::left_join(wfprogression::dat.timezone.names,by="name")

  closeAllConnections()
  return(dat.tz$tz_name)

}
