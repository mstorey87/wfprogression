#' Title
#'
#' @param fire_bbox Fire bounding box polygon
#'
#' @return String of time zone name
#' @export
#'
#' @examples
#' #fire_get_timezone(dat.bbox)
fire_get_timezone <- function(fire_bbox){

  #get the time zone based on footprint centroid and add a local time field
  dat.aus <- rnaturalearth::ne_states(country="Australia") %>%
    dplyr::select(name) %>%
    sf::st_transform(3112)
  dat.tz <- dat.aus%>%
    sf::st_intersection(sf::st_centroid(fire_bbox %>% sf::st_transform(3112) %>% sf::st_union())) %>%
    dplyr::left_join(dat.timezone.names,by="name")

  return(dat.tz$tz_name)

}
