#' Get Australian time zone of fire_bbox centroid
#'
#' @param fire_bbox Fire bounding box polygon
#'
#' @return String of time zone name e.g. "Australia/Sydney"
#' @export
#'
#' @examples
#' #fire_get_timezone(dat.bbox)
fire_get_timezone <- function(fire_bbox){

  dat.aus <- wfprogression::dat.aus

  dat.cent <- fire_bbox %>%
    sf::st_union() %>%
    sf::st_centroid() %>%
    sf::st_transform(3112) %>%
    sf::st_as_sf()

  #get nearest state to each point
  dat.tz <-dat.aus[sf::st_nearest_feature(dat.cent,dat.aus),] %>%
    dplyr::left_join(wfprogression::dat.timezone.names,by="name")

  #closeAllConnections()
  return(dat.tz$tz_name)

}
