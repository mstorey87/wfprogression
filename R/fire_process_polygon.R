#' Create a processed fire polygon and bounding box polygon
#'
#' @param fire_polygon An sf object which is the fire boundary polygon (or search area)
#' @param new_crs New epsg code if transformation is required. e.g. 4382
#' @param fire_name_col A character vector of the column in the polygon with the fire name (or id)
#' @param start_date_col A character vector of the column in the polygon with the fire start date. Column must be date format.
#' @param end_date_col A character vector of the column in the polygon with the fire end date. Column must be date format.
#' @param n_days_before Number of days before the start date to subtract, e.g. so images a few days before the fire can be searched for.
#' @param n_days_after Number of days after the end date to subtract, e.g. so images a few days after the fire can be searched for.
#' @param buffer_km How many km to buffer the fire polygon by during the creation of a bounding box.
#'
#' @return A list with one fire polygon and one bounding box polygon
#' @export
#'
#' @examples
#' #x <- fire_process_polygon(dat.fire.polygon,4283,"FireName","StartDate","EndDate",2,3,20)
fire_process_polygon <- function(fire_polygon,
                                 new_crs=NULL,
                                 fire_name_col,
                                 start_date_col,
                                 end_date_col,
                                 n_days_before,
                                 n_days_after,
                                 buffer_km){


  #select only the start and end date and fire name column
  fire_polygon_2 <- fire_polygon %>%
    dplyr::select(dplyr::all_of(c(fire_name_col,start_date_col,end_date_col)))

  #rename the columns
  names(fire_polygon_2) <- c("firename","startdate","enddate","geometry")


  #transform the fire polygon with new crs if user gives a new crs
  if(!is.null(new_crs)){

    print(paste0("transforming crs from ",sf::st_crs(fire_polygon_2)$input))
    fire_polygon_2 <- sf::st_transform(fire_polygon_2,new_crs)
  }


  #summarise fire polygon to ensure there is only of row
  fire_polygon_2 <- fire_polygon_2 %>%
    dplyr::summarise(firename,startdate,enddate)


  #check that there is only one row. If not end the function and produce message
  if(nrow(fire_polygon_2)>1){
    print("More than one unique combination of fire name, start date and end date. These must be all the same.")

  }else{


    #add a buffer of days either side of start and end date to search for imagery.
    #this may be useful if the start and end dates are wrong in the original file
    fire_polygon_2 <- fire_polygon_2 %>%
      dplyr::mutate(startdate_search=startdate-lubridate::days(n_days_before),
             enddate_search=enddate-lubridate::days(n_days_before))



    fire_bbox <- fire_polygon_2 %>%
      sf::st_transform(3112) %>%
      sf::st_buffer(buffer_km/1000) %>%
      sf::st_transform(sf::st_crs(fire_polygon_2)) %>%
      sf::st_bbox() %>%
      sf::st_as_sfc() %>%
      sf::st_as_sf()

    fire_bbox <- fire_bbox %>%
      cbind(sf::st_drop_geometry(fire_polygon_2))



    return(list(fire_polygon=fire_polygon_2,fire_bbox=fire_bbox))



  }








}
