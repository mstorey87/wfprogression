#' Search database for line scans
#'
#' @param fire_bbox A polygon, usually fire bounding box, to search for images
#' @param start_date The first date for which to search for images YYYY-mm-dd
#' @param end_date The last date for which to search for images YYYY-mm-dd
#'
#' @return sf object
#' @export
#'
#' @examples
#' #dat.scans.sf <- fire_search_scans(fire_bbox=dat.bbox,start_date=datestart,end_date=dateend)
fire_search_scans <- function(fire_bbox,start_date,end_date){

  #connect to database
  DB <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname = "cermb_fires",
                       user = "mstorey",
                       password = "bushfires",
                       host = "charus.ad.uow.edu.au",
                       port = 5432)



  #simplify fire polygon shape
  #this is useful if a complex fire polygon is the input.
    fire_bbox <- fire_bbox %>%
       sf::st_transform(4283) %>%
       sf::st_concave_hull(ratio = 0.8)

  #add times on to dates for searching
  start_date <- paste0(start_date," 00:00:00")
  end_date <- paste0(as.Date(end_date)+1," 00:00:00")

  #extract geometry of fire as text
  txt_geom <- sf::st_as_text(fire_bbox$geometry,EWKT=T)

  #create date part of query
  txt_date <- paste0("datetimelocal between '",start_date, "' AND '", end_date,"'")

  #create whole SQL query
  myquery <- paste0("SELECT * FROM fires.ras_convexhulls2 WHERE st_intersects(fires.ras_convexhulls2.geom,'",txt_geom,"') AND ", txt_date)

  #run query and get results
  x <- sf::st_read(dsn=DB,query=myquery) %>%
    sf::st_as_sf()

  #disconnect from database
  DBI::dbDisconnect(DB)

  if(nrow(x)==0){
    print("no results")
  }else{
    print(paste0(nrow(x)," results"))
    return(x)

  }

}
