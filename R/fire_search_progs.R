#' Search for fire progression polygons
#'
#' @param fire_bbox A polygon, usually fire bounding box, to search for images
#' @param start_date The first date for which to search for images YYYY-mm-dd
#' @param end_date The last date for which to search for images YYYY-mm-dd
#'
#' @return sf object
#' @export
#'
#' @examples
#' #dat.progs.sf <- fire_search_progs(fire_bbox=dat.bbox,start_date=datestart,end_date=dateend)
fire_search_progs <- function(fire_bbox=fire_bbox_polygon(),start_date="1990-01-01",end_date="2100-01-01",return_geom=TRUE){


  #connect to database
  DB <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname = "cermb_fires",
                       user = "mstorey",
                       password = "bushfires",
                       host = "charus.ad.uow.edu.au",
                       port = 5432)

  #simplify fire polygon shape for intersect
  #mainly useful when fire polygon is complex

  fire_bbox <- fire_bbox %>%
    sf::st_transform(4283) %>%
    sf::st_concave_hull(ratio = 0.8)

  #add times on to start and end dates
  start_date <- paste0(start_date," 00:00:00")
  end_date <- paste0(as.Date(end_date)+1," 00:00:00")

  #extract geometry of fire as text
  txt_geom <- sf::st_as_text(sf::st_geometry(fire_bbox),EWKT = T)

  #create date part of query
  txt_date <- paste0("dt_local between '",start_date, "' AND '", end_date,"'")

  #create whole SQL query
  #user input option to return results without geometry to save time
  #run query and get results
  if(return_geom==TRUE){
    myquery <- paste0("SELECT * FROM fires.progressions2 WHERE st_intersects(fires.progressions2.geom,'",txt_geom,"') AND ", txt_date)
    x <- sf::st_read(dsn=DB,query=myquery) %>%
      sf::st_as_sf()
  }

  if(return_geom==FALSE){
    myquery <- paste0("SELECT dt_utc,firetype,progid,notes,sourceshp,firename,season,sourcetype,aus_state,dt_local,scanname,id,s3name FROM fires.progressions2 WHERE st_intersects(fires.progressions2.geom,'",txt_geom,"') AND ", txt_date)
    x <- DBI::dbGetQuery(DB,myquery)
  }




  #disconnect from database
  DBI::dbDisconnect(DB)

  if(nrow(x)==0){
    print("no results")
  }else{
    print(paste0(nrow(x)," results"))
    return(x)

  }



}
