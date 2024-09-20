fire_search_progs <- function(fire_poly,start_date,end_date){


  #connect to database
  DB <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname = "cermb_fires",
                       user = "mstorey",
                       password = "bushfires",
                       host = "charus.ad.uow.edu.au",
                       port = 5432)

  #simplify fire polygon shape
  fire_poly <- fire_poly %>%
    sf::st_transform(4283) %>%
    sf::st_concave_hull(ratio = 0.8)

  #add times on to dates for searching
  start_date <- paste0(start_date," 00:00:00")
  end_date <- paste0(as.Date(end_date)+1," 00:00:00")

  #extract geometry of fire as text
  txt_geom <- sf::st_as_text(fire_poly$geometry,EWKT=T)

  #create date part of query
  txt_date <- paste0("datetime between '",start_date, "' AND '", end_date,"'")

  #create query
  myquery <- paste0("SELECT * FROM fires.progressions WHERE st_intersects(fires.progressions.geom,'",txt_geom,"') AND ", txt_date)

  #run query and get results
  x <- sf::st_read(dsn=DB,query=myquery)%>%
    sf::st_as_sf()

  DBI::dbDisconnect(DB)

  if(nrow(x)==0){
    print("no results")
  }else{
    print(paste0(nrow(x)," results"))
    return(x)

  }



}
