#' Find maximum fire spread line
#'
#' @param time_col Name of date time column (posix)
#' @param include_spots T/F to include spot fire in spread line creation. Data must have firetype column populated with "spot", "main" or "backburn"
#' @param include_backburns T/F to include backburns in spread line creation. Data must have firetype column populated with "spot", "main" or "backburn"
#' @param polygons Fire polygons sf object
#' @param convex_hull T/F whether to convert fire polygon to convex hull first
#' @param max_only T/F return only the max spread line
#' @param internal_only T/F return only lines completely within the fire polygon at time 2
#'
#' @return sf object
#' @export
#'
#' @examples
#' #
fire_max_spread_line <- function(polygons,time_col,include_spots=F,
                                 include_backburns=F,
                                 convex_hull=T,max_only=T,internal_only=F,
                                 max_minutes=360,
                                 densify_m=100){


  #ensure crs is projected
  polygons_crs <- sf::st_crs(polygons)$wkt
  checkmate::assert(stringr::str_detect(polygons_crs,"PROJCRS"),"Error: polygons must be projected crs")

  #add time column with standard name
  polygons$time <- polygons[[time_col]]

  #ensure time column is posxct
  checkmate::assert(unique(stringr::str_detect(class(polygons$time),"POSIX")),"Error: Time column must be posixct")


  dat.poly <- polygons

  #add a season variable if needed
  dat.poly <- dat.poly %>%
    dplyr::mutate(semes=lubridate::semester(time),yr=lubridate::year(time),season=ifelse(semes==1,paste0(yr-1,"-",yr),paste0(yr,"-",yr+1))) %>%
    dplyr::select(-semes,-yr)



  #keep only main fire polygon, or spot or backburn depending on user input
  if(include_spots==F & include_backburns == F)    dat.poly <- dat.poly %>% filter(firetype == "main")
  if(include_spots==F & include_backburns == T)    dat.poly <- dplyr::filter(firetype=="main"|firetype=="backburn")
  if(include_spots==T & include_backburns == T)    dat.poly <- dplyr::filter(firetype=="main"|firetype=="backburn"|firetype=="spot")
  if(include_spots==T & include_backburns == F)    dat.poly <- dplyr::filter(firetype=="main"|firetype=="spot")




  dat.poly <- dat.poly %>%

    #arrange in order of time and give a time id
    dplyr::arrange(time)
  #get convex hull of polygon to save processing time
  if(convex_hull==T) dat.poly <- dat.poly %>% sf::st_convex_hull()





  res <- list()
  for(i in 1:nrow(dat.poly)){
    print(i)

    #get current loop polygon
    dat.i <- dat.poly[i,]

    #get all prior polygons within
    #ensure we only consider prior polygons from the same season, otherwise the code will mistake fire from previous seasons as being part of the same fire
    dat.prior.all <- dat.poly %>%
      dplyr::filter(time < dat.i$time,season==dat.i$season)

    #filter by max_minutes input
    dat.prior.filtered <- dat.prior.all %>%
      dplyr::filter(time > dat.i$time - lubridate::minutes(max_minutes))

    #get only prior polygon that intersect current polygon. dat.prior.all can be empty from above time filter
    #intersect by unaltered polygons
    dat.prior.filtered <- dat.prior.filtered %>%
        sf::st_filter(dat.i,.predicate = sf::st_intersects)


    #skip if not prior intersecting polygon
    if(nrow(dat.prior.filtered)>0){

      #get only most recent prior polygon
      dat.prior <- dat.prior.filtered %>%
        dplyr::filter(time==max(time))


      dat.lines <- wfprogression::fire_RANN_nearest_points(dat.i,dat.prior,densify_m=densify_m,max_only = max_only)

      #combine attributes
      dat.lines <- cbind(dat.lines,sf::st_drop_geometry(dat.i))

      #get line distance
      dat.lines$line_metres <- as.numeric(sf::st_length(dat.lines))


      dat.lines <- dat.lines %>%
        #select final attributes
        dplyr::mutate(start_time=unique(dat.prior$time),
                      end_time=unique(dat.i$time),
                      timestep_hours=as.numeric(difftime(end_time,start_time,units="hours")),
                      line_km=(line_metres/1000),
                      ros_kmh=line_km/timestep_hours) %>%
        dplyr::select(start_time,end_time,line_km,timestep_hours,ros_kmh)


      #flag lines from results if it crosses polygons with prior times (can happen when fires merge), ie all prior times from same season (dat.prior.all)
      dat.lines.intersect <- sf::st_intersection(sf::st_geometry(dat.lines),dat.prior.all %>% dplyr::select(time_poly=time)) %>%
        sf::st_as_sf() %>%
        dplyr::mutate(len=as.numeric(sf::st_length(.))) %>%
        #check if any lines intersect y more than a cm. 0 should result when intersecting the line end with a boundary point, but sometimes there is a small error
        dplyr::filter(len>0.01)

      res[[i]] <- dat.lines %>% mutate(prior_intersects=nrow(dat.lines.intersect)>0)
    }






  }


  res.all <- do.call(rbind,res)


  return(res.all)
}

