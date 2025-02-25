#' Find maximum fire spread line
#'
#' @param time_col Name of date time column (posix)
#' @param id_col Name of unique id column to add to output lines
#' @param include_spots T/F to include spot fire in spread line creation. Data must have firetype column populated with "spot", "main" or "backburn"
#' @param include_backburns T/F to include backburns in spread line creation. Data must have firetype column populated with "spot", "main" or "backburn"
#' @param polygons Fire polygons sf object
#' @param convex_hull T/F whether to convert fire polygon to convex hull first
#' @param max_only T/F return only the max spread line
#' @param internal_only T/F return only lines completely within the fire polygon at time 2
#' @param min_minutes minimum time interval between progressions
#' @param max_minutes maximum time interval between progression
#' @param densify_m metres between vertices on polygon edge from which to measure spread
#'
#' @return sf object
#' @export
#'
#' @examples
#' #
fire_max_spread_line <- function(polygons,
                                 time_col,
                                 id_col,
                                 include_spots=F,
                                 include_backburns=F,
                                 convex_hull=T,
                                 max_only=T,
                                 internal_only=F,
                                 min_minutes=20,
                                 max_minutes=240,
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






  res <- list()
  for(i in 1:nrow(dat.poly)){
    print(i)

    #get current loop polygon
    dat.i <- dat.poly[i,]
    #get all prior polygons within
    #ensure we only consider prior polygons from the same season, otherwise the code will mistake fire from previous seasons as being part of the same fire
    dat.prior.all <- dat.poly %>%
      dplyr::filter(time < dat.i$time,season==dat.i$season)



    #filter by max_minutes input and min_minutes
    dat.prior.all <- dat.prior.all %>%
      #ensure all prior polygons are at least min_minutes prior. Filter out polygons too close
      dplyr::mutate(mins_diff=as.numeric(difftime(dat.i$dt_utc,dt_utc,"utc","mins")))


    #ensure each area of dat.prior.all has the time that fire first reach that spot
    # suppressMessages({
    #
    #   #ensure each area of dat.prior.all has the time that fire first reach that spot
    #   dat.prior.all.diff <- dat.prior.all %>%
    #     #arrange by time first, so first time is kept
    #     dplyr::arrange(time) %>%
    #     # st_cast("POLYGON") %>%
    #     sf::st_transform(4283) %>%
    #     sf::st_difference() %>%
    #     sf::st_transform(sf::st_crs(dat.i))
    #
    # })
    #


    dat.prior.filtered <- dat.prior.all %>%
      dplyr::filter(mins_diff >= min_minutes & mins_diff <= max_minutes)

    #get only prior polygon that intersect current polygon. dat.prior.all can be empty from above time filter
    #intersect by unaltered polygons
    dat.prior.filtered <- dat.prior.filtered %>%
      sf::st_filter(dat.i,.predicate = sf::st_intersects)

    #skip if not prior intersecting polygon
    if(nrow(dat.prior.filtered)>0){

      #get only most recent prior polygon
      dat.prior <- dat.prior.filtered %>%
        dplyr::filter(time==max(time))

      #ensure the prior polygon is completely contained by the first

      sf::st_geometry(dat.i) <- sf::st_union(sf::st_geometry(dat.i),sf::st_union(sf::st_geometry(dat.prior)))

      #get convex hull of polygon to save processing time
      #run this in the filtered data, as we need to maintain the polygons of dat.prior.all for a later intersect
      if(convex_hull==T){
        dat.i <- dat.i %>% sf::st_convex_hull()
        dat.prior <- dat.prior %>% sf::st_convex_hull()
      }


      dat.lines <- wfprogression::fire_RANN_nearest_points(dat.i,dat.prior,densify_m=densify_m,max_only = max_only,within_only = internal_only)

      #combine attributes
      dat.lines <- cbind(dat.lines,sf::st_drop_geometry(dat.i))


      #check for cases where the prior polygon is identical to another prior polygon in the section near the spread line end
      #this can happen when another section of the polygon has changed and been given a new time, so the section near the spread line does,
      #not change but is still given the new time.
      dat.lines.start <- sf::st_cast(sf::st_geometry(dat.lines),"POINT")[2,] %>%
        sf::st_as_sf() %>%
        #dplyr::mutate(id=dplyr::row_number()) %>%
        #intersect with all prior polygons and see if time in dat.prior is most recent
        #if there is an earlier time, the fire must have reached that point earlier than dat.prio
        sf::st_buffer(1)

      dat.prior.intersect <- dat.prior.all %>%
        dplyr::select(time) %>%
        sf::st_filter(dat.lines.start,.predictate=sf::st_intersects) %>%
        sf::st_drop_geometry()

      min_time_any <- min(dat.prior.intersect$time)
      min_time_any_diff <- as.numeric(difftime(dat.i$time,min_time_any,tz="utc",units="mins"))

      #get line distance
      dat.lines$line_metres <- as.numeric(sf::st_length(dat.lines))


      #if line metres is zero, skip to next
      if(dat.lines$line_metres==0){
        #print("skipping")
        next
      }


              #get line distance inside poly2
      dat.l.i.intersect <- sf::st_geometry(dat.lines) %>% sf::st_intersection(sf::st_geometry(dat.i))
      if(length(dat.l.i.intersect)==0){
        dat.lines$line_metres_internal <- 0
      }else{
             dat.lines$line_metres_internal <- as.numeric(sf::st_length(dat.l.i.intersect))
      }
      dat.lines$percent_internal <- round(dat.lines$line_metres_internal/dat.lines$line_metres*100,1)



      #get id of polygons that is where the line starts
      polyid <- unlist(unique(sf::st_is_within_distance(dat.lines,dat.prior,10)))
      start_rowids <- paste0(dat.prior$rowid[polyid],collapse=";")


      dat.lines <- dat.lines %>%
        #select final attributes
        dplyr::mutate(start_time=unique(dat.prior$time),
                      end_time=unique(dat.i$time),
                      start_pid=start_rowids,
                      end_pid=unique(dat.i$rowid),
                      timestep_mins=as.numeric(difftime(end_time,start_time,units="mins")),
                      line_km=(line_metres/1000),
                      percent_internal,
                      ros_kmh=line_km/(timestep_mins/60),
                      min_time_any=min_time_any,
                      min_time_any_diff=min_time_any_diff) %>%
        dplyr::select(start_time,end_time,start_pid,end_pid,line_km,percent_internal,timestep_mins,ros_kmh,min_time_any,min_time_any_diff)

      ##add line start and end coordinates and directions
      # Initialize results list
      line_results <- lapply(1:nrow(dat.lines), function(i) {
        # Extract the geometry of the current line
        line <- sf::st_geometry(dat.lines)[i] %>%
          sf::st_transform(4283)

        # Extract coordinates, reverse the order
        coords <- sf::st_coordinates(line)
        coords <-coords[nrow(coords):1, ]

        # Get start and end points
        start_point <- coords[1, ]
        end_point <- coords[nrow(coords), ]

        # Calculate direction (bearing in degrees)

        bearing_deg <- stplanr::line_bearing(line %>% sf::st_as_sf(),bidirectional = F)
        bearing_deg <- ifelse(bearing_deg < 180,bearing_deg+180,bearing_deg-180)

        # Return as a named vector
        return(data.frame(
          start_x_gda94 = start_point[1], start_y_gda94 = start_point[2],
          end_x_gda94 = end_point[1], end_y_gda94 = end_point[2],
          bearing = bearing_deg
        ))
      })

      dat.lines <- cbind(dat.lines,line_results) %>%
        sf::st_as_sf()


      #flag lines from results if it crosses polygons with prior times (can happen when fires merge), ie all prior times from same season (dat.prior.all)
      #remove those polygons where time diff is shorter than minimum first
      dat.prior.all.2 <- dat.prior.all %>% dplyr::filter(mins_diff >= min_minutes)
       dat.lines.intersect <- sf::st_intersection(sf::st_geometry(dat.lines),dat.prior.all.2) %>%
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

