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
  checkmate::assert(stringr::str_detect(polygons_crs,"GEOGCRS"),"Error: polygons must be projected crs")

  #add time column with standard name
  polygons$time <- polygons[[time_col]]

  #ensure time column is posxct
  checkmate::assert(unique(stringr::str_detect(class(polygons$time),"POSIX")),"Error: Time column must be posixct")


  dat.poly <- polygons


  #add a season variable if needed
  dat.poly <- dat.poly %>%
    dplyr::mutate(semes=lubridate::semester(time),yr=lubridate::year(time),season=ifelse(semes==1,paste0(yr-1,"-",yr),paste0(yr,"-",yr+1))) %>%
    dplyr::select(dplyr::all_of(c("time",id_col,"firetype","season"))) %>%
    #arrange in order of time and give a time id
    dplyr::arrange(time)

   sf::st_geometry(dat.poly) <- "geom"

  #keep only main fire polygon, or spot or backburn depending on user input
  if(include_spots==F & include_backburns == F)    dat.poly <- dat.poly %>% filter(firetype == "main")
  if(include_spots==F & include_backburns == T)    dat.poly <- dplyr::filter(firetype=="main"|firetype=="backburn")
  if(include_spots==T & include_backburns == T)    dat.poly <- dplyr::filter(firetype=="main"|firetype=="backburn"|firetype=="spot")
  if(include_spots==T & include_backburns == F)    dat.poly <- dplyr::filter(firetype=="main"|firetype=="spot")


  res <- list()
  for(i in 1:nrow(dat.poly)){
    print(i)

    #get current loop polygon and time
    dat.i <- dat.poly[i,]
    time.i <- dat.i$time

    #get all prior polygons from the same season
    dat.prior.all <- dat.poly %>%
      dplyr::filter(time < time.i,season==dat.i$season) %>%
      #calculate time difference between current time and all prior poly times
      dplyr::mutate(mins_diff=as.numeric(difftime(time.i,time,"utc","mins")))

    #test is any prior polygons are within user defined time frame. If not skip to next
    any_diff_within <- any(dat.prior.all$mins_diff >= min_minutes & dat.prior.all$mins_diff <= max_minutes)
    if(!any_diff_within) next


    #sf::sf_use_s2(TRUE)
    dat.prior.near <- dat.prior.all %>%
      #only polygons near the current poly are needed
      sf::st_filter(dat.i,.predicate = sf::st_is_within_distance,dist=units::set_units(1,"m"))

    #test again for polygon near current poly is any are within time frame
    #this will also return FALSE if dat.prior.near is empty
    any_diff_within <- any(dat.prior.near$mins_diff >= min_minutes & dat.prior.near$mins_diff <= max_minutes)
    if(!any_diff_within) next


    suppressMessages({


      #create a non-overlapping polygon, where each polygon is the first time of any polygon intersecting
      #ie first time fire reached that spot
      #st_difference seems to only work better in a geographic crs

      dat.diff <- dat.prior.near%>%
        dplyr::select(rowid, time) %>%
        #arrange by time first, so first time is kept
        rbind(dat.i %>% dplyr::select(rowid,time)) %>%
        dplyr::arrange(time) %>%
        sf::st_difference() %>%
        sf::st_transform(sf::st_crs(dat.i)) %>%
        sf::st_make_valid() %>%
        sf::st_cast("MULTIPOLYGON") %>%
        sf::st_cast("POLYGON")%>%
        dplyr::filter(!sf::st_is_empty(geom))%>%
        sf::st_make_valid()

      #filter out any tiny polygons
      dat.diff <- dat.diff[sf::st_area(dat.diff) > units::set_units(1, "m^2"), ]

    })


    #now
    dat.i.diff <- dat.diff %>%
      dplyr::filter(time==time.i)



    #see if anything exists in dat.i. Skip if empty
    #if empty, it means that a prior polygon completely overlaps with the current polygon (no difference)
    if(nrow(dat.i.diff)==0) next


    #get all prior polygons from the st_difference result
    #get only polygons that fall within timeframe
    dat.prior.filtered <- dat.diff %>%
      dplyr::filter(time < time.i) %>%
      #calculate time difference
      dplyr::mutate(mins_diff=as.numeric(difftime(time.i,time,"utc","mins"))) %>%
      dplyr::filter(mins_diff >= min_minutes & mins_diff <= max_minutes)

    #get only prior polygons and current polygons that are near each other
    #use within distance predicate
    suppressWarnings({
      dat.prior.filtered <- dat.prior.filtered  %>%
        sf::st_filter(dat.i.diff,.predicate = sf::st_is_within_distance,dist=units::set_units(1,"m"))
    })

    #skip if no prior intersecting polygon
    if(nrow(dat.prior.filtered)==0) next

    suppressWarnings({
      dat.i.diff <- dat.i.diff %>%
        sf::st_filter(dat.prior.filtered,.predicate = sf::st_is_within_distance,dist=units::set_units(1,"m")) %>%
        dplyr::summarise(time=unique(time),rowid=unique(rowid))
    })

    #skip if no current polygon left
    if(nrow(dat.i.diff)==0) next




    #get only most recent prior polygon
    dat.prior <- dat.prior.filtered %>%
      dplyr::filter(time==max(time))%>%
      sf::st_filter(dat.i.diff,.predicate = sf::st_is_within_distance,dist=units::set_units(1,"m"))

    #ensure the prior polygon is completely contained by the first

    #sf::st_geometry(dat.i) <- sf::st_union(sf::st_geometry(dat.i),sf::st_union(sf::st_geometry(dat.prior)))

    #get convex hull of polygon to save processing time
    #run this in the filtered data, as we need to maintain the polygons of dat.prior.all for a later intersect
    if(convex_hull==T){
      dat.i.diff <- dat.i.diff %>% sf::st_convex_hull()
      dat.prior <- dat.prior %>% sf::st_convex_hull()
    }




    dat.lines <- wfprogression::fire_RANN_nearest_points(dat.i.diff,dat.prior,densify_m=densify_m,max_only = max_only,within_only = internal_only)

    #combine attributes
    dat.lines <- cbind(dat.lines,sf::st_drop_geometry(dat.i.diff))


    #check for cases where the prior polygon is identical to another prior polygon in the section near the spread line end
    #this can happen when another section of the polygon has changed and been given a new time, so the section near the spread line does,
    #not change but is still given the new time.
    dat.lines.start <- sf::st_cast(sf::st_geometry(dat.lines),"POINT")[2,] %>%
      sf::st_as_sf() %>%
      sf::st_transform(3112) %>%
      #dplyr::mutate(id=dplyr::row_number()) %>%
      #intersect with all prior polygons and see if time in dat.prior is most recent
      #if there is an earlier time, the fire must have reached that point earlier than dat.prio
      sf::st_buffer(1) %>%
      sf::st_transform(sf::st_crs(dat.lines))

    dat.prior.intersect <- sf::st_intersection(dat.lines.start,dat.prior.all)

    min_time_any <- min(dat.prior.intersect$time)
    min_time_any_diff <- as.numeric(difftime(dat.i$time,min_time_any,tz="utc",units="mins"))

    #get line distance
    dat.lines$line_metres <- as.numeric(sf::st_length(dat.lines))


    #if line metres is zero, skip to next
    if(dat.lines$line_metres==0) next

    #get line distance inside poly2
    dat.l.i.intersect <- sf::st_geometry(dat.lines) %>% sf::st_intersection(sf::st_geometry(dat.i.diff))

    if(length(dat.l.i.intersect)==0){
      dat.lines$line_metres_internal <- 0
    }else{
      dat.lines$line_metres_internal <- as.numeric(sf::st_length(dat.l.i.intersect))
    }
    dat.lines$percent_internal <- round(dat.lines$line_metres_internal/dat.lines$line_metres*100,1)



    #get id of polygons that is where the line starts
    polyid <- unlist(unique(sf::st_is_within_distance(dat.lines,dat.prior,dist=units::set_units(1,"m"))))
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


    dat.lines <- dat.lines %>% mutate(prior_intersects=nrow(dat.lines.intersect)>0)

    res[[i]] <- dat.lines
  }









  res.all <- do.call(rbind,res)


  return(res.all)
}

