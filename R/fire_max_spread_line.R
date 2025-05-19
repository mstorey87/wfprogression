#' Find line of maximum fire spread
#'
#' @description
#' Takes a set of sf polygons with, each with a posixct datetime field, and find the line of maximum difference or spread from one polygon to the next.
#' The second polygon should completely contain the first, but this may work if that is not the case.
#' There are some option for calculating this by converting the polygons to convex hulls, which makes the process fast but is less accurate.
#' All spread lines can be returned, not just the single maximum spread line.
#' There is an option to ensure that the spread line is completely contained within the second fire progression polygon, but this works very slowly and can not always find an appropriate solution
#' min_minutes and max_minutes are used to set that max and min timestep between polygons. When there are two polygons with a difference in time outside the range, the process is not run.
#' The vertices on the boundary of the polygons are converted to points to find the max spread line. densify_m how often extra vertices should be added to the polygon boundary, especially on staight boundary sections, before the polygon is converted to points.
#' Have a smaller value (metres) will produce more accurate results, but will take more time to run.
#'
#' @param time_col Name of date time column (posix)
#' @param id_col Name of unique id column to add to output lines
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
                                 convex_hull=T,
                                 max_only=T,
                                 internal_only=F,
                                 min_minutes=20,
                                 max_minutes=300,
                                 densify_m=100){


  #ensure crs is projected
  polygons_crs <- sf::st_crs(polygons)$wkt
  checkmate::assert(stringr::str_detect(polygons_crs,"PROJCRS"),"Error: To speed up function, polygons must be projected crs")

  #add time column with standard name
  polygons$time <- polygons[[time_col]]
  polygons$rowid <- polygons[[id_col]]

  #ensure time column is posxct
  checkmate::assert(unique(stringr::str_detect(class(polygons$time),"POSIX")),"Error: Time column must be posixct")


  #create new object
  dat.poly <- polygons


  #add a season variable if needed
  dat.poly <- dat.poly %>%
    dplyr::mutate(semes=lubridate::semester(time),yr=lubridate::year(time),season=ifelse(semes==1,paste0(yr-1,"-",yr),paste0(yr,"-",yr+1))) %>%
    dplyr::select(dplyr::all_of(c("time","rowid","firetype","season"))) %>%
    #arrange in order of time and give a time id
    dplyr::arrange(time)

  #give the geomtery a name
  sf::st_geometry(dat.poly) <- "geom"

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

    #only polygons near the current poly are needed for main function
    #maintain the polygons of dat.prior.all for a later intersect
    dat.prior.filtered <- dat.prior.all %>%
      dplyr::filter(mins_diff >= min_minutes & mins_diff <= max_minutes) %>%
      sf::st_filter(dat.i,.predicate = sf::st_is_within_distance,dist=units::set_units(1,"m") )

    #test if any polygons near within timeframe. If not go to next iteration
    if(nrow(dat.prior.filtered)==0|!is.data.frame(dat.prior.filtered)) next

    #get only most recent prior polygon
    dat.prior <- dat.prior.filtered %>%
      dplyr::filter(time==max(time))%>%
      dplyr::summarise(time=unique(time),rowid=paste0(unique(rowid),collapse=";"))

    #get convex hull of polygon to save processing time
    if(convex_hull==T){
      dat.i<- dat.i %>% sf::st_convex_hull()
      dat.prior <- dat.prior %>% sf::st_convex_hull()
    }

    # dat.i.diff <- sf::st_difference(dat.i,dat.prior)
    # dat.prior.diff <- sf::st_difference(dat.prior.filtered) %>%
    #   dplyr::filter(time==max(time)) %>%
    #   sf::st_union()

    dat.lines <- wfprogression::fire_RANN_nearest_points(dat.i,dat.prior,densify_m=densify_m,max_only = max_only,within_only = internal_only)

    #combine attributes
    dat.lines <- cbind(dat.lines,sf::st_drop_geometry(dat.i))

    #check for cases where the prior polygon is identical to another prior polygon in the section near the spread line end
    #this can happen when another section of the polygon has changed and been given a new time, so the section near the spread line does,
    #not change but is still given the new time.
    dat.lines.start <- sf::st_cast(sf::st_geometry(dat.lines),"POINT")[2,] %>%
      sf::st_as_sf() %>%
      sf::st_transform(3112) %>%
      sf::st_buffer(1) %>%
      sf::st_transform(sf::st_crs(dat.lines))

    #intersect with all prior polygons and see if time in dat.prior is most recent
    #if there is an earlier time, the fire must have reached that point earlier than dat.prior
    dat.prior.intersect <- sf::st_intersection(dat.lines.start,dat.prior.all)

    min_time_any <- min(dat.prior.intersect$time)
    min_time_any_diff <- as.numeric(difftime(dat.i$time,min_time_any,tz="utc",units="mins"))

    #get line distance
    dat.lines$line_metres <- as.numeric(sf::st_length(dat.lines))

    #if line metres is zero, skip to next
    if(dat.lines$line_metres==0) next

    #get line distance inside poly2
    dat.l.i.intersect <- sf::st_geometry(dat.lines) %>% sf::st_intersection(sf::st_geometry(dat.i))

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
      dplyr::select(start_time,end_time,start_pid,end_pid,line_km,percent_internal,
                    timestep_mins,ros_kmh,min_time_any,min_time_any_diff)

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


    dat.lines <- dat.lines %>% dplyr::mutate(prior_intersects=nrow(dat.lines.intersect)>0)

    res[[i]] <- dat.lines
  }

  res.all <- do.call(rbind,res)


  return(res.all)
}

