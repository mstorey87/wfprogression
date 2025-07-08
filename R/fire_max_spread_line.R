#' Find lines of maximum fire spread between consecutive fire polygons
#'
#' @description
#' This function analyzes a series of fire progression polygons (sf objects) each timestamped with a POSIXct datetime field.
#' It identifies lines representing the maximum spatial spread of the fire between consecutive polygons in time.
#' The second polygon should ideally fully contain the first, but the function may still work if this is not exactly true.
#'
#' Key features:
#' - Optionally converts polygons to their convex hulls to speed up calculations (less accurate).
#' - Can return all spread lines or only the maximum spread line between polygon pairs.
#' - Optionally filters to only lines fully contained within the later polygon (slower and may not always find a solution - NOTE this option needs correcting, so is not included yet).
#' - Uses minimum and maximum time interval constraints (in minutes) between polygon pairs to control comparisons.
#' - Polygon boundaries are densified by adding extra vertices (every `densify_m` meters) to improve spatial accuracy of spread line detection.
#'
#' @param polygons An sf object of fire polygons with geometry and attribute data.
#' @param time_col Character; name of the POSIXct datetime column indicating the progression time.
#' @param id_col Character; name of a unique identifier column for the polygons, which will be retained in the output.
#' @param convex_hull Logical; if TRUE, polygons are converted to convex hulls before processing (faster, less precise).
#' @param max_only Logical; if TRUE, returns only the single maximum spread line per polygon pair.
# #' @param internal_only Logical; if TRUE, returns only spread lines fully contained within the later polygon.
#' @param min_minutes Numeric; minimum allowable time difference (minutes) between polygon pairs for analysis.
#' @param max_minutes Numeric; maximum allowable time difference (minutes) between polygon pairs for analysis.
#' @param densify_m Numeric; distance in meters at which to add extra vertices along polygon edges to increase spatial resolution.
#'
#' @return An sf object of line geometries representing fire spread lines, with attributes including start/end times, spread distance, rate of spread (km/h), and line bearing.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(lubridate)
#'
#' # CRS: GDA94 / MGA zone 56 (projected meters)
#' crs_proj <- 3112
#'
#' # Center point roughly near Canberra (in MGA Zone 56)
#' center <- c(149.1, -35.8)  # lon, lat
#'
#' # Function to create a simple ellipse polygon with fewer points (20)
#' create_simple_ellipse <- function(center_xy, a, b, n = 20, angle = 0) {
#'   t <- seq(0, 2*pi, length.out = n)
#'   x <- a * cos(t)
#'   y <- b * sin(t)
#'
#'   # rotation matrix
#'   theta <- angle * pi / 180
#'   rot_mat <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)), ncol = 2)
#'   coords <- cbind(x, y) %*% rot_mat
#'
#'   # translate to center
#'   coords[,1] <- coords[,1] + center_xy[1]
#'   coords[,2] <- coords[,2] + center_xy[2]
#'
#'   # close polygon by repeating first point
#'   coords <- rbind(coords, coords[1,])
#'   st_polygon(list(coords))
#' }
#'
#' # Project center to projected CRS coords
#' center_proj <- sf::st_transform(sf::st_sfc(sf::st_point(center), crs = 4326), crs_proj)
#' center_proj_xy <- sf::st_coordinates(center_proj)
#'
#' # Larger ellipse ~4000m x 2000m but with 20 points
#' poly2 <- create_simple_ellipse(center_proj_xy, a = 2000, b = 1000, angle = 30)
#'
#' # Smaller ellipse inside ~1000m x 600m with 20 points
#' poly1 <- create_simple_ellipse(center_proj_xy-900, a = 500, b = 300, angle = 30)
#'
#' # Create sf object with two polygons
#' fire_polygons <- sf::st_sf(
#'   fire_id = c("fire_1", "fire_1"),
#'   date = as.POSIXct(c("2023-01-01 12:00:00", "2023-01-01 14:00:00"), tz = "Australia/Sydney"),
#'   geometry = sf::st_sfc(poly1, poly2, crs = crs_proj)
#' )
#'
#'
#' spread_lines <- fire_max_spread_line(
#'   polygons = fire_polygons,
#'   time_col = "date",
#'   id_col = "fire_id",
#'   convex_hull = TRUE,
#'   max_only = TRUE,
#'   internal_only = FALSE,
#'   min_minutes = 30,
#'   max_minutes = 360,
#'   densify_m = 50
#' )
#'
#' # Print and plot
#' print(fire_polygons)
#' mapview::mapview(fire_polygons)+spread_lines
#' }
fire_max_spread_line <- function(polygons,
                                 time_col,
                                 id_col,
                                 convex_hull = TRUE,
                                 max_only = TRUE,
                                 #internal_only = FALSE,
                                 min_minutes = 20,
                                 max_minutes = 300,
                                 densify_m = 100) {

  #set this to false, i need to fix this part of the code later
  internal_only = FALSE

  # Ensure coordinate reference system (CRS) is projected for accurate distance measurements
  polygons_crs <- sf::st_crs(polygons)$wkt
  checkmate::assert(
    stringr::str_detect(polygons_crs, "PROJCRS"),
    "Error: To speed up function, polygons must use a projected CRS"
  )

  # Copy relevant columns with standard names for easier handling
  polygons$time <- polygons[[time_col]]
  polygons$rowid <- polygons[[id_col]]

  # Check that the time column is of POSIXct class
  checkmate::assert(
    all(stringr::str_detect(class(polygons$time), "POSIX")),
    "Error: Time column must be POSIXct"
  )

  dat.poly <- polygons

   # Add 'season' variable to group polygons by fire season (assumed to span half-years)
  dat.poly <- dat.poly %>%
    dplyr::mutate(
      semes = lubridate::semester(time),
      yr = lubridate::year(time),
      season = ifelse(semes == 1, paste0(yr - 1, "-", yr), paste0(yr, "-", yr + 1))
    ) %>%
    dplyr::select(dplyr::all_of(c("time", "rowid", "season"))) %>%
    dplyr::arrange(time)  # Sort by time ascending

  sf::st_geometry(dat.poly) <- "geom"  # Explicitly set geometry column name

  res <- list()

  # Iterate through each polygon to find max spread lines relative to previous polygons
  for (i in 1:nrow(dat.poly)) {
    print(paste("Processing polygon", i, "of", nrow(dat.poly)))

    dat.i <- dat.poly[i, ]
    time.i <- dat.i$time

    # Find prior polygons from the same season with time less than current polygon's time
    dat.prior.all <- dat.poly %>%
      dplyr::filter(time < time.i, season == dat.i$season) %>%
      dplyr::mutate(mins_diff = as.numeric(difftime(time.i, time, units = "mins")))

    # Filter prior polygons within time range and spatially near current polygon
    dat.prior.filtered <- dat.prior.all %>%
      dplyr::filter(mins_diff >= min_minutes & mins_diff <= max_minutes) %>%
      sf::st_filter(dat.i, .predicate = sf::st_is_within_distance, dist = units::set_units(1, "m"))

    # Skip if no suitable prior polygons found
    if (nrow(dat.prior.filtered) == 0 || !is.data.frame(dat.prior.filtered)) next

    # Select the most recent prior polygon
    dat.prior <- dat.prior.filtered %>%
      dplyr::filter(time == max(time)) %>%
      dplyr::summarise(time = unique(time), rowid = paste0(unique(rowid), collapse = ";"))

    # Optionally convert polygons to convex hulls to speed processing
    if (convex_hull) {
      dat.i <- dat.i %>% sf::st_convex_hull()
      dat.prior <- dat.prior %>% sf::st_convex_hull()
    }

    # Calculate spread lines between current and prior polygon boundaries
    dat.lines <- wfprogression::fire_RANN_nearest_points(
      dat.i,
      dat.prior,
      densify_m = densify_m,
      max_only = max_only,
      within_only = internal_only
    )

    #reverse lines to it reflects where spread started
    dat.lines <- sf::st_reverse(dat.lines)

    # Attach polygon attributes from current polygon to lines
    dat.lines <- cbind(dat.lines, sf::st_drop_geometry(dat.i))

    # Handle potential issues where prior polygon boundary points overlap other prior polygons (fire merging)
    dat.lines.start <- sf::st_cast(sf::st_geometry(dat.lines), "POINT")[1, ] %>%
      sf::st_as_sf() %>%
      sf::st_transform(3112) %>%
      sf::st_buffer(1) %>%
      sf::st_transform(sf::st_crs(dat.lines))

    # Find intersections with all prior polygons
    dat.prior.intersect <- suppressWarnings(sf::st_intersection(dat.lines.start, dat.prior.all))
    min_time_any <- min(dat.prior.intersect$time)
    min_time_any_diff <- as.numeric(difftime(dat.i$time, min_time_any, tz = "utc", units = "mins"))

    # Calculate total length of the spread line in meters
    dat.lines$line_metres <- as.numeric(sf::st_length(dat.lines))
    if (dat.lines$line_metres == 0) next

    # Calculate length of spread line inside the current polygon
    dat.l.i.intersect <- sf::st_geometry(dat.lines) %>% sf::st_intersection(sf::st_geometry(dat.i))
    dat.lines$line_metres_internal <- if (length(dat.l.i.intersect) == 0) 0 else as.numeric(sf::st_length(dat.l.i.intersect))

    # Percent of line length inside current polygon
    dat.lines$percent_internal <- round(dat.lines$line_metres_internal / dat.lines$line_metres * 100, 1)

    # Find IDs of polygons where the spread line starts
    polyid <- unlist(unique(sf::st_is_within_distance(dat.lines, dat.prior, dist = units::set_units(1, "m"))))
    start_rowids <- paste0(dat.prior$rowid[polyid], collapse = ";")

    # Add metadata about spread and rate of spread
    dat.lines <- dat.lines %>%
      dplyr::mutate(
        start_time = unique(dat.prior$time),
        end_time = unique(dat.i$time),
        start_pid = start_rowids,
        end_pid = unique(dat.i$rowid),
        timestep_mins = as.numeric(difftime(end_time, start_time, units = "mins")),
        line_km = line_metres / 1000,
        percent_internal = percent_internal,
        ros_kmh = line_km / (timestep_mins / 60),
        min_time_any = min_time_any,
        min_time_any_diff = min_time_any_diff
      ) %>%
      dplyr::select(
        start_time, end_time, start_pid, end_pid, line_km, percent_internal,
        timestep_mins, ros_kmh, min_time_any, min_time_any_diff
      )

    # Calculate start/end coordinates and bearing of each spread line
    line_results <- lapply(1:nrow(dat.lines), function(i) {
      line <- sf::st_geometry(dat.lines)[i] %>% sf::st_transform(4283)
       coords <- sf::st_coordinates(line)
      # coords <- coords[nrow(coords):1, ]  # reverse order to get correct direction

      start_point <- coords[1, ]
      end_point <- coords[nrow(coords), ]

      bearing_deg <- stplanr::line_bearing(line %>% sf::st_as_sf(), bidirectional = FALSE)
      # Normalize bearing to opposite direction (180 degrees shifted)
      bearing_deg <- ifelse(bearing_deg < 180, bearing_deg + 180, bearing_deg - 180)

      data.frame(
        start_x_gda94 = start_point[1], start_y_gda94 = start_point[2],
        end_x_gda94 = end_point[1], end_y_gda94 = end_point[2],
        bearing = bearing_deg
      )
    })

    dat.lines <- cbind(dat.lines, do.call(rbind, line_results)) %>% sf::st_as_sf()

    # Flag lines crossing polygons with prior times (may indicate fire merging with previous fires)
    dat.prior.all.2 <- dat.prior.all %>% dplyr::filter(mins_diff >= min_minutes)
    dat.lines.intersect <- sf::st_intersection(sf::st_geometry(dat.lines), dat.prior.all.2) %>%
      sf::st_as_sf() %>%
      dplyr::mutate(len = as.numeric(sf::st_length(.))) %>%
      dplyr::filter(len > 0.01)

    dat.lines <- dat.lines %>% dplyr::mutate(prior_intersects = nrow(dat.lines.intersect) > 0)

    res[[i]] <- dat.lines
  }

  # Combine results from all iterations
  res.all <- do.call(rbind, res)

  return(res.all)
}


#' #' Find line of maximum fire spread
#' #'
#' #' @description
#' #' Takes a set of sf polygons with, each with a posixct datetime field, and find the line of maximum difference or spread from one polygon to the next.
#' #' The second polygon should completely contain the first, but this may work if that is not the case.
#' #' There are some option for calculating this by converting the polygons to convex hulls, which makes the process fast but is less accurate.
#' #' All spread lines can be returned, not just the single maximum spread line.
#' #' There is an option to ensure that the spread line is completely contained within the second fire progression polygon, but this works very slowly and can not always find an appropriate solution
#' #' min_minutes and max_minutes are used to set that max and min timestep between polygons. When there are two polygons with a difference in time outside the range, the process is not run.
#' #' The vertices on the boundary of the polygons are converted to points to find the max spread line. densify_m how often extra vertices should be added to the polygon boundary, especially on staight boundary sections, before the polygon is converted to points.
#' #' Have a smaller value (metres) will produce more accurate results, but will take more time to run.
#' #'
#' #' @param time_col Name of date time column (posix)
#' #' @param id_col Name of unique id column to add to output lines
#' #' @param polygons Fire polygons sf object
#' #' @param convex_hull T/F whether to convert fire polygon to convex hull first
#' #' @param max_only T/F return only the max spread line
#' #' @param internal_only T/F return only lines completely within the fire polygon at time 2
#' #' @param min_minutes minimum time interval between progressions
#' #' @param max_minutes maximum time interval between progression
#' #' @param densify_m metres between vertices on polygon edge from which to measure spread
#' #'
#' #' @return sf object
#' #' @export
#' #'
#' #' @examples
#' #' #
#' fire_max_spread_line <- function(polygons,
#'                                  time_col,
#'                                  id_col,
#'                                  convex_hull=T,
#'                                  max_only=T,
#'                                  internal_only=F,
#'                                  min_minutes=20,
#'                                  max_minutes=300,
#'                                  densify_m=100){
#'
#'
#'   #ensure crs is projected
#'   polygons_crs <- sf::st_crs(polygons)$wkt
#'   checkmate::assert(stringr::str_detect(polygons_crs,"PROJCRS"),"Error: To speed up function, polygons must be projected crs")
#'
#'   #add time column with standard name
#'   polygons$time <- polygons[[time_col]]
#'   polygons$rowid <- polygons[[id_col]]
#'
#'   #ensure time column is posxct
#'   checkmate::assert(unique(stringr::str_detect(class(polygons$time),"POSIX")),"Error: Time column must be posixct")
#'
#'
#'   #create new object
#'   dat.poly <- polygons
#'
#'
#'   #add a season variable if needed
#'   dat.poly <- dat.poly %>%
#'     dplyr::mutate(semes=lubridate::semester(time),yr=lubridate::year(time),season=ifelse(semes==1,paste0(yr-1,"-",yr),paste0(yr,"-",yr+1))) %>%
#'     dplyr::select(dplyr::all_of(c("time","rowid","firetype","season"))) %>%
#'     #arrange in order of time and give a time id
#'     dplyr::arrange(time)
#'
#'   #give the geomtery a name
#'   sf::st_geometry(dat.poly) <- "geom"
#'
#'   res <- list()
#'   for(i in 1:nrow(dat.poly)){
#'     print(i)
#'
#'     #get current loop polygon and time
#'     dat.i <- dat.poly[i,]
#'     time.i <- dat.i$time
#'
#'     #get all prior polygons from the same season
#'     dat.prior.all <- dat.poly %>%
#'       dplyr::filter(time < time.i,season==dat.i$season) %>%
#'       #calculate time difference between current time and all prior poly times
#'       dplyr::mutate(mins_diff=as.numeric(difftime(time.i,time,"utc","mins")))
#'
#'     #only polygons near the current poly are needed for main function
#'     #maintain the polygons of dat.prior.all for a later intersect
#'     dat.prior.filtered <- dat.prior.all %>%
#'       dplyr::filter(mins_diff >= min_minutes & mins_diff <= max_minutes) %>%
#'       sf::st_filter(dat.i,.predicate = sf::st_is_within_distance,dist=units::set_units(1,"m") )
#'
#'     #test if any polygons near within timeframe. If not go to next iteration
#'     if(nrow(dat.prior.filtered)==0|!is.data.frame(dat.prior.filtered)) next
#'
#'     #get only most recent prior polygon
#'     dat.prior <- dat.prior.filtered %>%
#'       dplyr::filter(time==max(time))%>%
#'       dplyr::summarise(time=unique(time),rowid=paste0(unique(rowid),collapse=";"))
#'
#'     #get convex hull of polygon to save processing time
#'     if(convex_hull==T){
#'       dat.i<- dat.i %>% sf::st_convex_hull()
#'       dat.prior <- dat.prior %>% sf::st_convex_hull()
#'     }
#'
#'     # dat.i.diff <- sf::st_difference(dat.i,dat.prior)
#'     # dat.prior.diff <- sf::st_difference(dat.prior.filtered) %>%
#'     #   dplyr::filter(time==max(time)) %>%
#'     #   sf::st_union()
#'
#'     dat.lines <- wfprogression::fire_RANN_nearest_points(dat.i,dat.prior,densify_m=densify_m,max_only = max_only,within_only = internal_only)
#'
#'     #combine attributes
#'     dat.lines <- cbind(dat.lines,sf::st_drop_geometry(dat.i))
#'
#'     #check for cases where the prior polygon is identical to another prior polygon in the section near the spread line end
#'     #this can happen when another section of the polygon has changed and been given a new time, so the section near the spread line does,
#'     #not change but is still given the new time.
#'     dat.lines.start <- sf::st_cast(sf::st_geometry(dat.lines),"POINT")[2,] %>%
#'       sf::st_as_sf() %>%
#'       sf::st_transform(3112) %>%
#'       sf::st_buffer(1) %>%
#'       sf::st_transform(sf::st_crs(dat.lines))
#'
#'     #intersect with all prior polygons and see if time in dat.prior is most recent
#'     #if there is an earlier time, the fire must have reached that point earlier than dat.prior
#'     dat.prior.intersect <- sf::st_intersection(dat.lines.start,dat.prior.all)
#'
#'     min_time_any <- min(dat.prior.intersect$time)
#'     min_time_any_diff <- as.numeric(difftime(dat.i$time,min_time_any,tz="utc",units="mins"))
#'
#'     #get line distance
#'     dat.lines$line_metres <- as.numeric(sf::st_length(dat.lines))
#'
#'     #if line metres is zero, skip to next
#'     if(dat.lines$line_metres==0) next
#'
#'     #get line distance inside poly2
#'     dat.l.i.intersect <- sf::st_geometry(dat.lines) %>% sf::st_intersection(sf::st_geometry(dat.i))
#'
#'     if(length(dat.l.i.intersect)==0){
#'       dat.lines$line_metres_internal <- 0
#'     }else{
#'       dat.lines$line_metres_internal <- as.numeric(sf::st_length(dat.l.i.intersect))
#'     }
#'     dat.lines$percent_internal <- round(dat.lines$line_metres_internal/dat.lines$line_metres*100,1)
#'
#'
#'     #get id of polygons that is where the line starts
#'     polyid <- unlist(unique(sf::st_is_within_distance(dat.lines,dat.prior,dist=units::set_units(1,"m"))))
#'     start_rowids <- paste0(dat.prior$rowid[polyid],collapse=";")
#'
#'     dat.lines <- dat.lines %>%
#'       #select final attributes
#'       dplyr::mutate(start_time=unique(dat.prior$time),
#'                     end_time=unique(dat.i$time),
#'                     start_pid=start_rowids,
#'                     end_pid=unique(dat.i$rowid),
#'                     timestep_mins=as.numeric(difftime(end_time,start_time,units="mins")),
#'                     line_km=(line_metres/1000),
#'                     percent_internal,
#'                     ros_kmh=line_km/(timestep_mins/60),
#'                     min_time_any=min_time_any,
#'                     min_time_any_diff=min_time_any_diff) %>%
#'       dplyr::select(start_time,end_time,start_pid,end_pid,line_km,percent_internal,
#'                     timestep_mins,ros_kmh,min_time_any,min_time_any_diff)
#'
#'     ##add line start and end coordinates and directions
#'     # Initialize results list
#'     line_results <- lapply(1:nrow(dat.lines), function(i) {
#'       # Extract the geometry of the current line
#'       line <- sf::st_geometry(dat.lines)[i] %>%
#'         sf::st_transform(4283)
#'
#'       # Extract coordinates, reverse the order
#'       coords <- sf::st_coordinates(line)
#'       coords <-coords[nrow(coords):1, ]
#'
#'       # Get start and end points
#'       start_point <- coords[1, ]
#'       end_point <- coords[nrow(coords), ]
#'
#'       # Calculate direction (bearing in degrees)
#'
#'       bearing_deg <- stplanr::line_bearing(line %>% sf::st_as_sf(),bidirectional = F)
#'       bearing_deg <- ifelse(bearing_deg < 180,bearing_deg+180,bearing_deg-180)
#'
#'       # Return as a named vector
#'       return(data.frame(
#'         start_x_gda94 = start_point[1], start_y_gda94 = start_point[2],
#'         end_x_gda94 = end_point[1], end_y_gda94 = end_point[2],
#'         bearing = bearing_deg
#'       ))
#'     })
#'
#'     dat.lines <- cbind(dat.lines,line_results) %>%
#'       sf::st_as_sf()
#'
#'
#'     #flag lines from results if it crosses polygons with prior times (can happen when fires merge), ie all prior times from same season (dat.prior.all)
#'     #remove those polygons where time diff is shorter than minimum first
#'     dat.prior.all.2 <- dat.prior.all %>% dplyr::filter(mins_diff >= min_minutes)
#'     dat.lines.intersect <- sf::st_intersection(sf::st_geometry(dat.lines),dat.prior.all.2) %>%
#'       sf::st_as_sf() %>%
#'       dplyr::mutate(len=as.numeric(sf::st_length(.))) %>%
#'       #check if any lines intersect y more than a cm. 0 should result when intersecting the line end with a boundary point, but sometimes there is a small error
#'       dplyr::filter(len>0.01)
#'
#'
#'     dat.lines <- dat.lines %>% dplyr::mutate(prior_intersects=nrow(dat.lines.intersect)>0)
#'
#'     res[[i]] <- dat.lines
#'   }
#'
#'   res.all <- do.call(rbind,res)
#'
#'
#'   return(res.all)
#' }
#'
