#' Find maximum fire spread line
#'
#' @param polygon SF polygons. Polygon at time two should completely cover time 1 polygon. Time 3 completely cover time 2 etc.
#' @param time_col Name of date time column (posix)
#' @param include_spots T/F to include spot fire in spread line creation. Data must have firetype column populated with "spot", "main" or "backburn"
#' @param include_backburns T/F to include backburns in spread line creation. Data must have firetype column populated with "spot", "main" or "backburn"
#'
#' @return sf object
#' @export
#'
#' @examples
#' #
fire_max_spread_line <- function(polygons,time_col,include_spots=F,include_backburns=F,convex_hull=T,max_only=T,internal_only=F){

  polygons$time <- polygons[[time_col]]


   dat.cnvx <- polygons %>%
    #convert crs to projected so we can use meters
    st_transform(3112)


    #keep only main fire polygon, or spot or backburn depending on user input
    if(include_spots==F & include_backburns == F)    dat.cnvx <- dat.cnvx %>% filter(firetype == "main")
    if(include_spots==F & include_backburns == T)    dat.cnvx <- dplyr::filter(firetype=="main"|firetype=="backburn")
    if(include_spots==T & include_backburns == T)    dat.cnvx <- dplyr::filter(firetype=="main"|firetype=="backburn"|firetype=="spot")
    if(include_spots==T & include_backburns == F)    dat.cnvx <- dplyr::filter(firetype=="main"|firetype=="spot")


    #ensure all main polygon are in one multipolygon
   dat.cnvx <- dat.cnvx %>%
     dplyr::group_by(time) %>%
     dplyr::summarise()%>%
    #arrange in order of time and give a time id
     dplyr::arrange(time) %>%

     dplyr::mutate(timeid=dplyr::row_number())

    #get convex hull of polygon to save processing time
    if(convex_hull==T) dat.cnvx <- dat.cnvx %>% sf::st_convex_hull()
    #ensure there are points at least every 100 m
   dat.cnvx <- dat.cnvx %>% smoothr::densify(max_distance = 100)

   #split by time to create separate objects
  dat.cnvx.split <- split(dat.cnvx,as.factor(dat.cnvx$timeid))

  #measure distance from each point on polygon time 2 to polygon time 1
  res <- list()
  for(i in 1:length(dat.cnvx.split)){

    #skip first spread time
    if(i >1){
      #measure lines from current time to time prior. Summarise time prior to single feature to simplify processing
      #convert second polygon time to points first
      dat.points <- sf::st_cast(dat.cnvx.split[[i]],"POINT")
      dat.lines <- sf::st_nearest_points( dat.points, dat.cnvx.split[[i-1]]) %>%
        sf::st_as_sf()
      #combine with original points
      dat.lines <- cbind(dat.lines,sf::st_drop_geometry(dat.cnvx.split[[i]]))

      #get line distance
      dat.lines$line_metres <- as.numeric(sf::st_length(dat.lines))

      #arrange by distance
      dat.lines <- dat.lines %>%
        dplyr::arrange(dplyr::desc(line_metres))

      #remove any line that go any distance outside the second time polygon based on user input
      if(internal_only==T){

        dat.lines$internal <- apply(st_within(dat.lines,dat.cnvx.split[[i]],sparse = F),MARGIN = 1,max)
        #give zero m lines a internal flag
        dat.lines <- dat.lines %>%
          dplyr::mutate(internal=ifelse(line_metres==0|internal==1,1,0)) %>%
          dplyr::mutate(internal=as.logical(internal)) %>%
          dplyr::filter(internal)


      }

      res[[i]] <- dat.lines %>%
        dplyr::mutate(start_time=unique(dat.cnvx.split[[i-1]]$time),
               end_time=unique(dat.cnvx.split[[i]]$time),
               timestep_hours=as.numeric(difftime(end_time,start_time,units="hours")),
               line_km=(line_metres/1000),
               ros_kmh=line_km/timestep_hours) %>%
        dplyr::select(start_time,end_time,line_km,timestep_hours,ros_kmh)

    }


  }

  res.all <- do.call(rbind,res)



  if(max_only==TRUE){

    res.all <- res.all %>% dplyr::group_by(end_time) %>% dplyr::filter(line_km==max(line_km)) %>% dplyr::slice_sample(n=1) %>% dplyr::ungroup()
  }

  return(res.all)
}

