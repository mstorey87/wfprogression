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
fire_max_spread_line <- function(polygons,time_col,include_spots=F,include_backburns=F,convex_hull=T,max_only=T,internal_only=F){

  #add time column with standard name
  polygons$time <- polygons[[time_col]]


   dat.cnvx <- polygons %>%
    #convert crs to projected so we can use meters
    st_transform(3112)


    #keep only main fire polygon, or spot or backburn depending on user input
    if(include_spots==F & include_backburns == F)    dat.cnvx <- dat.cnvx %>% filter(firetype == "main")
    if(include_spots==F & include_backburns == T)    dat.cnvx <- dplyr::filter(firetype=="main"|firetype=="backburn")
    if(include_spots==T & include_backburns == T)    dat.cnvx <- dplyr::filter(firetype=="main"|firetype=="backburn"|firetype=="spot")
    if(include_spots==T & include_backburns == F)    dat.cnvx <- dplyr::filter(firetype=="main"|firetype=="spot")




   dat.cnvx <- dat.cnvx %>%

    #arrange in order of time and give a time id
     dplyr::arrange(time)
    #get convex hull of polygon to save processing time
    if(convex_hull==T) dat.cnvx <- dat.cnvx %>% sf::st_convex_hull()

    #ensure there are points at least every 100 m
   dat.cnvx <- dat.cnvx %>% smoothr::densify(max_distance = 100)


   ##merge together touching polygons and give each group a unique id.
   #this is so lines aren't created between different fires that happen to have progressions at the same time.
   dat.union <- dat.cnvx %>%
     sf::st_union() %>%
     sf::st_as_sf() %>%
     sf::st_cast("MULTIPOLYGON") %>%
     sf::st_cast("POLYGON") %>%
     dplyr::mutate(union_id=row_number())

   #use intersection transfer the union id
   dat.cnvx <- dat.cnvx %>%
     sf::st_intersection(dat.union) %>%
     #ensure all main polygon are in one multipolygon
      dplyr::group_by(time,union_id) %>%
      dplyr::summarise()


   #get unique union_id groups and loop through
   unique_unionid <- unique(dat.cnvx$union_id)



   res.0 <- list()
   for(i in 1:length(unique_unionid)){

     #get current union id group
     dat.u <- dat.cnvx %>%
       dplyr::filter(union_id==unique_unionid[i]) %>%
       dplyr::arrange(time)

     #split polygons by time and loop through each
     dat.cnvx.split <-  split(dat.u,as.factor(dat.u$time))

     res <- list()
     for(xi in 1:length(dat.cnvx.split)){



       #skip first spread time because there is no prior polygon to measure ROS
       if(xi >1){
         #measure lines from current time to time prior. Summarise time prior to single feature to simplify processing
         #convert second polygon time to points first
         dat.time.1 <- dat.cnvx.split[[xi-1]]
         dat.time.2 <- dat.cnvx.split[[xi]]


         #convert polygon time2 vertices to points
         dat.points <- sf::st_cast(dat.time.2,"POINT")

         #get nearest line from each time 2 vertice (point) back to the time 1 polygon
         dat.lines <- sf::st_nearest_points( dat.points, dat.time.1) %>%
           sf::st_as_sf()

         #combine attributes
         dat.lines <- cbind(dat.lines,sf::st_drop_geometry(dat.time.2))

         #get line distance
         dat.lines$line_metres <- as.numeric(sf::st_length(dat.lines))

         #arrange by distance
         dat.lines <- dat.lines %>%
           dplyr::arrange(dplyr::desc(line_metres))

         #remove any line that goes any distance outside the second time polygon based on user input
         if(internal_only==T){

           dat.lines$internal <- apply(st_within(dat.lines,dat.time.2,sparse = F),MARGIN = 1,max)
           #give zero m lines a internal flag
           dat.lines <- dat.lines %>%
             dplyr::mutate(internal=ifelse(line_metres==0|internal==1,1,0)) %>%
             dplyr::mutate(internal=as.logical(internal)) %>%
             dplyr::filter(internal)


         }

         res[[xi]] <- dat.lines %>%
           #select final attributes
           dplyr::mutate(start_time=unique(dat.time.1$time),
                         end_time=unique(dat.time.2$time),
                         unionid=unique_unionid[i],
                         timestep_hours=as.numeric(difftime(end_time,start_time,units="hours")),
                         line_km=(line_metres/1000),
                         ros_kmh=line_km/timestep_hours) %>%
           dplyr::select(start_time,end_time,line_km,timestep_hours,ros_kmh,unionid)

       }




     }

     #keep all records from current union_id group. Some will have no results when there is only on time in a group.
     res.0[[i]] <- do.call(rbind,res)

   }

   res.all <- do.call(rbind,res.0)

   if(max_only==TRUE){

     res.all <- res.all %>% dplyr::group_by(end_time,unionid) %>% dplyr::filter(line_km==max(line_km)) %>% dplyr::slice_sample(n=1) %>% dplyr::ungroup()
   }


  #  #split by time to create separate objects
  # dat.cnvx.split <- split(dat.cnvx,as.factor(dat.cnvx$timeid))
  #
  # #measure distance from each point on polygon time 2 to polygon time 1
  # res <- list()
  # for(i in 1:length(dat.cnvx.split)){
  #
  #   #skip first spread time
  #   if(i >1){
  #     #measure lines from current time to time prior. Summarise time prior to single feature to simplify processing
  #     #convert second polygon time to points first
  #     dat.time.1 <- dat.cnvx.split[[i-1]]
  #     dat.time.2 <- dat.cnvx.split[[i]]
  #
  #
  #
  #     dat.points <- sf::st_cast(dat.time.2,"POINT")
  #     dat.lines <- sf::st_nearest_points( dat.points, dat.time.1) %>%
  #       sf::st_as_sf()
  #     #combine with original points
  #     dat.lines <- cbind(dat.lines,sf::st_drop_geometry(dat.time.2))
  #
  #     #get line distance
  #     dat.lines$line_metres <- as.numeric(sf::st_length(dat.lines))
  #
  #     #arrange by distance
  #     dat.lines <- dat.lines %>%
  #       dplyr::arrange(dplyr::desc(line_metres))
  #
  #     #remove any line that go any distance outside the second time polygon based on user input
  #     if(internal_only==T){
  #
  #       dat.lines$internal <- apply(st_within(dat.lines,dat.time.2,sparse = F),MARGIN = 1,max)
  #       #give zero m lines a internal flag
  #       dat.lines <- dat.lines %>%
  #         dplyr::mutate(internal=ifelse(line_metres==0|internal==1,1,0)) %>%
  #         dplyr::mutate(internal=as.logical(internal)) %>%
  #         dplyr::filter(internal)
  #
  #
  #     }
  #
  #     res[[i]] <- dat.lines %>%
  #       dplyr::mutate(start_time=unique(dat.time.1$time),
  #              end_time=unique(dat.time.2$time),
  #              timestep_hours=as.numeric(difftime(end_time,start_time,units="hours")),
  #              line_km=(line_metres/1000),
  #              ros_kmh=line_km/timestep_hours) %>%
  #       dplyr::select(start_time,end_time,line_km,timestep_hours,ros_kmh)
  #
  #   }
  #
  #
  # }
  #
  # res.all <- do.call(rbind,res)
  #
  #
  #
  # if(max_only==TRUE){
  #
  #   res.all <- res.all %>% dplyr::group_by(end_time) %>% dplyr::filter(line_km==max(line_km)) %>% dplyr::slice_sample(n=1) %>% dplyr::ungroup()
  # }

  return(res.all)
}

