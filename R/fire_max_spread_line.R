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


   dat.poly <- polygons %>%
    #convert crs to projected so we can use meters
    st_transform(3112)

   #add a season variable if needed
   dat.poly <- dat.poly %>%
     dplyr::mutate(semes=lubridate::semester(dt_local),yr=lubridate::year(dt_local),season=ifelse(semes==1,paste0(yr-1,"-",yr),paste0(yr,"-",yr+1))) %>%
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

     #get current loop polygon
     dat.i <- dat.poly[i,]

     #get all prior polygons that intersect current loop polygon
     dat.prior.all <- dat.poly %>%
       dplyr::filter(dt_local < dat.i$dt_local)%>%
       #ensure we only consider prior polygons from the same season, otherwise the code will mistake fire from previous seasons as being part of the same fire
       dplyr::filter(season==dat.i$season) %>%
       dplyr::filter(as.logical(sf::st_intersects(.,dat.i)))


     # #the script requires the second poly to contain the first. Filter out examples when this is not the case, or at least there is not enough crossover
     # dat.intersect <- sf::st_intersection(sf::st_geometry(dat.i),sf::st_geometry(dat.prior.all)) %>%
     #   sf::st_as_sf()%>%
     #   dplyr::mutate(area=as.numeric(sf::st_area(.)),area_prc=area/as.numeric(sf::st_area(dat.i))*100) %>%
     #   sf::st_drop_geometry() %>%
     #   summarise(area_prc=sum(area_prc))

     #skip if not prior intersecting polygon
     if(nrow(dat.prior.all)>0 ){

       #get only most recent prior polygon
       dat.prior <- dat.prior.all %>%
         dplyr::filter(dt_local==max(dt_local))

       #convert second polygon to points every x metres along boundary
       #dat.i <- dat.i %>% smoothr::densify(max_distance = 100)

       #take different between poly1 and poly2, with snap tolerance to avoid tiny polygons
       dat.i <- sf::st_snap(dat.i,sf::st_union(dat.prior),tolerance=10) %>%
         sf::st_buffer(0) %>%
         sf::st_difference(.,sf::st_union(dat.prior)) %>%
         sf::st_cast("MULTIPOLYGON") %>%
         sf::st_cast("POLYGON") %>%
         dplyr::mutate(area=as.numeric(sf::st_area(.))) %>%
         dplyr::filter(area>1) %>%
         dplyr::summarise(time=unique(time),rowid=unique(rowid))

       dat.points <- sf::st_cast(sf::st_geometry(dat.i),"POINT")

       #get nearest line from each time 2 vertice (point) back to the time 1 polygon
       dat.lines <- sf::st_nearest_points( dat.points, sf::st_union(dat.prior)) %>%
         sf::st_as_sf()


       #combine attributes
       dat.lines <- cbind(dat.lines,sf::st_drop_geometry(dat.i))

       #get line distance
       dat.lines$line_metres <- as.numeric(sf::st_length(dat.lines))

       #arrange by distance
       dat.lines <- dat.lines %>%
         dplyr::arrange(dplyr::desc(line_metres))

       #remove any line that goes any distance outside the second time polygon based on user input
       if(internal_only==T){

         dat.lines$internal <- apply(st_within(dat.lines,dat.i,sparse = F),MARGIN = 1,max)
         #give zero m lines a internal flag
         dat.lines <- dat.lines %>%
           dplyr::mutate(internal=ifelse(line_metres==0|internal==1,1,0)) %>%
           dplyr::mutate(internal=as.logical(internal)) %>%
           dplyr::filter(internal)


       }

       if(max_only==TRUE){

         dat.lines <- dat.lines %>% dplyr::filter(line_metres==max(line_metres)) %>% dplyr::slice_sample(n=1)
       }

       dat.lines <- dat.lines %>%
         #select final attributes
         dplyr::mutate(start_time=unique(dat.prior$time),
                       end_time=unique(dat.i$time),
                       timestep_hours=as.numeric(difftime(end_time,start_time,units="hours")),
                       line_km=(line_metres/1000),
                       ros_kmh=line_km/timestep_hours) %>%
         dplyr::select(start_time,end_time,line_km,timestep_hours,ros_kmh)


       #remove lines from results if it crosses polygons with prior times (can happen when fires merge)
       dat.lines.intersect <- sf::st_intersection(sf::st_geometry(dat.lines),dat.prior.all %>% dplyr::select(dt_local_poly=dt_local)) %>%
         sf::st_as_sf() %>%
         dplyr::mutate(len=as.numeric(sf::st_length(.))) %>%
         #check if any lines intersect y more than a cm. 0 should result when intersecting the line end with a boundary point, but sometimes there is a small error
         dplyr::filter(len>0.01)

       # if(nrow(dat.lines.intersect)>0){
       #      dat.lines.x <- dat.lines %>% dplyr::filter(start_time < min(dat.lines.intersect$dt_local_poly))
       #
       # }

       res[[i]] <- dat.lines %>% mutate(prior_intersects=nrow(dat.lines.intersect)>0)
     }






   }


   res.all <- do.call(rbind,res)



   #get unique union_id groups and loop through
   # unique_unionid <- unique(dat.poly$union_id)
   #
   #
   #
   # res.0 <- list()
   # for(i in 1:length(unique_unionid)){
   #
   #   #get current union id group
   #   dat.u <- dat.poly %>%
   #     dplyr::filter(union_id==unique_unionid[i]) %>%
   #     dplyr::arrange(time)
   #
   #   #split polygons by time and loop through each
   #   dat.poly.split <-  split(dat.u,as.factor(dat.u$time))
   #
   #   res <- list()
   #   for(xi in 1:length(dat.poly.split)){
   #
   #
   #
   #     #skip first spread time because there is no prior polygon to measure ROS
   #     if(xi >1){
   #       #measure lines from current time to time prior. Summarise time prior to single feature to simplify processing
   #       #convert second polygon time to points first
   #       dat.time.1 <- dat.poly.split[[xi-1]]
   #       dat.time.2 <- dat.poly.split[[xi]]
   #
   #
   #       #convert polygon time2 vertices to points
   #       dat.points <- sf::st_cast(dat.time.2,"POINT")
   #
   #       #get nearest line from each time 2 vertice (point) back to the time 1 polygon
   #       dat.lines <- sf::st_nearest_points( dat.points, dat.time.1) %>%
   #         sf::st_as_sf()
   #
   #       #combine attributes
   #       dat.lines <- cbind(dat.lines,sf::st_drop_geometry(dat.time.2))
   #
   #       #get line distance
   #       dat.lines$line_metres <- as.numeric(sf::st_length(dat.lines))
   #
   #       #arrange by distance
   #       dat.lines <- dat.lines %>%
   #         dplyr::arrange(dplyr::desc(line_metres))
   #
   #       #remove any line that goes any distance outside the second time polygon based on user input
   #       if(internal_only==T){
   #
   #         dat.lines$internal <- apply(st_within(dat.lines,dat.time.2,sparse = F),MARGIN = 1,max)
   #         #give zero m lines a internal flag
   #         dat.lines <- dat.lines %>%
   #           dplyr::mutate(internal=ifelse(line_metres==0|internal==1,1,0)) %>%
   #           dplyr::mutate(internal=as.logical(internal)) %>%
   #           dplyr::filter(internal)
   #
   #
   #       }
   #
   #       res[[xi]] <- dat.lines %>%
   #         #select final attributes
   #         dplyr::mutate(start_time=unique(dat.time.1$time),
   #                       end_time=unique(dat.time.2$time),
   #                       unionid=unique_unionid[i],
   #                       timestep_hours=as.numeric(difftime(end_time,start_time,units="hours")),
   #                       line_km=(line_metres/1000),
   #                       ros_kmh=line_km/timestep_hours) %>%
   #         dplyr::select(start_time,end_time,line_km,timestep_hours,ros_kmh,unionid)
   #
   #     }
   #
   #
   #
   #
   #   }
   #
   #   #keep all records from current union_id group. Some will have no results when there is only on time in a group.
   #   res.0[[i]] <- do.call(rbind,res)
   #
   # }
   #
   # res.all <- do.call(rbind,res.0)
   #
   # if(max_only==TRUE){
   #
   #   res.all <- res.all %>% dplyr::group_by(end_time,unionid) %>% dplyr::filter(line_km==max(line_km)) %>% dplyr::slice_sample(n=1) %>% dplyr::ungroup()
   # }


  #  #split by time to create separate objects
  # dat.poly.split <- split(dat.poly,as.factor(dat.poly$timeid))
  #
  # #measure distance from each point on polygon time 2 to polygon time 1
  # res <- list()
  # for(i in 1:length(dat.poly.split)){
  #
  #   #skip first spread time
  #   if(i >1){
  #     #measure lines from current time to time prior. Summarise time prior to single feature to simplify processing
  #     #convert second polygon time to points first
  #     dat.time.1 <- dat.poly.split[[i-1]]
  #     dat.time.2 <- dat.poly.split[[i]]
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

