#' Fire nearest neighbour lines using RANN packages
#'
#' @param poly sf polygon of fire progression
#' @param poly.prior sf polygon of fire progression, the one captured before poly
#' @param densify_m minimum distance between vertices on the polygon for which to search for maximum spread line
#' @param max_only T/F return only single maximum spread line
#'
#' @return sf lines
#' @export
#'
#' @examples
#' #
fire_RANN_nearest_points <- function(poly,poly.prior,densify_m,within_only=F,max_only=T,within_buffer_m=10){


  #poly to points function that is faster that st_cast
  fn_poly_to_points <- function(poly_x,densify_m){

    if(densify_m > 0){



      poly_x1 <- poly_x %>%
        sf::st_segmentize(dfMaxLength =  densify_m) %>%
        sf::st_sf() %>%
        sf::st_coordinates() %>%
        as.data.frame() %>%
        sf::st_as_sf(coords=c("X","Y"),crs=sf::st_crs(poly_x))
      return(poly_x1)
    }else{
      return(poly_x)
    }

  }


  poly.union <- sf::st_union(sf::st_geometry(poly),sf::st_geometry(poly.prior)) %>%
    sf::st_make_valid()


  points.A <- fn_poly_to_points(poly.prior,densify_m)
  points.B <- fn_poly_to_points(poly.union,densify_m)







  #if input for internal only is FALSE, use faster code to extract max ROS line, which can cross poly boundaries

  if(within_only==F){

    # Extract coordinates from both sf objects
    coords2 <- sf::st_coordinates(points.B)[, 1:2]  # X and Y for point1
    coords1 <- sf::st_coordinates(points.A)[, 1:2]  # X and Y for point2

    # Define k as the number of neighbors you want to get for each point in point1
    k_neighbors <- 1  # Get the nearest neighbors

    # Use RANN::nn2() to find nearest neighbors and distances between all points in point1 to point2
    nn_result <- RANN::nn2(coords1, coords2, k = k_neighbors)  # k_neighbors specifies the number of neighbors

    # Combine distances and indices into a data.frame for easy analysis
    distances_df <- data.frame(
      point1_index = rep(1:nrow(coords2), each = k_neighbors),
      point2_index = as.vector(nn_result$nn.idx),
      distance = as.vector(nn_result$nn.dists)
    )

    if(max_only==T){

      distances_df <- distances_df %>%
        dplyr::filter(distance==max(distance)) %>%
        dplyr::sample_n(1)
    }


    # Create a list to hold the LINESTRING geometries
    line_geometries <- lapply(1:nrow(distances_df), function(i) {
      # Extract the coordinates of the two points
      point1_coords <- coords2[distances_df$point1_index[i], ]
      point2_coords <- coords1[distances_df$point2_index[i], ]

      # Create the LINESTRING geometry connecting the points
      sf::st_linestring(rbind(point1_coords, point2_coords))
    })

    # Convert the list of LINESTRINGs to an sf object
    lines_sf <- sf::st_sf(geometry = sf::st_sfc(line_geometries, crs = sf::st_crs(points.A))) %>%
      sf::st_as_sf()




    return(lines_sf)


  }

  #if within only is required, use slower code that tests if lines are within the time 2 polygon



  if(within_only==T){

    poly.diff <- sf::st_difference(sf::st_geometry(poly),sf::st_geometry(poly.prior)) %>% sf::st_union()
    poly.diff.buff <- sf::st_buffer(poly.diff,1)

    #Remove vertices from poly that are too close to prior poly
    mypred.withindist <- function(x,y) !sf::st_is_within_distance(x,y,dist = within_buffer_m)
    points.B.x <- points.B %>%
      sf::st_filter(poly.prior,.predicate=mypred.withindist)

    #get overall farthest distance from poly vertices to poly prior vertices.
    #arrange poly vertices by farthest distance, which will speed up loop below
    nn_result.top <- RANN::nn2(sf::st_coordinates(points.A)[, 1:2],
                               sf::st_coordinates(points.B.x)[, 1:2] , k = 1) %>% # k_neighbors specifies the number of neighbors
      as.data.frame(nrow=2)
    points.B.x$dist=nn_result.top$nn.dists
    points.B.x <- points.B.x %>%
      dplyr::arrange(desc(dist))


    #extract coords for filter poly vertices
    coords2 <- sf::st_coordinates(points.B.x)[, 1:2]  # X and Y for point1
    coords1_original <- sf::st_coordinates(points.A)[, 1:2]


    # Define k as the number of neighbors you want to get for each point in point1
    k_neighbors <- 1  # Get the  nearest neighbors each time function is called in loop

    #list for sf line results
    lines.out <- list()

    #loop through each vertex from poly
    for(xi in 1:nrow(coords2)){

      print(xi)
      #get current vertex
      coords2.xi <- matrix(coords2[xi,],nrow = 1)

      #get all coordinates from prior poly. Needs to be reset each time, because while loop below remove rows if resulting line is not within poly
      coords1 <- coords1_original  # X and Y for point2


      #set blank data frame as test for the while loop
      #test if coords1 is still matrix. This will be F when poly 1 point left
      lines_sf <- data.frame()

      while(nrow(lines_sf)==0 & is.matrix(coords1)){

        # Use RANN::nn2() to find nearest neighbors and distances between vertex from poly and all vertices from prior poly
        nn_result <- RANN::nn2(coords1, coords2.xi , k = k_neighbors)  # k_neighbors specifies the number of neighbors

        #make line from coords and see if it crosses edge of poly. If true, repeat
        lines_sf <- sf::st_linestring(rbind(coords2.xi, coords1[nn_result$nn.idx[[1]], ]))
        lines_sf.crosses <- lengths(sf::st_crosses(lines_sf,poly.diff.buff))>0

        if(!lines_sf.crosses ){

          lines_sf <- lines_sf%>%
            sf::st_sfc(crs=sf::st_crs(points.A)) %>%
            sf::st_as_sf() %>%
            dplyr::mutate(dist=as.numeric(sf::st_length(.))) %>%
            dplyr::filter(dist==min(dist)) %>%
            dplyr::sample_n(1)

          lines.out[[xi]] <- lines_sf
          break

        }else{

          lines_sf <- data.frame()
          coords1 <- coords1[-nn_result$nn.idx[[1]],]


        }

      }
    }

    x <- do.call(rbind,lines.out)
    x <- x %>%
      sf::st_as_sf()

    x.max <- x %>%
      dplyr::mutate(dist_m=as.numeric(sf::st_length(.))) %>%
      dplyr::filter(dist_m==max(dist_m))

    return(x.max)



  }

}


