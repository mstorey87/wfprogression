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
fire_RANN_nearest_points <- function(poly,poly.prior,densify_m,max_only=T){


  #poly to points function that is faster that st_cast
  fn_poly_to_points <- function(poly_x,densify_m){

    if(densify_m > 0){



      poly_x1 <- poly_x %>%
        sf::st_segmentize(dfMaxLength =  densify_m) %>%
        sf::st_coordinates() %>%
        as.data.frame() %>%
        sf::st_as_sf(coords=c("X","Y"),crs=sf::st_crs(poly_x))
      return(poly_x1)
    }else{
      return(poly_x)
    }

  }
  poly.union <- sf::st_union(sf::st_geometry(poly),sf::st_geometry(poly.prior)) %>%
    sf::st_union()


  points.A <- fn_poly_to_points(poly.prior,densify_m)
  points.B <- fn_poly_to_points(poly.union,densify_m)




  # Extract coordinates from both sf objects
  coords1 <- sf::st_coordinates(points.B)[, 1:2]  # X and Y for point1
  coords2 <- sf::st_coordinates(points.A)[, 1:2]  # X and Y for point2

  # Define k as the number of neighbors you want to get for each point in point1
  k_neighbors <- 1  # Get the 10 nearest neighbors

  # Use RANN::nn2() to find nearest neighbors and distances between all points in point1 to point2
  nn_result <- RANN::nn2(coords2, coords1, k = k_neighbors)  # k_neighbors specifies the number of neighbors

  # Combine distances and indices into a data.frame for easy analysis
  distances_df <- data.frame(
    point1_index = rep(1:nrow(coords1), each = k_neighbors),
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
    point1_coords <- coords1[distances_df$point1_index[i], ]
    point2_coords <- coords2[distances_df$point2_index[i], ]

    # Create the LINESTRING geometry connecting the points
    sf::st_linestring(rbind(point1_coords, point2_coords))
  })

  # Convert the list of LINESTRINGs to an sf object
  lines_sf <- sf::st_sf(geometry = sf::st_sfc(line_geometries, crs = sf::st_crs(points.A))) %>%
    sf::st_as_sf()
  # dplyr::mutate(line_metres=as.numeric(sf::st_length(.)))

  #
  #   if(internal_only==T){
  #     lines_sf$internal <- apply(sf::st_within(lines_sf,poly,sparse = F),MARGIN = 1,max)
  #     lines_sf$intersects_prior <- apply(sf::st_intersects(lines_sf,poly.prior,sparse = F),MARGIN = 1,max)
  #
  #     #give zero m lines a internal flag
  #     lines_sf <- lines_sf %>%
  #       dplyr::mutate(internal=ifelse(line_metres==0|internal==1,1,0)) %>%
  #       dplyr::mutate(internal=as.logical(internal)) %>%
  #       dplyr::filter(internal)
  #   }
  #get maximum distance only
  # if(max_only==T){
  #
  #   lines_sf <- lines_sf %>%
  #     dplyr::filter(line_metres==max(line_metres)) %>%
  #     dplyr::sample_n(1)
  # }













  #
  #
  #   #get nearest points for each in prior to each in current poly
  #   nearest <- RANN::nn2(data = sf::st_coordinates(points.A),
  #                        query = sf::st_coordinates(points.B),
  #                        k = nrow(points.A)) %>%
  #     as.data.frame() %>%
  #     dplyr::mutate(nid=row_number()) %>%
  #     cbind(points.B %>% dplyr::select(-dplyr::everything())) %>%
  #     sf::st_as_sf()
  #
  #
  #
  #   #get maximum distance only
  #   if(max_only==T){
  #
  #     nearest <- nearest %>%
  #       dplyr::filter(nn.dists==max(nn.dists)) %>%
  #       dplyr::sample_n(1)
  #   }
  #
  #   #extract coords for each end of lines to be drawn based on results of RANN::nn2()
  #   nearest.A <- points.A[nearest$nn.idx,] %>% dplyr::select(-dplyr::everything())
  #
  #   # Extract coordinates as matrices
  #   coords.A<- sf::st_coordinates(nearest.A)[, 1:2] %>% matrix(ncol = 2) # X and Y for point1
  #   coords.B <- sf::st_coordinates(nearest)[, 1:2] %>% matrix(ncol = 2)  # X and Y for point2
  #
  #   # Combine coordinates into matrices for each line
  #   line_matrices <- lapply(1:nrow(coords.A), function(i) {
  #     rbind(coords.A[i, ], coords.B[i, ])  # Combine the two points for each line
  #   })
  #
  #   # Create LINESTRING geometries directly
  #   lines <- sf::st_sfc(lapply(line_matrices, sf::st_linestring), crs = sf::st_crs(points.A))
  #
  #   # Convert to sf object
  #   lines_sf <- sf::st_sf(geometry = lines) %>%
  #     sf::st_as_sf() %>%
  #     dplyr::mutate(len = as.numeric(sf::st_length(.)))



  return(lines_sf)

}


