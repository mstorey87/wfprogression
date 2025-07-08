#' Find maximum fire spread line using nearest-neighbor search (RANN)
#'
#' @description
#' This function finds lines of maximum spread between two consecutive fire progression polygons
#' using a fast nearest-neighbor search powered by the **RANN** package.
#'
#' The function works by converting polygon boundaries to dense point sets and then connecting
#' the farthest pairs of points between the earlier (`poly.prior`) and later (`poly`) polygons.
#' Optionally, you can restrict lines to only those fully contained within the later polygon,
#' which is slower but sometimes useful for strict containment.
#'
#' @param poly An `sf` polygon of the later fire progression.
#' @param poly.prior An `sf` polygon of the earlier fire progression.
#' @param densify_m Numeric. Minimum distance in meters between vertices when densifying the polygons' boundaries.
#' @param within_only Logical. If `TRUE`, return only spread lines that are fully contained within `poly` (slower).
#' @param max_only Logical. If `TRUE` (default), return only the single maximum spread line.
#' @param within_buffer_m Numeric. Buffer distance (meters) to exclude vertices near `poly.prior` when `within_only = TRUE`.
#' @param radius Numeric. Search radius in meters for nearest-neighbor matching (default `1`).
#'
#' @return An `sf` object of LINESTRING geometries representing maximum fire spread lines.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' # Create two simple overlapping squares
#' poly1 <- st_polygon(list(rbind(
#'   c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)
#' )))
#' poly2 <- st_polygon(list(rbind(
#'   c(0.5,0.5), c(1.5,0.5), c(1.5,1.5), c(0.5,1.5), c(0.5,0.5)
#' )))
#'
#' poly_sf1 <- st_sf(geometry = st_sfc(poly1),crs=4326)
#' poly_sf2 <- st_sf(geometry = st_sfc(poly2),crs=4326)
#'
#' # Find maximum spread line
#' line_sf <- fire_RANN_nearest_points(
#'   poly = poly_sf2,
#'   poly.prior = poly_sf1,
#'   densify_m = 0,
#'   max_only = TRUE
#' )
#'
#' plot(st_geometry(poly_sf1), col = NA, border = "blue")
#' plot(st_geometry(poly_sf2), add = TRUE, col = NA, border = "red")
#' plot(st_geometry(line_sf), add = TRUE, col = "black", lwd = 2)
#' }
fire_RANN_nearest_points <- function(poly, poly.prior, densify_m, within_only = FALSE, max_only = TRUE, within_buffer_m = 10, radius = 1) {

  # Helper: Convert polygon boundary to dense points
  fn_poly_to_points <- function(poly_x, densify_m) {
    if (densify_m > 0) {
      poly_x1 <- poly_x %>%
        sf::st_segmentize(dfMaxLength = densify_m) %>%
        sf::st_sf() %>%
        sf::st_coordinates() %>%
        as.data.frame() %>%
        sf::st_as_sf(coords = c("X", "Y"), crs = sf::st_crs(poly_x))
      return(poly_x1)
    } else {
      return(poly_x)
    }
  }

  # Create union geometry of both polygons (for within tests)
  poly.union <- sf::st_union(sf::st_geometry(poly), sf::st_geometry(poly.prior)) %>%
    sf::st_make_valid()

  # Convert both polygons to points
  points.A <- fn_poly_to_points(poly.prior, densify_m)
  points.B <- fn_poly_to_points(poly.union, densify_m)

  # --- OPTION 1: Faster method, allows crossing poly boundary ---
  if (within_only == FALSE) {
    # Extract coordinates
    coords2 <- sf::st_coordinates(points.B)[, 1:2]
    coords1 <- sf::st_coordinates(points.A)[, 1:2]

    # Find nearest neighbors (using RANN)
    nn_result <- RANN::nn2(coords1, coords2, k = 1)

    distances_df <- data.frame(
      point1_index = 1:nrow(coords2),
      point2_index = nn_result$nn.idx,
      distance = nn_result$nn.dists
    )

    if (max_only == TRUE) {
      distances_df <- distances_df %>%
        dplyr::filter(distance == max(distance)) %>%
        dplyr::sample_n(1)
    }

    # Create LINESTRING geometries
    line_geometries <- lapply(1:nrow(distances_df), function(i) {
      point1_coords <- coords2[distances_df$point1_index[i], ]
      point2_coords <- coords1[distances_df$point2_index[i], ]
      sf::st_linestring(rbind(point1_coords, point2_coords))
    })

    lines_sf <- sf::st_sf(geometry = sf::st_sfc(line_geometries, crs = sf::st_crs(points.A)))
    return(lines_sf)
  }

  # --- OPTION 2: Restrict lines to be fully within poly (slower) ---
  if (within_only == TRUE) {
    poly.diff <- sf::st_difference(sf::st_geometry(poly), sf::st_geometry(poly.prior)) %>% sf::st_union()
    poly.diff.buff <- sf::st_buffer(poly.diff, 1)

    mypred.withindist <- function(x, y) !sf::st_is_within_distance(x, y, dist = within_buffer_m)
    points.B.x <- points.B %>%
      sf::st_filter(poly.prior, .predicate = mypred.withindist)

    nn_result.top <- RANN::nn2(sf::st_coordinates(points.A)[, 1:2],
                               sf::st_coordinates(points.B.x)[, 1:2], k = 1)
    points.B.x$dist <- nn_result.top$nn.dists
    points.B.x <- points.B.x %>%
      dplyr::arrange(dplyr::desc(dist))

    coords2 <- sf::st_coordinates(points.B.x)[, 1:2]
    coords1_original <- sf::st_coordinates(points.A)[, 1:2]

    lines.out <- list()

    for (xi in 1:nrow(coords2)) {
      coords2.xi <- matrix(coords2[xi, ], nrow = 1)
      coords1 <- coords1_original

      lines_sf <- data.frame()

      while (nrow(lines_sf) == 0 && is.matrix(coords1)) {
        nn_result <- RANN::nn2(coords1, coords2.xi, k = 1)

        lines_sf_try <- sf::st_linestring(rbind(coords2.xi, coords1[nn_result$nn.idx[[1]], ]))
        crosses <- lengths(sf::st_crosses(lines_sf_try, poly.diff.buff)) > 0

        if (!crosses) {
          lines_sf <- lines_sf_try %>%
            sf::st_sfc(crs = sf::st_crs(points.A)) %>%
            sf::st_as_sf() %>%
            dplyr::mutate(dist = as.numeric(sf::st_length(.))) %>%
            dplyr::filter(dist == min(dist)) %>%
            dplyr::sample_n(1)

          lines.out[[xi]] <- lines_sf
          break
        } else {
          lines_sf <- data.frame()
          coords1 <- coords1[-nn_result$nn.idx[[1]], ]
        }
      }
    }

    x <- do.call(rbind, lines.out) %>% sf::st_as_sf()
    x.max <- x %>%
      dplyr::mutate(dist_m = as.numeric(sf::st_length(.))) %>%
      dplyr::filter(dist_m == max(dist_m))

    return(x.max)
  }

}



#' #' Fire nearest neighbour lines using RANN packages
#' #'
#' #' @param poly sf polygon of fire progression
#' #' @param poly.prior sf polygon of fire progression, the one captured before poly
#' #' @param densify_m minimum distance between vertices on the polygon for which to search for maximum spread line
#' #' @param max_only T/F return only single maximum spread line
#' #'
#' #' @return sf lines
#' #' @export
#' #'
#' #' @examples
#' #' #
#' fire_RANN_nearest_points <- function(poly,poly.prior,densify_m,within_only=F,max_only=T,within_buffer_m=10,radius=1){
#'
#'
#'   #poly to points function that is faster that st_cast
#'   fn_poly_to_points <- function(poly_x,densify_m){
#'
#'     if(densify_m > 0){
#'
#'
#'
#'       poly_x1 <- poly_x %>%
#'         sf::st_segmentize(dfMaxLength =  densify_m) %>%
#'         sf::st_sf() %>%
#'         sf::st_coordinates() %>%
#'         as.data.frame() %>%
#'         sf::st_as_sf(coords=c("X","Y"),crs=sf::st_crs(poly_x))
#'       return(poly_x1)
#'     }else{
#'       return(poly_x)
#'     }
#'
#'   }
#'
#'
#'   poly.union <- sf::st_union(sf::st_geometry(poly),sf::st_geometry(poly.prior)) %>%
#'     sf::st_make_valid()
#'
#'
#'   points.A <- fn_poly_to_points(poly.prior,densify_m)
#'   points.B <- fn_poly_to_points(poly.union,densify_m)
#'
#'
#'
#'
#'
#'
#'
#'   #if input for internal only is FALSE, use faster code to extract max ROS line, which can cross poly boundaries
#'
#'   if(within_only==F){
#'
#'     # Extract coordinates from both sf objects
#'     coords2 <- sf::st_coordinates(points.B)[, 1:2]  # X and Y for point1
#'     coords1 <- sf::st_coordinates(points.A)[, 1:2]  # X and Y for point2
#'
#'     # Define k as the number of neighbors you want to get for each point in point1
#'     k_neighbors <- 1  # Get the nearest neighbors
#'
#'     # Use RANN::nn2() to find nearest neighbors and distances between all points in point1 to point2
#'     nn_result <- RANN::nn2(coords1, coords2, k = k_neighbors)  # k_neighbors specifies the number of neighbors
#'
#'     # Combine distances and indices into a data.frame for easy analysis
#'     distances_df <- data.frame(
#'       point1_index = rep(1:nrow(coords2), each = k_neighbors),
#'       point2_index = as.vector(nn_result$nn.idx),
#'       distance = as.vector(nn_result$nn.dists)
#'     )
#'
#'     if(max_only==T){
#'
#'       distances_df <- distances_df %>%
#'         dplyr::filter(distance==max(distance)) %>%
#'         dplyr::sample_n(1)
#'     }
#'
#'
#'     # Create a list to hold the LINESTRING geometries
#'     line_geometries <- lapply(1:nrow(distances_df), function(i) {
#'       # Extract the coordinates of the two points
#'       point1_coords <- coords2[distances_df$point1_index[i], ]
#'       point2_coords <- coords1[distances_df$point2_index[i], ]
#'
#'       # Create the LINESTRING geometry connecting the points
#'       sf::st_linestring(rbind(point1_coords, point2_coords))
#'     })
#'
#'     # Convert the list of LINESTRINGs to an sf object
#'     lines_sf <- sf::st_sf(geometry = sf::st_sfc(line_geometries, crs = sf::st_crs(points.A))) %>%
#'       sf::st_as_sf()
#'
#'
#'
#'
#'     return(lines_sf)
#'
#'
#'   }
#'
#'   #if within only is required, use slower code that tests if lines are within the time 2 polygon
#'
#'
#'
#'   if(within_only==T){
#'
#'     poly.diff <- sf::st_difference(sf::st_geometry(poly),sf::st_geometry(poly.prior)) %>% sf::st_union()
#'     poly.diff.buff <- sf::st_buffer(poly.diff,1)
#'
#'     #Remove vertices from poly that are too close to prior poly
#'     mypred.withindist <- function(x,y) !sf::st_is_within_distance(x,y,dist = within_buffer_m)
#'     points.B.x <- points.B %>%
#'       sf::st_filter(poly.prior,.predicate=mypred.withindist)
#'
#'     #get overall farthest distance from poly vertices to poly prior vertices.
#'     #arrange poly vertices by farthest distance, which will speed up loop below
#'     nn_result.top <- RANN::nn2(sf::st_coordinates(points.A)[, 1:2],
#'                                sf::st_coordinates(points.B.x)[, 1:2] , k = 1) %>% # k_neighbors specifies the number of neighbors
#'       as.data.frame(nrow=2)
#'     points.B.x$dist=nn_result.top$nn.dists
#'     points.B.x <- points.B.x %>%
#'       dplyr::arrange(dplyr::desc(dist))
#'
#'
#'     #extract coords for filter poly vertices
#'     coords2 <- sf::st_coordinates(points.B.x)[, 1:2]  # X and Y for point1
#'     coords1_original <- sf::st_coordinates(points.A)[, 1:2]
#'
#'
#'     # Define k as the number of neighbors you want to get for each point in point1
#'     k_neighbors <- 1  # Get the  nearest neighbors each time function is called in loop
#'
#'     #list for sf line results
#'     lines.out <- list()
#'
#'     #loop through each vertex from poly
#'     for(xi in 1:nrow(coords2)){
#'
#'       print(xi)
#'       #get current vertex
#'       coords2.xi <- matrix(coords2[xi,],nrow = 1)
#'
#'       #get all coordinates from prior poly. Needs to be reset each time, because while loop below remove rows if resulting line is not within poly
#'       coords1 <- coords1_original  # X and Y for point2
#'
#'
#'       #set blank data frame as test for the while loop
#'       #test if coords1 is still matrix. This will be F when poly 1 point left
#'       lines_sf <- data.frame()
#'
#'       while(nrow(lines_sf)==0 & is.matrix(coords1)){
#'
#'         # Use RANN::nn2() to find nearest neighbors and distances between vertex from poly and all vertices from prior poly
#'         nn_result <- RANN::nn2(coords1, coords2.xi , k = k_neighbors)  # k_neighbors specifies the number of neighbors
#'
#'         #make line from coords and see if it crosses edge of poly. If true, repeat
#'         lines_sf <- sf::st_linestring(rbind(coords2.xi, coords1[nn_result$nn.idx[[1]], ]))
#'         lines_sf.crosses <- lengths(sf::st_crosses(lines_sf,poly.diff.buff))>0
#'
#'         if(!lines_sf.crosses ){
#'
#'           lines_sf <- lines_sf%>%
#'             sf::st_sfc(crs=sf::st_crs(points.A)) %>%
#'             sf::st_as_sf() %>%
#'             dplyr::mutate(dist=as.numeric(sf::st_length(.))) %>%
#'             dplyr::filter(dist==min(dist)) %>%
#'             dplyr::sample_n(1)
#'
#'           lines.out[[xi]] <- lines_sf
#'           break
#'
#'         }else{
#'
#'           lines_sf <- data.frame()
#'           coords1 <- coords1[-nn_result$nn.idx[[1]],]
#'
#'
#'         }
#'
#'       }
#'     }
#'
#'     x <- do.call(rbind,lines.out)
#'     x <- x %>%
#'       sf::st_as_sf()
#'
#'     x.max <- x %>%
#'       dplyr::mutate(dist_m=as.numeric(sf::st_length(.))) %>%
#'       dplyr::filter(dist_m==max(dist_m))
#'
#'     return(x.max)
#'
#'
#'
#'   }
#'
#' }
#'
#'
