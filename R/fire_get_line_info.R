#' Get start an end coords and directions of an sf line
#'
#' @param sf lines input
#'
#' @return data frame
#' @export
#'
#' @examples
#' #
fire_get_line_info <- function(sf_lines) {

  # Initialize results list
  results <- lapply(1:nrow(sf_lines), function(i) {
    # Extract the geometry of the current line
    line <- sf::st_geometry(sf_lines)[i]

    # Extract coordinates
    coords <- sf::st_coordinates(line)

    # Get start and end points
    end_point <- coords[1,1:2 ]
    start_point <- coords[nrow(coords), 1:2]

    crs1 <- sf::st_crs(line)
    if(stringr::str_starts(crs1$wkt,"GEOGCRS")){

      direction <- geosphere::bearing(start_point,end_point)


    }

    if(stringr::str_starts(crs1$wkt,"PROJCRS")){


      # Compute dx and dy
      dx <- end_point[1] - start_point[1]
      dy <- end_point[2] - start_point[2]

      # Compute initial bearing in radians (relative to X-axis)
      theta <- atan2(dy, dx)  # atan2(y, x) -> standard math convention

      # Convert to degrees
      bearing <- theta * (180 / pi)

      # Adjust to compass bearing (relative to north)
      direction <- (90 - bearing) %% 360  # Rotate so that 0° is north
    }



    dist <- as.numeric(sf::st_length(line))/1000


    # Return as a named vector
    return(c(
      start_x = start_point[1], start_y = start_point[2],
      end_x = end_point[1], end_y = end_point[2],
      direction = direction,
      dist=dist
    ))
  })

  # Convert results to a data frame
  results_df <- do.call(rbind, results) %>% as.data.frame()
  names(results_df) <- paste0(c("start_x","start_y","end_x","end_y","bearing","line_km"),"_epsg",sf::st_crs(lines)$epsg)

  return(results_df)
}
