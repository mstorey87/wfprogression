# Get start and end coords and directions of a line
#' Line coords and direction
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
    start_point <- coords[1, ]
    end_point <- coords[nrow(coords), ]

    # Calculate direction (bearing in degrees)
    direction <- atan2(end_point[2] - start_point[2], end_point[1] - start_point[1]) * (180 / pi)
    if (direction < 0) direction <- direction + 360

    # Return as a named vector
    return(c(
      start_x = start_point[1], start_y = start_point[2],
      end_x = end_point[1], end_y = end_point[2],
      epsg=sf::st_crs(lines)$epsg,
      direction = direction
    ))
  })

  # Convert results to a data frame
  results_df <- do.call(rbind, results)
  return(as.data.frame(results_df))
}
