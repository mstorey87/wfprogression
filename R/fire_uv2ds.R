#' Wind speed and direction from U and V
#'
#' @param u u component from BARRA data
#' @param v v component from BARRA data
#'
#' @return speed (m/s) and direction
#' @export
#'
#' @examples
#' # Example U and V components
#' u <- c(3, -2, 0)
#' v <- c(4, 2, -5)
#'
#' # Calculate wind speed and direction
#' fire_uv2ds(u, v)
#'
#' # Returns a matrix:
#' #      dir    speed
#' # [1,]  36.87 5.00
#' # [2,] 317.10 2.83
#' # [3,] 180.00 5.00
fire_uv2ds <- function(u, v) {

  rad2deg <- function(rad) {
    (rad * 180) / (pi)
  }
  deg2rad <- function(deg) {
    (deg * pi) / (180)
  }


  ###### DIRECTION
  direction <- atan2(u, v)
  direction <- rad2deg(direction)
  direction[direction < 0] <- 360 + direction[direction < 0]

  ###### SPEED
  speed <- sqrt((u * u) + (v * v))

  ######
  res <- cbind(dir = direction, speed = speed)
  return(res)
}
