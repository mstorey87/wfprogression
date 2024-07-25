#' Landsat tiles for Australia
#'
#' A subset of landsat tiles with Path and Row that intersect Australia
#' @format ## `dat.landsat.pathrow`
#' An sf object with 404 rows and 4 columns:
#' \describe{
#'   \item{PATH}{Landsat Path Number}
#'   \item{ROW, iso3}{Landsat Row number}
#'   \item{WRSPR}{Path and Row string}
#'   ...
#' }
#' @source <https://www.usgs.gov/media/files/landsat-wrs-2-descending-path-row-shapefile>
"Used in a function to find which landsat tiles intersect with a fire, then construct paths to search NCI"
