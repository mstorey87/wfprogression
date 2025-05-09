% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{dat.landsat.pathrow}
\alias{dat.landsat.pathrow}
\title{Landsat tiles for Australia}
\format{
\subsection{\verb{Used in a function to find which landsat tiles intersect with a fire, then construct paths to search NCI}}{

An sf object with 404 rows and 4 columns:
\describe{
\item{PATH}{Landsat Path Number}
\item{ROW, iso3}{Landsat Row number}
\item{WRSPR}{Path and Row string}
...
}
}
}
\source{
\url{https://www.usgs.gov/media/files/landsat-wrs-2-descending-path-row-shapefile}
}
\usage{
dat.landsat.pathrow
}
\description{
A subset of landsat tiles with Path and Row that intersect Australia
}
\keyword{datasets}
