% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fire_download_himiwari.R
\name{fire_download_himiwari}
\alias{fire_download_himiwari}
\title{Download Himawari images from BOM thredds server}
\usage{
fire_download_himiwari(
  fire_bbox,
  df_download,
  bands = c("B07", "B04", "B01"),
  dest_folder
)
}
\arguments{
\item{fire_bbox}{Fire bounding box polygon}

\item{df_download}{Data frame of himawari paths from fire_search_himiwari() function}

\item{bands}{Himawari bands to download. Default is 7, 4 and 1}

\item{dest_folder}{File local destination}
}
\value{
Writes geotifs to disk
}
\description{
Download Himawari images from BOM thredds server
}
\examples{
# fire_download_himiwari(fire_bbox = dat.bbox,df_download = dat.himiwari,dest_folder = outdir)
}
