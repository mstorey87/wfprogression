# library(tidyverse)
#
#
# fire_search_linescans <- function(fire_bbox)
#
#
#   ### S3 credentials
#   Sys.setenv("AWS_ACCESS_KEY_ID" = "G36SR9LPSLLNUT5CK6UO",
#              "AWS_SECRET_ACCESS_KEY" = "8//nU9A0YChIhnE7vU2VyZ6+r2T4NDjLI1aVuJDn",
#              "AWS_DEFAULT_REGION" = "s3",
#              "AWS_S3_ENDPOINT" = "its.uow.edu.au")
#
# S3_URL <- "https://s3.its.uow.edu.au"
# RASTER_BUCKET <- "uow-rb01186-ros-model"
#
# buck1 <- aws.s3::get_bucket_df(RASTER_BUCKET,prefix="rasters")
#
# buck1.df <- buck1 %>%
#   dplyr::select(Key,Bucket) %>%
#   dplyr::mutate(filename=basename(Key))

#
#
# dat.fire.polygon <- sf::read_sf(path_fire_history) %>%
#   #dplyr::filter(FireName==chr_fire_name & Label == chr_season_firetype)%>%
#   dplyr::filter(stringr::str_detect(FireName,"Yarrabin")) %>%
#   sf::st_transform(4283) %>%
#   sf::st_concave_hull(ratio = 0.8)
#
# st_geometry(dat.fire.polygon) <- "geometry"
#
#
# fire_poly <- dat.fire.polygon
# start_date <- "2013-01-06"
# end_date <- "2013-02-12"

#
#
#

#
# scans <- fire_search_scans(dat.fire.polygon,dat.fire.polygon$StartDate,dat.fire.polygon$EndDate)
