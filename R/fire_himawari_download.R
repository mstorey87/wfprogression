#' Download Himawari images from BOM THREDDS server
#'
#' @description
#' Accepts the output of \code{\link{fire_himawari_search}()} to clip and download data from BOM's NCI THREDDS server.
#' Because Himawari data is coarse resolution, the \code{fire_bbox} polygon is buffered by 100 km before clipping.
#' Different band combinations can be selected; the default (bands 7, 4, 1) is good for visualizing active fires.
#' Output GeoTIFF files will be RGB composites for daytime images, but single-band (first band in \code{bands})
#' for nighttime images.
#'
#' @param fire_bbox An \code{sf} polygon defining the fire bounding box.
#' @param df_download Data frame of Himawari paths returned from \code{\link{fire_himawari_search}()}.
#' @param bands Character vector of Himawari bands to download. Default is \code{c("B07","B04","B01")}.
#'              The first band is used for night images if RGB is not possible. Can be a single band.
#' @param dest_folder Local folder path to save downloaded GeoTIFF files.
#'
#' @return Writes GeoTIFF files to \code{dest_folder}.
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Define a bounding box polygon (replace with your actual bbox)
#' dat.bbox <- st_as_sfc("POLYGON((150 -34, 150 -33.5, 150.5 -33.5, 150.5 -34, 150 -34))", crs = 4326)
#'
#' # Define start and end times for search (POSIXct)
#' datestart <- as.POSIXct("2023-01-01 00:00:00", tz = "Australia/Sydney")
#' dateend <- as.POSIXct("2023-01-02 00:00:00", tz = "Australia/Sydney")
#'
#' # Search for Himawari images (e.g., every 60 minutes)
#' dat.himawari <- fire_himawari_search(
#'   fire_bbox = dat.bbox,
#'   start_time = datestart,
#'   end_time = dateend,
#'   timestep_minutes = 60
#' )
#'
#' # Specify destination folder
#' outdir <- tempdir()
#'
#' # Download Himawari images to folder
#' fire_himawari_download(
#'   fire_bbox = dat.bbox,
#'   df_download = head(dat.himawari,14),
#'   dest_folder = outdir
#' )
#' }
fire_himawari_download <- function(fire_bbox,df_download,bands=c("B07","B04","B01"),dest_folder){

  #transform bbox to himiwari crs
  #buffer first to capture wider area
  bbox_him <-fire_bbox %>%
    sf::st_transform(3112) %>%
    sf::st_buffer(100000) %>%
    sf::st_transform("+proj=geos +lon_0=140.7 +h=35785863 +x_0=0 +y_0=0 +a=6378137 +b=6356752.3 +units=m +no_defs") %>%
    sf::st_bbox()

  #filter and arrange bands by user input
  df_download <- df_download %>%
    dplyr::filter(band %in% bands)%>%
    dplyr::arrange(factor(band,levels=rev(bands)))


  #loop through each time and make an rgb or single band depending on time of day
  times_unique <- unique(df_download$datetimelocal_chr)
  for(i in 1:length(times_unique)){

    dat.i <- df_download %>%
      dplyr::filter(datetimelocal_chr==times_unique[i])

    if(dat.i$daynight[1]=="day" & nrow(dat.i)==3 & length(bands)==3){

      #stream and crop nc using tidync and terra
      dat.i <- dat.i %>%
        dplyr::mutate(nc=purrr::map(path_download,tidync::tidync),
                      rast=purrr::map(nc,~.x %>%
                                        tidync::activate(.x$variable$name[6]) %>%
                                        tidync::hyper_filter(y = y > bbox_him[2] & y < bbox_him[4],
                                                             x = x > bbox_him[1] & x < bbox_him[3]) %>%

                                        tidync::hyper_tibble() %>%
                                        dplyr::select(x,y,dplyr::everything())%>%
                                        terra::rast()))

      #combine to rgb, resampling to match cell sizes
      r <- c(dat.i$rast[[3]] ,
             dat.i$rast[[2]] %>% terra::resample(dat.i$rast[[3]]),
             dat.i$rast[[1]]%>% terra::resample(dat.i$rast[[3]]))


      #if not day or not all bands exist, just output band 7
    }else{

      dat.i <- dat.i %>%
        dplyr::filter(band==bands[1])

      #stream and crop
      b <- tidync::tidync(dat.i$path_download)
      b.local <- b  %>%
        tidync::activate(b$variable$name[6]) %>%
        tidync::hyper_filter(y = y > bbox_him[2] & y < bbox_him[4],
                             x = x > bbox_him[1] & x < bbox_him[3]) %>%

        tidync::hyper_tibble() %>%
        dplyr::select(x,y,dplyr::everything())
      r <- terra::rast(b.local)





    }

    #use terra to stretch and write file
    r <- r %>%  terra::stretch()
    #set Himawari crs
    terra::crs(r) <- "+proj=geos +lon_0=140.7 +h=35785863 +x_0=0 +y_0=0 +a=6378137 +b=6356752.3 +units=m +no_defs"

    #write to disk

    outtiftemp <-tempfile(pattern=paste0(dat.i$datetimelocal_chr[1],"_",dat.i$satellite[1],"_",paste0(bands,collapse = ""),"_"),fileext = ".tif")

    terra::writeRaster(r,outtiftemp,overwrite=T)

    outtif <- paste0(dest_folder,"\\",basename(outtiftemp))
    if(!outtiftemp==outtif) file.copy(outtiftemp,outtif,overwrite = T)

  }

}


#' #' Download Himawari images from BOM thredds server
#' #'
#' #' @description
#' #' Accepts the output of fire_himiwari_search() to clip and download data from BOMS' NCI thredds server
#' #' Because Himawari data is course resolution, the fire_bbox is buffered by 100 km before clipping.
#' #' Different bands combination can be selected. 7-4-1 is good for seeing active fire.
#' #' Output tif will be an RGB for day time images, but single band (first band in the 'bands' parameter) for night time images.
#' #'
#' #'
#' #'
#' #' @param fire_bbox Fire bounding box polygon
#' #' @param df_download Data frame of himawari paths from fire_himiwari_search() function
#' #' @param bands Himawari bands to download. Default is 7, 4 and 1. First band used for night images. Can be a single band.
#' #' @param dest_folder File local destination
#' #'
#' #' @return Writes geotifs to disk
#' #' @export
#' #'
#' #' @examples
#' #' # fire_download_himiwari(fire_bbox = dat.bbox,df_download = dat.himiwari,dest_folder = outdir)
#' fire_himawari_download <- function(fire_bbox,df_download,bands=c("B07","B04","B01"),dest_folder){
#'
#'
#'   #transform bbox to himiwari crs
#'   #buffer first to capture wider area
#'   bbox_him <-fire_bbox %>%
#'     sf::st_transform(3112) %>%
#'     sf::st_buffer(100000) %>%
#'     sf::st_transform("+proj=geos +lon_0=140.7 +h=35785863 +x_0=0 +y_0=0 +a=6378137 +b=6356752.3 +units=m +no_defs") %>%
#'     sf::st_bbox()
#'
#'     #filter and arrange bands by user input
#'   df_download <- df_download %>%
#'     dplyr::filter(band %in% bands)%>%
#'     dplyr::arrange(factor(band,levels=rev(bands)))
#'
#'
#'   #loop through each time and make an rgb or single band depending on time of day
#'   times_unique <- unique(df_download$datetimelocal_chr)
#'   for(i in 1:length(times_unique)){
#'
#'     dat.i <- df_download %>%
#'       dplyr::filter(datetimelocal_chr==times_unique[i])
#'
#'     if(dat.i$daynight[1]=="day" & nrow(dat.i)==3 & length(bands)==3){
#'
#'       #stream and crop nc using tidync and terra
#'       dat.i <- dat.i %>%
#'         dplyr::mutate(nc=purrr::map(path_download,tidync::tidync),
#'                       rast=purrr::map(nc,~.x %>%
#'                                         tidync::activate(.x$variable$name[6]) %>%
#'                                         tidync::hyper_filter(y = y > bbox_him[2] & y < bbox_him[4],
#'                                                              x = x > bbox_him[1] & x < bbox_him[3]) %>%
#'
#'                                         tidync::hyper_tibble() %>%
#'                                         dplyr::select(x,y,dplyr::everything())%>%
#'                                         terra::rast()))
#'
#'       #combine to rgb, resampling to match cell sizes
#'       r <- c(dat.i$rast[[3]] ,
#'              dat.i$rast[[2]] %>% terra::resample(dat.i$rast[[3]]),
#'              dat.i$rast[[1]]%>% terra::resample(dat.i$rast[[3]]))
#'
#'
#'       #if not day or not all bands exist, just output band 7
#'     }else{
#'
#'       dat.i <- dat.i %>%
#'         dplyr::filter(band==bands[1])
#'
#'       #stream and crop
#'       b <- tidync::tidync(dat.i$path_download)
#'       b.local <- b  %>%
#'         tidync::activate(b$variable$name[6]) %>%
#'         tidync::hyper_filter(y = y > bbox_him[2] & y < bbox_him[4],
#'                              x = x > bbox_him[1] & x < bbox_him[3]) %>%
#'
#'         tidync::hyper_tibble() %>%
#'         dplyr::select(x,y,dplyr::everything())
#'       r <- terra::rast(b.local)
#'
#'
#'
#'
#'
#'     }
#'
#'     #use terra to stretch and write file
#'     r <- r %>%  terra::stretch()
#'     #set Himawari crs
#'     terra::crs(r) <- "+proj=geos +lon_0=140.7 +h=35785863 +x_0=0 +y_0=0 +a=6378137 +b=6356752.3 +units=m +no_defs"
#'
#'     #write to disk
#'
#'     outtiftemp <-tempfile(pattern=paste0(dat.i$datetimelocal_chr[1],"_",dat.i$satellite[1],"_",paste0(bands,collapse = ""),"_"),fileext = ".tif")
#'
#'     terra::writeRaster(r,outtiftemp,overwrite=T)
#'
#'     outtif <- paste0(dest_folder,"\\",basename(outtiftemp))
#'     if(!outtiftemp==outtif) file.copy(outtiftemp,outtif,overwrite = T)
#'
#'   }
#'
#' }
