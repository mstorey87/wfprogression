#' Download Himawari images from BOM thredds server
#'
#' @param fire_bbox Fire bounding box polygon
#' @param df_download Data frame of himawari paths from fire_search_himiwari() function
#' @param bands Himawari bands to download. Default is 7, 4 and 1
#' @param dest_folder File local destination
#'
#' @return Writes geotifs to disk
#' @export
#'
#' @examples
#' # fire_download_himiwari(fire_bbox = dat.bbox,df_download = dat.himiwari,dest_folder = outdir)
fire_download_himiwari <- function(fire_bbox,df_download,bands=c("B07","B04","B01"),dest_folder){


  #transform bbox to himiwari crs
  #buffer first to capture wider area
  bbox_him <-fire_bbox %>%
    sf::st_transform(3112) %>%
    sf::st_buffer(60000) %>%
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

    outtiftemp <-paste0(tempdir(),"\\",dat.i$datetimelocal_chr[1],"_",
                        dat.i$satellite[1],"_",paste0(bands,collapse = ""),".tif")
    terra::writeRaster(r,outtiftemp)

    outtif <- paste0(dest_folder,"\\",dat.i$datetimelocal_chr[1],"_",
                     dat.i$satellite[1],"_",paste0(bands,collapse = ""),".tif")
    file.copy(outtiftemp,outtif,overwrite = T)

  }

}
