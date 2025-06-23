#' Sample monthly precipitation from AGCD
#'
#' @param dat sf object with geometry and time column
#' @param time_utc_col character of time column name
#'
#' @returns data frame
#' @export
#'
#' @examples
#' #
fire_sample_agcd <- function(dat,time_utc_col,buffer_m){

  dat[["tm"]] <- dat[[time_utc_col]]

  dat.x <- dat %>%
    sf::st_transform(4326)%>%
    mutate(yr=lubridate::year(tm),
           mnth=lubridate::month(tm),
           thredds_path=paste0("https://thredds.nci.org.au/thredds/dodsC/zv2/agcd/v2-0-3/precip/total/r001/01month/agcd_v2_precip_total_r001_monthly_",yr,".nc"))






  dat.sampled <- list()
  thredds_path.i.check <- "x"
  for(i in 1:nrow(dat.x)){


    bbox <- sf::st_bbox(dat.x[i,])

    thredds_path.i <- dat.x$thredds_path[i]

    #load nc. check that if its already been loaded. if not use last loaded nc
    if(thredds_path.i != thredds_path.i.check)  nc_conn <- tidync::tidync(thredds_path.i)

    #get the time in the format of the nc
    datetimeutc_nc <- nc_conn$transforms$time %>%
      dplyr::filter(index==dat.x$mnth)


    nc_filt <- nc_conn %>%
      tidync::activate("precip") %>%

      #filter roughly by lat long and to exact time
      tidync::hyper_filter(time= time==datetimeutc_nc$time,
                           lat = lat >= bbox[2]-1 & lat <= bbox[4]+1,
                           lon = lon >= bbox[1]-1 & lon <= bbox[3]+1
      ) %>%

      tidync::hyper_tibble() %>%

      #select required columns for conversion to raster (lon and lat first)
      dplyr::select(lon,lat,"precip")

    nc_filt$lon <- as.numeric(nc_filt$lon)
    nc_filt$lat <- as.numeric(nc_filt$lat)


    #ideally would turn this into a raster. But it terra::rast get uneven grid error. So will use sf sampling method instead
    #find all grid centres within buffer_m of line
    x <- sf::st_as_sf(nc_filt,coords=c("lon","lat"),crs=4326)
    x.within <- sf::st_is_within_distance(x %>% sf::st_transform(3112),dat.x %>% sf::st_transform(3112),dist = buffer_m)
    x <- x %>%
      dplyr::filter(lengths(x.within)!=0)

    z <- x %>%
      sf::st_drop_geometry() %>%
      dplyr::summarise(agcd_precip_mean=mean(precip),
                       agcd_precip_min=min(precip),
                       agcd_precip_max=max(precip),
                       agcd_precip_median=median(precip),
                       agcd_date=datetimeutc_nc$timestamp)

    dat.sampled[[i]] <- z


    #check if a new thredds path is needed on the next iteration. if not just use previous iteration to save time
    thredds_path.i.check <- thredds_path.i

  }

  z.all <- do.call(rbind,dat.sampled)

  return(z.all)




}
