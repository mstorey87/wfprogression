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


  #ensure time column is posxct
  checkmate::assert(unique(stringr::str_detect(class(dat$tm),"POSIX")),"Error: Time column must be posixct, timezone UTC")
  checkmate::assert(lubridate::tz(dat$tm)=="UTC","Error: Time column must be posixct, timezone UTC (all caps)")


  dat.x <- dat %>%
    sf::st_transform(4326)%>%
    dplyr::mutate(yr=lubridate::year(tm),
                  mnth=lubridate::month(tm),
                  thredds_path=paste0("https://thredds.nci.org.au/thredds/dodsC/zv2/agcd/v2-0-3/precip/total/r001/01month/agcd_v2_precip_total_r001_monthly_",yr,".nc"),
                  input_rowid=dplyr::row_number())


  #arrange sorting by thredds_path, to save loading each thredds path multiple times
  unq_paths <- unique(dat.x$thredds_path)




  dat.sampled.all <- list()

  for(i in 1:length(unq_paths)){

    dat.i <- dat.x %>% dplyr::filter(thredds_path==unq_paths[i])
    mnths.i <- unique(dat.i$mnth)




    nc_conn <- tidync::tidync(unq_paths[i])

    #get the time in the format of the nc. Index is month
    datetimeutc_nc.all <- nc_conn$transforms$time %>%
      dplyr::filter(index==mnths.i) %>%
      data.frame()


    #loop through each month
    dat.sampled <- list()
    for(xi in 1:nrow(datetimeutc_nc.all)){

      #filter nc data to one month
      datetimeutc_nc <- datetimeutc_nc.all[xi,]

      #filter sample data to one month
      dat.xi <- dat.i %>%
        dplyr::filter(mnth==datetimeutc_nc$index)

      #put a check here in case months don't match for some reason
      checkmate::assert(lubridate::month(datetimeutc_nc$timestamp)==unique(dat.xi$mnth),"Error: NC month doesn't match sample data month")


      bbox <- sf::st_bbox(dat.xi)



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
      x <- sf::st_as_sf(nc_filt,coords=c("lon","lat"),crs=4326) %>%
        sf::st_transform(3112)

      #buffer input lines to sample precip points
      dat.xi.buff <- dat.xi %>% sf::st_transform(3112) %>%
        sf::st_buffer(buffer_m) %>%
        dplyr::select(input_rowid)

      #intersect buffers and summarise results
      suppressWarnings({
        s <- sf::st_intersection(x,dat.xi.buff)
        })


      z <- s %>%
        sf::st_drop_geometry() %>%
        dplyr::group_by(input_rowid) %>%
        dplyr::summarise(agcd_precip_mean=mean(precip),
                         agcd_precip_min=min(precip),
                         agcd_precip_max=max(precip),
                         agcd_precip_median=median(precip),
                         agcd_date=datetimeutc_nc$timestamp)










         dat.sampled[[xi]] <- z
    }

    dat.sampled.all[[i]] <- do.call(rbind,dat.sampled)

  }

  dat.sampled.all.2 <- do.call(rbind,dat.sampled.all) %>%
    dplyr::arrange(input_rowid)

  return(dat.sampled.all.2)




}
