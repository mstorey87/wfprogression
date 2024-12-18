# dat <- readRDS("D:\\temp\\lines.rds") %>%
#   head(50) %>%
#   sf::st_transform(3112) %>%
#   sf::st_segmentize(dfMaxLength = 100) %>%
#   dplyr::mutate(line_id=dplyr::row_number()) %>%
#   sf::st_cast("POINT")
# time_col_utc <- "start_time"
# barraid="C2"
# varnames=c("sfcWind","tas")


#' Sample an sf object for BARRA data
#'
#' @param dat sf object
#' @param time_col_utc character string of name of datetime column (posix, utc)
#' @param barraid R2 (12 km product) or C2 (~4km product)
#' @param varnames vector of barra variable names. BARRA variable name e.g. sfcWind (surface wind), tas (temperature), hurs (RH), vas and uas (wind components).  http://www.bom.gov.au/research/publications/researchreports/BRR-067.pdf
#'
#' @return sample sf object
#' @export
#'
#' @examples
#' #
fire_barra_sample_all <- function(dat,time_col_utc,barraid="C2",varnames){
  #add time column with standard name
  dat$time <- dat[[time_col_utc]]

  #ensure time column is posxct
  checkmate::assert(unique(stringr::str_detect(class(dat$time),"POSIX")),"Error: Time column must be posixct, timezone UTC")
  checkmate::assert(lubridate::tz(dat$start_time)=="utc","Error: Time column must be posixct, timezone UTC")


  dat <- dat %>%
    #add local time posix, convert to utc, round utc time (to match barra times)
    dplyr::mutate(time_round=lubridate::round_date(time,"hour"),
                  id=dplyr::row_number()) %>%
    sf::st_transform(4326)

  print("Data not yet available after March 2024, removing records after this date")
  dat <- dat %>%
    #currently data is only available until march 2024, but this will be updated.
    #so filter out any dates after march
    dplyr::filter(time_round < as.POSIXct("2024-04-01 00:00:00",tz="utc"))


  #add columns of barra paths for each variable
  dat <- dat %>%
    dplyr::mutate(yrmnth=format(time_round,format = "%Y%m"))

  for(v in varnames){
    dat[[v]] <- wfprogression::fire_barra_path(datetimeutc=dat$time_round,barraid=barraid,varname=v)
  }



  #create a list of sf objects, split by year and month, because nc files are monthly
  dat.split <- split(dat,dat$yrmnth)



  #this functions loops the main barra sampling function.
  # you can run this function with a different barra variable each time
  #data need a time_round column
  res.all.vars <- list()
  for(v in varnames){
   # print(v)

    res.list <- list()

    for (i in 1:length(dat.split)) {
      print(paste0(v," ",names(dat.split)[i]))
      #data for current iteration (all associated with same nc)
      dat.i <- dat.split[[i]] %>%
        dplyr::mutate(ncpath=wfprogression::fire_barra_path(datetimeutc=time_round,barraid=barraid,varname=v)) %>%
        dplyr::select(id,time_round,ncpath)

      #connect to nc
      nc_conn <- tidync::tidync(unique(dat.i$ncpath))

      #create a list of sf object associated with current nc, each with unique times
      dat.split.time <- split(dat.i,dat.i$time_round)

      #for each sf object of the same datetime, run the barra nc sampling function
      res <- purrr::map(dat.split.time,~wfprogression::fire_barra_sample(nc_conn,unique(.x$time_round),.x,v))

      res.list[[i]] <-  do.call(rbind,res) %>%
        sf::st_drop_geometry() %>%
        dplyr::select(-ncpath)
    }


    res.all.vars[[v]] <- do.call(rbind,res.list)

  }



  #run loop sampling function for each variable
  #res.all <- do.call(rbind,res.all.vars)



  res.all <- purrr::reduce(res.all.vars, dplyr::left_join, by = c('id','time_round')) %>%
    dplyr::left_join(dat %>% dplyr::select(id),by="id") %>%
    sf::st_as_sf() %>%
    dplyr::arrange(id)


  return(res.all)

}
