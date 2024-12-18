dat <- readRDS("D:\\temp\\lines.rds")
time_col_utc <- "start_time"
barraid="C2"
varnames=c("sfcWind","tas")

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
  for(v in varnames){

    res.list <- list()

    for (i in 1:length(dat.split)) {
      print(i)
      #data for current iteration (all associated with same nc)
      dat <- dat.split[[i]] %>%
        dplyr::mutate(ncpath=wfprogression::fire_barra_path(datetimeutc=time_round,barraid=barraid,varname=v)) %>%
        dplyr::select(id,time_round,ncpath)

      #connect to nc
      nc_conn <- tidync::tidync(unique(dat$ncpath))

      #create a list of sf object associated with current nc, each with unique times
      dat.split.time <- split(dat,dat$time_round)

      #for each sf object of the same datetime, run the barra nc sampling function
      res <- purrr::map(dat.split.time,~wfprogression::fire_barra_sample(nc_conn,unique(.x$time_round),.x,v))

      res.list[[i]] <-  do.call(rbind,res)
    }


    res.all <- do.call(rbind,res.list)

    return(res.all)


  }



  #run loop sampling function for each variable
  res.wind <- fn_loop_sample(dat.drops.split,"sfcWind","C2")
  res.tas <- fn_loop_sample(dat.drops.split,"tas","C2")
  res.hurs <- fn_loop_sample(dat.drops.split,"hurs","C2")
  res.vas <- fn_loop_sample(dat.drops.split,"vas","C2")
  res.uas <- fn_loop_sample(dat.drops.split,"uas","C2")






}
