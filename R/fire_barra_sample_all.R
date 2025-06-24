#' Sample an sf object for BARRA data.
#'
#' @param dat sf object, points lines or polygons
#' @param time_col_utc name string of datetime column (posix, utc)
#' @param barraid R2 (12 km BARRA product) or C2 (~4km BARRA product)
#' @param varnames Vector of barra variable names. BARRA variable name e.g. sfcWind (surface wind), tas (temperature), hurs (RH), vas and uas (wind components).  http://www.bom.gov.au/research/publications/researchreports/BRR-067.pdf
#' @param timestep Timestep of data to search for in BARRA. Will accept "hourly", "daily" or "monthly"
#' @param extract_fun Function to summarise sampled variable when it has been sampled with a line or polygon. "mean","sum","min" or "max" accepted
#'
#' @return sample sf object
#' @export
#'
#' @examples
#' #
fire_barra_sample_all <- function(dat,time_col_utc,barraid="C2",varnames,timestep="hourly",extract_fun="mean"){
  #add time column with standard name
  dat$time <- dat[[time_col_utc]]


  #ensure time column is posxct
  checkmate::assert(unique(stringr::str_detect(class(dat$time),"POSIX")),"Error: Time column must be posixct, timezone UTC")
  checkmate::assert(lubridate::tz(dat$time)=="UTC","Error: Time column must be posixct, timezone UTC (all caps)")

  dat$input_rowid <- seq_len(nrow(dat))


  dat <- dat %>%
    #add local time posix, convert to utc, round utc time (to match barra times)
    dplyr::mutate(time_round=lubridate::round_date(time,"hour")) %>%
    sf::st_transform(4326)

  # print("Data not yet available after March 2024, removing records after this date")
  # dat <- dat %>%
  #   #currently data is only available until march 2024, but this will be updated.
  #   #so filter out any dates after march
  #   dplyr::filter(time_round < as.POSIXct("2024-04-01 00:00:00",tz="UTC"))


  #add columns of barra paths for each variable
  dat <- dat %>%
    dplyr::mutate(yrmnth=format(time_round,format = "%Y%m"))

  # for(v in varnames){
  #   dat[[v]] <- wfprogression::fire_barra_path(datetimeutc=dat$time_round,barraid=barraid,varname=v)
  # }



  #check currently available barra data
  #check one random path. All should be the same.
  #make path match input timestep, but variable to check can default to sfcWind
  checkmate::assert(timestep %in% c("hourly","daily","monthly"),"Error: timestep must be hourly, daily or monthly")
  if(timestep == "daily"){
    catalog_path <- "https://thredds.nci.org.au/thredds/catalog/ob53/output/reanalysis/AUST-04/BOM/ERA5/historical/hres/BARRA-C2/v1/day/sfcWind/latest/catalog.html"
  }
  if(timestep=="hourly"){
    catalog_path <- "https://thredds.nci.org.au/thredds/catalog/ob53/output/reanalysis/AUST-04/BOM/ERA5/historical/hres/BARRA-C2/v1/1hr/sfcWind/latest/catalog.html"
  }
  if(timestep=="monthly"){
    catalog_path <- "https://thredds.nci.org.au/thredds/catalog/ob53/output/reanalysis/AUST-04/BOM/ERA5/historical/hres/BARRA-C2/v1/mon/sfcWind/latest/catalog.html"
  }

  response <- httr::GET(catalog_path)
  content_xml <- httr::content(response, as = "text")

  # Collapse to one string (if it's a vector of lines)
  html_text <- paste(content_xml, collapse = "\n")

  # Use regular expression to find .nc filenames
  nc_files <- regmatches(html_text, gregexpr("[^\">/]+\\.nc", html_text))[[1]]
  nc_files <- nc_files[stringr::str_detect(nc_files,"BARRA")]

  #get year Month of each file
  nc_yearmonth <- as.numeric(substr(nc_files,nchar(nc_files)-8,nchar(nc_files)-3))

  min.nc <- as.character(min(nc_yearmonth))
  max.nc <- as.character(max(nc_yearmonth))

  mes.1 <- paste0("BARRA currently available ",month.name[as.numeric(substr(min.nc,5,6))]," ",substr(min.nc,1,4)," to ",month.name[as.numeric(substr(max.nc,5,6))]," ",substr(max.nc,1,4))

  print(mes.1)
  print("Outside dates will return NA")








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
      yr.mnth.dat <- unique(names(dat.split)[i])
      mes.2 <- paste0(month.name[as.numeric(substr(yr.mnth.dat,5,6))]," ",substr(yr.mnth.dat,1,4))
      print(paste0("sampling ",v," ",mes.2))
      #data for current iteration (all associated with same nc)
      dat.i <- dat.split[[i]] %>%
        dplyr::mutate(ncpath=wfprogression::fire_barra_path(datetimeutc=time_round,barraid=barraid,timestep = timestep,varname=v)) %>%
        dplyr::select(input_rowid,time_round,ncpath)

      #check is sample data is within available barra range

      if(as.numeric(yr.mnth.dat) >= as.numeric(min.nc) & as.numeric(yr.mnth.dat) <= as.numeric(max.nc)){





      #connect to nc
      nc_conn <- tidync::tidync(unique(dat.i$ncpath))

      #create a list of sf object associated with current nc, each with unique times
      dat.split.time <- split(dat.i,dat.i$time_round)

      #for each sf object of the same datetime, run the barra nc sampling function
      res <- purrr::map(dat.split.time,~wfprogression::fire_barra_sample(nc_conn,unique(.x$time_round),.x,v,extract_fun=extract_fun))

      res.list[[i]] <-  do.call(rbind,res) %>%
        sf::st_drop_geometry() %>%
        dplyr::select(-ncpath)
    }else{
      print(paste0("BARRA currently unavailable for ",mes.2))
      res.list[[i]] <- NA
      }
    }


    res.all.vars[[v]] <- do.call(rbind,res.list)

  }



  #run loop sampling function for each variable
  #res.all <- do.call(rbind,res.all.vars)



  res.all <- purrr::reduce(res.all.vars, dplyr::left_join, by = c('input_rowid','time_round')) %>%
    dplyr::left_join(dat %>% dplyr::select(input_rowid),by="input_rowid") %>%
    sf::st_as_sf() %>%
    dplyr::arrange(input_rowid)


  return(res.all)

}
