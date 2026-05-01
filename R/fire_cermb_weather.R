#' Sample AWS BOM date from CERMB database
#'
#' @param sf_point sf point used to find nearest BOM AWS station
#' @param datetime vector of posixct. Single or multiple times.
#' @param buffer_dist_km distance in km around sample point to find weather stations
#' @param dbpassword password for the CERMB database
#'
#' @returns An `sf` object with AWS weather
#' @export
#'
#' @examples
#' #
fire_cermb_weather <- function(sf_point,sf_point_id,datetime,buffer_dist_km,dbpassword){

  # Check inputs
  checkmate::assert(inherits(datetime, "POSIXct"), "Error: times must be POSIXct")
  checkmate::assert(inherits(sf_point, "sf"))
  checkmate::assert(!is.na(sf::st_crs(sf_point)), "sf_point must have a valid CRS")
  checkmate::assert( all(lubridate::minute(datetime)==0) & all(lubridate::second(datetime)==0),"Error: Please round times to hourly (use lubridate::round_date)")



  #round date - superseded now as I apply checkmate to ensure dates are rounded before function
  datetime_round <- datetime#lubridate::round_date(datetime,"hour")
  #message(glue::glue("{datetime} rounded to {datetime_round} ({format(datetime,'%Z')})."))



  DB <- DBI::dbConnect(RPostgres::Postgres(),

                       dbname = "cermb_weather",

                       user = "mstorey",

                       password = dbpassword,

                       host = "charus.ad.uow.edu.au",

                       port = 5432)

  on.exit(DBI::dbDisconnect(DB), add = TRUE)

  #define time zones for each state
  get_aus_timezone <- function(state) {

    tz_map <- c(
      "NSW" = "Australia/Sydney",
      "ACT" = "Australia/Sydney",
      "VIC" = "Australia/Melbourne",
      "TAS" = "Australia/Hobart",
      "QLD" = "Australia/Brisbane",
      "SA"  = "Australia/Adelaide",
      "NT"  = "Australia/Darwin",
      "WA"  = "Australia/Perth"
    )

    # Return timezone if matched, otherwise NA
    tz_map[state]
  }


  #get station locations
  sf_stations <- sf::st_read(dsn=DB,layer = "stations")

  #add timezone for later
  sf_stations <- sf_stations %>% dplyr::mutate(tzone=get_aus_timezone(state))

  #get the list of aws stations, and convert to sf
  aws_stations <- DBI::dbGetQuery(DB, "SELECT * FROM aws_stations")%>%
    dplyr::left_join(sf_stations %>% dplyr::select(station,tzone),by="station") %>%
    sf::st_as_sf()

  #transform to same as stations
  sf_point <- sf_point %>%
    sf::st_transform(sf::st_crs(sf_stations))

  #get broad bounding box for stations to search
  sf_bbox <- sf_point %>%
    sf::st_transform(3112) %>%
    sf::st_buffer(buffer_dist_km*1000) %>%
    sf::st_transform(sf::st_crs(sf_stations))

  #filter spatially
  sf_stations_filter <- aws_stations  %>%
    sf::st_filter(sf_bbox,.predicate=sf::st_intersects)


  station_list <- paste0(sf_stations_filter$station,collapse=",")

  #when datetime is vector, get min and max for initial filter by date
  date_std_x <- as.Date(datetime)
  min_date_std_x <- min(date_std_x)
  max_date_std_x <- max(date_std_x)
  #get the data for those stations for date first
  dat_aws <- DBI::dbGetQuery(DB,glue::glue("SELECT * FROM aws WHERE date_std >= '{min_date_std_x-1}' AND date_std <= '{max_date_std_x+1}' and station IN ({station_list})"))
  #dat_aws <- DBI::dbGetQuery(DB,glue::glue("SELECT * FROM aws WHERE date_std = '{date_std_x}' AND hour_std = '{hour_std_x}' and min_std = '{min_std_x}' and station IN ({station_list})"))

  #create a datetime utc column
  dat_aws <- dat_aws %>%
    dplyr::left_join(sf_stations_filter %>% sf::st_drop_geometry() %>% dplyr::select(station,tzone),by="station")

  #do this separately for each state as posixt column can't have multiple time zones
  dat_aws_split <- split(dat_aws,dat_aws$tzone)
  for(i in 1:length(dat_aws_split)){
    dat_aws_split[[i]] <- dat_aws_split[[i]] %>%
      dplyr::mutate(datetime=as.POSIXct(paste0(date_local," ",hour_local,":",min_local),format="%Y-%m-%d %H:%M",tz="Australia/Sydney"),
                    datetimeutc=lubridate::with_tz(datetime,"UTC"),
                    datetime=format(datetime,format="%Y-%m-%d %H:%M:%S"))

  }

  dat_aws <- do.call(rbind,dat_aws_split)%>%
    dplyr::left_join(sf_stations_filter %>% dplyr::select(station),by="station") %>%
    sf::st_as_sf()


  #get closest weather for each unique time
  res.xi <- list()
  for(xi in 1:length(datetime_round)){
    #filter by time and remove

    datetimeutc1=lubridate::with_tz(datetime_round[xi],tz="UTC")
    #remove any missing rows
    dat_aws_xi <- dat_aws %>%
      dplyr::filter(datetimeutc == datetimeutc1) %>%
      #don't include if missing any of these variables
      dplyr::filter(!is.na(temperature) & !is.na(relhumidity) & !is.na(windspeed) & !is.na(winddir))


    #get nearest station
    #dat_aws_xi <- dat_aws_xi[sf::st_nearest_feature(sf_point,dat_aws_xi),]

    #get distance
    dat_aws_xi$distance_km <- as.numeric(sf::st_distance(dat_aws_xi,sf_point))/1000

    #filter by user defined buffer distance

    res.xi[[xi]] <- dat_aws_xi


  }

  dat_aws_res <- do.call(rbind,res.xi)
  dat_aws_res$sf_point_id <- sf_point_id


  return(dat_aws_res)
}

