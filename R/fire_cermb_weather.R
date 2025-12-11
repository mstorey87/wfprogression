#' Sample AWS BOM date from CERMB database
#'
#' @param sf_point sf point used to find nearest BOM AWS station
#' @param datetimelocal posixct object in local timezone
#' @param dbpassword password for the CERMB database
#'
#' @returns An `sf` object with AWS weather
#' @export
#'
#' @examples
#' #
fire_cermb_weather <- function(sf_point,datetimelocal,dbpassword){

  # Check inputs
  checkmate::assert(inherits(datetimelocal, "POSIXct"), "Error: times must be POSIXct")
  checkmate::assert(inherits(sf_point, "sf"))
  checkmate::assert(!is.na(sf::st_crs(sf_point)), "sf_point must have a valid CRS")


  #round date
  datetimelocal_round <- lubridate::round_date(datetimelocal,"hour")
  message(glue::glue("{datetimelocal} rounded to {datetimelocal_round} ({format(datetimelocal,'%Z')})."))



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
      "ACT" = "Australia/Canberra",
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
    sf::st_buffer(500000) %>%
    sf::st_transform(sf::st_crs(sf_stations))

  #filter spatially
  sf_stations_filter <- aws_stations  %>%
    sf::st_filter(sf_bbox,.predicate=sf::st_intersects)


  station_list <- paste0(sf_stations_filter$station,collapse=",")

  date_std_x <- as.Date(datetimelocal)
  #get the data for those stations
  dat_aws <- DBI::dbGetQuery(DB,glue::glue("SELECT * FROM aws WHERE date_std >= '{date_std_x-1}' AND date_std <= '{date_std_x+1}' and station IN ({station_list})"))
  #dat_aws <- DBI::dbGetQuery(DB,glue::glue("SELECT * FROM aws WHERE date_std = '{date_std_x}' AND hour_std = '{hour_std_x}' and min_std = '{min_std_x}' and station IN ({station_list})"))

  #create a datetime utc column
  dat_aws <- dat_aws %>%
    dplyr::left_join(sf_stations_filter %>% sf::st_drop_geometry() %>% dplyr::select(station,tzone),by="station")

  #do this separately for each state as posixt column can't have multiple time zones
  dat_aws_split <- split(dat_aws,dat_aws$tzone)
  for(i in 1:length(dat_aws_split)){
    dat_aws_split[[i]] <- dat_aws_split[[i]] %>%
      dplyr::mutate(datetimelocal=as.POSIXct(paste0(date_local," ",hour_local,":",min_local),format="%Y-%m-%d %H:%M"),
                    datetimeutc=lubridate::with_tz(datetimelocal,"UTC"),
                    datetimelocal=format(datetimelocal,format="%Y-%m-%d %H:%M:%S"))

  }

  dat_aws <- do.call(rbind,dat_aws_split)

    #filter by time and remove

  datetimeutc1=lubridate::with_tz(datetimelocal_round,tz="UTC")
  #remove any missing rows
  dat_aws <- dat_aws %>%
    dplyr::filter(datetimeutc==datetimeutc1) %>%
    #don't include if missing any of these variables
    dplyr::filter(!is.na(temperature) & !is.na(relhumidity) & !is.na(windspeed) & !is.na(winddir))

  #reduce station list
  sf_stations_filter2 <- sf_stations_filter %>%
    dplyr::filter(station %in% dat_aws$station)

  #get nearest station
  sf_stations_filter2 <- sf_stations_filter2[sf::st_nearest_feature(sf_point,sf_stations_filter2),]

  #filter observations
  dat_aws <- dat_aws %>%
    dplyr::filter(station %in% sf_stations_filter2$station) %>%
    dplyr::left_join(sf_stations_filter2 %>% dplyr::select(station),by="station") %>%
    sf::st_as_sf()


  dat_aws$distance_km <- as.numeric(sf::st_distance(dat_aws,sf_point))/1000

  return(dat_aws)
}

