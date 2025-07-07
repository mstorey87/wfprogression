#' Sample LFMC or flammability from ANU THREDDS
#'
#' Downloads and samples live fuel moisture content (LFMC) or flammability data
#' from the ANU THREDDS server for a given time and spatial feature.
#'
#' @param datetimeutc POSIXct datetime in UTC timezone.
#' @param sf_data An `sf` object (point or polygon).
#' @param varname Variable to sample: `"fmc"` (live fuel moisture) or `"flam"` (flammability).
#' @param allcells Logical. If `TRUE`, return a data frame of all raster cells intersecting `sf_data`. If `FALSE`, return a single summarised value.
#' @param extract_fun Function to summarise raster values (`"mean"` by default).
#'
#' @return An `sf` object input row id, extracted values, percent of cells that were NA.
#' @export
#'
#' @examples
#' library(sf)
#' # Create a line near Canberra, Australia for 2019
#' my_line <- st_sfc(
#'   st_linestring(rbind(c(149.1, -35.3), c(149.2, -35.25))),
#'   crs = 4326
#' )
#' my_sf <- st_sf(id = 1, geometry = my_line)
#'
#' # Sample FMC for 1 Jan 2019
#' result <- fire_anu_sample(
#'   datetimeutc = as.POSIXct("2019-01-01 00:00:00", tz = "UTC"),
#'   sf_data = my_sf,
#'   varname = "fmc",
#'   allcells = FALSE
#' )
#' print(result)
fire_anu_sample <- function(datetimeutc, sf_data, varname, allcells = FALSE, extract_fun = "mean") {
  # Validate input
  checkmate::assert(
    stringr::str_detect(class(datetimeutc)[1], "POSIXct"),
    "Error: datetimeutc must be POSIXct"
  )

  # Ensure datetime is UTC
  start_time <- lubridate::with_tz(datetimeutc, tz = "UTC")

  # Get bounding box in WGS84
  bbox <- sf::st_bbox(sf::st_transform(sf_data, 4326))

  # Parse year and date
  myyear <- lubridate::year(start_time)
  mydate <- as.Date(start_time)

  # Match variable name
  varname2 <- switch(
    varname,
    "fmc" = "lfmc_median",
    "flam" = "flammability",
    stop("Invalid varname: must be 'fmc' or 'flam'")
  )

  # Build NetCDF path
  ncpath <- paste0(
    "https://thredds.nci.org.au/thredds/dodsC/ub8/au/FMC/mosaics/",
    varname, "_c6_", myyear, ".nc"
  )

  # Open NetCDF
  nc_conn <- tidync::tidync(ncpath)

  # Find most recent date before requested date
  nc_dates <- as.Date(nc_conn$transforms$time$timestamp)
  valid_dates <- nc_dates[nc_dates < mydate]

  if (length(valid_dates) == 0) {
    # Try previous year if no valid date found
    ncpath <- paste0(
      "https://thredds.nci.org.au/thredds/dodsC/ub8/au/FMC/mosaics/",
      varname, "_c6_", myyear - 1, ".nc"
    )
    nc_conn <- tidync::tidync(ncpath)
    nc_dates <- as.Date(nc_conn$transforms$time$timestamp)
    valid_dates <- nc_dates[nc_dates < mydate]
  }

  if (length(valid_dates) == 0) stop("No suitable date found in NetCDF files.")

  closest_date <- tail(valid_dates, 1)
  datediff <- as.numeric(difftime(mydate, closest_date, units = "days"))

  message("Note: extracted date is ", datediff, " days before requested date.")


  # Match NetCDF time index for this date
  datetimeutc_nc <- nc_conn$transforms$time %>%
    dplyr::filter(timestamp == as.character(closest_date)) %>%
    dplyr::pull(time)

  # Extract relevant raster data
  raster_df <- nc_conn %>%
    tidync::activate(varname2) %>%
    tidync::hyper_filter(
      time = time == datetimeutc_nc,
      latitude = latitude >= bbox["ymin"] - 1 & latitude <= bbox["ymax"] + 1,
      longitude = longitude >= bbox["xmin"] - 1 & longitude <= bbox["xmax"] + 1
    ) %>%
    tidync::hyper_tibble() %>%
    dplyr::select(longitude, latitude, dplyr::all_of(varname2))

  # Convert to raster
  r <- terra::rast(raster_df)
  terra::crs(r) <- "EPSG:4326"

  # Extract values
  vals <- terra::extract(r, sf_data, raw = FALSE, ID = FALSE)

  if (isTRUE(allcells)) {
    sf_data[[varname]] <- list(vals)
    result <- sf_data
  } else {

    #calculate how many cells that cross line are NA
    prc_NA <- sum(is.na(vals[,1]))/length(vals[,1])*100

    #extract value summarised
    vals <- terra::extract(r, sf_data, fun = extract_fun, ID = FALSE,na.rm=T)
    names(vals)=paste0(names(vals),"_",extract_fun)


    result <- cbind(sf_data, vals,prc_NA)

    #give more meaningful names
    names(result)[3]=paste0(names(result)[2],"_",names(result)[3])
  }

  return(result)
}



#' #' Sample LFMC or flammability from ANU THREDDS
#' #'
#' #' @param datetimeutc posix datetime with utc timezone
#' #' @param sf_data sf object
#' #' @param varname variable to sample. "fmc" (live fuel moisture content) or "flam" flammability
#' #' @param allcells return a dataframe of all cells that intersect sf_data, or single summarised value
#' #' @param extract_fun how to summarise sample
#' #'
#' #' @returns sf object
#' #' @export
#' #'
#' #' @examples
#' fire_anu_sample <- function(datetimeutc,sf_data,varname,allcells=F,extract_fun="mean"){
#'
#'   #download from ANU thredds
#'   #https://geonetwork.nci.org.au/geonetwork/srv/eng/catalog.search#/metadata/f1104_2906_7276_3004
#'
#'   checkmate::assert(stringr::str_detect(class(datetimeutc)[1],"POSIXct"),"Error: times must be posixct")
#'   start_time <- lubridate::with_tz(datetimeutc,tz="utc")
#'
#'   #get bounding box of all point for filtering data
#'   bbox <- sf::st_bbox(sf_data %>% sf::st_transform(4326))
#'
#'   myyear <- lubridate::year(datetimeutc)
#'
#'   mydate <- as.Date(datetimeutc)
#'   #recalc varname to use for filtering nc
#'
#'   if(varname=="fmc") varname2="lfmc_median"
#'   if(varname=="flam") varname2="flammability"
#'
#'
#'   ncpath <- paste0("https://thredds.nci.org.au/thredds/dodsC/ub8/au/FMC/mosaics/",varname,"_c6_",myyear,".nc")
#'   nc_conn <- tidync::tidync(ncpath)
#'   #get the most recent date before mydate from the nc dates
#'   nc_timestamps <- nc_conn$transforms$time$timestamp %>%
#'     as.Date()
#'   nc_timestamps <- nc_timestamps[nc_timestamps < mydate]
#'   nc_timestamps <- nc_timestamps[length(nc_timestamps)]
#'
#'   #if date not found, try in file from year prior
#'   if(length(nc_timestamps)==0){
#'
#'     ncpath <- paste0("https://thredds.nci.org.au/thredds/dodsC/ub8/au/FMC/mosaics/",varname,"_c6_",myyear-1,".nc")
#'     nc_conn <- tidync::tidync(ncpath)
#'     #get the most recent date before mydate from the nc dates
#'     nc_timestamps <- nc_conn$transforms$time$timestamp %>%
#'       as.Date()
#'     nc_timestamps <- nc_timestamps[nc_timestamps < mydate]
#'     nc_timestamps <- nc_timestamps[length(nc_timestamps)]
#'
#'
#'
#'
#'
#'
#'   }
#'
#'   #check if date is close enough, with says a few weeks
#'   datediff=as.numeric(difftime(as.Date(mydate),as.Date(nc_timestamps),units = "days"))
#'   if(datediff > 20){
#'     print(paste0("exctrated date is ", datediff, " days prior to your date"))
#'   }
#'
#'
#'  #now extract the time that matches the date found above
#'   #this just ensures we get time in the appropriate format to filter with in the next step
#'   datetimeutc_nc <- nc_conn$transforms$time %>%
#'     dplyr::filter(timestamp==as.character(nc_timestamps)) %>% .$time
#'
#'
#'   #active nc variable
#'   b.local <- nc_conn  %>%
#'     tidync::activate(varname2) %>%
#'
#'     #filter roughly by lat long and to exact time
#'     tidync::hyper_filter(time= time==datetimeutc_nc,
#'                          latitude = latitude >= bbox[2]-1 & latitude <= bbox[4]+1,
#'                          longitude = longitude >= bbox[1]-1 & longitude <= bbox[3]+1) %>%
#'
#'
#'
#'
#'
#'     tidync::hyper_tibble() %>%
#'
#'     #convert time and filter again. More of a check that the time conversion I did above produces the same results
#'     #this could be excluded
#'     # dplyr::mutate(dtutc=as.POSIXct(time*24*60*60,origin=ncorigin,tz="UTC")) %>%
#'     # dplyr::mutate(dtutc=as.POSIXct(time,tz="UTC")) %>%
#'     # dplyr::filter(dtutc==datetimeutc)%>%
#'
#'     #select required columns for conversion to raster (lon and lat first)
#'     dplyr::select(longitude,latitude,dplyr::all_of(varname2))
#'
#'   #create raster
#'   r <- terra::rast(b.local)
#'   terra::crs(r) <- "epsg:4326" #not sure if its GDA94 or WGS84
#'
#'   #extract raster values if point type
#'   # if(max(str_detect(st_geometry_type(sf_data),"POINT"))==1){
#'   #   res <- terra::extract(r,sf_data,ID=F)
#'   #}
#'
#'   #extract raster values if polygon type. fun argument ignorned if sampling with points
#'   # if(max(str_detect(st_geometry_type(sf_data),"POLY"))==1){
#'   #
#'   #if not returning all cell, return single row with mean
#'   if(allcells==F){
#'     res <- terra::extract(r,sf_data,fun=extract_fun,ID=F)
#'     res <- cbind(sf_data,res)
#'
#'   }
#'
#'   #if returning all cells, add nested list
#'   if(allcells==T){
#'     res <- terra::extract(r,sf_data,raw=F,ID=F)
#'     sf_data[[varname]] <- list(res)
#'     res <- sf_data
#'   }
#'
#'   #}
#'
#'
#'
#'
#'   return(res)
#'
#'
#'
#'
#' }
