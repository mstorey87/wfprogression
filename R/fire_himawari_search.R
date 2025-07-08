#' Search for Himawari-8/9 satellite data on NCI THREDDS
#'
#' @description
#' This function searches the Bureau of Meteorology (BOM) Himawari 8 and 9 satellite image archives
#' hosted on the NCI THREDDS server: \url{https://geonetwork.nci.org.au/geonetwork/srv/eng/catalog.search#/metadata/f8433_0020_1861_5916}.
#'
#' It finds file paths for imagery covering the specified fire bounding box and time range.
#' Himawari images are available every 10 minutes; use a larger timestep to reduce the volume of results.
#' For recent fires (within ~3 weeks), near real-time (NRT) data will also be searched automatically.
#'
#' The output is a data frame listing the file paths for all available band × time combinations.
#'
#' @param fire_bbox An `sf` polygon that defines the spatial search area (e.g., your fire boundary or bounding box).
#' @param start_time A `POSIXct` date-time (local time) marking the beginning of the search window. Converted to UTC internally.
#' @param end_time A `POSIXct` date-time (local time) marking the end of the search window. Converted to UTC internally.
#' @param timestep_minutes Timestep in minutes for selecting imagery (must be a multiple of 10). E.g., `10` for every image or `60` for hourly checks.
#'
#' @return A data frame with available file names, UTC and local times, download URLs, bands, and satellite names.
#' If no data is found, a message is printed and `NULL` is returned.
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' # Example: define a small bounding box somewhere in Australia
#' my_bbox <- sf::st_as_sfc(sf::st_bbox(c(xmin = 150, ymin = -35, xmax = 151, ymax = -34), crs = 4326)) |> sf::st_sf()
#'
#' # Define time window for search (last month, hourly)
#' my_start <- as.POSIXct("2023-11-01 00:00:00", tz = "Australia/Sydney")
#' my_end <- as.POSIXct("2023-11-01 12:00:00", tz = "Australia/Sydney")
#'
#' # Run search
#' dat_himawari <- fire_himawari_search(
#'   fire_bbox = my_bbox,
#'   start_time = my_start,
#'   end_time = my_end,
#'   timestep_minutes = 60
#' )
#'
#' head(dat_himawari)
#' }
fire_himawari_search <- function(fire_bbox,
                                 start_time,
                                 end_time,
                                 timestep_minutes = 10) {

  checkmate::assert_class(start_time,classes = "POSIXct")
  checkmate::assert_class(end_time,classes = "POSIXct")

  nci_path <- "https://thredds.nci.org.au/thredds/catalog/ra22/satellite-products/arc/obs/himawari-ahi/fldk/v1-0"

  # Round input times to nearest multiple of timestep_minutes (max 60)
  timestep_minutes_round <- ifelse(timestep_minutes > 60, 60, timestep_minutes)

  # Convert to UTC and round
  start_time <- lubridate::with_tz(start_time, tz = "UTC")
  start_time <- lubridate::round_date(start_time, unit = paste0(timestep_minutes_round, " minutes"))
  end_time <- lubridate::with_tz(end_time, tz = "UTC")
  end_time <- lubridate::round_date(end_time, unit = paste0(timestep_minutes_round, " minutes"))

  dates_fires <- seq(start_time, end_time, by = paste0(timestep_minutes, " mins"))

  # Get local timezone name based on bbox
  my_tz <- wfprogression::fire_get_timezone(fire_bbox)

  # Build base dataframe with expected paths and timestamps
  dat.paths <- data.frame(datetime_utc = dates_fires) %>%
    dplyr::mutate(
      datetimeutc_path = format(datetime_utc, format = "/%Y/%m/%d/%H%M"),
      path_catalog = paste0(nci_path, datetimeutc_path, "/catalog.html"),
      datetimelocal = lubridate::with_tz(datetime_utc, tz = my_tz),
      datetimelocal_chr = format(datetimelocal, format = "%Y%m%d%H%M%S"),
      date_local = format(datetimelocal, format = "%Y-%m-%d"),
      hour_local = lubridate::hour(datetimelocal),
      minutes_local = lubridate::minute(datetimelocal),
      daynight = ifelse(hour_local >= 8 & hour_local <= 18, "day", "night")
    ) %>%
    dplyr::mutate(
      file_name = purrr::map(path_catalog, ~wfprogression::fire_thredds_list(.x, "ABOM_OBS_|ABOM_OBS_|ABOM_OBS_"))
    )

  # If the requested time range is very recent, search NRT data too
  weeks_diff <- as.numeric(difftime(Sys.Date(), end_time, units = "weeks"))
  if (weeks_diff <= 3) {
    dat.paths.nrt <- dat.paths %>%
      dplyr::mutate(
        path_catalog = stringr::str_replace(path_catalog, "/arc/obs/", "/nrt/obs/"),
        file_name = purrr::map(path_catalog, ~wfprogression::fire_thredds_list(.x, "ABOM_OBS_|ABOM_OBS_|ABOM_OBS_"))
      )

    dat.paths <- rbind(dat.paths, dat.paths.nrt)
  }

  dat.paths <- dat.paths %>%
    tidyr::unnest(cols = "file_name") %>%
    dplyr::filter(!is.na(file_name))

  if (nrow(dat.paths) > 0) {
    dat.paths <- dat.paths %>%
      dplyr::mutate(
        path_download = stringr::str_replace(path_catalog, "catalog", "dodsC"),
        path_download = stringr::str_replace(path_download, "catalog.html", file_name),
        band = substr(file_name, 29, 31),
        satellite = substr(file_name, 50, 58)
      ) %>%
      dplyr::select(
        datetime_utc, datetimelocal, datetimelocal_chr, date_local,
        hour_local, minutes_local, daynight, path_catalog,
        file_name, path_download, band, satellite
      ) %>%
      dplyr::filter(datetime_utc >= start_time & datetime_utc <= end_time)

    return(dat.paths)

  } else {
    print("No Himawari images found")
  }
}
