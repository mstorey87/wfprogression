#' Construct HTTPS URL to a BARRA data file on NCI THREDDS server
#'
#' Generates the full URL string pointing to a specific BARRA NetCDF file
#' on the NCI THREDDS server based on datetime, resolution, timestep, and variable.
#'
#' @param datetimeutc POSIXct datetime in UTC timezone, ideally rounded to the nearest hour.
#' @param barraid Character; either `"R2"` (12 km BARRA product) or `"C2"` (~4 km BARRA product).
#' @param timestep Character; time resolution of data, one of `"hourly"`, `"daily"`, or `"monthly"`.
#' @param varname Character; BARRA variable name (e.g., `"sfcWind"` for surface wind, `"tas"` for temperature,
#'   `"hurs"` for relative humidity, `"uas"` and `"vas"` for wind components).
#'   See [BOM report](http://www.bom.gov.au/research/publications/researchreports/BRR-067.pdf) for details.
#'
#' @return Character string giving the full HTTPS URL to the requested BARRA NetCDF file.
#' @export
#'
#' @examples
#' \dontrun{
#' fire_barra_path(
#'   datetimeutc = as.POSIXct("2019-12-01 10:00:00", tz = "UTC"),
#'   barraid = "R2",
#'   timestep = "hourly",
#'   varname = "sfcWind"
#' )
#' }
fire_barra_path <- function(datetimeutc, barraid, timestep, varname) {
  # Validate inputs
  checkmate::assert_choice(barraid, c("R2", "C2"), .var.name = "barraid")
  checkmate::assert_choice(timestep, c("hourly", "daily", "monthly"), .var.name = "timestep")
  checkmate::assert_string(varname)

  # Map barraid to internal product code used in paths
  barraid1 <- switch(barraid,
                     R2 = "AUS-11",
                     C2 = "AUST-04")

  # Format year and month string (e.g., "201912")
  yrmnth <- format(datetimeutc, "%Y%m")

  # Base URL path components by timestep
  timestep_folder <- switch(timestep,
                            hourly = "1hr",
                            daily = "day",
                            monthly = "mon")

  # Construct filename matching NCI naming convention
  file_thredds <- paste0(
    varname, "_",
    barraid1, "_ERA5_historical_hres_BOM_BARRA-",
    barraid, "_v1_", timestep_folder, "_",
    yrmnth, "-", yrmnth, ".nc"
  )

  # Construct base NCI THREDDS URL
  nci_path <- paste0(
    "https://thredds.nci.org.au/thredds/dodsC/ob53/output/reanalysis/",
    barraid1, "/BOM/ERA5/historical/hres/BARRA-",
    barraid, "/v1/", timestep_folder, "/", varname, "/latest"
  )

  # Full URL to the file
  full_url <- paste0(nci_path, "/", file_thredds)

  return(full_url)
}


#' #' Construct https string to a BARRA file on NCI
#' #'
#' #' @param datetimeutc posixct datetime in utc rounded to nearest hour
#' #' @param barraid R2 (12 km  BARRA product) or C2 (~4km BARRA product)
#' #' @param timestep Timestep of data to search for in BARRA. Will accept "hourly","daily" or "monthly"
#' #' @param varname BARRA variable name e.g. sfcWind (surface wind), tas (temperature), hurs (RH), vas and uas (wind components).  http://www.bom.gov.au/research/publications/researchreports/BRR-067.pdf
#' #'
#' #' @return string
#' #' @export
#' #'
#' #' @examples
#' #' #fn_barra_path(datetimeutc = as.POSIXct("2019-12-01 10:00:00",tz="utc"),barraid = "R2",varname = "sfcWind")
#'
#' fire_barra_path <- function(datetimeutc,barraid,timestep,varname){
#'   #construct thredds file path
#'   if(barraid=="R2")  barraid1="AUS-11"
#'   if(barraid=="C2")  barraid1="AUST-04"
#'
#'   #year and month for path
#'   yrmnth <- format(datetimeutc,format = "%Y%m")
#'
#'   checkmate::assert(timestep %in% c("hourly","daily","monthly"),"Error: timestep must be hourly, daily or monthly")
#'
#'   #construct the path
#'   if(timestep=="hourly"){
#'     file_thredds <- paste0(varname,"_",barraid1,"_ERA5_historical_hres_BOM_BARRA-",barraid,"_v1_1hr_",yrmnth,"-",yrmnth,".nc")
#'     nci_path <-  paste0("https://thredds.nci.org.au/thredds/dodsC/ob53/output/reanalysis/",barraid1,"/BOM/ERA5/historical/hres/BARRA-",barraid,"/v1/1hr/",varname,"/latest")
#'     pth=paste0(nci_path,"/",file_thredds)
#'
#'
#'   }
#'
#'   if(timestep=="daily"){
#'     file_thredds <- paste0(varname,"_",barraid1,"_ERA5_historical_hres_BOM_BARRA-",barraid,"_v1_day_",yrmnth,"-",yrmnth,".nc")
#'     nci_path <-  paste0("https://thredds.nci.org.au/thredds/dodsC/ob53/output/reanalysis/",barraid1,"/BOM/ERA5/historical/hres/BARRA-",barraid,"/v1/day/",varname,"/latest")
#'     pth=paste0(nci_path,"/",file_thredds)
#'
#'
#'   }
#'
#'
#'   if(timestep=="monthly"){
#'     file_thredds <- paste0(varname,"_",barraid1,"_ERA5_historical_hres_BOM_BARRA-",barraid,"_v1_mon_",yrmnth,"-",yrmnth,".nc")
#'     nci_path <-  paste0("https://thredds.nci.org.au/thredds/dodsC/ob53/output/reanalysis/",barraid1,"/BOM/ERA5/historical/hres/BARRA-",barraid,"/v1/mon/",varname,"/latest")
#'     pth=paste0(nci_path,"/",file_thredds)
#'
#'
#'   }
#'
#'   return(pth)
#'
#'
#' }
