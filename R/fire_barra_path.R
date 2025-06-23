#' Construct https string to a BARRA file on NCI
#'
#' @param datetimeutc posixct datetime in utc rounded to nearest hour
#' @param barraid R2 (12 km  BARRA product) or C2 (~4km BARRA product)
#' @param timestep Timestep of data to search for in BARRA. Will accept "hourly" or "daily"
#' @param varname BARRA variable name e.g. sfcWind (surface wind), tas (temperature), hurs (RH), vas and uas (wind components).  http://www.bom.gov.au/research/publications/researchreports/BRR-067.pdf
#'
#' @return string
#' @export
#'
#' @examples
#' #fn_barra_path(datetimeutc = as.POSIXct("2019-12-01 10:00:00",tz="utc"),barraid = "R2",varname = "sfcWind")

fire_barra_path <- function(datetimeutc,barraid,timestep,varname){
  #construct thredds file path
  if(barraid=="R2")  barraid1="AUS-11"
  if(barraid=="C2")  barraid1="AUST-04"

  #year and month for path
  yrmnth <- format(datetimeutc,format = "%Y%m")

  #construct the path
  if(timestep=="hourly"){
    file_thredds <- paste0(varname,"_",barraid1,"_ERA5_historical_hres_BOM_BARRA-",barraid,"_v1_1hr_",yrmnth,"-",yrmnth,".nc")
    nci_path <-  paste0("https://thredds.nci.org.au/thredds/dodsC/ob53/output/reanalysis/",barraid1,"/BOM/ERA5/historical/hres/BARRA-",barraid,"/v1/1hr/",varname,"/latest")
    pth=paste0(nci_path,"/",file_thredds)


  }

  if(timestep=="daily"){
    file_thredds <- paste0(varname,"_",barraid1,"_ERA5_historical_hres_BOM_BARRA-",barraid,"_v1_day_",yrmnth,"-",yrmnth,".nc")
    nci_path <-  paste0("https://thredds.nci.org.au/thredds/dodsC/ob53/output/reanalysis/",barraid1,"/BOM/ERA5/historical/hres/BARRA-",barraid,"/v1/day/",varname,"/latest")
    pth=paste0(nci_path,"/",file_thredds)


  }

  return(pth)


}
