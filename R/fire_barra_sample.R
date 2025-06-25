#' Function to stream and sample BARRA weather netcdf
#'
#' @description
#' Take an existing nc connection (tidync::tidync()) and sample for the locations in the input sf points
#' see end of this document for variable names http://www.bom.gov.au/research/publications/researchreports/BRR-067.pdf
#' example here of R2 variables on thredds https://thredds.nci.org.au/thredds/catalog/ob53/output/reanalysis/AUS-11/BOM/ERA5/historical/hres/BARRA-R2/v1/1hr/catalog.html
#' some variables are sfcWind (surface wind), tas (temperature), hurs (RH), vas and uas (wind components). These are the values on the hour. Some variables
#' @param nc_conn netcdf connection using tidync::tidync()
#' @param datetimeutc posixct datetime in utc. Must match https path of nc connection
#' @param sf_data sf object with one row with locations to sample barra data
#' @param varname BARRA variable name. Must match var name used in https path. e.g. sfcWind (surface wind), tas (temperature), hurs (RH), vas and uas (wind components).
#' @param allcells logical. Return all cells in BARRA data that intersect an input polygon? Leave F for point data.
#' @param timestep Timestep of data to search for in BARRA. Will accept "hourly", "daily" or "monthly"
#' @param extract_fun If not returning all cells for an input polygon, how should sampled cells be summarised in terra::extract, e.g. mean, max
#'
#' @return sf object with new columns for sampled variable
#' @export
#'
#' @examples
#' #
fire_barra_sample<- function(nc_conn,datetimeutc,sf_data,varname,allcells=F,timestep,extract_fun="mean"){
  #see end of this document for variable names http://www.bom.gov.au/research/publications/researchreports/BRR-067.pdf
  #example here of R2 variables on thredds https://thredds.nci.org.au/thredds/catalog/ob53/output/reanalysis/AUS-11/BOM/ERA5/historical/hres/BARRA-R2/v1/1hr/catalog.html
  #some variables are sfcWind (surface wind), tas (temperature), hurs (RH), vas and uas (wind components). These are the values on the hour. Some variables
  #have variations e.g. tasmean is mean hourly temperature


  #get bounding box of all point for filtering data
  #transform to lat lon first
  sf_data <- sf_data %>% sf::st_transform(4326)

  bbox <- sf::st_bbox(sf_data)


  #get the time in the format of the nc
  #if hourly or daily, datetime is needed.
  #if monthly, each file will only have on datetime, so just return one layer
  checkmate::assert(timestep %in% c("hourly","daily","monthly"),"Error: timestep must be hourly, daily or monthly")

  if(timestep == "hourly" ){
    datetimeutc_nc <- nc_conn$transforms$time %>%
    dplyr::filter(timestamp==format(datetimeutc,format="%Y-%m-%d %H:%M:%S")|timestamp==format(datetimeutc,format="%Y-%m-%dT%H:%M:%S")) %>% .$time

  }

  if(timestep=="daily"){

    checkmate::assert(unique(lubridate::hour(datetimeutc)) == 12,"Error: to sample daily data, 12 PM times UTC are needed")
    datetimeutc_nc <- nc_conn$transforms$time %>%
      dplyr::filter(timestamp==format(datetimeutc,format="%Y-%m-%d %H:%M:%S")|timestamp==format(datetimeutc,format="%Y-%m-%dT%H:%M:%S")) %>% .$time

  }


  if(timestep %in% c("monthly")){
    datetimeutc_nc <- nc_conn$transforms$time$time[1]
  }

  #ensure only one time is returned. There shouldn't be any reason, but just an extra check
  checkmate::assert( length(datetimeutc_nc)==1,"Error with nc time filtering")



  #active nc variable
    b.local <- nc_conn  %>%
      tidync::activate(varname) %>%

    #filter roughly by lat long and to exact time
    tidync::hyper_filter(time= time==datetimeutc_nc,
                         lat = lat >= bbox[2]-1 & lat <= bbox[4]+1,
                         lon = lon >= bbox[1]-1 & lon <= bbox[3]+1
                         ) %>%





    tidync::hyper_tibble() %>%

    #convert time and filter again. More of a check that the time conversion I did above produces the same results
    #this could be excluded
    # dplyr::mutate(dtutc=as.POSIXct(time*24*60*60,origin=ncorigin,tz="UTC")) %>%
    # dplyr::mutate(dtutc=as.POSIXct(time,tz="UTC")) %>%
    # dplyr::filter(dtutc==datetimeutc)%>%

    #select required columns for conversion to raster (lon and lat first)
    dplyr::select(lon,lat,dplyr::all_of(varname))

  #create raster
  r <- terra::rast(b.local)
  terra::crs(r) <- "epsg:4326" #use 4326 as suggested here https://opus.nci.org.au/spaces/NDP/pages/264241306/FAQs+-+BOM+BARRA2+ob53#FAQsBOMBARRA2(ob53)-Whatistheprojectiondatumofthedata?



  #if not returning all cell, return single row with mean
  if(allcells==F){
      res <- terra::extract(r,sf_data,fun=extract_fun,ID=F)
      names(res) <- paste0(names(res),"_",extract_fun)
      res <- cbind(sf_data,res)

  }

  #if returning all cells, add nested list
  if(allcells==T){
    res <- terra::extract(r,sf_data,raw=F,ID=F)
    sf_data[[varname]] <- list(res)
    res <- sf_data
  }






  return(res)




}
