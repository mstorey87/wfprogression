#' Function to stream and sample BARRA weather netcdf
#'
#' @param nc_conn netcdf connection using tidync::tidync()
#' @param datetimeutc posixct datetime in utc. Must match https path of nc connection
#' @param sf_points sample point (sf object)
#' @param varname BARRA variable name. Must match var name used in https path.
#'
#' @return sf object with new columns for sampled variable
#' @export
#'
#' @examples
#' #
fire_barra_sample<- function(nc_conn,datetimeutc,sf_points,varname,allcells=F,extract_fun="mean"){
  #see end of this document for variable names http://www.bom.gov.au/research/publications/researchreports/BRR-067.pdf
  #example here of R2 variables on thredds https://thredds.nci.org.au/thredds/catalog/ob53/output/reanalysis/AUS-11/BOM/ERA5/historical/hres/BARRA-R2/v1/1hr/catalog.html
  #some variables are sfcWind (surface wind), tas (temperature), hurs (RH), vas and uas (wind components). These are the values on the hour. Some variables
  #have variations e.g. tasmean is mean hourly temperature


  #get the origin time from the nc file
  ncorigin <- ncmeta::nc_atts(nc_conn$source$source,variable = "time")$value$units
  ncorigin <- stringr::str_replace(ncorigin,"days since ","")

  #convert our time to nc format
  datetimeutc_nc <- as.numeric(datetimeutc-as.POSIXct(ncorigin,tz="utc"))

  #get bounding box of all point for filtering data
  bbox <- sf::st_bbox(sf_points)

  b.local <- nc_conn  %>%
    tidync::activate(varname) %>%
    #filter roughly by lat long and to exact time
    tidync::hyper_filter(time= time==datetimeutc_nc,
                         lat = lat >= bbox[2]-1 & lat <= bbox[4]+1,
                         lon = lon >= bbox[1]-1 & lon <= bbox[3]+1) %>%

    tidync::hyper_tibble() %>%

    #convert time and filter again. More of a check that the time conversion I did above produces the same results
    #this could be excluded
    dplyr::mutate(dtutc=as.POSIXct(time*24*60*60,origin=ncorigin,tz="UTC")) %>%
    dplyr::filter(dtutc==datetimeutc)%>%

    #select required columns for conversion to raster (lon and lat first)
    dplyr::select(lon,lat,dplyr::everything(),-time,-dtutc)

  #create raster
  r <- terra::rast(b.local)
  terra::crs(r) <- "epsg:4283" #not sure if its GDA94 or WGS84

  #extract raster values if point type
  # if(max(str_detect(st_geometry_type(sf_points),"POINT"))==1){
  #   res <- terra::extract(r,sf_points,ID=F)
  #}

  #extract raster values if polygon type. fun argument ignorned if sampling with points
  # if(max(str_detect(st_geometry_type(sf_points),"POLY"))==1){
  #
  #if not returning all cell, return single row with mean
  if(allcells==F){
      res <- terra::extract(r,sf_points,fun=extract_fun,ID=F)
      res <- cbind(sf_points,res)

  }

  #if returning all cells, add nested list
  if(allcells==T){
    res <- terra::extract(r,sf_points,raw=F,ID=F)
    sf_points[[varname]] <- list(res)
    res <- sf_points
  }

  #}




  return(res)




}
