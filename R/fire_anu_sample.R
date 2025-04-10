fire_anu_sample <- function(datetimeutc,sf_data,varname,allcells=F,extract_fun="mean"){

  #download from ANU thredds
  #https://geonetwork.nci.org.au/geonetwork/srv/eng/catalog.search#/metadata/f1104_2906_7276_3004

  checkmate::assert(stringr::str_detect(class(datetimeutc)[1],"POSIXct"),"Error: times must be posixct")
  start_time <- lubridate::with_tz(datetimeutc,tz="utc")

  #get bounding box of all point for filtering data
  bbox <- sf::st_bbox(sf_data %>% sf::st_transform(4326))

  myyear <- lubridate::year(datetimeutc)

  mydate <- as.Date(datetimeutc)
  #recalc varname to use for filtering nc

  if(varname=="fmc") varname2="lfmc_median"
  if(varname=="flam") varname2="flammability"


  ncpath <- paste0("https://thredds.nci.org.au/thredds/dodsC/ub8/au/FMC/mosaics/",varname,"_c6_",myyear,".nc")
  nc_conn <- tidync::tidync(ncpath)
  #get the most recent date before mydate from the nc dates
  nc_timestamps <- nc_conn$transforms$time$timestamp %>%
    as.Date()
  nc_timestamps <- nc_timestamps[nc_timestamps < mydate]
  nc_timestamps <- nc_timestamps[length(nc_timestamps)]

  #if date not found, try in file from year prior
  if(length(nc_timestamps)==0){

    ncpath <- paste0("https://thredds.nci.org.au/thredds/dodsC/ub8/au/FMC/mosaics/",varname,"_c6_",myyear-1,".nc")
    nc_conn <- tidync::tidync(ncpath)
    #get the most recent date before mydate from the nc dates
    nc_timestamps <- nc_conn$transforms$time$timestamp %>%
      as.Date()
    nc_timestamps <- nc_timestamps[nc_timestamps < mydate]
    nc_timestamps <- nc_timestamps[length(nc_timestamps)]






  }

  #check if date is close enough, with says a few weeks
  datediff=as.numeric(difftime(as.Date(mydate),as.Date(nc_timestamps),units = "days"))
  if(datediff > 20){
    print(paste0("exctrated date is ", datediff, " days prior to your date"))
  }


 #now extract the time that matches the date found above
  #this just ensures we get time in the appropriate format to filter with in the next step
  datetimeutc_nc <- nc_conn$transforms$time %>%
    dplyr::filter(timestamp==as.character(nc_timestamps)) %>% .$time


  #active nc variable
  b.local <- nc_conn  %>%
    tidync::activate(varname2) %>%

    #filter roughly by lat long and to exact time
    tidync::hyper_filter(time= time==datetimeutc_nc,
                         latitude = latitude >= bbox[2]-1 & latitude <= bbox[4]+1,
                         longitude = longitude >= bbox[1]-1 & longitude <= bbox[3]+1) %>%





    tidync::hyper_tibble() %>%

    #convert time and filter again. More of a check that the time conversion I did above produces the same results
    #this could be excluded
    # dplyr::mutate(dtutc=as.POSIXct(time*24*60*60,origin=ncorigin,tz="UTC")) %>%
    # dplyr::mutate(dtutc=as.POSIXct(time,tz="UTC")) %>%
    # dplyr::filter(dtutc==datetimeutc)%>%

    #select required columns for conversion to raster (lon and lat first)
    dplyr::select(longitude,latitude,dplyr::all_of(varname2))

  #create raster
  r <- terra::rast(b.local)
  terra::crs(r) <- "epsg:4326" #not sure if its GDA94 or WGS84

  #extract raster values if point type
  # if(max(str_detect(st_geometry_type(sf_data),"POINT"))==1){
  #   res <- terra::extract(r,sf_data,ID=F)
  #}

  #extract raster values if polygon type. fun argument ignorned if sampling with points
  # if(max(str_detect(st_geometry_type(sf_data),"POLY"))==1){
  #
  #if not returning all cell, return single row with mean
  if(allcells==F){
    res <- terra::extract(r,sf_data,fun=extract_fun,ID=F)
    res <- cbind(sf_data,res)

  }

  #if returning all cells, add nested list
  if(allcells==T){
    res <- terra::extract(r,sf_data,raw=F,ID=F)
    sf_data[[varname]] <- list(res)
    res <- sf_data
  }

  #}




  return(res)




}
