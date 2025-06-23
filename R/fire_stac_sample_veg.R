#' Search for Geoscience Australia STAC browser for vegetation data
#'
#' @description
#' Search Geoscience Aus. STAC browser for vegetation data that intersect polygon and match search dates.
#' A max of 999 records will be returned, so location and or date may need to be limited.
#' Info on STAC browser is here: https://knowledge.dea.ga.gov.au/guides/setup/gis/stac/
#'
#' @param sf_object An sf object
#' @param start_time The first time for which to search for images (posixct required). This will be converted to utc.
#' @param end_time The last time for which to search for images (posixct required). This will be converted to utc.
#' @param collection_name Names of which Landsat and Sentinel 2 products to search for. Allowed names are: "ga_ls_fc_pc_cyear_3","ga_ls_landcover_class_cyear_3","ga_srtm_dem1sv1_0","ga_ls_fc_3". https://knowledge.dea.ga.gov.au/notebooks/Beginners_guide/03_Products_and_measurements/
#'
#' @return data frame with image paths
#' @export
#'
#' @examples
#' #dat.stac <- fire_search_stac(fire_bbox = dat.bbox,start_time = datestart,end_time = dateend)
fire_stac_sample_veg <- function(sf_object,
                                 start_time=NULL,
                                 end_time=NULL,
                                 collection_name){

  #product list here
  #https://knowledge.dea.ga.gov.au/notebooks/Beginners_guide/03_Products_and_measurements/

  checkmate::assert(inherits(sf_object, "sf"),"Error: sf input object needed")

  #combine sf object and buffer
  sf_bbox <- sf_object %>%
    sf::st_union() %>%
    sf::st_as_sf() %>%
    sf::st_transform(3577) %>%
    sf::st_buffer(1000) %>%
    sf::st_transform(4326)

  #get time difference between start and end to use for checking later
  timediff <- as.numeric(difftime(end_time,start_time,units = "days"))


  if(!is.null(start_time)){
    checkmate::assert(stringr::str_detect(class(start_time)[1],"POSIXct"),"Error: times must be posixct")
    start_time <- lubridate::with_tz(start_time,tz="UTC")
    start_time <- format(start_time, "%Y-%m-%dT%H:%M:%SZ")


  }
  if(!is.null(end_time)){

    checkmate::assert(stringr::str_detect(class(end_time)[1],"POSIXct"),"Error: times must be posixct")
    end_time <- lubridate::with_tz(end_time,tz="UTC")

    end_time <- format(end_time, "%Y-%m-%dT%H:%M:%SZ")

  }





  ##############################################################query based on location, date and collection name


  #connect to stac source - digital earth australia
  #see information here: https://knowledge.dea.ga.gov.au/guides/setup/gis/stac/
  stac_source <- rstac::stac(
    "https://explorer.dea.ga.gov.au/stac"
  )

  #create a datetime variable that will work in the stac query.
  #format depends on the collection name (collection_name)
  #if dem is to be extracted, no datetime is needed
  if(collection_name=="ga_srtm_dem1sv1_0"){
    datetime_chr=NULL
    message("sampling ", collection_name)
  }

  #if yearly data is to be extracted, the 1st of jan is needed
  if(collection_name %in% c("ga_ls_landcover_class_cyear_3","ga_ls_fc_pc_cyear_3")){
    year <- lubridate::year(start_time)
    #if sampling yearly data, we probably need the year before the fire?
    my_time1 <- paste0(year,"-01-01T00:00:00Z")
    datetime_chr=paste0(my_time1,"/",my_time1)
    message(paste0("sampling ",collection_name," for ", datetime_chr))
  }

  if(collection_name=="ga_ls_fc_3"){
    #if using daily fpc, sample for a large range of days. Landsat doesn't capture all days, so a short period will results in null values
    checkmate::assert(timediff > 14,"Error: When sampling daily data, longer between start and end is needed because Landsat data is not available for all days")
    datetime_chr=paste0(start_time,"/",end_time)
    message(paste0("sampling ",collection_name," for ", datetime_chr))
  }

  stac_query <- rstac::stac_search(
    q = stac_source,
    bbox = sf::st_bbox(sf::st_transform(sf_bbox,4326)),#which crs??
    collections = collection_name,
    datetime = datetime_chr,
    limit = 999
  )
  executed_stac_query <- rstac::get_request(stac_query)
  items_list <- executed_stac_query$features



  #only the properties that exist will be returned
  #this makes it flexible when selecting different products
  items_df2 <- purrr::map_df(items_list, function(item) {
    dplyr::tibble(
      title = item$properties$title,
      product = item$properties$`odc:product`,
      datetime = item$properties$datetime,
      end_datetime=item$properties$end_datetime,
      start_datetime=item$properties$start_datetime,
      epsg=item$properties$`proj:epsg`,
      instrument=paste0(item$properties$instruments,collapse = ";"),
      assets = item$assets

    )
  })


  #extract required paths to data
  dat.x <- items_df2 %>%
    dplyr::mutate(href=purrr::map(assets,~.x$href),
                  asset_title=purrr::map(assets,~.x$title)) %>%
    tidyr::unnest(c(href,asset_title)) %>%
    dplyr::filter(stringr::str_ends(href,".tif"),asset_title != "qa") %>%
    dplyr::select(-assets)

  #add a name for the sampled variable based on which names are in the df.
  #product won't be in the df for dem
  if("product" %in% names(dat.x)){
    dat.x <-dat.x %>%  dplyr::mutate(sample_name=paste0(product,"__",asset_title))
  }else{
    dat.x <- dat.x %>% dplyr::mutate(sample_name=paste0(asset_title))
  }







  #test if any images were found. If not stop
  if(!nrow(dat.x)>0){
    message("no stac data found for ", collection_name," ",datetime_chr)
    return(NULL)
  }


  #if data was found.....
  #combine data and calculate https path and date times strings for file names
  dat.aws <- dat.x %>%
    dplyr::mutate(href=stringr::str_replace(href,"s3://dea-public-data","https://dea-public-data.s3.ap-southeast-2.amazonaws.com")) %>%
    dplyr::mutate(r=purrr::map(href,~terra::rast(.x,vsi=T))) %>%
    dplyr::mutate(res=purrr::map(r,~terra::extract(.x,sf::st_transform(sf_object,sf::st_crs(.x)),ID=T,xy=T,cell=T)))

  #combine and remove duplicated ID columns
  res <- dat.aws %>%
    dplyr::select(sample_name,instrument,dplyr::matches("datetime"),res) %>%
    dplyr::mutate(res=purrr::map(res,~setNames(.x,c("sf_id","value","cell_no","cell_x","cell_y")))) %>%
    tidyr::unnest(cols=res) %>%
    dplyr::arrange(sample_name,sf_id,cell_no) %>%
    #because the data comes in tiles, it will return NA values where sampling off tile
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(sample_name=stringr::str_replace(sample_name,"_href","")
                  ,
                  #seems to be slight offset in cell x and y from dems
                  #calculate rounded x and ys
                  cell_x=round(cell_x,5),
                  cell_y=round(cell_y,5)
    )

  #restructure
  res <- tidyr::pivot_wider(res,values_from = value,names_from = sample_name)

  return(res)
}

