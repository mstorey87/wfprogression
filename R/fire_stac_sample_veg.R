#' Search for Landsat and Sentinel 2 data
#'
#' @description
#' Search Geoscience Aus. STAC browser for Landsat and Sentinel 2 images that intersect polygon and match search dates.
#' This will return a data frame of images path to input to fire_stac_download() function.
#' A max of 999 records will be returned, so location and or date may need to be limited.
#' Info on STAC browser is here: https://knowledge.dea.ga.gov.au/guides/setup/gis/stac/
#'
#' @param fire_bbox A polygon, usually fire bounding box, to search for images
#' @param start_time The first time for which to search for images (posixct required). This will be converted to utc.
#' @param end_time The last time for which to search for images (posixct required). This will be converted to utc.
#' @param collection_names Names of which Landsat and Sentinel 2 products to search for. Defaults to all products from Landsat 5 onwards.
#'
#' @return data frame with image paths
#' @export
#'
#' @examples
#' #dat.stac <- fire_search_stac(fire_bbox = dat.bbox,start_time = datestart,end_time = dateend)
fire_stac_sample_veg <- function(sf_object,
                                 year,
                                 collection_names=c(#"fc_percentile_albers_annual",
                                                    #"fc_percentile_albers_seasonal",
                                                    "ga_ls_fc_pc_cyear_3")){

  #product list here
  #https://knowledge.dea.ga.gov.au/notebooks/Beginners_guide/03_Products_and_measurements/


  my_time1 <- paste0(year,"-01-01T00:00:00Z")


  #combine sf object and buffer

  sf_bbox <- sf_object %>%
    sf::st_union() %>%
    sf::st_as_sf() %>%
    sf::st_transform(3577) %>%
    sf::st_buffer(1000) %>%
    sf::st_transform(4326)




  #connect to stac source - digital earth australia
  #see information here: https://knowledge.dea.ga.gov.au/guides/setup/gis/stac/
  stac_source <- rstac::stac(
    "https://explorer.dea.ga.gov.au/stac"
  )

  #colls <- stac_source %>% rstac::collections() %>% rstac::get_request()



  #query based on location, date and collection name
  stac_query <- rstac::stac_search(
    q = stac_source,
    bbox = sf::st_bbox(sf::st_transform(sf_bbox,4326)),#which crs??
    collections = collection_names,
    datetime = paste0(my_time1,"/",my_time1),
    limit = 999
  )
  executed_stac_query <- rstac::get_request(stac_query)



  #extract details of each band available from the query results
  #extract items properties including path and url
  dat.x <- purrr::map(executed_stac_query$features,~tibble::tibble(pv_pc_90_href=.x$assets$pv_pc_90$href,
                                                                   pv_pc_50_href=.x$assets$pv_pc_50$href,
                                                                   npv_pc_90_href=.x$assets$npv_pc_90$href,
                                                                   npv_pc_50_href=.x$assets$npv_pc_50$href,
                                                                   bs_pc_90_href=.x$assets$bs_pc_90$href,
                                                                   bs_pc_50_href=.x$assets$bs_pc_50$href,
                                                                       product=.x$collection,
                                                                   datetime=.x$properties$datetime)) %>%
    dplyr::bind_rows()


  #test if any images were found
  if(nrow(dat.x)>0){










    #combine data and calculate https path and date times strings for file names
    dat.aws <- dat.x %>%
      dplyr::mutate(date=as.Date(datetime,format="%Y-%m-%d"),
                    #tile and date string to use in output file name
                    tile=paste0(purrr::map_chr(stringr::str_split(basename(pv_pc_90_href),"_"),7)))%>%
      tidyr::pivot_longer(matches("href")) %>%
      mutate(value=stringr::str_replace(value,"s3://dea-public-data","https://dea-public-data.s3.ap-southeast-2.amazonaws.com")) %>%
      dplyr::mutate(r=purrr::map(value,~terra::rast(.x,vsi=T))) %>%
      dplyr::mutate(res=purrr::map(r,~terra::extract(.x,sf::st_transform(sf_object,sf::st_crs(.x)),ID=T,xy=T,cell=T)))

    #combine and remove duplicated ID columns
    res <- dat.aws %>%
      select(name,date,res) %>%
      dplyr::mutate(res=purrr::map(res,~setNames(.x,c("sf_id","value","cell_no","cell_x","cell_y")))) %>%
      tidyr::unnest(cols=res) %>%
      dplyr::arrange(date,name,sf_id,cell_no) %>%
      #because the data comes in tiles, it will return NA values where sampling off tile
      dplyr::filter(!is.na(value)) %>%
      dplyr::mutate(name=stringr::str_replace(name,"_href",""))

    res2 <- tidyr::pivot_wider(res,id_cols = c("cell_no","cell_x","cell_y","sf_id","date"))


    rm(dat.aws)












    return(res2)

  }else{

    print("no stac data found")
  }
}

