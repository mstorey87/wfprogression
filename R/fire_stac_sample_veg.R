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
#' @param year_prior Should the FPC be from the year prior to the start date?
#'
#' @return data frame with image paths
#' @export
#'
#' @examples
#' #dat.stac <- fire_search_stac(fire_bbox = dat.bbox,start_time = datestart,end_time = dateend)
fire_stac_sample_veg <- function(sf_object,
                                 start_time=NULL,
                                 end_time=NULL,
                                 collection_names=c(#"fc_percentile_albers_annual",
                                   #"fc_percentile_albers_seasonal",
                                   "ga_ls_fc_pc_cyear_3",
                                   "ga_ls_landcover_class_cyear_3",
                                   "ga_srtm_dem1sv1_0",
                                   "ga_ls_fc_3")){

  #product list here
  #https://knowledge.dea.ga.gov.au/notebooks/Beginners_guide/03_Products_and_measurements/



  #combine sf object and buffer

  sf_bbox <- sf_object %>%
    sf::st_union() %>%
    sf::st_as_sf() %>%
    sf::st_transform(3577) %>%
    sf::st_buffer(1000) %>%
    sf::st_transform(4326)

  #start_time=as.POSIXct("2019-02-01 12:00:00")
  #end_time=as.POSIXct("2019-03-01 12:00:00")

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





















  #connect to stac source - digital earth australia
  #see information here: https://knowledge.dea.ga.gov.au/guides/setup/gis/stac/
  stac_source <- rstac::stac(
    "https://explorer.dea.ga.gov.au/stac"
  )

  # colls <- stac_source %>% rstac::collections() %>% rstac::get_request()

  # x <-  purrr::map(colls$collections,~data.frame(.x$title,.x$description) ) %>% dplyr::bind_rows()

  #query based on location, date and collection name

  all.res <- list()
  for(xi in 1:length(collection_names)){

    cname <- collection_names[xi]

    #if dem is to be extracted, no datetime is needed
    if(cname=="ga_srtm_dem1sv1_0")  datetime_chr=NULL

    #if yearly data is to be extracted, the 1st of jan is needed
    if(cname %in% c("ga_ls_landcover_class_cyear_3","ga_ls_fc_pc_cyear_3")){
      year <- lubridate::year(start_time)
      #if sampling yearly data, we probably need the year before the fire
      my_time1 <- paste0(year,"-01-01T00:00:00Z")
      datetime_chr=paste0(my_time1,"/",my_time1)


    }

    if(cname=="ga_ls_fc_3"){

      # #if using daily fpc, sample for at least 1 month prior
      # start_time <- as.POSIXct(start_time,format="%Y-%m-%dT%H:%M:%SZ",tz="utc")
      # start_time <- start_time-lubridate::weeks(4)
      # start_time <- format(start_time, "%Y-%m-%dT%H:%M:%SZ")

      datetime_chr=paste0(start_time,"/",end_time)
    }











    stac_query <- rstac::stac_search(
      q = stac_source,
      bbox = sf::st_bbox(sf::st_transform(sf_bbox,4326)),#which crs??
      collections = cname,
      datetime = datetime_chr,
      limit = 999
    )
    executed_stac_query <- rstac::get_request(stac_query)



    #extract details of each band available from the query results
    #extract items properties including path and url
    # dat.x <- purrr::map(executed_stac_query$features,~tibble::tibble(pv_pc_90_href=.x$assets$pv_pc_90$href,
    #                                                                  pv_pc_50_href=.x$assets$pv_pc_50$href,
    #                                                                  npv_pc_90_href=.x$assets$npv_pc_90$href,
    #                                                                  npv_pc_50_href=.x$assets$npv_pc_50$href,
    #                                                                  bs_pc_90_href=.x$assets$bs_pc_90$href,
    #                                                                  bs_pc_50_href=.x$assets$bs_pc_50$href,
    #                                                                      product=.x$collection,
    #                                                                  datetime=.x$properties$datetime)) %>%
    #   dplyr::bind_rows()
    #
    #
    # dat.x <- rstac::assets_url(executed_stac_query) %>%
    #   data.frame(href=.,product=tools::file_path_sans_ext(basename(.))) %>%
    #   dplyr::filter(stringr::str_ends(href,".tif"))


    # items_list <- executed_stac_query$features
    # items_df <- purrr::map_df(items_list, function(item) {
    #   tibble(
    #     title = item$properties$title,
    #     product = item$properties$`odc:product`,
    #     datetime = item$properties$datetime,
    #     end_datetime=item$properties$end_datetime,
    #     start_datetime=item$properties$start_datetime,
    #     epsg=item$properties$`proj:epsg`
    #
    #   )
    # })

    items_list <- executed_stac_query$features
    #only the properties that exist will be returned
    #this makes it flexible when selecting different products
    items_df2 <- purrr::map_df(items_list, function(item) {
      tibble(
        title = item$properties$title,
        product = item$properties$`odc:product`,
        datetime = item$properties$datetime,
        end_datetime=item$properties$end_datetime,
        start_datetime=item$properties$start_datetime,
        epsg=item$properties$`proj:epsg`,
        instrument=paste0(item$properties$instruments,collapse = ";"),

        assets = item$assets #%>%
        # tibble::enframe() %>%
        # dplyr::mutate(href=purrr::map(value,~.x$href),
        #               title=purrr::map(value,~.x$title)) %>%
        # tidyr::unnest(href) %>%
        # dplyr::filter(stringr::str_ends(href,".tif")) %>%
        # dplyr::select(-value)

      )
    })



    dat.x <- items_df2 %>%
      dplyr::mutate(href=purrr::map(assets,~.x$href),
                    asset_title=purrr::map(assets,~.x$title)) %>%
      tidyr::unnest(c(href,asset_title)) %>%
      dplyr::filter(stringr::str_ends(href,".tif"),asset_title != "qa") %>%
      dplyr::select(-assets)

    #add a name for the sampled variable based on which names are in the df.
    #"product won't be in the df for dem
    if("product" %in% names(dat.x)){

      dat.x <-dat.x %>%  dplyr::mutate(sample_name=paste0(product,"__",asset_title))
    }else{
      dat.x <- dat.x %>% dplyr::mutate(sample_name=paste0(asset_title))

    }



    #test if any images were found
    if(nrow(dat.x)>0){










      #combine data and calculate https path and date times strings for file names
      dat.aws <- dat.x %>%
        #tidyr::pivot_longer(matches("href")) %>%
        mutate(href=stringr::str_replace(href,"s3://dea-public-data","https://dea-public-data.s3.ap-southeast-2.amazonaws.com")) %>%
        dplyr::mutate(r=purrr::map(href,~terra::rast(.x,vsi=T))) %>%
        dplyr::mutate(res=purrr::map(r,~terra::extract(.x,sf::st_transform(sf_object,sf::st_crs(.x)),ID=T,xy=T,cell=T)))

      #combine and remove duplicated ID columns
      res <- dat.aws %>%
        select(sample_name,instrument,dplyr::matches("datetime"),res) %>%
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




      res <- tidyr::pivot_wider(res,values_from = value,names_from = sample_name)


      #rm(dat.aws)










      all.res[[cname]] <- res

      #return(res2)

    }else{

      print("no stac data found")
    }

  }
  return(all.res)
}

