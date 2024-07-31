#' Search NCI thredds server for Landsat data
#'
#' @param fire_bbox bounding box sf polygon of a fire
#'
#' @return a data frame with download paths of landsat imagery
#' @export
#'
#' @examples
#' #x <- fire_process_polygon(dat.fire.polygon,4283,"FireName","StartDate","EndDate",2,2,buffer_km = 20)
#' #z <- fire_search_landsat(x$fire_bbox)
fire_search_landsat <- function(fire_bbox){

  #find the landsat tiles that intersect with the fire bounding box
  #path and row number will be used to search for landsat files
  lsat_tiles <- dat.landsat.pathrow %>%
    sf::st_transform(sf::st_crs(fire_bbox)) %>%
    dplyr::filter(sf::st_intersects(.,fire_bbox,sparse = F)[,1])

  #the path to the thredds catalogue of landsat data held by Geoscience Australia
  #path to each bucket/folder name of Landsat 7, 8 and 9. Landsat 5 (1984 - 2013) is also available.
  path_thredds <- paste0("https://thredds.nci.org.au/thredds/catalog/xu18/",
                       c("ga_ls7e_ard_3", "ga_ls8c_ard_3","ga_ls9c_ard_3",
                         "ga_ls5t_ard_3"),
                       "/")


  #get (landsat) path and row combination to create thredds path
  pathrow <- paste0(lsat_tiles$PATH,"/",lsat_tiles$ROW,"/")

  #get all dates to search for images from fire start to fire end
  #this will be used to create thredds paths
  dates_fires <- seq(fire_bbox$startdate_search,fire_bbox$enddate_search,by="1 day")

  #create vector with all combinations of sensor and pathrow
  path_1 <- expand.grid(path_thredds,pathrow) %>%
    dplyr::mutate(path_1=paste0(Var1,Var2)) %>%
    .$path_1

  #create datafram will all combinations of path_1 and date
  dat.thredds <- expand.grid(path_1,dates_fires) %>%
    dplyr::mutate(Var1=as.character(Var1),
           path_catalog=paste0(Var1,format(Var2,format="%Y/%m/%d/"),"catalog.html")) %>%
    #head(10) %>%

    #use rvest based function to create a list of all files in each catalog path
    dplyr::mutate(file_name=purrr::map(path_catalog,~fire_thredds_list(.x,"\\.tif$|\\.yaml$|\\.jpg$|\\.sha1$|\\.json$"))) %>%

    #unnest the results
    tidyr::unnest(cols="file_name") %>%

    #create download path
    dplyr::mutate(path_download=str_replace(path_catalog,"catalog","fileServer"),
                  path_download=str_replace(path_download,"catalog.html",file_name)) %>%

    dplyr::select(Date=Var2,path_catalog,file_name,path_download)


  return(dat.thredds)

}
