#' Search NCI thredds server for sentinel 2 data
#'
#' @param fire_bbox bounding box sf polygon of a fire
#' @param start_date start date to search for sentinel 2 images (YYYY-mm-dd)
#' @param end_date last date to search for sentinel 2 images (YYYY-mm-dd)
#'
#' @return a data frame with download paths of sentinel 2 imagery
#' @export
#'
#' @examples
#' #x <- fire_process_polygon(dat.fire.polygon,4283,"FireName","StartDate","EndDate",2,2,buffer_km = 20)
#' #z <- fire_search_sentinel2(x$fire_bbox)
fire_search_sentinel2 <- function(fire_bbox,start_date,end_date){

  #find the sentinel2 tiles that intersect with the fire bounding box
  #path and row number will be used to search for sentinel2 files
  s2_tiles <- dat.sentinel2.pathrow %>%
    sf::st_transform(sf::st_crs(fire_bbox)) %>%
    dplyr::filter(sf::st_intersects(.,fire_bbox,sparse = F)[,1])

  #the path to the thredds catalogue of sentinel2 data held by Geoscience Australia
  #path to each bucket/folder name of sentinel2 7, 8 and 9. sentinel2 5 (1984 - 2013) is also available.
  path_thredds <- paste0("https://thredds.nci.org.au/thredds/catalog/ka08/",
                         c("ga_s2am_ard_3", "ga_s2bm_ard_3"),
                         "/")


  #add (sentinel2) path and row combinations to thredds path
  path_1 <- expand.grid(path_thredds,s2_tiles$Name) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(),as.character)) %>%
    dplyr::mutate(path_1=paste0(Var1,substr(Var2,1,2),"/",substr(Var2,3,5),"/")) %>%
    .$path_1


  #add in all dates to search for images from fire start to fire end
  #format for thredds path
  dates_fires <- seq(start_date,end_date,by="1 day")

  dat.thredds <- expand.grid(path_1,dates_fires) %>%
    dplyr::mutate(Var1=as.character(Var1),
                  path_catalog=paste0(Var1,format(Var2,format="%Y/%m/%d_interim/"),"catalog.html"))

  #some file paths have "interim" and some don't, so we need to search both versions
  dat.thredds <- rbind(dat.thredds,dat.thredds %>% dplyr::mutate(path_catalog=stringr::str_replace(path_catalog,"_interim","")))

  dat.thredds <- dat.thredds %>%
    #use rvest based function to create a list of all directories in each catalog path. This will be a directory that contain the image date and time, will files below that.
    #Get the directories that exist first. The directory names contain numbers and the letter T
    dplyr::mutate(file_name=purrr::map(path_catalog,~fire_thredds_list(.x,"[T][0-9]"))) %>%
    #update paths to add file names within each directory that exists
    dplyr::filter(!is.na(file_name)) %>%
    dplyr::mutate(path_catalog=paste0(stringr::str_replace(path_catalog,"catalog.html",""),file_name,"/catalog.html")) %>%

    #use rvest based function to create a list of all files in each catalog path
    dplyr::mutate(file_name=purrr::map(path_catalog,~fire_thredds_list(.x,"\\.tif$|\\.yaml$|\\.jpg$|\\.sha1$|\\.json$"))) %>%

    #unnest the results
    tidyr::unnest(cols="file_name") %>%

    #create download path
    dplyr::mutate(path_download=stringr::str_replace(path_catalog,"catalog","fileServer"),
                  path_download=paste0(stringr::str_replace(path_download,"catalog.html",""),file_name)) %>%

    dplyr::select(Date=Var2,path_catalog,file_name,path_download)



  print(paste0("No images for ",paste0(dat.thredds.NA$Date,collapse = ", ")))
  print(paste0("Images found for ",paste0(dat.thredds.exists$Date,collapse = ", ")))

  if(!is.null(regex_filter)){

    dat.thredds <- dat.thredds %>%
      dplyr::filter(stringr::str_detect(file_name,regex_filter))
  }




  return(dat.thredds)

}
