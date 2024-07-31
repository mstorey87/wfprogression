#' Download a list of thredds (or other) files
#'
#' @param df_download a data frame with a list of path in a path_download column
#' @param dest_folder destination folder
#'
#' @return Nothing. Downloads files to destination folder
#' @export
#'
#' @examples
#' #x <- fire_process_polygon(dat.fire.polygon,4283,"FireName","StartDate","EndDate",2,2,buffer_km = 20)
#' #z <- fire_search_landsat(x$fire_bbox)
#' #fire_download_thredds(z,"C:/data")
fire_download_thredds <- function(df_download,dest_folder){

  df_download <- df_download %>%
    dplyr::filter(!is.na(file_name)) %>%
    dplyr::mutate(dest_file=paste0(dest_folder,"/",file_name))

  download.file(df_download$path_download,df_download$dest_file,mode="wb")


}
