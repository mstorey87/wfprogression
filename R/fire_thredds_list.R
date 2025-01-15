#' Internal function that list files in thredds https path
#'
#' @param catalog_url catalog.html path
#' @param regex regex filter string
#'
#' @return file names
#' @export
#'
#' @examples
#'  #dplyr::mutate(df,
#'  #file_name=purrr::map(path_catalog,~fire_thredds_list(.x,"\\.tif$|\\.yaml$|\\.jpg$|\\.sha1$|\\.json$")))
fire_thredds_list <- function(catalog_url,regex){

  tryCatch(
    {
      thredds_filenames <- rvest::read_html(catalog_url) %>%
        rvest::html_text() %>%

        # split text into a vector of strings
        stringr::str_split(pattern = "[\r\n]+") %>%
        `[[`(1) %>%

        # subset to those strings that name a file that we want
        stringr::str_subset(pattern = regex) %>%

        # remove leading and trailing whitespace from file names
        stringr::str_trim()

      #print(catalog_url," ok")

      closeAllConnections()
      return(thredds_filenames)


    },
    error=function(e) {
      #message('An Error Occurred')
      #print(paste0(catalog_url," ",e))
      return(NA)
    },
    warning=function(w) {
      #message('A Warning Occurred')
      #print(w)

    }
  )




}


