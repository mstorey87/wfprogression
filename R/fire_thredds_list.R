#' List files from a THREDDS catalog URL filtered by a regex pattern
#'
#' @description
#' This internal utility function fetches the content of a THREDDS catalog HTML page for Himawari data from BOM,
#' extracts all file names listed on that page, and returns only those that match a
#' specified regular expression pattern. It is useful for programmatically listing netcdf files at a path for a specific time
#' e.g.`https://thredds.nci.org.au/thredds/catalog/ra22/satellite-products/arc/obs/himawari-ahi/fldk/latest/2025/01/01/0000/catalog.html`
#'
#' The function safely handles errors by returning NA if the catalog cannot be accessed.
#' It also trims whitespace and filters the filenames according to the regex provided.
#'
#' @param catalog_url Character. The URL to the THREDDS catalog HTML page for a specific time (e.g., `https://thredds.nci.org.au/thredds/catalog/ra22/satellite-products/arc/obs/himawari-ahi/fldk/latest/2025/01/01/0000/catalog.html`).
#' @param regex Character. A regular expression pattern used to filter the filenames returned. (e.g. `ABOM_OBS_` returns observations for each band)
#'
#' @return Character vector of filtered file names from the catalog page, or NA if there was an error accessing the page.
#' @export
#'
#' @examples
#' fire_thredds_list(catalog_url="https://thredds.nci.org.au/thredds/catalog/ra22/satellite-products/arc/obs/himawari-ahi/fldk/latest/2025/01/01/0000/catalog.html",
#'                   regex="ABOM_OBS_")
fire_thredds_list <- function(catalog_url,regex){

  tryCatch(
    {
      thredds_filenames <- rvest::read_html(catalog_url) %>%
        rvest::html_text() %>%

        # Split the entire page text into lines
        stringr::str_split(pattern = "[\r\n]+") %>%
        `[[`(1) %>%

        # Filter lines that match the file pattern regex
        stringr::str_subset(pattern = regex) %>%

        # Remove any leading or trailing whitespace from each filename
        stringr::str_trim()

      # Close all open connections to avoid resource leaks
      closeAllConnections()

      # Return the filtered list of file names
      return(thredds_filenames)
    },
    error=function(e) {
      # In case of error (e.g., network issue), close connections and return NA
      closeAllConnections()
      return(NA)
    },
    warning=function(w) {
      # Warnings are silently ignored here but could be logged if needed
    }
  )
}


#' #' Internal function that list files in thredds https path
#' #'
#' #' @param catalog_url catalog.html path
#' #' @param regex regex filter string
#' #'
#' #' @return file names
#' #' @export
#' #'
#' #' @examples
#' #'  #dplyr::mutate(df,
#' #'  #file_name=purrr::map(path_catalog,~fire_thredds_list(.x,"\\.tif$|\\.yaml$|\\.jpg$|\\.sha1$|\\.json$")))
#' fire_thredds_list <- function(catalog_url,regex){
#'
#'   tryCatch(
#'     {
#'       thredds_filenames <- rvest::read_html(catalog_url) %>%
#'         rvest::html_text() %>%
#'
#'         # split text into a vector of strings
#'         stringr::str_split(pattern = "[\r\n]+") %>%
#'         `[[`(1) %>%
#'
#'         # subset to those strings that name a file that we want
#'         stringr::str_subset(pattern = regex) %>%
#'
#'         # remove leading and trailing whitespace from file names
#'         stringr::str_trim()
#'
#'       #print(catalog_url," ok")
#'
#'       closeAllConnections()
#'       return(thredds_filenames)
#'
#'
#'     },
#'     error=function(e) {
#'       #message('An Error Occurred')
#'       #print(paste0(catalog_url," ",e))
#'       closeAllConnections()
#'       return(NA)
#'     },
#'     warning=function(w) {
#'       #message('A Warning Occurred')
#'       #print(w)
#'
#'     }
#'   )
#'
#'
#'
#'
#' }
#'
#'
