#' Function to try loading a remote NetCDF file with retry on failure
#'
#' @param url nc URL, usually on THREDDS
#' @param max_tries number of times to try loading nc again in case of error
#' @param wait_seconds how long to wait between each try, in seconds
#'
#' @returns nc connection
#' @export
#'
#'
#' @examples
#' #
fire_tidync_safe <- function(url, max_tries = 5, wait_seconds = 10) {
  for (i in 1:max_tries) {
    result <- try(tidync::tidync(url), silent = TRUE)

    if (!inherits(result, "try-error")) {
      message("Success on attempt ", i)
      return(result)
    } else {
      message("Attempt ", i, " failed: ", conditionMessage(attr(result, "condition")))
      if (i < max_tries) {
        message("Retrying in ", wait_seconds, " seconds...")
        Sys.sleep(wait_seconds)
      } else {
        message("Failed after ", max_tries, " attempts.")
        return(NULL)
      }
    }
  }
}
