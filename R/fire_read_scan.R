fire_read_scan <- function(s3name){

  #if only the file name is input, add in the path
  if(!stringr::str_detect(s3name,"https")){
    s3name <- paste0("https://s3.its.uow.edu.au/uow-rb01186-ros-model/scansgda94/",s3name,".tif")
  }

  ras <- terra::rast(s3name)

  return(ras)

}
