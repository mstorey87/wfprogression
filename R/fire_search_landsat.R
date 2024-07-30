fire_search_landsat <- function(fire_bbox,dest_folder){

  #find the landsat tiles that intersect with the fire bounding box
  #path and row number will be used to search for landsat files
  lsat_tiles <- dat.landsat.pathrow %>%
    dplyr::filter(sf::st_intersects(.,fire_bbox,sparse = F)[,1])

  #the path to the thredds catalogue of landsat data held by Geoscience Australia
  #path to each bucket/folder name of Landsat 7, 8 and 9. Landsat 5 (1984 - 2013) is also available.
  path_thredds <- paste0("https://thredds.nci.org.au/thredds/fileServer/xu18/",
                       c("ga_ls7e_ard_3", "ga_ls8c_ard_3","ga_ls9c_ard_3"),
                       "/")


  #add (landsat) path and row combination to thredds path
  pathrow <- paste0(lsat_tiles$PATH,"/",lsat_tiles$ROW,"/")

  dat.thredds <- expand.grid(path_thredds,pathrow) %>%
    mutate(path_1=paste0(Var1,Var2))


  #add in all dates to search for images from fire start to fire end
  #format for thredds path
  dates_fires <- seq(fire_bbox$startdate_search,fire_bbox$enddate_search,by="1 day")

  dat.thredds <- expand.grid(dat.thredds$path_1,dates_fires) %>%
    mutate(Var1=as.character(Var1),
           path_download=paste0(Var1,format(Var2,format="%Y/%m/%d/"))) %>%
      #construct file name for band 1 to check if it exists
    #add a destination path
  mutate(sat=substr(Var1,nchar(Var1)-21,nchar(Var1)-15),
         pathrow=stringr::str_replace(substr(Var1,nchar(Var1)-7,nchar(Var1)-1),"/",""),

         path_download=paste0(path_download,
                              sat,
                              "_nbar_3-0-0_",pathrow,"_",
                              Var2,
                              "_final_band01.tif"),

         dest_file=paste0(dest_folder,"/",basename(path_download)))



  #download the files that exist
  for(i in 1:nrow(dat.thredds)){

    pth <- dat.thredds$path_download[i]
    res <- tryCatch(curl::curl_download(pth,dat.thredds$dest_file[i]),
                               error = function(e) 'does not exist')

    #if it doesn't exist, try replace the 3-0-0 string
    if(res=="does not exist"){

      pth <- str_replace(pth,"3-0-0","3-2-1")

      tryCatch(curl::curl_download(pth,dat.thredds$dest_file[i]),
             error = function(e) 'does not exist')

    }



  }





  return(lsat_tiles)

}
