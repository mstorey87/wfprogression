fire_search_copernicus <- function(fire_bbox,
                                   start_date,
                                   end_date,
                                   dest_folder,
                                   collection_names=c("SENTINEL-3"),
                                   user_copernicus="mstorey@uow.edu.au",
                                   password_copernicus="rR$VRQFfFfg2D5"){


  #connect to stac source - copernicus
  stac_source <- rstac::stac(
    "https://catalogue.dataspace.copernicus.eu/stac"
  )


  #query based on location, date and collection name
  stac_query <- rstac::stac_search(
    q = stac_source,
    bbox = sf::st_bbox(sf::st_transform(fire_bbox,4326)),#which crs??
    collections = collection_names,
    datetime = paste0(start_date,"T00:00:00Z","/",end_date,"T00:00:00Z"),
     limit = 5000
  )

  date_seq <- as.POSIXct(paste0(seq(as.Date(start_date),as.Date(end_date),by="1 day"),"T00:00:00Z"),tz="UTC")
  date_seq_2 <- date_seq+lubridate::days(1)
  dates_chr <- paste0(format(date_seq,format="%Y-%m-%dT%H:%M:%SZ"),"/",format(date_seq_2,format="%Y-%m-%dT%H:%M:%SZ"))


  #create stac query for each date to avoid limit on items return for each query
  stac_queries <- purrr::map(dates_chr,~rstac::stac_search(
    q = stac_source,
    bbox = sf::st_bbox(sf::st_transform(fire_bbox,4326)),#which crs??
    collections = collection_names,
    datetime = .x,
    limit = 1000
  ))


  executed_stac_query <- purrr::map(stac_queries, rstac::get_request)

  #loop through each and   extract items properties including path and url

  res.list <- list()
  for(i in 1:length(executed_stac_query)){
    exec.i <- executed_stac_query[[i]]

    #extract items properties including path and url
    dat.x <- purrr::map(exec.i$features,~tibble::tibble(sen3_path=.x$assets$PRODUCT$href,
                                                        filename=basename(.x$assets$PRODUCT$alternate$s3$href),
                                                 satellite=.x$collection,
                                                 product=.x$properties$productType,
                                                 datetime=.x$properties$datetime)) %>%
      dplyr::bind_rows()

    res.list[[i]] <- dat.x
  }

  #combine the results
  dat.x.all <- do.call(rbind,res.list)






  #generate time and date and output name fields
  dat.x.all <- dat.x.all %>%
    dplyr::mutate(datetimeutc=as.POSIXct(datetime,format="%Y-%m-%dT%H:%M:%OS",tz="UTC"),
                  datetimelocal=lubridate::with_tz(datetimeutc,tz=dat.tz$tz_name),
                  datetimelocal_chr=format(datetimelocal,format="%Y%m%d_%H%M%S"),
                  outfile=paste0(dest_folder,"\\",filename,".zip"))%>%

    #filter to RBT, which is radiances
    dplyr::filter(product=="SL_1_RBT___")%>%

    #we only need one image for each time. Remove one of the files with identical time
    #sort by processing numbers first.
    dplyr::mutate(processing=substr(filename,87,87),
                  NNN=substr(filename,92,94))%>%
    dplyr::group_by(datetimelocal_chr) %>%
    dplyr::arrange(dplyr::desc(processing),dplyr::desc(NNN)) %>%
    dplyr::filter(dplyr::row_number()==min(dplyr::row_number())) %>%
    dplyr::ungroup()


  #download from odata using curl commands
  #run this first and then add refresh token into code below

  #generate token to download the data from copernicus
  mytoken <- system(paste0('curl -s -X POST https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token -H "Content-Type: application/x-www-form-urlencoded" -d "username=',
                           user_copernicus,'" -d "password=',password_copernicus,'" -d "grant_type=password" -d "client_id=cdse-public"'), intern = T)



  #extract refresh token
  myrefreshtoken <- purrr::map_chr(stringr::str_split(mytoken,"refresh_token"),2)
  myrefreshtoken <- purrr::map_chr(stringr::str_split(myrefreshtoken,":"),2)
  myrefreshtoken <- purrr::map_chr(stringr::str_split(myrefreshtoken,","),1)
  myrefreshtoken <- substr(myrefreshtoken,2,nchar(myrefreshtoken)-1)


  for(i in 1:nrow(dat.x.all)){

    if(!file.exists(dat.x.all$outfile[i])){

      token_reset <- system(paste0('curl -s -X POST https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token -H "Content-Type: application/x-www-form-urlencoded" -d "grant_type=refresh_token" -d "refresh_token=',
                                   myrefreshtoken,'" -d "client_id=cdse-public"'), intern = T)

      token <- purrr::map_chr(stringr::str_split(token_reset,":"),2)
      token <- purrr::map_chr(stringr::str_split(token,","),1)
      token <- substr(token,2,nchar(token)-1)

      nchar(token)
      #if reset token times out and sends a short message, generate new token
      if(nchar(token) < 1000){
        mytoken <- system(paste0('curl -s -X POST https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token -H "Content-Type: application/x-www-form-urlencoded" -d "username=',
                          user_copernicus,'" -d "password=',password_copernicus,'" -d "grant_type=password" -d "client_id=cdse-public"'), intern = T)

        #Refresh token
        myrefreshtoken <- substr(map_chr(str_split(mytoken,"refresh_token"),2),4,754)

        token_reset <- system(paste0('curl -s -X POST https://identity.dataspace.copernicus.eu/auth/realms/CDSE/protocol/openid-connect/token -H "Content-Type: application/x-www-form-urlencoded" -d "grant_type=refresh_token" -d "refresh_token=',
                                     myrefreshtoken,'" -d "client_id=cdse-public"'), intern = T)

        token <- map_chr(str_split(token_reset,":"),2)
        token <- map_chr(str_split(token,","),1)
        token <- substr(token,2,nchar(token)-1)


      }


      sys_command <- paste0('curl -H "Authorization: Bearer ',token,'" "',dat.x.all$sen3_path[i],
                            '" --location-trusted --output ',dat.x.all$outfile[i])



      system(sys_command)
    }

  }






}
