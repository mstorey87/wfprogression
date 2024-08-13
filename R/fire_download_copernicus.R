fire_download_copernicus <-  function(fire_bbox,
                                      dest_folder,
                                      user_copernicus="mstorey@uow.edu.au",
                                      password_copernicus="rR$VRQFfFfg2D5"){

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
