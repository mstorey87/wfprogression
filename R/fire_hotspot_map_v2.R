#' Create geoferenced hotspot and WMS image
#'
#' @param fire_bbox Polygon of fire or area to search for images
#' @param start_date First date from which to search for images
#' @param end_date Final date to search for image
#' @param mapkey A mapkey reuired to download hotspots, https://firms.modaps.eosdis.nasa.gov/api/map_key/
#' @param dest_folder Folder to save output tifs other files used in processing, including hotspots
#' @param add_hotspots T/F to add or exclude hotspots from output images
#' @param outwidth width in pixels of output tif
#'
#' @return Write a geotiff to disk. Also writes hotspots csv
#' @export
#'
#' @examples
#' #fire_hotspot_map(fire_bbox = dat.bbox,start_date = datestart,end_date = dateend,dest_folder = outdir)
fire_hotspot_map_v2 <- function(fire_bbox,start_time,end_time,mapkey="a5452249ca7c7a4ee2e1e6da787f57cc",dest_folder,add_hotspots=T,outwidth=500){


  checkmate::assert(stringr::str_detect(class(start_time)[1],"POSIXct"),"Error: times must be posixct")
  checkmate::assert(stringr::str_detect(class(end_time)[1],"POSIXct"),"Error: times must be posixct")


  mytz=wfprogression::fire_get_timezone(fire_bbox)

  start_time=lubridate::with_tz(start_time,tz=mytz)
  end_time=lubridate::with_tz(end_time,tz=mytz)

  #create a series of GIBS and hotspots maps
  #define the sequence of dates
  start_date=format(start_time,format="%Y-%m-%d")
  end_date=format(end_time,format="%Y-%m-%d")

  date_seq <- seq(as.Date(start_date),as.Date(end_date)+1,by="1 day")#dates are in utc, so include day post to capture local date

  if(add_hotspots==T){

    add_hotspots_test=T

    #download hotspots
    hotspots <- wfprogression::fire_search_hotspots(fire_bbox,mapkey,
                                                    min(date_seq),
                                                    max(date_seq),
                                                    dest_folder)
    message("hotspots downloaded")
    if(nrow(hotspots)>0){
      hotspots <- hotspots  %>%
        dplyr::filter(datelocal %in% date_seq,
                      datetimelocal>start_time & datetimelocal<end_time) %>%
        sf::st_transform(4326)
    }


    #if no hotspots are found, don't add hotspots to map
    if(nrow(hotspots)==0){
      add_hotspots_test=F

    }

  }else{
    hotspots=NULL
  }



  #define all the WMS layers that will be used. All of these have infrered or thermal bands that highlight active fire.
  #these can be viewed through NASA worldview
  wms_layers <- c("MODIS_Aqua_CorrectedReflectance_Bands721",
                  "MODIS_Terra_CorrectedReflectance_Bands721",
                  "VIIRS_SNPP_CorrectedReflectance_BandsM11-I2-I1",
                  "VIIRS_NOAA20_CorrectedReflectance_BandsM11-I2-I1",
                  "MODIS_Aqua_Brightness_Temp_Band31_Night",
                  "MODIS_Terra_Brightness_Temp_Band31_Night",
                  "VIIRS_NOAA21_Brightness_Temp_BandI5_Night",
                  "VIIRS_NOAA20_Brightness_Temp_BandI5_Night",
                  "VIIRS_SNPP_Brightness_Temp_BandI5_Night")



  #for each day, create and save the wms map image
  for(i in 1:length(date_seq)){
    date_string <- paste0(date_seq[i],"/",date_seq[i])
    message(paste("loop ",date_string))

    for(wms in wms_layers){
      #message(wms)
      # Define temporary file names for PNG and TIFF outputs
      outpng1 <- tempfile(fileext = ".png")
      outpng2 <- tempfile(fileext = ".png")
      outpng3 <- tempfile(fileext = ".png")
      outtif1 <- tempfile(fileext = ".tif")
      outtif2 <- tempfile(pattern = paste0("_",date_seq[i],"_",wms,"_"),fileext = ".tif")



      #calculate pixels for output png
      bbox <- sf::st_bbox(fire_bbox)
      width <- bbox$xmax - bbox$xmin
      height <- bbox$ymax - bbox$ymin

      # Calculate the ratio of width to height
      ratio <- height / width
      outwidth <- outwidth
      outheight <- round(outwidth * ratio)

      #message("putting query together")
      wms_url <- 'https://gibs.earthdata.nasa.gov/wms/epsg4326/best/wms.cgi?'
      # Define the parameters for the WMS request (replace with your desired parameters)
      params <- list(
        SERVICE = "WMS",
        VERSION = "1.1.1",         # WMS version (can vary based on your service)
        REQUEST = "GetMap",
        LAYERS = wms,     # Specify the layer you want to download
        SRS = "EPSG:4326",         # Coordinate reference system
        BBOX = paste0(bbox, collapse = ","), # Bounding box (left,bottom,right,top)
        WIDTH = outwidth,          # Image width in pixels
        HEIGHT = outheight,        # Image height in pixels
        FORMAT = "image/png",      # Output format (PNG)
        TRANSPARENT = "TRUE",
        TIME = date_string         # Transparent background (optional)
      )

      # Send the WMS request and get the image response
      response <- httr::GET(wms_url, query = params)
      #message("got response")

      # Check if the request was successful
      if (httr::status_code(response) == 200) {
        # Save the image content to a file
        img_data <- httr::content(response, "raw")
        writeBin(img_data, outpng1)
        #message("Image saved as wms_image.png")
      } else {
        stop("Failed to download image. HTTP status: ", httr::status_code(response))
      }

      # Convert PNG to geotiff
      img <- png::readPNG(outpng1)
      #grid::grid.raster(img)
      r1 <- terra::rast(img)
      r1 <- c(r1[[1]], r1[[2]], r1[[3]])
      r1 <- terra::stretch(r1)
      terra::crs(r1) <- sf::st_crs(4326)$wkt
      terra::ext(r1) <- terra::ext(fire_bbox)

      # Write the RGB raster to a TIFF file
      terra::writeRaster(r1, outtif1, overwrite = TRUE)
      #message("first tif")


      #add hotspots if user wants them and if there are any
      #add hotspots
      if(add_hotspots==T){

        #if no hotspots have been found, just export image
        if(nrow(hotspots)==0 & add_hotspots==T){
          file.copy(outtif1,paste0(dest_folder,"\\x",basename(outtif2)),overwrite=T)
          next
        }

        #create unique values for legend and filter hotspots by time and satellite
        #get abbreviation wms layer name that will match sat_name column in hotspots
        wms_lyr_name_abbrev <- stringr::str_split(wms,"_")
        wms_lyr_name_abbrev <- tolower(paste0(wms_lyr_name_abbrev[[1]][1],"_",wms_lyr_name_abbrev[[1]][2]))




        #filter hotspots to day or night depending on wms layer name
        if(stringr::str_detect(wms,"Night")){


          #if night time, filter by acq_date (UTC)
          hotspots2 <- hotspots %>%
            dplyr::filter(daynight=="N",
                          acq_date==date_seq[i])
        }else{
          hotspots2 <- hotspots %>%
            dplyr::filter(daynight=="D",
                          datelocal==date_seq[i])

        }


        #add some useful columns for satellite/sensor name and a label for the map
        hotspots2 <- hotspots2 %>%
          dplyr::filter(sat_name==wms_lyr_name_abbrev) %>%
          dplyr::mutate(label=paste0(sat_name," ",datetimelocal)) %>%
          sf::st_transform(4326)


        #message("hotspots 2 filtered")



        if(nrow(hotspots2)==0 & add_hotspots==T){
          file.copy(outtif1,paste0(dest_folder,"\\x",basename(outtif2)),overwrite=T)
          next
        }

        if(nrow(hotspots2)>0){

          #load wms capture and add points
          #raster_image <- terra::rast(outtif1)

          # Convert the raster to a data frame (x, y coordinates and RGB values)
          hotspots2$datetimelocal <- as.factor(hotspots2$datetimelocal)

          # Define a color palette for the factor levels
          n_levels <- length(levels(hotspots2$datetimelocal))

          if (n_levels < 3) {
            # Assign custom colors for fewer than 3 levels
            if (n_levels == 1) {
              color_palette <- c("red")  # Only one level, color red
            } else if (n_levels == 2) {
              color_palette <- c("red", "blue")  # Two levels, color red and blue
            }
          } else {
            # If there are 3 or more levels, use a color palette
            color_palette <- RColorBrewer::brewer.pal(n_levels, "Set1")
          }

          # Map each factor level to a color
          hotspots2$color <- color_palette[as.integer(hotspots2$datetimelocal)]

          # Set up the PNG output with transparent background
          png(outpng2,
              width = outwidth + (0.1 * outwidth),
              height = outheight + (0.1 * outheight),
              bg = "transparent",
              units = "px")

          # Remove unnecessary space around the plot
          par(mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0))

          # Normalize point size based on output width
          point_size <- (outwidth / 1000)  # Adjust the scaling factor as needed

          # Plot the RGB raster
          terra::plotRGB(r1, axes = FALSE, mar = c(0, 0, 0, 0))

          # Overlay the points with color based on 'datetimelocal' factor levels
          plot(hotspots2["datetimelocal"], add = TRUE,
               col = hotspots2$color, pch = 16, cex = point_size)

          # Get unique color groups from the factor levels
          unique_colors <- levels(hotspots2$datetimelocal)

          # Scale the label size relative to the image width
          label_size <- 0.7 * (outwidth / 500)

          # Add labels to one point from each color group
          for (color in unique_colors) {
            # Get the  pointf from the current color group
            point <- hotspots2[hotspots2$datetimelocal == color, ]

            # Extract themean coordinates of the points
            x_coord <- mean(sf::st_coordinates(point)[,1])
            y_coord <- mean(sf::st_coordinates(point)[,2])


            # Add a bold text label with the matching point color
            text(x_coord, y_coord, labels = color,
                 col = point$color[1], cex = label_size, pos = 4, font = 2)
          }

          # Close the output device
          dev.off()

          #message("png 2 saved")

          # Read and process the image with magick
          r <- magick::image_read(outpng2)
          r <- magick::image_trim(r, 1)
          r <- magick::image_resize(r, paste0(outwidth, "x", outheight, "!"))

          # Save the processed image
          magick::image_write(r, outpng3)
          #message("PNG cropped")

          # Convert the trimmed image to a raster
          img <- png::readPNG(outpng3)
          x <- terra::rast(img)
          x <- c(x[[1]], x[[2]], x[[3]])

          terra::crs(x) <- sf::st_crs(4326)$wkt
          terra::ext(x) <- terra::ext(r1)

          # Write the RGB raster to a TIFF file
          #get most common time in hotspots
          count_table <- table(hotspots2$datetimelocal)
          most_common <- names(count_table)[which.max(count_table)][[1]]
          most_common <- format(as.POSIXct(as.character(most_common)),format="%Y%m%d%H%M%S")
          outtif2 <- tempfile(pattern = paste0(most_common,"_",date_seq[i],"_",wms,"_"),fileext = ".tif")
          terra::writeRaster(x, outtif2, overwrite = TRUE)

          outtif3 <- paste0(dest_folder,"\\",basename(outtif2))
          if(!outtif2==outtif3) file.copy(outtif2,outtif3,overwrite=T)
          #message("raster written")
          #message(list.files())

        }else{
          #print("no hotspots 1")
          # Write the RGB raster to a TIFF file
          terra::writeRaster(r1, outtif2, overwrite = TRUE)
          file.copy(outtif2,paste0(dest_folder,"\\",basename(outtif2)),overwrite=T)

          outtif3 <- paste0(dest_folder,"\\",basename(outtif2))
          if(!outtif2==outtif3) file.copy(outtif2,outtif3,overwrite=T)
        }


      }else{

        #print("no hotspots wanted")
        # Write the RGB raster to a TIFF file
        terra::writeRaster(r1, outtif2, overwrite = TRUE)
        file.copy(outtif2,paste0(dest_folder,"\\",basename(outtif2)),overwrite=T)

        outtif3 <- paste0(dest_folder,"\\",basename(outtif2))
        if(!outtif2==outtif3) file.copy(outtif2,outtif3,overwrite=T)
      }



    }


  }
}
