library(sf)
library(tidyverse)
library(shiny)
library(shinyFiles)
library(shinyWidgets)
library(leaflet)
library(mapedit)
library(wfprogression)
library(webshot)

map <- leaflet() %>%
  addTiles() %>% 
  #setView(lng = 134.3143, lat = -27.7631, zoom = 3)
  setView(lng = 149.95, lat = -32.1, zoom = 9)

options(timeout = 1000)  # Set timeout for downloads

ui <- fluidPage(
  titlePanel("Search Fire Images and Progressions"),
  
  sidebarLayout(
    sidebarPanel(
      h5( "Select dates and times using mouse"),
      airDatepickerInput("fromdate", "Select Start", timepicker = TRUE, value = "2017-02-12 00:00:00"),
      airDatepickerInput("todate", "Select End", timepicker = TRUE, value = "2017-02-13 00:00:00"),
      
      h5( "Sources"),
      checkboxInput("progscheck", "Progression polygons", FALSE),
      checkboxInput("imagescheck", "Line scans", FALSE),
      checkboxInput("GAstac", "Sentinel 2 and Landsat", FALSE),
      checkboxInput("gibs", "MODIS and VIIRS", FALSE),
      
      tags$hr(style = "border-top: 1px solid black; margin: 3px 0;"),
      h5( "Himawari imagery. 
      There is a ~30 day lag in image availability. 
      10 minute imagery available. 
          User can select (minutes) timestep for download. Band 14 may be useful for detecting PyroCb"),
      checkboxInput("H89", "Himawari", FALSE),
      checkboxInput("H89pyro", "Himawari band 14", FALSE),
      numericInput("him_minutes","Himawari timestep (minutes)", value = 240, min = 10, max = 1440,step=10),
      
      tags$hr(style = "border-top: 2px solid black; margin: 5px 0;"),
      
      h5( "Search for data only (faster) or search+prepare for download (slower)"),
      
      switchInput(inputId = "gather", 
                  label = "Switch option",
                  value = FALSE, 
                  onLabel = "Prepare ZIP", 
                  offLabel = "Search only", 
                  onStatus = "info",  # Light blue when ON
                  offStatus = "dark"),  # Dark when OFF
      
      actionButton('runquery', 'Run', class = "action-btn"),
      tags$hr(style = "border-top: 2px solid black; margin: 5px 0;"),
      #br(),
      downloadButton("downloadScans", "Download ZIP"),
    ),
    
    mainPanel(
      tags$div(style = "font-size: 18px; font-weight: bold; color: #2E3A59;", 
               "Draw one polygon"), 
      editModUI("map"),
      br(),
      fluidRow(column(12, uiOutput("display_text")))
    )
  )
)

server <- function(input, output, session) {
  
  edits <- callModule(editMod, leafmap = map, id = "map")
  
  text_list <- reactiveVal(list("Search results:"))
  zip_file_path <- reactiveVal(NULL)  # Store the path of the zip file
  
  observeEvent(input$runquery, {
    # Reset the text_list before starting the new query
    text_list(list("Search results:"))
    
    geom <- edits()$finished
    if (!is.null(geom)) {
      
      # Define output directory (inside tempdir)
      dir.out <- file.path(tempdir(), "data_out")
      if (!dir.exists(dir.out)) dir.create(dir.out, recursive = TRUE)
      
      timestart <- input$fromdate
      timeend <- input$todate
      new_text <- paste("Searched ", timestart, "to", timeend)
      text_list(append(text_list(), list(new_text)))
      
      my_bbox <- geom %>% st_as_sf()
      
      ########## Start Progress Bar ##########
      withProgress(message = "Processing...", value = 0, {
        
        ########## Progressions ##########
        if (input$progscheck == TRUE) {
          incProgress(0.15, detail = "Searching Progressions")  # Update progress
          geomTF = ifelse(input$gather == TRUE, T, F)
          progs <- fire_search_progs(my_bbox, timestart, timeend, return_geom = geomTF)
          
          if (is.data.frame(progs)) {    
            new_text <- paste0(length(unique(progs$dt_local)), " progs found")
            
            if (input$gather == T) {
              # Save progs as GeoPackage (GPKG) in temp directory
              gpkg_file <- file.path(dir.out, "progs.gpkg")
              sf::st_write(progs, gpkg_file, delete_dsn = TRUE)
            }
            
          } else {
            new_text <- "0 progression polygons found"
          }
          text_list(append(text_list(), list(new_text)))

        }
        
        ########## Line Scans ##########
        if (input$imagescheck == TRUE) {
          incProgress(0.15, detail = "Searching Line scans")  # Update progress
          scans <- fire_search_scans(my_bbox, timestart, timeend)
          
          if (is.data.frame(scans)) {    
            new_text <- paste0(nrow(scans), " scans found")
            
            if (input$gather == T) {
              # Save scans as GeoPackage (GPKG) in temp directory
              gpkg_file <- file.path(dir.out, "scans.gpkg")
              sf::st_write(scans, gpkg_file, delete_dsn = TRUE)
            }
            
          } else {
            new_text <- "0 scans found"
          }
          text_list(append(text_list(), list(new_text)))

        }
        
        ########## Landsat & Sentinel Data ##########
        if (input$GAstac == TRUE) {
          incProgress(0.15, detail = "Searching Landsat and Sentinel 2")  # Update progress
          stac <- fire_search_stac(my_bbox, timestart, timeend)
          
          if (is.data.frame(stac)) {
            new_text <- paste0(length(unique(stac$datetimelocal_chr)), " Landsat or Sentinel images found.")
            
            if (input$gather == T) {
              # Save images in the SAME DIRECTORY as scans
              fire_download_stac(my_bbox, stac, dest_folder = dir.out)
            }
          } else {
            new_text <- "0 Landsat or Sentinel images found"
          }
          text_list(append(text_list(), list(new_text)))

        }
        
        ########## Gibs  #################
        
        if (input$gibs == TRUE) {
          incProgress(0.15, detail = "Searching VIIRS and MODIS")  # Update progress
          new_text <- paste0("several gibs images per day")
          text_list(append(text_list(), list(new_text)))
          
          if (input$gather == TRUE) {
            fire_hotspot_map(my_bbox, timestart, timeend, dest_folder = dir.out)
          }
          
        }
        
        
        
        ########## Himawari Data ##########
        if (input$H89 == TRUE) {
          incProgress(0.15, detail = "Searching Himawari")  # Update progress
          him <- fire_search_himawari(my_bbox, timestart, timeend, timestep_minutes = as.numeric(input$him_minutes))
          if (is.data.frame(him)) {
            new_text <- paste0(length(unique(him$datetimelocal_chr)), " Himawari images")
            
            if (input$gather == TRUE) {
              fire_download_himiwari(my_bbox, him, dest_folder = dir.out)
               if(input$H89pyro==TRUE){
                 fire_download_himiwari(my_bbox, him, dest_folder = dir.out,bands = c("B14"))
               }
            }
          } else {
            new_text <- "0 Himawari images"
          }
          text_list(append(text_list(), list(new_text)))
 
        }
        
        ########## Zip the files ##########
        if (input$gather == T) {
          zip_file <- file.path(dirname(dir.out), "data_out.zip")
          if(file.exists(zip_file)){
            file.remove(zip_file)
          }
          
          filelist <- list.files(dir.out, full.names = TRUE, pattern = "\\.(gpkg|tif|shp)$")
          
          if (length(filelist) > 0) {
            
            setProgress(0.8, detail = "Preparing ZIP")  # Final progress update
            zip(zip_file, files = filelist, extras = "-j")  # Use -j flag
            zip_file_path(zip_file)  # Store ZIP path for download
            
            ########## Cleanup: Remove all files in `data_out` ##########
            unlink(dir.out, recursive = TRUE, force = TRUE)
            
            new_text <- paste0("Data ready. Click Download ZIP")
            text_list(append(text_list(), list(new_text)))
            
          } else {
            new_text <- paste0("no files to prepare")
            text_list(append(text_list(), list(new_text)))
          }

        }
        
      })
    }
  })
  
  output$downloadScans <- downloadHandler(
    filename = function() { 
      paste("fire_data", ".zip", sep = "")
    },
    content = function(file) {
      zip_file <- zip_file_path()
      if (!is.null(zip_file) && file.exists(zip_file)) {
        # Copy the zip file to the user's download location
        success <- file.copy(zip_file, file)
        
        # ✅ If copy is successful, delete the original zip file
        if (success) {
          #file.remove(zip_file)
        }
      }
    }
  )
  
  output$display_text <- renderUI({
    lapply(text_list(), function(txt) {
      tags$p(txt)
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
