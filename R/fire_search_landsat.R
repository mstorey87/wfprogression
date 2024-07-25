fire_search_landsat <- function(fire_bbox){

  #find the landsat tiles that intersect with the fire bounding box
  #path and row number will be used to search for landsat files
  lsat_tiles <- dat.landsat.pathrow %>%
    filter(st_intersects(.,fire_bbox,sparse = F)[,1])

  return(lsat_tiles)

}
