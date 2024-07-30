#function to give spatial reference to exported png
fn_geo_png <- function(input_path,output_path,poly){
  img <- png::readPNG(input_path)
  x <- raster::brick(img)
  x <- raster::dropLayer(x,4)
  raster::crs(x) <- sf::st_crs(4326)$wkt
  raster::extent(x) <- raster::extent(poly)
  x <- raster::stretch(x,maxv=254)
  raster::writeRaster(x, output_path, format="GTiff", overwrite=TRUE,datatype="INT1U")
}
