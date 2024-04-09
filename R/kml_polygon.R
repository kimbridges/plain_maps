#' Convert a Google Earth Pro polygon to geographic coordinates (lat, lon).
#'
#' @param overlay_kml The polygon from Google Earth Pro.
#' @return A dataframe with columns for lat, lon.
#' @examples
#' overlay <- kml_polygon(kml_object)
#' @export

## Convert a KML polygon created in Google Earth
## to geographic (lat, lon) coordinates in a
## labeled data frame.

kml_polygon <- function(overlay_kml) {
  
  ## Read the KML data from an external file.
  kml_sf <- sf::st_read(overlay_kml,
                    stringsAsFactors = FALSE) 
  
  ## Extract the geometry data.
  lon <- sf::st_coordinates(kml_sf$geometry)[,1]
  lat <- sf::st_coordinates(kml_sf$geometry)[,2]
  
  ## Create a data frame with the correct column names.  
  overlay_coord <- data.frame(cbind(lon,lat))
  colnames(overlay_coord) <- c("lon","lat")
  
  return(overlay_coord)
  
} ## end function kml_polygon
