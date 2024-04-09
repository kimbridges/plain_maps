#' Convert a dataframe of UTM coordinates to lat, lon coordinates.
#'
#' @param location The set of UTM coordinates.
#' @param zone A number indicating the UTM section.
#' @param hemisphere Whether "N" or "S".
#' @param km If the conversion should be km (FALSE = degrees)
#' @return A data frame with lat, lon values
#' @examples
#' niihau_deg <- UTM_convert(niihau_shp,zone=4,hemisphere="N",km=FALSE)
#' @export

## Function to convert from UTM to x,y degrees

UTM_convert <- function(location,
                        zone       = 4,
                        hemisphere = "N",
                        km         = FALSE){
  
  ## Defaults are for Hawaii
  
  ## Separate out the set of coordinates (UTM)  
  coord_set <- unlist(location)
  
  ## Find the number of items and set the wrap limits
  c_items <- length(coord_set)
  c_lon_end <- c_items/2
  c_lat_start <- c_lon_end + 1
  
  ## Create parallel lists of the lat and long
  long <- location[1:c_lon_end]
  lat  <- location[c_lat_start:c_items]
  
  ## Put the lists together as columns & make data.frame
  c_location <- cbind(long,lat)
  c_location <- data.frame(c_location)
  
  ## Use the OCE package function to do the conversion
  lon_lat <- OCE::utm2lonlat(c_location$long, 
                        c_location$lat, 
                        zone       = zone, 
                        hemisphere = hemisphere, 
                        km         = FALSE)
  
  ## Make the result a data.frame
  c_location <- data.frame(lon_lat)
  
  ## Rename the coordinate columns to lon, lat.
  colnames(c_location) <- c("lon","lat")
  
  ## Return the converted coordinates
  return(c_location)
  
} ## End function UTM_convert
