#' Create bounding box coordinates from ll, ur coordinates.
#'
#' @param corners A dataframe with ll and ur lat, lon.
#' @return A dataframe with five rows of lat, lon.
#' @examples
#' two_points <- data.frame(lon=c(100,110),lat=c(20,25))
#' bb <- corner2poly(two_points)
#' @export

## Use the ll and ur corner coordinates to create
## a data frame with labeled columns (lat, lon) for
## the corners (repeating the start corner) of a
## bounding-box polygon.

corner2poly <- function(corners){
  five_lon <- c(corners$lon[1], 
                corners$lon[2], 
                corners$lon[2], 
                corners$lon[1],
                corners$lon[1])
  five_lat <- c(corners$lat[1], 
                corners$lat[1],
                corners$lat[2], 
                corners$lat[2],
                corners$lat[1])
  five_poly <- data.frame(cbind(five_lon, five_lat))
  colnames(five_poly) <- c("lon", "lat")
  
  return(five_poly)
  
} ## end function corner2poly
