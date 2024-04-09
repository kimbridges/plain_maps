#' Find the row number of a border polygon closest to a point.
#'
#' @param border A dataframe with lat, lon data describing the border outline.
#' @param point A pair of lat, lon values.
#' @return The row number of the border dataframe closest to the point.
#' @examples
#' nearby <- data.frame(lat = 15, lon = -130)
#' row_number <- find_closest(boundary, nearby)
#' @export

## Find the closest point on a border polygon.

find_closest <- function(border, point){
  
  ## Initialize.
  closest <- 99999.9
  c_point <- NULL
  
  ## Find the length of the border.
  nrow(border)
  for(i in 1:nrow(border)) { ## row loop
    b_point <- c(border$lon[[i]], border$lat[[i]])
    p_point <- c(point$lon, point$lat)
    distance <- geosphere::distm(
      b_point,
      p_point, 
      fun = distHaversine)
    if(distance < closest) {
      c_point$index <- i
      c_point$lat   <- border$lat[[i]]
      c_point$lon   <- border$lon[[i]]
      c_point$distance <- distance
      closest <- distance } ## end if
    
  } ## end row loop
  
  return(c_point)
  
} ## end function find_closest
