#' Extract a border segment and join to the ends of an incomplete polygon.
#'
#' @param border A polygon of lat, lon that outlines an area.
#' @param overlay An incomplete polygon of lat, lon to be completed with an extraction from the border.
#' @return A polygon with lat, lon.
#' @examples
#' finished_overlay <- complete_outline(peru, overlay)
#' @export

## Add an extracted section of a coastline border to an 
## incomplete overlay polygon. The result is a complete 
## overlay that coincides with the extracted section of 
## coastline.

complete_outline <- function(border, overlay){
  
  ## Uses a section of a border to complete an outline of a polygon.
  ## The ends of the overlay outline must come close to the section
  ## of the border that is to be used.
  
  ## Both the border and overlay must consist of data frames with
  ## lat and lon columns for the coordinates.
  
  ## Get the two locations from the overlay that will be connectors.
  first_point <- data.frame(
    lat = overlay$lat[[1]], 
    lon = overlay$lon[[1]])
  
  last_point  <- data.frame(
    lat = overlay$lat[[nrow(overlay)]], 
    lon = overlay$lon[[nrow(overlay)]])
  
  b_point1 <- find_closest(border = border, 
                          point  = first_point)
  
  b_point2 <- find_closest(border = border, 
                          point  = last_point)
  
  ## Get the locations to cut the border.
  cut_1 <- min(b_point1$index,b_point2$index)
  cut_2 <- max(b_point1$index,b_point2$index)
  
  ## Extract a section of the border
  ## The order in the slice reverses the segment order
  cut_section <- border |>
    dplyr::slice(cut_2:cut_1) 
  
  ## Connect the border to the partial overlay
  full_outline <- rbind(cut_section, overlay)
  
  return(full_outline)
  
} ## end function complete_outline
