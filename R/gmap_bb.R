#' Extract the bounding box from a data frame created by site_google_basemap function.
#'
#' @param gmap A dataframe created with the site_google_basemap function.
#' @return A rectangular polygon that is the same as the gmap outline.
#' @examples
#' overlay_poly <- gmap_bb(data_map)
#' @export

gmap_bb <- function(gmap) {
  
## Get the bounding box from the map created
## with site_google_basemap.
## This uses the ggmap function bb2bbox.
bbox <- bb2bbox(attr(gmap, "bb"))

## Put the values into an appropriate format.
lon <- c(bbox[1], bbox[3])
lat <- c(bbox[2], bbox[4])
range <- data.frame(cbind(lon,lat))

## Make the range points into a polygon.
box_poly <- corner2poly(range)

return(box_poly)

} ## End gmap_bb