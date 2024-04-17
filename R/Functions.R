## FUNCTIONS
##
## region_plot
## corner2poly
## kml_polygon
## find_closest
## complete_outline
## UTM_convert
## gmap_bb

###################################################

## Simplify the process of plotting a map that was
## extracted from map_data.

## Plot a region extracted using map_data. 
region_plot <- function(region,
                        fill      = "darkseagreen3",
                        color     = "black",
                        linewidth = 0.5,
                        coord     = 1.3,
                        mytheme   = NULL){
  plot <- ggplot(data=region,aes(x=long,
                                 y=lat, 
                                 group=group)) +
    geom_polygon(fill      = fill,
                 color     = color,
                 linewidth = linewidth) +
    coord_fixed(coord) +
    mytheme
  
  return (plot)
  
} ## end function region_plot

###################################################

## Use the ll and ur corner coordinates to create
## a data frame with labeled columns (lat, lon) for
## the corners (repeating the start corner) of a
## polygon.

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

###################################################

## Convert a KML polygon created in Google Earth
## to geographic (lat, lon) coordinates in a
## labeled data frame.

kml_polygon <- function(overlay_kml) {
  
  ## Read the KML data from an external file.
  kml_sf <- st_read(overlay_kml,
                    stringsAsFactors = FALSE) 
  
  ## Extract the geometry data.
  lon <- st_coordinates(kml_sf$geometry)[,1]
  lat <- st_coordinates(kml_sf$geometry)[,2]
  
  ## Create a data frame with the correct column names.  
  overlay_coord <- data.frame(cbind(lon,lat))
  colnames(overlay_coord) <- c("lon","lat")
  
  return(overlay_coord)
  
} ## end function kml_polygon

###################################################

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
    distance <- distm(
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

###################################################

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
  
  b_point1 <-find_closest(border = border, 
                          point  = first_point)
  
  b_point2 <-find_closest(border = border, 
                          point  = last_point)
  
  ## Get the locations to cut the border.
  cut_1 <- min(b_point1$index,b_point2$index)
  cut_2 <- max(b_point1$index,b_point2$index)
  
  ## Extract a section of the border
  ## The order in the slice reverses the segment order
  cut_section <- border |>
    slice(cut_2:cut_1) 
  
  ## Connect the border to the partial overlay
  full_outline <- rbind(cut_section, overlay)
  
  return(full_outline)
  
} ## end function complete_outline

###################################################

## Function to convert from UTM to x,y degrees
## Uses Package OCE

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
  lon_lat <- utm2lonlat(c_location$long, 
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

###################################################

## Function that extracts the bounding box from a
## map created using site_google_map and returns
## the result as a rectangular bounding box

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

###################################################
