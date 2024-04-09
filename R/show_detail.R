#' Place an overlay on a map polygon.
#'
#' @param polygon The map outline.
#' @param cc The geographic (lat,lon) corners (ll,ur) of the overlay.
#' @param p_linewidth With of the line surrounding the overlay.
#' @param p_color Color of the line surrouding the overlay.
#' @param p_fill Color for the inside fo the overlay.
#' @param p_alpha Transparency of the overlay.
#' @return A plot of the map with an overlay polygon.
#' @examples
#' new_map <- show_detail(namibia, skeleton_coast, p_color="orange", p_alpha=0.5)
#' @export

show_detail <- function(polygon, cc, 
                        p_linewidth= 0.5,
                        p_color="black",
                        p_fill="gray80",
                        p_alpha=1.0) {
  
  ## Define the cut-out polygon as a data frame.
  cut_lon <- c(cc$lon[1], cc$lon[2], cc$lon[2], cc$lon[1])
  cut_lat <- c(cc$lat[1], cc$lat[1], cc$lat[2], cc$lat[2])
  cut_poly <- data.frame(cbind(cut_lon, cut_lat))
  colnames(cut_poly) <- c("lon", "lat")
  
  ## Convert the cut-out polygon to the sf format.
  o_cut <- cut_poly %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
    summarise(geometry = sf::st_combine(geometry)) %>%
    sf::st_cast("POLYGON")
  
  ## Convert the detail outline to the sf format
  d_polygon <- polygon %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    summarise(geometry = sf::st_combine(geometry)) %>%
    sf::st_cast("POLYGON")
  
  ## Crop out the cut-outline from the detail outline.
  detail_cropped <- sf::st_intersection(o_cut, d_polygon)
  
  ## Extract lat,lon coordinates of the cropped detail outline.
  d_geometry <- data.frame(sf::st_coordinates(sf::st_cast(detail_cropped$geometry,"POLYGON")))
  
  ## Plot the coordinate data. Note use of xlim, ylim.
  detail_plot <- ggplot2::ggplot(data=d_geometry, aes(x=X, y=Y)) +
    ggplot2::geom_polygon(fill      = p_fill,
                 color     = p_color,
                 linewidth = p_linewidth,
                 alpha     = p_alpha) +
    ggplot2::xlim(cut_lon[[1]], cut_lon[[2]]) + 
    ggplot2::ylim(cut_lat[[2]], cut_lat[[3]]) +
    ggplot2::coord_fixed(1.1)
  
  return(detail_plot)
  
} ## End function show_detail
