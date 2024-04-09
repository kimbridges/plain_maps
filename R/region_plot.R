#' Create a map from borders extracted from map_data.
#'
#' @param region map_data extract.
#' @param fill The color for the interior (in quotes).
#' @param color The color for the outline (in quotes).
#' @param linewidth Size of the outline line.
#' @param coord A scale factor to adjust lat,lon distortion.
#' @param mytheme Name of a theme.
#' @return A ggplot-compatible map.
#' @examples
#' region_plot(peru)
#' region_plot(spain_france, fill="brown", linewidth=0.3)
#' region_plot(hawaii, coord=1.1)
#' @export

## Simplify the process of plotting a map that was
## extracted from map_data.

## Plot a region extracted using map_data. 
region_plot <- function(region,
                        fill      = "darkseagreen3",
                        color     = "black",
                        linewidth = 0.5,
                        coord     = 1.3,
                        mytheme   = NULL){
  plot <- ggplot2::ggplot(data=region,aes(x=long,
                                 y=lat, 
                                 group=group)) +
    ggplot2::geom_polygon(fill      = fill,
                 color     = color,
                 linewidth = linewidth) +
    ggplot2::coord_fixed(coord) +
    mytheme
  
  return (plot)
  
} ## end function region_plot
