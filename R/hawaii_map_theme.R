#' Format without gridlines, color ocean, surround with a line.
#'
#' @param border_color A color code or name for the map outline.
#' @param ocean_color A color code or name for the ocean color.
#' @return code for the theme to use with ggplot.
#' @examples
#' hawaii_map_theme(ocean_color = "blue")
#' @export
hawaii_map_theme <- function(
    border_color = "black",
    ocean_color = "lightsteelblue1"){
  theme(
  panel.border = element_rect(color     = border_color, 
                              fill      = NA, 
                              linewidth = 0.7),   
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text.x      = element_blank(), 
  axis.ticks.x     = element_blank(), 
  axis.text.y      = element_blank(), 
  axis.ticks.y     = element_blank(),
  axis.title.x     = element_blank(),
  axis.title.y     = element_blank(),
  panel.background = element_rect(fill = ocean_color))
}
  