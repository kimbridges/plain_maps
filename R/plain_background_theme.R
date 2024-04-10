#' Format plot without gridlines or axis annotation.
#'
#' @param x Not used.
#' @return code for the theme to use with ggplot.
#' @examples
#' plain_background_theme()
#' @export
plain_background_theme <- function (x=NULL) {
  theme(
  panel.border     = element_blank(),   
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.text.x      = element_blank(), 
  axis.ticks.x     = element_blank(), 
  axis.text.y      = element_blank(), 
  axis.ticks.y     = element_blank(),
  axis.title.x     = element_blank(),
  axis.title.y     = element_blank(),
  panel.background = element_blank())  
}
  