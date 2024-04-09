show_overlay <- function(polygon, cc, 
                         p_linewidth= 0.5,
                         p_color="black",
                         p_fill="gray80",
                         p_alpha=1.0,
                         c_linewidth= 0.7,
                         c_color="red",
                         c_fill="darkseagreen3",
                         c_alpha=0.3){
  
  ## Define the cut-out polygon as a data frame.
  cut_lon <- c(cc$lon[1], cc$lon[2], cc$lon[2], cc$lon[1])
  cut_lat <- c(cc$lat[1], cc$lat[1], cc$lat[2], cc$lat[2])
  cut_poly <- data.frame(cbind(cut_lon, cut_lat))
  colnames(cut_poly) <- c("lon", "lat")
  
  ## Convert the cut-out polygon to the sf format.
  o_cut <- cut_poly %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
  
  ## Convert the overview outline to the sf format.
  o_polygon <- overview_outline %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
  
  ## Create a plot with the cut-out on top of the outline.
  overview_plot <- ggplot() +
    geom_sf(data=o_polygon, 
            linewidth=p_linewidth,
            color=p_color,
            fill=p_fill,
            alpha=p_alpha) +
    geom_sf(data=o_cut, 
            linewidth=c_linewidth,
            color=c_color,
            fill=c_fill,
            alpha=c_alpha)
  
  return(overview_plot)
  
} ## End function show_overlay
