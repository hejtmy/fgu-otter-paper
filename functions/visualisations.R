BOX_CONTEXT_A <- list(x = c(0, 420), y = c(0, 420))
BOX_CONTEXT_B <- list(x = c(0, 420), y = c(0, 420))

paper_heatmap <- function(data, arena){
  box <- ifelse(arena == "A", BOX_CONTEXT_A, BOX_CONTEXT_B)
  box <- BOX_CONTEXT_A
  obj <- combine_all(data)
  plt <- ggplot() +
    stat_density_2d(
      geom = "raster",
      data = obj$position$data,
      aes(x=position_x, y = position_y, fill = after_stat(density)),
      contour = FALSE
    ) +
    gradient_style() +
    lims(x = box$x, y = box$y) +
    coord_fixed(xlim = box$x, ylim = box$y) +
    theme_bw() +
    heatmap_theme() +
    labs(x = "", y = "") +
    guides(fill = "none") +
    #geom_point(data = obj$position$data, mapping = aes(position_x, position_y)) +
    #geom_box_room() +
    theme(panel.background = element_rect(fill = "transparent"),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  return(plt) 
}

#' Styling functions for the heatmap and paths
gradient_style <- function(){
  return(scale_fill_gradientn(colours = heatmap_color()))
}

heatmap_theme <- function(){
  return(theme(panel.background = element_rect(fill = heatmap_color()[1]),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.border = element_blank()))
}

heatmap_color <- function(){
  return(rev(rainbow(100, start=0, end=0.7)))
}

area_presence_scale <- function(darkside){
  presence_colors <- c("#e2e2e2", "#000000")
  presence_colors <- setNames(presence_colors, 
                              c(other_side_name(darkside), darkside))
  return(presence_colors)
}

geom_box_room_local <- function(box, color = "grey20", size = 1.25,
                                fill = "white", ...){
  return(geom_rect(aes(xmin=box$x[1], xmax=box$x[2],
                       ymin=box$y[1], ymax=box$y[2]),
                   color = color, size = size, fill=fill, ...))
}
