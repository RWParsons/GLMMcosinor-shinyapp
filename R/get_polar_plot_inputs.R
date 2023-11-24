get_polar_plot_inputs <- function(cc_obj,
                                  component_index,
                                  component_num,
                                  polar_plot_view_toggle,
                                  overlay_parameter_info,
                                  ellipse_opacity,
                                  clockwise,
                                  ci_level,
                                  n_breaks,
                                  grid_angle_segments, 
                                  radial_units,
                                  start,
                                  text_size,
                                  text_opacity
                                  ){
  
  
  if(is.null(polar_plot_view_toggle)){
    view = "full"
  } else {
    view = polar_plot_view_toggle
  }
  if(component_num>1) {
    show_component_labels  = TRUE
  } else {
    show_component_labels  = FALSE        
  }
  
  if(is.null(overlay_parameter_info)){
    polar_plot_overlay_parameter_info <- FALSE
  } else {
    polar_plot_overlay_parameter_info <-overlay_parameter_info
  }
  if(is.null(ellipse_opacity)){
    ellipse_opacity <- 0.3
  } else {
    ellipse_opacity <- ellipse_opacity
  }
  
  if(is.null(clockwise) || !clockwise){
    clockwise <- FALSE
  } else {
    clockwise <- TRUE
  }
  
  if(is.null(n_breaks)){
    n_breaks <- 5
  } else {
    n_breaks <- n_breaks
    if(n_breaks > 100){
      n_breaks = 100
    }
  }
  
  if(is.null(grid_angle_segments)){
    grid_angle_segments <- 8
  } else {
    grid_angle_segments <- grid_angle_segments
  }
  
  if(is.null(radial_units)){
    radial_units <- "radians"
  } else {
    radial_units <- radial_units
  }
  
  if(is.null(start)){
    start <- "right"
  } else {
    start <- start
  }
  
  if(is.null(text_size)){
    text_size <- 3
  } else {
    text_size <- text_size
  }
  
  if(is.null(text_opacity)){
    text_opacity <- 0.5
  } else {
    text_opacity <- text_opacity
  }

  
  polar_plot_object <- polar_plot(cc_obj, 
                                  component_index = component_index, 
                                  show_component_labels = show_component_labels,
                                  view = view, 
                                  overlay_parameter_info = polar_plot_overlay_parameter_info, 
                                  ci_level = ci_level,
                                  ellipse_opacity = ellipse_opacity, 
                                  clockwise = clockwise,
                                  n_breaks = n_breaks,
                                  grid_angle_segments = grid_angle_segments, 
                                  radial_units = radial_units,
                                  start = start,
                                  text_size = text_size,
                                  text_opacity = text_opacity)
  return(polar_plot_object)
}