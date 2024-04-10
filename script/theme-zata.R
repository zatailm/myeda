library(ggplot2)

theme_zata <- function(
    plot_bg_color = "white", 
    panel_bg_color = NULL,
    panel_grid_color = "gray85",
    pan_gr_maj_x = TRUE,
    pan_gr_maj_y = TRUE,
    panel_border_color = NULL,
    pan_space = unit(1, 'lines'),
    show_panel_border = TRUE, 
    axis_txt_size = rel(1),
    axis_txt_color = "black",
    axis_txt_face = "plain",
    axis_txt_angle = 0,
    axis_txt_hjust = .5,
    axis_txt_vjust = .5,
    axis_title_size = rel(1),
    axis_title_color = "black",
    axis_title_face = "plain",
    ticks_len = unit(.15, 'cm'),
    leg_position = "right",
    leg_title = FALSE,
    leg_txt_size = rel(.7),
    leg_txt_color = "black",
    leg_txt_face = "plain",
    leg_title_size = rel(.8),
    leg_title_color = "black",
    leg_title_face = "italic",
    leg_key_size = NULL,
    leg_key_height = unit(.5, 'lines'),
    leg_key_width = unit(.5, 'lines'),
    leg_key_space_x = unit(.25, 'lines'),
    leg_key_space_y = unit(.25, 'lines'),
    leg_jus = 'right',
    strip_txt_size = rel(1),
    strip_txt_color = "black",
    strip_txt_face = "plain",
    strip_bg_color = NULL,
    strip_bg_fill = "white",
    strip_bg_alpha = 1
) {
  
  if (show_panel_border) {
    panel_border <- element_rect(
      color = panel_border_color, 
      fill = NA)
  } else {
    panel_border <- element_blank()
  }
  
  if (leg_title) {
    if (leg_position == 'top' || leg_position == 'bottom') {
      legend_title <- element_text(
        size = leg_title_size, 
        color = leg_title_color, 
        face = leg_title_face, 
        vjust = 1,
        margin = margin(t = -0.15, r = .5, unit = "lines"))
      legend_text <- element_text(
        size = leg_txt_size, 
        color = leg_txt_color, 
        face = leg_txt_face, 
        vjust = 1,
        margin = margin(r = 4, l = 2)
      )
    } else {
      legend_title <- element_text(
        size = leg_title_size, 
        color = leg_title_color, 
        face = leg_title_face)
      legend_text <- element_text(
        size = leg_txt_size, 
        color = leg_txt_color, 
        face = leg_txt_face,
        vjust = 1)
    }
  } else {
    legend_title <- element_blank()
    legend_text <- element_text(
      size = leg_txt_size, 
      color = leg_txt_color, 
      face = leg_txt_face)
  }
  
  if (pan_gr_maj_x) {
    grid_x <- element_line(
      color = panel_grid_color, 
      linetype = 'dashed')
  } else {
    grid_x <- element_blank()
  }
  
  if (pan_gr_maj_y) {
    grid_y <- element_line(
      color = panel_grid_color, 
      linetype = 'dashed')
  } else {
    grid_y <- element_blank()
  }  
  
  theme(
    plot.background = element_rect(fill = plot_bg_color),
    panel.background = element_rect(
      fill = ifelse(
        is.null(panel_bg_color), 
        plot_bg_color, 
        panel_bg_color),
      color = panel_border_color),
    panel.grid.major = element_line(color = panel_grid_color),
    panel.grid.major.x = grid_x,
    panel.grid.major.y = grid_y,
    panel.grid.minor = element_blank(),
    panel.border = panel_border,
    panel.spacing = pan_space,
    axis.text.x = element_text(
      size = axis_txt_size, 
      color = axis_txt_color, 
      face = axis_txt_face,
      angle = axis_txt_angle, 
      hjust = axis_txt_hjust, 
      vjust = axis_txt_vjust),
    axis.text.y = element_text(
      size = axis_txt_size, 
      color = axis_txt_color, 
      face = axis_txt_face),
    axis.title.x = element_text(
      size = axis_title_size, 
      color = axis_title_color, 
      face = axis_title_face),
    axis.title.y = element_text(
      size = axis_title_size, 
      color = axis_title_color, 
      face = axis_title_face),
    axis.ticks.length = ticks_len,
    legend.title = legend_title,
    legend.text = legend_text,
    legend.position = leg_position,
    legend.key.size = leg_key_size,
    legend.key.height = leg_key_height,
    legend.key.width = leg_key_width,
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    legend.justification = leg_jus,
    legend.key.spacing.x = leg_key_space_x,
    legend.key.spacing.y = leg_key_space_y,
    strip.text = element_text(
      size = strip_txt_size, 
      color = strip_txt_color, 
      face = strip_txt_face),
    strip.background = element_rect(
      fill = ifelse(
        is.null(strip_bg_color), 
        strip_bg_fill, 
        strip_bg_color), 
      strip_bg_alpha)
  )
}

# TASK:
#   some element has to be adjusted (title spacing, panel grid color, etc.)
