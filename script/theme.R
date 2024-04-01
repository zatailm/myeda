theme_zvis_open <- function(font_size = 14, line_size = 0.5, rel_small = 12/14,
                            rel_tiny = 11/14, rel_large = 16/14) {
  half_line <- font_size/2
  cowplot::theme_half_open(font_size = font_size, line_size = line_size,
                           rel_small = rel_small, rel_tiny = rel_tiny, rel_large = rel_large) %+replace%
    theme(plot.margin = ggplot2::margin(half_line/2, 1.5, half_line/2,
                                        1.5), complete = TRUE)
}
theme_zvis_grid <- function(font_size = 14, line_size = 0.5, rel_small = 12/14,
                            rel_tiny = 11/14, rel_large = 16/14, colour = "grey90") {
  half_line <- font_size/2
  cowplot::theme_minimal_grid(font_size = font_size, line_size = line_size,
                              rel_small = rel_small, rel_tiny = rel_tiny, rel_large = rel_large,
                              colour = colour) %+replace% theme(plot.margin = ggplot2::margin(half_line/2,
                                                                                              1.5, half_line/2, 1.5), complete = TRUE)
}
theme_zvis_hgrid <- function(font_size = 14, line_size = 0.5, rel_small = 12/14,
                             rel_tiny = 11/14, rel_large = 16/14, colour = "grey90") {
  half_line <- font_size/2
  cowplot::theme_minimal_hgrid(font_size = font_size, line_size = line_size,
                               rel_small = rel_small, rel_tiny = rel_tiny, rel_large = rel_large,
                               colour = colour) %+replace% theme(plot.margin = ggplot2::margin(half_line/2,
                                                                                               1.5, half_line/2, 1.5), complete = TRUE)
}
theme_zvis_vgrid <- function(font_size = 14, line_size = 0.5, rel_small = 12/14,
                             rel_tiny = 11/14, rel_large = 16/14, colour = "grey90") {
  half_line <- font_size/2
  cowplot::theme_minimal_vgrid(font_size = font_size, line_size = line_size,
                               rel_small = rel_small, rel_tiny = rel_tiny, rel_large = rel_large,
                               colour = colour) %+replace% theme(plot.margin = ggplot2::margin(half_line/2,
                                                                                               1.5, half_line/2, 1.5), complete = TRUE)
}
theme_zvis_map <- function(font_size = 14, line_size = 0.5, rel_small = 12/14,
                           rel_tiny = 11/14, rel_large = 16/14) {
  half_line <- font_size/2
  cowplot::theme_map(font_size = font_size, line_size = line_size, rel_small = rel_small,
                     rel_tiny = rel_tiny, rel_large = rel_large) %+replace% 
    theme(plot.margin = ggplot2::margin(half_line/2,1.5, half_line/2, 1.5), complete = TRUE)
}
theme_zvis_noy <- function(font_size = 14, line_size = 0.5, rel_small = 12/14,
                           rel_tiny = 11/14, rel_large = 16/14) {
  half_line <- font_size/2
  cowplot::theme_half_open(font_size = font_size, line_size = line_size,
                           rel_small = rel_small, rel_tiny = rel_tiny, rel_large = rel_large) %+replace%
    theme(plot.margin = ggplot2::margin(half_line/2, 1.5, half_line/2,
                                        1.5), axis.title.y = element_blank(), axis.text.y = element_blank(),
          axis.ticks.y = element_blank(), axis.line.y = element_blank(),
          legend.position = "top", legend.justification = "right", legend.key.size = unit(8,
                                                                                          "pt"), axis.title.x = element_text(size = 9), complete = TRUE)
}
theme_zvis_nox <- function(font_size = 14, line_size = 0.5, rel_small = 12/14,
                           rel_tiny = 11/14, rel_large = 16/14) {
  half_line <- font_size/2
  cowplot::theme_half_open(font_size = font_size, line_size = line_size,
                           rel_small = rel_small, rel_tiny = rel_tiny, rel_large = rel_large) %+replace%
    theme(plot.margin = ggplot2::margin(half_line/2, 1.5, half_line/2,
                                        1.5), axis.title.x = element_blank(), axis.text.x = element_blank(),
          axis.ticks.x = element_blank(), axis.line.x = element_blank(),
          legend.position = "top", legend.justification = "right", legend.key.size = unit(8,
                                                                                          "pt"), axis.title.y = element_blank(), plot.title.position = "plot",
          plot.caption.position = "plot", complete = TRUE)
}

pal.zata <- c(
  '#3498db','#0d0887','#f1c40f','#e74c3c','#AD2A8E',
  '#00C19B','#fba238', '#990000', '#4DAF4A', '#000000','#999999'
) 

pal.zata.grey <- c(
  '#FAFAFA','#F5F5F5','#EEEEEE','#E0E0E0','#BDBDBD',
  '#9E9E9E','#757575','#616161','#424242','#212121'
)

my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")
# cols <- c("#FAAB18", "#1380A1","#990000", "#588300")
# zcol <- c('#154360', '#ff5733', '#ffc300', '#1abc9c') 
# zcol <- c('#E41A1C', '#377EB8', '#4DAF4A', '#FF7F00')

# palette(my_cols)

scale_fill_zata <- function(...) {
  discrete_scale('fill', 'classic', manual_pal(values = pal.zata), ...)
}

scale_color_zata <- function(...) {
  discrete_scale('color', 'classic', manual_pal(values = pal.zata), ...)
}

theme_set(theme_zvis_grid(9) +
            theme(panel.border = element_rect(linewidth = rel(1), color = 'black'),
                  panel.grid.major = element_line(linetype = 'dashed'),
                  axis.ticks.length = unit(.2, "cm"), 
                  axis.ticks = element_line(color = 'black'),
                  plot.title = element_text(size = 8),
                  panel.spacing = unit(1, "lines"),
                  axis.title = element_text(size = 8),
                  legend.position = 'top',
                  legend.justification = 'right'))

zcol <- c('#E41A1C', '#377EB8', '#4DAF4A', '#FF7F00', '#AD2A8E', '#f1c40f', 
          '#99445E', '#0d0887', '#616161', '#990000', '#bdbdbd')

scalecolzt <- function(...) {
  discrete_scale('color', 'classic', manual_pal(values = zcol), ...)}
scalefilzt <- function(...) {
  discrete_scale('fill', 'classic', manual_pal(values = zcol), ...)}

space <- plot_spacer()
layw2 <- plot_layout(width = c(5, .5, 5))
layw3 <- plot_layout(width = c(5, .25, 5, .25, 5))
layh2 <- plot_layout(height = c(5, .5, 5))
layh3 <- plot_layout(height = c(5, .25, 5, .35, 5))
