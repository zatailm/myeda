theme_zata <- function(base_size = 9, base_family = '',
                       line_size = .5,
                       rect_size = base_size / 22,
                       txt_scale = 1,
                       border    = TRUE,
                       x_grid    = TRUE,
                       y_grid    = TRUE,
                       leg_pos   = 'right',
                       leg_jus   = 'center',
                       x_title   = TRUE,
                       y_title   = TRUE,
                       strip_bg  = 'gray90',
                       tit_pos   = 'plot',
                       cap_pos   = 'plot',
                       x_ticks   = TRUE,
                       y_ticks   = TRUE,
                       x_text    = TRUE,
                       y_text    = TRUE,
                       tit_jus   = 'left',
                       cap_jus   = 'right',
                       leg_loc   = 'plot',
                       x_text_angle = 'horizontal'
) {
  
  rel_tiny  = txt_scale * (11/14)
  rel_small = txt_scale * (12/14)
  rel_large = txt_scale * (14/14)
  
  half_line  <- base_size / 2
  small_size <- rel_small * base_size
  
  if (border) {
    panel_border <- element_rect(color = 'black', fill = NA)
  } else {
    panel_border <- element_blank()
  }
  
  if (x_grid) {
    x_grid <- element_line(linewidth = rel(.5), linetype = 'dashed', color = 'gray90')
  } else {
    x_grid <- element_blank()
  }
  
  if (y_grid) {
    y_grid <- element_line(linewidth = rel(.5), linetype = 'dashed', color = 'gray90')
  } else {
    y_grid <- element_blank()
  }
  
  if (tit_jus == 'left') {
    tit_jus = 0
  } else if (tit_jus == 'right') {
    tit_jus = 1
  } else if (tit_jus == 'center') {
    tit_jus = .5
  } else {
    tit_jus = 0
  }
  
  if (cap_jus == 'left') {
    cap_jus = 0
  } else if (cap_jus == 'right') {
    cap_jus = 1
  } else if (cap_jus == 'center') {
    cap_jus = .5
  } else {
    cap_jus = 1
  }
  
  if (x_title) {
    x_title <- element_text(margin = margin(t = half_line / 2), vjust = 1)
  } else {
    x_title <- element_blank()
  }
  
  if (y_title) {
    y_title <- element_text(angle = 90, margin = margin(r = half_line / 2), vjust = 1)
  } else {
    y_title <- element_blank()
  }
  
  if (x_ticks) {
    x_ticks <- element_line(color = 'black')
  } else {
    x_ticks <- element_blank()
  }
  
  if (y_ticks) {
    y_ticks <- element_line(color = 'black')
  } else {
    y_ticks <- element_blank()
  }
  
  if (x_text) {
    if (x_text_angle == 'vertical') {
      x_text <- element_text(angle = 90, hjust = 1, vjust = .4)
    } else {
      x_text <- element_text(margin = margin(t = small_size / 4), vjust = 1)
    }
    
  } else {
    x_text <- element_blank()
  }
  
  if (y_text) {
    y_text <- element_text(margin = margin(r = small_size / 4), hjust = 1)
  } else {
    y_text <- element_blank()
  }
  
  theme(
    line                = element_line(color = 'black', linewidth = line_size,linetype = 1, lineend = 'butt'),
    rect                = element_rect(fill = 'white', color = 'black', linewidth = line_size, linetype = 1),
    text                = element_text(family = base_family, face = 'plain', color = 'black', size = base_size,
                                       lineheight = .9, hjust = .5, vjust = .5, angle = 0,
                                       margin = margin(), debug = FALSE),
    
    axis.line           = element_blank(),
    axis.line.x         = NULL,
    axis.line.y         = NULL,
    axis.text           = element_text(size = small_size, color = 'black'),
    axis.text.x         = x_text,
    axis.text.x.top     = element_text(margin = margin(b = small_size / 4), vjust = 0),
    axis.text.y         = y_text,
    axis.text.y.right   = element_text(margin = margin(l = small_size / 4), hjust = 0),
    axis.text.r         = element_text(margin = margin(l = .8 * half_line/ 2, r = .8 * half_line / 2), hjust = .5),
    axis.ticks          = element_line(color = 'black'),
    axis.ticks.x        = x_ticks,
    axis.ticks.y        = y_ticks,
    axis.ticks.length   = unit(half_line / 1.2, 'pt'),
    axis.ticks.length.x        = NULL,
    axis.ticks.length.x.top    = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y        = NULL,
    axis.ticks.length.y.left   = NULL,
    axis.ticks.length.y.right  = NULL,
    axis.minor.ticks.length    = rel(.75),
    axis.title.x        = x_title,
    axis.title.x.top    = element_text(margin = margin(b = half_line / 2), vjust = 0),
    axis.title.y        = y_title,
    axis.title.y.right  = element_text(angle = -90, margin = margin(l = half_line / 2), vjust = 1),
    
    legend.location       = leg_loc,
    legend.background     = element_blank(),
    legend.spacing        = unit(base_size, 'pt'),
    legend.spacing.x      = NULL,
    legend.spacing.y      = NULL,
    legend.margin         = margin(0, 0, 0, 0),
    legend.key            = NULL,
    legend.key.size       = unit(1.1 * base_size, 'pt'),
    legend.key.height     = NULL,
    legend.key.width      = NULL,
    legend.key.spacing    = unit(half_line, 'pt'),
    legend.text           = element_text(size = rel(rel_small), vjust = 1),
    legend.title          = element_text(size = rel(rel_small), vjust = 1, hjust = 0, face = 'italic'),
    legend.ticks.length   = rel(.5),
    legend.position       = leg_pos,
    legend.direction      = NULL,
    legend.justification  = leg_jus,
    legend.box            = NULL,
    legend.box.margin     = margin(0, 0, 0, 0),
    legend.box.background = element_blank(),
    legend.box.spacing    = unit(base_size, 'pt'),
    
    panel.background   = element_blank(),
    panel.border       = panel_border,
    panel.grid.major.x = x_grid,
    panel.grid.minor.x = NULL,
    panel.grid.major.y = y_grid,
    panel.grid.minor.y = NULL,
    panel.spacing      = unit(half_line, 'pt'),
    panel.spacing.x    = NULL,
    panel.spacing.y    = NULL,
    panel.ontop        = FALSE,
    
    strip.background      = element_rect(fill = strip_bg, color = NA),
    strip.clip            = 'inherit',
    strip.text            = element_text(color = 'grey10', size = rel(rel_small),
                                         margin = margin(half_line / 2, half_line / 2, half_line / 2, half_line / 2)),
    strip.text.x          = element_text(vjust = 1.5), # NULL,
    strip.text.y          = element_text(angle = -90),
    strip.text.y.left     = element_text(angle = 90),
    strip.placement       = 'inside',
    strip.placement.x     = NULL,
    strip.placement.y     = NULL,
    strip.switch.pad.grid = unit(half_line / 2, 'pt'),
    strip.switch.pad.wrap = unit(half_line / 2, 'pt'),
    
    plot.background       = element_rect(color = 'white'),
    plot.title            = element_text(face = 'bold', size = rel(rel_large),
                                         hjust = tit_jus, vjust = 1,
                                         margin = margin(b = half_line)),
    plot.title.position   = tit_pos,
    plot.subtitle         = element_text(size = rel(rel_small), hjust = tit_jus, vjust = 1,
                                         margin = margin(b = half_line)),
    plot.caption          = element_text(size = rel(rel_tiny), hjust = cap_jus, vjust = 1,
                                         margin = margin(t = half_line)),
    plot.caption.position = cap_pos,
    plot.tag              = element_text(face = 'bold', size = rel(1.2),
                                         hjust = 0, vjust = .7),
    plot.tag.position     = 'topleft',
    plot.margin           = margin(half_line, half_line, half_line, half_line),
    
    complete = TRUE
  )
}

pal.zata <- c(
  '#3498db', '#0d0887', '#f1c40f', '#e74c3c', '#AD2A8E',
  '#00C19B', '#fba238', '#990000', '#4DAF4A', '#000000', '#999999'
)

pal.zata.grey <- c(
  '#FAFAFA', '#F5F5F5', '#EEEEEE', '#E0E0E0', '#BDBDBD',
  '#9E9E9E', '#757575', '#616161', '#424242', '#212121'
)

zcol <- c(
  '#E41A1C', '#377EB8', '#4DAF4A', '#FF7F00', '#AD2A8E', '#f1c40f',
  '#99445E', '#0d0887', '#616161', '#990000', '#bdbdbd'
)

my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")

scale_fill_zata <- function(...) {
  discrete_scale('fill', 'classic', manual_pal(values = pal.zata), ...)
}

scale_color_zata <- function(...) {
  discrete_scale('color', 'classic', manual_pal(values = pal.zata), ...)
}

scalecolzt <- function(...) {
  discrete_scale('color', 'classic', manual_pal(values = zcol), ...)
}
scalefilzt <- function(...) {
  discrete_scale('fill', 'classic', manual_pal(values = zcol), ...)
}

space <- plot_spacer()
layw2 <- plot_layout(width = c(5, .5, 5))
layw3 <- plot_layout(width = c(5, .25, 5, .25, 5))
layh2 <- plot_layout(height = c(5, .5, 5))
layh3 <- plot_layout(height = c(5, .25, 5, .35, 5))
