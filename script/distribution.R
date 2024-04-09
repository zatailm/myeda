
# compare and plot ----------------------------------------------------------------------------

do.scan <- function(data) {
  column_types <- sapply(data, function(col) {
    if (is.factor(col) || is.character(col))
      "discrete"
    else if (is.numeric(col))
      "continuous"
    else "other"
  })
  num_discrete_columns <- sum(column_types == "discrete")
  num_continuous_columns <- sum(column_types == "continuous")
  missing_columns <- sum(colSums(is.na(data)) > 0)
  complete_rows <- sum(complete.cases(data))
  missing_observations <- sum(is.na(data))
  num_columns_in_dataset <- ncol(data)
  num_rows_in_dataset <- nrow(data)
  results_data <- data.frame(
    Metric = c("Discrete Columns", "Continuous Columns",
               "Missing Columns", "Complete Rows", "Missing Observations", 
               "Number of Columns", "Number of Rows"), 
    Count = c(num_discrete_columns, num_continuous_columns, 
              missing_columns, complete_rows, missing_observations, num_columns_in_dataset,
              num_rows_in_dataset))
  return(results_data)
}

compare <- function(data, clean = TRUE) {
  p <- ggplot(data = data, aes(x = Metric, y = Count, fill = Metric)) +
    geom_bar(stat = 'identity', width = .7) +
    geom_text(aes(label = Count), hjust = -.2, size = 2.7) +
    scale_y_continuous(trans = 'log1p', expand = expansion(mult = c(0, .2))) +
    scale_fill_zata() +
    theme(legend.position = 'none', plot.title.position = 'plot',
          axis.text.x = element_blank(), axis.line.x = element_blank(),
          panel.grid.major.x = element_blank()) +
    coord_flip()
  if (clean) {
    p <- p + theme(axis.text.y = element_blank()) + 
      labs(x = NULL, y = NULL, title = 'Dataset Characteristics (Processed)', 
           caption = 'Logarithmic scaled bar')
  } else {
    p <- p + labs(x = NULL, y = NULL, title = 'Dataset Characteristics (Pre-processed)')
  }
  return(p)
}

p_raw <- compare(do.scan(df_acled_raw), clean = FALSE)
p_clean <- compare(do.scan(acled))

# location map --------------------------------------------------------------------------------

map.loc <- function(layera, layerb, show_legend = TRUE, wrap = FALSE) {
  p <- ggplot() +
    geom_sf(data = layera) + 
    geom_sf(data = layerb, aes(color = EVENT_TYPE), size = 1, alpha = 0.2, show.legend = show_legend) + 
    theme_void() +
    theme(legend.position = ifelse(show_legend, 'right', 'none'), legend.title = element_text(size = 8)) +
    scale_color_manual(name = "Event Type", values = pal.zata)
  if (wrap) {
    p <- p + facet_wrap(~EVENT_TYPE, ncol = 3)
  }
  return(p)
}

df_sf_adm <- st_as_sf(acled, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

p_loc_map <- map.loc(prvnc, df_sf_adm, show_legend = TRUE, wrap = FALSE)
p_loc_map_type <- map.loc(prvnc, df_sf_adm, show_legend = FALSE, wrap = TRUE) 

# choropleth events and fatalities ------------------------------------------------------------

map.plot <- function(data, title, breaks, labels, option) {
  ggplot() +
    geom_sf(data = data, aes(fill = n), lwd = NA) +
    scale_fill_viridis_c(
      option = option, begin = .05, end = .95, trans = 'log10',
      direction = 1, breaks = breaks, labels = labels, na.value = pal.zata.grey[4]) +
    scale_x_continuous(expand = expansion(mult = c(.03, .03))) +
    theme_void(10) +
    theme(
      legend.position = 'bottom', legend.text = element_text(size = 7),
      legend.key.height = unit(3, 'pt'), legend.key.width = unit(10, 'pt'),
      plot.title = element_text(hjust = 0.5, size = 8),
      plot.margin = ggplot2::margin(0, 0, 0, 0, 'pt'), legend.justification = 'right',
      legend.title = element_blank(), legend.margin = ggplot2::margin(0, 20, 0, 0)) +
    labs(title = title)
}

df_adm <- acled %>% 
  count(ADMIN1) %>%
  mutate(n = replace(n, n == 0, NA)) %>%
  mutate(TYPE = "EVENT")

df_fat <- acled %>% 
  group_by(ADMIN1) %>% 
  summarize(n = sum(FATALITIES)) %>% 
  mutate(n = replace(n, n == 0, NA)) %>%
  mutate(TYPE = "FATALITIES")

df_prv_geo <- prvnc %>% 
  left_join(rbind(df_adm, df_fat), by = c('PROVINSI' = 'ADMIN1'))

df_sf_evn <- df_prv_geo %>% filter(TYPE == 'EVENT')
df_sf_fat <- df_prv_geo %>% filter(TYPE == 'FATALITIES')

p_geo_adm <- map.plot(
  data = df_sf_evn, 
  title = "Event", 
  breaks = na.omit(c(min(df_sf_evn$n, na.rm = T), max(df_sf_evn$n, na.rm = T))), 
  labels = na.omit(c(min(df_sf_evn$n, na.rm = T), max(df_sf_evn$n, na.rm = T))), 
  option = "D")

p_geo_fat <- map.plot(
  data = df_sf_fat, 
  title = "Fatalities", 
  breaks = na.omit(c(min(df_sf_fat$n, na.rm = T), max(df_sf_fat$n, na.rm = T))), 
  labels = na.omit(c(min(df_sf_fat$n, na.rm = T), max(df_sf_fat$n, na.rm = T))), 
  option = "C")

# comparing event and fatalities plot ---------------------------------------------------------

com.plot <- function(data, x, y1, y2, abr = TRUE) {
  p <- data %>%
    ggplot(aes(x = {{x}})) +
    geom_col(aes(y = sqrt({{y1}}), fill = 'Events'), position = 'identity', width = .6) +
    geom_col(aes(y = -sqrt({{y2}}), fill = 'Fatalities'), position = 'identity', 
             width = .6) +
    geom_text(aes(y = sqrt({{y1}}) + .1, label = {{y1}}), position = 'identity',
              size = 2.5, hjust = -.3, vjust = .35, angle = 90) +
    geom_text(aes(y = -sqrt({{y2}}) - .1, label = {{y2}}), position = 'identity',
              size = 2.5, hjust = 1.3, vjust = .35, angle = 90) +
    scale_y_continuous(expand = expansion(mult = c(.25, .35))) +
    theme(
      axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
      axis.title.y = element_blank(), panel.border = element_blank(),
      panel.grid.major.y = element_blank()) +
    scale_fill_manual(
      values = c('Events' = '#3e4a89', 'Fatalities' = '#fca636'),
      guide = guide_legend(title = NULL)) +
    labs(x = l$prab, caption = 'Square root scaled bar')
  
  if (abr) {
    return(p)
  } else {
    p <- p + theme(axis.text.x = element_text(angle = 90, vjust = .35, hjust = 1))
  }
  return(p)
}

df_event_adm <- acled %>% group_by(ADMIN1_ABR, ADMINID) %>% count(ADMIN1_ABR)
df_adm_fat <- acled %>% group_by(ADMIN1_ABR, ADMINID) %>% 
  summarize(n = sum(FATALITIES), .groups = 'drop') %>% arrange(desc(n)) %>% ungroup()
df_comp_evnfat <- left_join(df_event_adm, df_adm_fat, by = c("ADMIN1_ABR", "ADMINID")) %>%
  rename(Events = n.x, Fatalities = n.y)

p_compare_reg <- com.plot(data = df_comp_evnfat, x = ADMIN1_ABR, y1 = Events, y2 = Fatalities)

# comparing events and fatalities based on cluster --------------------------------------------

set.seed(123)
df_clus_evn <- acled %>% group_by(ADMIN1_ABR) %>% summarize(EVENT = n())
kmeansevt <- kmeans(df_clus_evn[, c("EVENT")], centers = 3)
kmeansevt$cluster <- factor(kmeansevt$cluster, levels = c(1, 3, 2))
df_clus_evn$CL <- as.factor(kmeansevt$cluster)
df_clus_evn <- df_clus_evn %>% mutate(CLST = recode(CL, "1" = "Medium", "2" = "Low", "3" = "High"))

set.seed(123)
df_clus_fat <- acled %>% group_by(ADMIN1_ABR) %>% summarize(FATAL = sum(FATALITIES))
kmeansfat <- kmeans(df_clus_fat[, c("FATAL")], centers = 3)
kmeansfat$cluster <- factor(kmeansfat$cluster, levels = c(1, 3, 2))
df_clus_fat$CL <- as.factor(kmeansfat$cluster)
df_clus_fat <- df_clus_fat %>% mutate(CLST = recode(CL, "1" = "High", "2" = "Medium", "3" = "Low"))

df_clus_evn$CLST <- factor(df_clus_evn$CLST, levels = c("Low", "Medium", "High"))
df_clus_fat$CLST <- factor(df_clus_fat$CLST, levels = c("Low", "Medium", "High"))

p_compare_clust <- ggplot() +
  geom_col(data = df_clus_evn, aes(x = ADMIN1_ABR, y = sqrt(EVENT), fill = CLST), position = "identity", width = .6) +
  geom_col(data = df_clus_fat, aes(x = ADMIN1_ABR, y = -sqrt(FATAL), fill = CLST), position = "identity", width = .6) +
  geom_text(data = df_clus_evn, aes(x = ADMIN1_ABR, y = sqrt(EVENT) + .1, label = EVENT), position = 'identity',
            size = 2.5, hjust = -.3, vjust = .35, angle = 90) +
  geom_text(data = df_clus_fat, aes(x = ADMIN1_ABR, y = -sqrt(FATAL) - .1, label = FATAL), position = 'identity',
            size = 2.5, hjust = 1.3, vjust = .35, angle = 90) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("High" = zcol[1], "Medium" = zcol[6], "Low" = zcol[3])) +
  guides(fill = guide_legend(reverse = TRUE, title.position = 'top', title = NULL)) +
  theme(legend.position = 'top', legend.justification = 'right', 
        axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(), 
        axis.title.y = element_text(hjust = 1),
        axis.title.y.right = element_text(hjust = 1),
        panel.border = element_blank()) +
  xlab('Province (Abbreviation)') +
  labs(y = "Event", y2 = "Fatalities") +
  scale_y_continuous(expand = c(.25, .35), sec.axis = sec_axis(~ -., name = "Fatalities"))

p_clust_evnfat <- p_compare_clust +
  plot_annotation(
    caption = 'Square root scaled bar',
    theme = theme(plot.title = element_text(size = 9))
  )

# distribution via scatter --------------------------------------------------------------------

scat.plot <- function(data, geom = "jitter", method = "density", axis_text = TRUE) {
  if (geom == "jitter") {
    p <- ggplot(data, aes(x = factor(ADMIN1_ABR), y = EVENT_DATE)) +
      geom_jitter(aes(color = ADMIN1_ABR, fill = ADMIN1_ABR), pch = 20,
                  position = position_jitter(0.2), cex = 1.2)
  } else if (geom == "sina") {
    if (method == "density") {
      p <- ggplot(data, aes(x = factor(ADMIN1_ABR), y = EVENT_DATE)) +
        geom_sina(method = 'density', aes(color = ADMIN1_ABR, fill = ADMIN1_ABR), size = .3, pch = 20) +
        geom_violin(color = zcol[1], fill = '#ffffff00', linewidth = .3)
    } else if (method == "boxplot") {
      p <- ggplot(data, aes(x = factor(ADMIN1_ABR), y = EVENT_DATE)) +
        geom_sina(method = 'density', aes(color = ADMIN1_ABR, fill = ADMIN1_ABR), 
                  size = .3, pch = 20, alpha = .7, color = 'darkgrey') +
        geom_boxplot(width = .4, color = zcol[1], fill = '#fde72570', size = .3, 
                     outlier.shape = 20, outlier.size = .3, position = position_nudge(.2))
    }
  }
  
  p <- p + 
    scale_color_viridis(option = 'D', discrete = TRUE, begin = .2, end = .95) +
    scale_fill_viridis(option = 'D', discrete = TRUE, begin = .2, end = .95) +
    scale_y_datetime(breaks = seq(from = min(data$EVENT_DATE), to = max(data$EVENT_DATE), by = "2 years"), 
                     date_labels = "%Y") +
    theme(legend.position = 'none', panel.border = element_blank()) 
  
  if (!axis_text) {
    p <- p + theme(axis.text.x = element_blank()) +
      theme(axis.title = element_blank())
  } else {
    p <- p + theme(axis.title.y = element_blank()) +
      labs(x = 'Province (Abbreviation)')
  }
  
  return(p)
}

df_distr_adm_evn <- acled %>% mutate(ADMIN1_ABR = fct_rev(fct_infreq(ADMIN1_ABR)))

p_disj <- scat.plot(df_distr_adm_evn, geom = "jitter", axis_text = FALSE)
p_diss <- scat.plot(df_distr_adm_evn, geom = "sina", method = "density", axis_text = FALSE)
p_disb <- scat.plot(df_distr_adm_evn, geom = "sina", method = "boxplot")
p_dissc <- wrap_plots(p_disj, p_diss, p_disb, ncol = 1)

# Heatmap -------------------------------------------------------------------------------------

create.heatmap <- function(data, xdat, ydat, value, viridis, numeric = FALSE, pass.scale = TRUE) {
  if (!all(c(xdat, ydat, value) %in% names(data))) {
    stop('Columns not found in data!')
  }
  
  p <- data %>%
    ggplot(aes(x = !!sym(xdat), y = reorder(!!sym(ydat), !!sym(value)))) +
    geom_tile(aes(fill = !!sym(value)), color = '#000000', linewidth = 0.25) +
    scale_y_discrete(position = 'right') +
    scale_fill_viridis(
      option = viridis, trans = 'log10', begin = 0.2, end = 1,
      breaks = round(10^seq(log10(1), log10(max(data[[value]], na.rm = TRUE)), length.out = 4)),
      name = paste(l$frq, ' / ', l$bln),
      guide = guide_colorbar(direction = "horizontal"),
      na.value = "#440154"
    ) +
    theme(
      legend.position = 'top', legend.justification = 'right',
      legend.key.height = unit(1.5, 'mm'), legend.key.width = unit(10, 'mm'),
      legend.title = element_text(size = 6), legend.text = element_text(size = 6),
      legend.ticks = element_blank(), panel.border = element_blank(),
      axis.text = element_text(size = 6), 
      axis.ticks.length = unit(1, 'mm')
    ) 
  
  if (numeric) {
    p <- p + scale_x_continuous(breaks = seq(0, 108, 4), expand = c(0, 0))
  } else {
    if (pass.scale) {
      return(p)
    } else {
      p <- p + 
        scale_x_date(
          breaks = seq(as.Date(min(data[[xdat]])) + years(1), as.Date(max(data[[xdat]])), by = "1 year"),
          date_labels = "%Y",
          expand = c(0, 0)
        )
    }
  }
  return(p)
}

df_heat_adm_evn <- acled %>%
  mutate(EVENT_DATE = floor_date(EVENT_DATE, unit = 'month'), EVENT_DATE = as.Date(EVENT_DATE)) %>%
  count(EVENT_DATE, ADMIN1_ABR) %>%
  complete(ADMIN1_ABR, EVENT_DATE, fill = list(n = 0))

breaks1 <- as.Date(c('2019-09-01', '2020-10-01', '2022-09-01'))
breaks2 <- seq(as.Date(min(df_heat_adm_evn$EVENT_DATE)) + months(3),
               as.Date(max(df_heat_adm_evn$EVENT_DATE)), by = "2 year")
combined_breaks <- as.Date(union(breaks1, breaks2))

ccol <- ifelse(combined_breaks %in% breaks1, zcol[1], 'black')
date_labels <- ifelse(combined_breaks %in% breaks1, "%b-%y", "%b-%y")

p_heat_evn <- create.heatmap(
  data = df_heat_adm_evn,
  xdat = 'EVENT_DATE',
  ydat = 'ADMIN1_ABR',
  value = 'n',
  viridis = 'D',
  numeric = FALSE,
  pass.scale = TRUE) +
  scale_x_date(breaks = combined_breaks, date_labels = date_labels, expand = c(0, 0)) +
  theme(axis.text.x = element_text(color = ccol)) +
  labs(x = l$thn, y = NULL)

df_heat_adm_fat <- acled %>%
  mutate(EVENT_DATE = floor_date(EVENT_DATE, unit = 'month'), EVENT_DATE = as.Date(EVENT_DATE)) %>%
  group_by(EVENT_DATE, ADMIN1_ABR) %>%
  summarise(n = sum(FATALITIES), .groups = 'drop') %>%
  complete(ADMIN1_ABR, EVENT_DATE, fill = list(n = 0))

p_heat_fat <- create.heatmap(
  df_heat_adm_fat, 'EVENT_DATE', 'ADMIN1_ABR', 'n', 'C', numeric = FALSE, pass.scale = FALSE) +
  labs(x = l$thn, y = NULL)

df_heat_evn_typ <- acled %>%
  mutate(EVENT_DATE = floor_date(EVENT_DATE, unit = 'month'), EVENT_DATE = as.Date(EVENT_DATE)) %>%
  count(EVENT_DATE, EVENT_TYPE_SRT) %>%
  complete(EVENT_DATE, EVENT_TYPE_SRT, fill = list(n = 0))

p_heat_typ_evn <- create.heatmap(
  df_heat_evn_typ, 'EVENT_DATE', 'EVENT_TYPE_SRT', 'n', 'D', numeric = FALSE, pass.scale = FALSE) +
  labs(x = l$thn, y = NULL)

df_heat_typ_fat <- acled %>%
  mutate(EVENT_DATE = floor_date(EVENT_DATE, unit = 'month'), EVENT_DATE = as.Date(EVENT_DATE)) %>%
  group_by(EVENT_DATE, EVENT_TYPE_SRT) %>%
  summarise(n = sum(FATALITIES), .groups = 'drop') %>%
  complete(EVENT_DATE, EVENT_TYPE_SRT, fill = list(n = 0))

p_heat_typ_fat <- create.heatmap(
  df_heat_typ_fat, 'EVENT_DATE', 'EVENT_TYPE_SRT', 'n', 'C', numeric = FALSE, pass.scale = FALSE) +
  labs(x = l$thn, y = NULL)

# NOTE : when using numeric xdat, numeric and pass.scale should be TRUE
# df_heat_adm_evn <- acled %>%
#   count(CMONTH, ADMIN1_ABR) %>%
#   complete(ADMIN1_ABR, CMONTH, fill = list(n = 0))
# 
# create.heatmap(
#   data = df_heat_adm_evn,
#   xdat = 'CMONTH',
#   ydat = 'ADMIN1_ABR',
#   value = 'n',
#   viridis = 'C',
#   numeric = TRUE,
#   pass.scale = TRUE
# )

# admin2 and admin3 events - fatalities -------------------------------------------------------

p.adm <- function(df, tit, xlab, event = TRUE) {
  p <- df %>% ggplot(aes(x = fct_reorder(adm, freq), y = freq)) +
    geom_col(aes(fill = freq), width = .6, show.legend = F) +
    geom_text(aes(label = freq), hjust = -.3, vjust = .5, size = 2.5) +
    scale_y_continuous(expand = expansion(mult = c(.01, .2))) +
    theme(axis.ticks.x = element_blank(), axis.text.x = element_blank(),
          panel.grid.major.x = element_blank(), panel.border = element_blank()) +
    labs(title = tit, x = xlab, y = NULL) +
    coord_flip()
  if (event) {
    p <- p + scale_fill_viridis_c(option = 'D', trans = 'sqrt', begin = .2, end = .95)
  } else {
    p <- p + scale_fill_viridis_c(option = 'C', trans = 'log2', begin = .2, end = .95)
  }
  return(p)
}

nn <- 7
df_frq_adm2 <- acled %>% count(ADMIN2) %>% arrange(desc(n)) %>% top_n(n = nn) %>%
  set_names(c('adm', 'freq'))
df_frq_adm3 <- acled %>% count(ADMIN3) %>% arrange(desc(n)) %>% na.omit() %>%
  top_n(n = nn) %>% set_names(c('adm', 'freq'))
df_fat_adm2 <- acled %>% group_by(ADMIN2) %>% summarize(fat = sum(FATALITIES)) %>%
  arrange(desc(fat)) %>% na.omit() %>% top_n(n = nn) %>% set_names(c('adm', 'freq'))
df_fat_adm3 <- acled %>% group_by(ADMIN3) %>% summarize(fat = sum(FATALITIES)) %>%
  arrange(desc(fat)) %>% na.omit() %>% top_n(n = nn) %>% set_names(c('adm', 'freq'))

pa1 <- p.adm(df_frq_adm2, 'Events', 'Regency/City')
pa2 <- p.adm(df_frq_adm3, 'Events', 'District')
pb1 <- p.adm(df_fat_adm2, 'Fatalities', 'Regency/City', event = FALSE)
pb2 <- p.adm(df_fat_adm3, 'Fatalities', 'District', event = FALSE)

p_adm_evt_fat <- pa1 + pa2 + pb1 + pb2 + plot_layout(nrow = 2)

# compare event type and their fatalities -----------------------------------------------------

df_type_frq <- acled %>% count(EVENT_TYPE) %>% rename(Event = n)
df_type_fat <- acled %>% group_by(EVENT_TYPE) %>% 
  summarize(Fatalities = sum(FATALITIES, na.rm = TRUE))
df_typ <- left_join(df_type_frq, df_type_fat, by = 'EVENT_TYPE')

p_typ <- df_typ %>%
  ggplot(aes(x = fct_rev(EVENT_TYPE))) +
  geom_col(aes(y = sqrt(Fatalities), fill = 'Total Fatalities'), 
           position = 'identity', width = .5) +
  geom_text(aes(y = sqrt(Fatalities) + 0.1, label = Fatalities), 
            position = 'identity', size = 2.5, hjust = -0.3, vjust = .4) +
  geom_col(aes(y = -sqrt(Event), fill = 'Total Events'), 
           position = 'identity', width = .5) +
  geom_text(aes(y = -sqrt(Event) - 0.1, label = Event), 
            position = 'identity', size = 2.5, hjust = 1.3, vjust = .3) +
  scale_y_continuous(expand = expansion(mult = c(.15, .2))) +
  theme(legend.position = 'top', legend.justification = 'right',
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(), axis.title = element_blank(),
        panel.border = element_blank()) +
  scale_fill_manual(
    values = c('Total Events' = '#3e4a89', 'Total Fatalities' = '#fca636'),
    guide = guide_legend(title = NULL)) +
  labs(x = l$prab, caption = 'Square root scaled bar') + coord_flip()

# stacked bar for event types -----------------------------------------------------------------

df_evttype_adm1 <- acled %>% count(ADMIN1_ABR, EVENT_TYPE_SRT)

p_evt_adm1_prc <- df_evttype_adm1 %>%
  ggplot(aes(x = ADMIN1_ABR, y = n, fill = EVENT_TYPE_SRT)) +
  geom_col(width = .6, position = 'fill') +
  scale_y_continuous(expand = expansion(mult = c(.02, .02)), labels = percent) +
  theme(panel.border = element_blank()) +
  scale_fill_zata() + labs(x = 'Province (Abbreviation)', y = NULL) +
  guides(fill = guide_legend(nrow = 1, title = NULL, title.theme = element_text(size = 8)))

df_con_ym <- acled %>% 
  group_by(YEAR, MONTH, EVENT_TYPE_SRT, .drop = TRUE) %>%
  summarise(freq = n(), .groups = 'drop_last') %>%
  rename(year = YEAR, month = MONTH, event = EVENT_TYPE_SRT)

p_con_ym <- df_con_ym %>%
  ggplot(aes(x = month, y = freq, fill = event)) +
  geom_col(position = 'fill', width = .6) +
  scale_x_continuous(breaks = seq(1, 12, 1), expand = expansion(mult = c(.02, .02))) +
  scale_y_continuous(
    breaks = seq(.0, 1, .5),
    expand = expansion(mult = c(.05, .05))) +
  scale_fill_zata() +
  guides(fill = guide_legend(nrow = 1)) +
  theme(panel.border = element_blank()) +
  facet_wrap(~year, nrow = 3, ncol = 3) +
  labs(x = "Number of Month", y = "Proportion", fill = NULL)

# yearly aggregated event type amount ---------------------------------------------------------

df_con_y_raw <- acled %>% 
  group_by(YEAR, EVENT_TYPE_SRT, .drop = TRUE) %>%
  summarise(total = n(), .groups = 'drop_last') %>%
  rename(year = YEAR, event = EVENT_TYPE_SRT)

df_type_year <- expand.grid(year = unique(df_con_y_raw$year), event = unique(df_con_y_raw$event))

df_con_y <- df_type_year %>%
  left_join(df_con_y_raw, by = c("year", "event")) %>%
  mutate(total = replace(total, is.na(total), 0))

p_con_y <- df_con_y %>%
  ggplot(aes(x = factor(year), total)) +
  geom_bar(stat = 'identity', aes(fill = event), width = .5) +
  geom_text(
    stat = 'summary',
    aes(angle = 90, label = after_stat(y), group = year),
    fun = sum, vjust = .3, hjust = -.5, size = 2.1
  ) +
  scale_y_continuous(expand = expansion(mult = c(.02, .8))) +
  scale_fill_zata() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = .4, size = 6),
    legend.position = 'none',
    panel.border = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank()
  ) +
  facet_wrap(~ event, scale = 'free_y', nrow = 1) +
  labs(x = 'Year', y = 'Number of Event')

# admin1 rank by event types ------------------------------------------------------------------

etypes <- c(l$bat, l$ervl, l$prt, l$rts, l$devl, l$vacl)
scan.evn <- function(x) {
  acled %>% filter(EVENT_TYPE == {{ x }}) %>%  count(ADMINID, sort = TRUE) %>% 
    rename(total = n) %>% top_n(5, wt = total) %>% arrange(desc(total))}
ls.evn.adm <- lapply(etypes, function(etypes) {
  scan.evn(etypes) %>% mutate(type = etypes)})

scan.fat <- function(x) {
  acled %>% filter(EVENT_TYPE == {{ x }}) %>% group_by(ADMINID) %>%
    summarize(total = sum(FATALITIES)) %>% filter(total > 0) %>%
    top_n(5, wt = total) %>%arrange(desc(total))}

ls.fat.adm <- lapply(etypes, function(etypes) {
  scan.fat(etypes) %>% mutate(type = etypes)})

create.ef <- function(df, tit, d = TRUE) {
  p <- df %>% filter(total > 0) %>%
    ggplot(aes(x = reorder(ADMINID, total), y = total, fill = total)) +
    geom_col(width = 0.6) +
    geom_text(aes(label = total), hjust = -0.5, vjust = 0.3, size = 2.5) +
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.3))) +
    theme(legend.position = 'none', plot.margin = ggplot2::margin(5,0,0,0, 'pt'),
          plot.title = element_text(size = 8),
          panel.border = element_blank(), axis.text.x = element_blank(),
          axis.ticks.x = element_blank(), panel.grid.major.x = element_blank(),
          axis.title = element_blank()) +
    labs(title = tit) + coord_flip()
  
  if (d) {
    p <- p + scale_fill_viridis_c(option = 'D', trans = 'sqrt', begin = 0.05, end = 0.95)
  } else {
    p <- p + scale_fill_viridis_c(option = 'C', trans = 'sqrt', begin = 0.05, end = 0.95)
  }
}

pe <- lapply(seq_along(ls.evn.adm), function(i) {create.ef(ls.evn.adm[[i]], etypes[i])})
pevn_adm <- (pe[[1]] + space + pe[[2]] + space + pe[[3]] + layw3) / space / 
  (pe[[4]] + space + pe[[5]] + space + pe[[6]] + layw3) + layh2

pf <- lapply(seq_along(ls.fat.adm), function(i) {create.ef(ls.fat.adm[[i]], etypes[i], d = FALSE)})
pfat_adm <- (pf[[1]] + space + pf[[2]] + space + pf[[3]] + layw3) / space / 
  (pf[[4]] + space + pf[[5]] + space + pf[[6]] + layw3) + layh2

# choropleth for event types fatalities -------------------------------------------------------

df_fat <- acled %>% group_by(ADMIN1, EVENT_TYPE) %>% 
  summarise(FATAL = sum(FATALITIES), .groups = 'drop') %>%
  mutate(FATAL = replace(FATAL, FATAL == 0, NA)) %>%
  complete(ADMIN1, EVENT_TYPE, fill = list(FATAL = NA))

df_sf_adm_type_fat <- prvnc %>%
  left_join(df_fat, by = c('PROVINSI' = 'ADMIN1'))

p_cho_adm_fat <- ggplot() +
  geom_sf(data = df_sf_adm_type_fat, aes(fill = FATAL), lwd = NA) +
  scale_fill_viridis_c(
    option = 'C', trans = 'log1p', na.value = pal.zata.grey[4], direction = 1, 
    breaks = c(1, max(df_sf_adm_type_fat$FATAL, na.rm = TRUE)), begin = .05, end = .95, 
    name = l$nn) +
  scale_x_continuous(expand = expansion(mult = c(.03, .03))) +
  theme_void(10) +
  theme(
    legend.position = 'bottom', legend.justification = 'center',
    legend.text = element_text(size = 7), 
    legend.key.height = unit(3, 'pt'), legend.key.width = unit(15, 'pt'), 
    plot.title = element_text(face = 'bold', size = 9),
    strip.text.x = element_text(face = 'bold')) +
  facet_wrap(~ EVENT_TYPE)

# choropleth for event types distribution -----------------------------------------------------

df_evn <- acled %>% group_by(ADMIN1, EVENT_TYPE) %>% 
  summarise(EVENT = n(), .groups = 'drop') %>%
  mutate(EVENT = replace(EVENT, EVENT == 0, NA)) %>%
  complete(ADMIN1, EVENT_TYPE, fill = list(EVENT = NA))

df_adm_evn <- prvnc %>%
  left_join(df_evn, by = c('PROVINSI' = 'ADMIN1'))

p_cho_adm_evn <- ggplot() +
  geom_sf(data = df_adm_evn, aes(fill = EVENT), lwd = NA) +
  scale_fill_viridis_c(
    option = 'D', trans = 'log10', na.value = pal.zata.grey[4], direction = 1, 
    breaks = c(1, max(df_adm_evn$EVENT, na.rm = TRUE)), begin = .3, end = .95, 
    name = l$nn) +
  scale_x_continuous(expand = expansion(mult = c(.03, .03))) +
  theme_void(10) +
  theme(
    legend.position = 'bottom', legend.justification = 'center',
    legend.text = element_text(size = 7), 
    legend.key.height = unit(3, 'pt'), legend.key.width = unit(15, 'pt'), 
    plot.title = element_text(face = 'bold', size = 9),
    strip.text.x = element_text(face = 'bold')) +
  facet_wrap(~ EVENT_TYPE)

# density 2d ----------------------------------------------------------------------------------

create.den2d <- function(data, x, y, fill = FALSE, point = FALSE) {
  p <- data %>%
    ggplot(aes(x = {{x}}, y = {{y}})) +
    scale_x_continuous(breaks = seq(1, 107, 7)) +
    scale_y_continuous(trans = 'log1p') +
    theme(axis.title.y = element_blank(), axis.text.y = element_blank(),
          axis.ticks.y = element_blank(), panel.grid.major.y = element_blank()) +
    xlab('Number of Month (Continuous)')
  if (fill) {
    p <- p + geom_density_2d_filled(show.legend = 'none') +
      scale_fill_viridis_d(option = 'inferno')
  } else if (point) {
    p <- p + geom_point(size = .5, alpha = .5, color = zcol[6]) +
      geom_density_2d(color = zcol[1])
  } else {
    p <- p + geom_density_2d(color = zcol[1])
  }
  return(p)
}

df_for_density <- acled %>% group_by(CMONTH, EVENT_TYPE_SRT) %>% summarise(n = n(), .groups = 'drop_last')

pden2d <- create.den2d(df_for_density, CMONTH, n, point = TRUE) + space + 
  create.den2d(df_for_density, CMONTH, n,fill = TRUE, point = FALSE) + 
  layw2 + plot_layout(axis_titles = 'collect') +
  plot_annotation(
    title = NULL,
    caption = 'Logarithmic scaled y axis',
    theme = theme(plot.title = element_text(size = 9))
  )

# event and fatalities by admin by year -------------------------------------------------------

create.ef <- function(df, x, y, tit, d = TRUE) {
  p <- df %>% filter(total > 0) %>%
    ggplot(aes(x = reorder({{x}}, {{y}}), y = {{y}}, fill = {{y}})) +
    geom_col(width = 0.6) +
    geom_text(aes(label = {{y}}), hjust = -0.5, vjust = 0.3, size = 2.5) +
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.3))) +
    theme(legend.position = 'none', plot.margin = ggplot2::margin(5,0,0,0, 'pt'),
          plot.title = element_text(size = 8),
          panel.border = element_blank(), axis.text.x = element_blank(),
          axis.ticks.x = element_blank(), panel.grid.major.x = element_blank(),
          axis.title = element_blank()) +
    labs(title = tit) + coord_flip()
  
  if (d) {
    p <- p + scale_fill_viridis_c(option = 'D', trans = 'sqrt', begin = 0.05, end = 0.95)
  } else {
    p <- p + scale_fill_viridis_c(option = 'C', trans = 'sqrt', begin = 0.05, end = 0.95)
  }
}

# sub event types frequencies -----------------------------------------------------------------

df_subtype <- data.frame(table(acled$EVENT_TYPE, acled$SUB_EVENT_TYPE)) %>% 
  filter(Freq != 0) %>% 
  rename(type = Var1, subtype = Var2)

mtpe <- c(
  'Violence against civilians' = 'VAC', 'Explosions/Remote violence' = 'ERV', 
  'Battles' = 'Battles', 'Strategic developments' = 'Str.Dev.', 'Protests' = 'Protests',
  'Riots' = 'Riots'
)

df_subtype <- df_subtype %>% mutate(typ = recode(as.character(type), !!!mtpe))
subbr <- c(
  'Abduction/forced disappearance' = 'Abduction',
  'Air/drone strike' = 'Air strike',
  'Armed clash' = 'Armed clash',
  'Arrests' = 'Arrest',
  'Attack' = 'Attack',
  'Change to group/activity' = 'Group change',
  'Disrupted weapons use' = 'Disrupted weapons',
  'Excessive force against protesters' = 'Excessive force',
  'Government regains territory' = 'Gov.terr. regained',
  'Grenade' = 'Grenade',
  'Looting/property destruction' = 'Property destruction',
  'Mob violence' = 'Mob violence',
  'Non-violent transfer of territory' = 'Terr. transfer',
  'Other' = 'Other',
  'Peaceful protest' = 'Peaceful protest',
  'Protest with intervention' = 'Protest intervention',
  'Remote explosive/landmine/IED' = 'Explosive',
  'Sexual violence' = 'Sexual violence',
  'Suicide bomb' = 'Suicide bomb',
  'Violent demonstration' = 'Violent demo.'
)

df_subtype <- df_subtype %>% mutate(Sub = recode(as.character(subtype), !!!subbr))
df_subtype$Sub <- factor(df_subtype$Sub, levels = df_subtype$Sub[order(df_subtype$type)])

p_subtype <- df_subtype %>%
  ggplot(aes(x = fct_rev(reorder(Sub, Freq)), y = Freq, fill = as.factor(type))) +
  geom_bar(stat = 'identity', position = 'dodge', width = .6) +
  geom_text(aes(y = Freq, label = Freq), position = 'identity',
            size = 2.5, hjust = -.3, vjust = .3, angle = 90) +
  scale_y_continuous(trans = 'log', expand = expansion(mult = c(.01, .5))) +
  scale_fill_zata() +
  theme(legend.position = 'right', panel.border = element_blank(), 
        panel.grid.major.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = .2),
        plot.margin = unit(c(0,60,0,60), 'pt'),
        plot.caption.position = "plot", legend.title = element_text(size = 8)) +
  guides(fill = guide_legend(title = "Event Types", size = 5)) + 
  labs(x = 'Sub Event Types', y = NULL, caption = 'Logarithmic scaled bar')

# actor interaction net -----------------------------------------------------------------------

df_act_interaction <- acled %>%
  group_by(ACT1, ACT2) %>%
  summarize(freq = n(), .groups = 'drop') %>%
  filter(ACT1 != "n/a" & ACT2 != "n/a") %>%
  arrange(desc(freq))

graph_data <- df_act_interaction %>%
  as_tbl_graph(directed = TRUE, node_key = "Actor", edge = c("ACT1", "ACT2"))

pnet <- ggraph(graph_data, layout = "auto") + 
  geom_edge_link(aes(width = freq), alpha = 0.8, color = zcol[1]) +
  geom_node_point(size = 4, color = zcol[1]) +
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = name), color = 'black', size = 2.5, repel = TRUE,
                 position = "identity") +
  scale_edge_width(range = c(0.1, 1)) +
  labs(edge_width = "Freq.") +
  theme_zvis_map(8) + 
  theme(plot.margin = unit(c(0,0,0,0), 'pt'), legend.title = element_text(size = 8), 
        legend.text = element_text(size = 7), plot.title = element_text(hjust = 0.5))

ptil <- ggplot(df_act_interaction, aes(ACT1, ACT2, fill = freq)) + 
  geom_tile() + scale_fill_viridis(discrete = F)

# actor occurance -----------------------------------------------------------------------------

create.plactr <- function(df, x, y, opt, tit) {
  p <- df %>% 
    ggplot(aes(x = reorder({{x}}, {{y}}), y = {{y}})) +
    geom_col(aes(fill = {{y}}), width = .6) +
    geom_text(aes(label = {{y}}), vjust = .4, hjust = -.3, size = 2.5) +
    scale_y_continuous(trans = 'sqrt', expand = expansion(mult = c(.02, .25))) +
    scale_fill_viridis_c(option = opt, trans = 'log', begin = .05, end = .95) +
    theme(legend.position = 'none', panel.border = element_blank(),
          panel.grid.major.x = element_blank(), axis.text.x = element_blank(),
          axis.ticks.x = element_blank(), axis.title = element_blank()) +
    labs(title = tit) +
    coord_flip()
  return(p)
}

df_cas_act <- acled %>%
  count(ACT1, ACT2, wt = FATALITIES) %>%
  mutate(actors = ifelse(is.na(ACT2), ACT1, ACT2), total = n) %>%
  group_by(actors) %>%
  summarize(total = sum(total)) %>%
  arrange(desc(total)) %>%
  filter(total > 0) 

df_act <- acled %>%
  count(ACT1) %>%
  rename(actors = ACT1, total = n) %>%
  filter(!is.na(actors)) %>%
  bind_rows(acled %>%
              count(ACT2) %>%
              rename(actors = ACT2, total = n) %>%
              filter(!is.na(actors))) %>%
  group_by(actors) %>%
  summarize(total = sum(total)) %>%
  arrange(desc(total))

p_actor <- create.plactr(df_act, actors, total, 'D', 'Actor Occurance')
p_actor_fat <- create.plactr(df_cas_act, actors, total, 'C', 'Actors Contributions to Fatalities')
p_actor <- p_actor + space + p_actor_fat + layw2

# actor interaction ---------------------------------------------------------------------------

df_intr <- acled %>% count(INTERACTION) %>% arrange(desc(n))

p_interaction <- df_intr %>%
  ggplot(aes(x = fct_rev(reorder(INTERACTION, n)), y = n)) +
  geom_col(aes(fill = n), width = 0.7) +
  geom_shadowtext(
    data = subset(df_intr, n < max(df_intr$n)),
    aes(label = INTERACTION),
    hjust = -.1,
    vjust = -.5,
    nudge_x = 0.3,
    colour = 'black',
    bg.colour = NA,
    bg.r = 0.2,
    size = 2.5,
    angle = 90
  ) +
  geom_text(
    data = subset(df_intr, n >= max(df_intr$n)),
    aes(x = INTERACTION, y = 0, label = INTERACTION),
    hjust = -.1,
    vjust = -.5,
    nudge_x = .3,
    color = 'black',
    size = 2.5,
    angle = 90
  ) +
  scale_y_continuous(trans = 'sqrt', labels = function(x) as.character(x / 1000 * 10)) +
  scale_fill_viridis_c(option = 'viridis', trans = 'log') +
  theme(panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = 'none') +
  labs(x = 'Type of Interactions', y = 'Total (x1000)', caption = 'Square root scaled bar') 

# wordcloud: filtered -------------------------------------------------------------------------

stop.words <- stopwords('en')

create.wc <- function(data, src.in, src, words, rem.words = stop.words, min, max) {
  note <- data %>% filter({{src.in}} == src) %>% dplyr::select({{words}})
  text <- paste(note, collapse = " ")
  corpus <- Corpus(VectorSource(text))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, rem.words)
  tdm <- TermDocumentMatrix(corpus)
  m <- as.matrix(tdm)
  word_freqs <- sort(rowSums(m), decreasing = TRUE)
  set.seed(1234)
  wordcloud(words = names(word_freqs), freq = word_freqs, min.freq = min,
            max.words = max, random.order = FALSE, colors = brewer.pal(8, "Dark2"), 
            rot.per = .35, scale = c(2, .4))
}

# NOTE : 
# 1. wordcloud called directly inside Rmd using this example:
# create.wc(acled, CMONTH, 57, NOTES, 30) or create.wc(acled, EVENT_TYPE_SRT, 'Battles', NOTES, 30)
# To assign it as variable, use 'myvariable <- recordPlot()' after calling create.wc()
# 2. be careful in determining stopwords!
# 3. add this to access some parameters:
# p <- recordPlot()
# return(list(plot = p, tdm = tdm, freqs = as.data.frame(word_freqs)))

# wordcloud: comparison and commonality -------------------------------------------------------

prep.texts <- function(data, src.in, src, words, lab, rem.words) {
  notes_list <- lapply(lab, function(src) {
    data %>%
      filter({{src.in}} == src) %>%
      dplyr::select({{words}}) %>%
      mutate(labels = src)
  })
  
  corp.list <- lapply(notes_list, function(x) VCorpus(VectorSource(toString(x))))
  corp.all <- corp.list[[1]]
  for (i in 2:length(src)) {corp.all <- c(corp.all, corp.list[[i]])}
  
  corp.all <- tm_map(corp.all, content_transformer(tolower))
  corp.all <- tm_map(corp.all, removePunctuation)
  corp.all <- tm_map(corp.all, removeNumbers)
  corp.all <- tm_map(corp.all, function(x) removeWords(x, rem.words))
  
  doc.tm <- TermDocumentMatrix(corp.all)
  doc.tm.mat <- as.matrix(doc.tm)
  colnames(doc.tm.mat) <- src
  doc.tm.clean <- removeSparseTerms(doc.tm, 0.8)
  doc.tm.clean.mat <- as.matrix(doc.tm.clean)
  colnames(doc.tm.clean.mat) <- src
  
  index <- as.logical(sapply(rownames(doc.tm.clean.mat), function(x) (nchar(x)>3) ))
  result <- doc.tm.clean.mat[index,]
  
  return(result)
}

# alluvial actors -----------------------------------------------------------------------------

df_filt_act12 <- acled %>%
  dplyr::select(ACT1, ACT2) %>%
  na.omit() 

df_act_inter <- df_filt_act12 %>%
  group_by(ACT1, ACT2) %>%
  summarise(count = n(), .groups = 'drop') %>%
  arrange(desc(count)) %>%
  rename(ACTOR1 = ACT1, ACTOR2 = ACT2)

p_alluvial <- ggplot(df_act_inter, aes(axis1 = ACTOR1, axis2 = ACTOR2, y = log(count))) +
  geom_alluvium(aes(fill = ACTOR1)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2, angle = 90) +
  scale_x_discrete(limits = c('ACTOR1', 'ACTOR2'), expand = c(0.15, 0.05)) +
  scale_fill_zata() +
  theme(plot.margin = unit(c(0,0,0,0), 'pt'), panel.border = element_blank(), 
        panel.grid.major = element_blank(), axis.text.x = element_blank(), 
        axis.ticks = element_blank(), axis.title = element_blank(),
        legend.position = 'none') +
  coord_flip()

# stream events weekly ------------------------------------------------------------------------

df_dist_evn_stream <- acled %>%
  mutate(EVENT_DATE = floor_date(EVENT_DATE, unit = 'week'), EVENT_DATE = as.Date(EVENT_DATE)) %>%
  group_by(EVENT_DATE, EVENT_TYPE_SRT) %>%
  summarise(total = n(), .groups = 'drop') %>%
  ungroup()

p_stream_evn <- ggplot(df_dist_evn_stream, aes(x = EVENT_DATE, y = total, fill = EVENT_TYPE_SRT)) +
  geom_stream() +
  scale_fill_manual(values = pal.zata) +
  scale_x_date(breaks = seq(as.Date("2015-01-01"), as.Date("2023-12-31"), 
                            by = "2 year"), date_labels = "%Y") + 
  theme(panel.border = element_blank(), legend.title = element_text(size = 8)) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(x = 'Year', y = 'Value', fill = 'Event Type')

# event type boxplot --------------------------------------------------------------------------

px <- ggplot(acled) +
  aes(x = YEAR, y = EVENT_TYPE, fill = EVENT_TYPE) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  theme_minimal()

py <- ggplot(df_prv_geo) +
  aes(fill = n) +
  geom_sf(size = 1.2) +
  scale_fill_viridis_c(option = "viridis", direction = 1) +
  theme_minimal() +
  facet_wrap(vars(TYPE))

pz <- ggplot(acled) +
  aes(x = "", y = YEAR, fill = EVENT_TYPE) +
  geom_violin(adjust = 1L, scale = "area") +
  scale_fill_hue(direction = 1) +
  theme_minimal()
