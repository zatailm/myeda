##-------------------------------------------------------------------------------- EVENT VS FATAL

## map
df_adm_n <- acled %>% count(ADMIN1) %>% mutate(TYPES = "EVENT")
df_fat <- acled %>% group_by(ADMIN1) %>% summarize(n = sum(FATALITIES)) %>% 
  mutate(TYPES = "FATALITIES")
df_prv <- rbind(df_adm_n, df_fat)
df_prv_geo <- prvnc %>% left_join(df_prv, by = c('PROVINSI' = 'ADMIN1'))

map.plot <- function(data, title, fill_col, breaks, labels, option) {
  ggplot() +
    geom_sf(data = data, aes(fill = n), lwd = NA) +
    scale_fill_viridis_c(
      option = option, begin = .05, end = .95, trans = 'log10',
      direction = -1, breaks = breaks, labels = labels, na.value = pal.zata.grey[4]
    ) +
    scale_x_continuous(expand = expansion(mult = c(.03, .03))) +
    theme_void(10) +
    theme(
      legend.position = 'bottom', legend.text = element_text(size = 7),
      legend.key.height = unit(3, 'pt'), legend.key.width = unit(10, 'pt'),
      plot.title = element_text(hjust = 0.5, face = 'bold', size = 9),
      plot.margin = ggplot2::margin(0, 0, 0, 0, 'pt'), legend.justification = 'left',
      legend.title = element_blank()
    ) +
    labs(title = title)
}

df_geo_adm <- df_prv_geo %>% filter(TYPES == "EVENT")
df_geo_adm <- df_geo_adm[, c('PROVINSI', 'n', 'geometry')]
df_geo_fat <- df_prv_geo %>% filter(TYPES == "FATALITIES")
df_geo_fat <- df_geo_fat[, c('PROVINSI', 'n', 'geometry')]

p_geo_adm <- map.plot(df_geo_adm, "Event", "", c(min(df_geo_adm$n), max(df_geo_adm$n)), 
                         c(min(df_geo_adm$n), max(df_geo_adm$n)), "D")
p_geo_fat <- map.plot(df_geo_fat, "Fatalities", "", c(2, 320), c(2, 320), "C")

## bar

df_evt_adm1 <- acled %>% count(ADMIN1) %>% top_n(n = 10, wt = n)
df_evt_adm2 <- acled %>% count(ADMIN2) %>% top_n(n = 10, wt = n)
df_evt_adm3 <- acled %>% count(ADMIN3) %>% na.omit() %>% top_n(n = 10, wt = n) 

df_fat_adm1 <- acled %>% group_by(ADMIN1) %>% summarize(n = sum(FATALITIES)) %>%
  arrange(desc(n)) %>% ungroup()
df_fat_adm2 <- acled %>% group_by(ADMIN2) %>% summarize(n = sum(FATALITIES)) %>%
  filter(!is.na(ADMIN2)) %>% slice_max(order_by = n, n = 10) %>% ungroup()
df_fat_adm3 <- acled %>% group_by(ADMIN3) %>% summarize(n = sum(FATALITIES)) %>%
  filter(!is.na(ADMIN3)) %>% slice_max(order_by = n, n = 10) %>% ungroup()

df_evt_adm_all <- acled %>% group_by(ADMIN1_ABR, ADMINID) %>% count(ADMIN1_ABR)
df_fat_adm_all <- acled %>% group_by(ADMIN1_ABR, ADMINID) %>% 
  summarize(n = sum(FATALITIES), .groups = 'drop') %>% arrange(desc(n)) %>% ungroup()
df_adm_evt_fat <- left_join(df_evt_adm_all, df_fat_adm_all, by = c("ADMIN1_ABR", "ADMINID")) %>%
  rename(Events = n.x, Fatalities = n.y)

com.plot <- function(prv, abr = TRUE) {
  p <- df_adm_evt_fat %>%
    ggplot(aes(x = {{prv}})) +
    geom_col(aes(y = sqrt(Events), fill = 'Events'), position = 'identity', width = .6) +
    geom_col(aes(y = -sqrt(Fatalities), fill = 'Fatalities'), position = 'identity', width = .6) +
    geom_text(aes(y = sqrt(Events) + .1, label = Events), position = 'identity',
              size = 2.5, hjust = -.3, vjust = .35, angle = 90) +
    geom_text(aes(y = -sqrt(Fatalities) - .1, label = Fatalities), position = 'identity',
              size = 2.5, hjust = 1.3, vjust = .35, angle = 90) +
    scale_y_continuous(
      # name = 'Values (log1p scaled)',
      # breaks = c(-log1p(0), -log1p(10), -log1p(100), -log1p(1000)),
      # labels = c(0, 10, 100, 1000),
      expand = expansion(mult = c(.25, .35))) +
    theme(
      axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
      axis.title.y = element_blank(), panel.border = element_blank(),
      panel.grid.major.y = element_blank()) +
    scale_fill_manual(
      values = c('Events' = '#3e4a89', 'Fatalities' = '#fca636'),
      guide = guide_legend(title = NULL)) +
    labs(x = l$prab, caption = 'Logarithmic scaled bar')
  
  if (abr) {
    return(p)
  } else {
    p <- p + theme(axis.text.x = element_text(angle = 90, vjust = .35, hjust = 1))
  }
  
  return(p)
}

##--

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

p_cluster <- ggplot() +
  geom_col(data = df_clus_evn, aes(x = ADMIN1_ABR, y = sqrt(EVENT), fill = CLST), position = "identity", width = .6) +
  geom_col(data = df_clus_fat, aes(x = ADMIN1_ABR, y = -sqrt(FATAL), fill = CLST), position = "identity", width = .6) +
  geom_text(data = df_clus_evn, aes(x = ADMIN1_ABR, y = sqrt(EVENT) + .1, label = EVENT), position = 'identity',
            size = 2.5, hjust = -.3, vjust = .35, angle = 90) +
  geom_text(data = df_clus_fat, aes(x = ADMIN1_ABR, y = -sqrt(FATAL) - .1, label = FATAL), position = 'identity',
            size = 2.5, hjust = 1.3, vjust = .35, angle = 90) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("High" = zcol[1], "Medium" = zcol[6], "Low" = zcol[3])) +
  # scale_y_continuous(expand = c(.25, .3)) +
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
  # labs(caption = 'Square root scaled bar')


# p_cluster <- ggplot() +
#   geom_col(data = df_clus_evn, aes(x = ADMIN1_ABR, y = sqrt(EVENT), fill = CLST), position = "identity", width = .6) +
#   geom_col(data = df_clus_fat, aes(x = ADMIN1_ABR, y = -sqrt(FATAL), fill = CLST), position = "identity", width = .6) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
#   scale_fill_manual(values = c("High" = zcol[1], "Medium" = zcol[6], "Low" = zcol[3])) +
#   guides(fill = guide_legend(reverse = TRUE, title.position = 'top', title = NULL)) +
#   theme(
#     legend.position = 'top',
#     legend.justification = 'right',
#     axis.text.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     panel.grid.major.y = element_blank(),
#     panel.border = element_blank(),
#     axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt"), vjust = 0.5),
#     axis.title.y.right = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0, unit = "pt"), vjust = 0.5, color = "red")
#   ) +
#   xlab('Province (Abbreviation)') +
#   labs(y = "Sumbu Y Positif", y2 = "Sumbu Y Negatif") +
#   scale_y_continuous(sec.axis = sec_axis(~ -., name = "Sumbu Y Negatif"))
# 
# print(p_cluster)

# Dist. scatt
generate_plot <- function(data, geom = "jitter", method = "density", axis_text = TRUE) {
  if (geom == "jitter") {
    p <- ggplot(data, aes(x = factor(ADMIN1_ABR), y = EVENT_DATE)) +
      geom_jitter(aes(color = ADMIN1_ABR, fill = ADMIN1_ABR), size = .3, pch = 20)
  } else if (geom == "sina") {
    if (method == "density") {
      p <- ggplot(data, aes(x = factor(ADMIN1_ABR), y = EVENT_DATE)) +
        geom_sina(method = 'density', aes(color = ADMIN1_ABR, fill = ADMIN1_ABR), size = .3, pch = 20) +
        geom_violin(color = zcol[1], fill = '#ffffff00', linewidth = .3)
    } else if (method == "boxplot") {
      p <- ggplot(data, aes(x = factor(ADMIN1_ABR), y = EVENT_DATE)) +
        geom_sina(method = 'density', aes(color = ADMIN1_ABR, fill = ADMIN1_ABR), size = .3, pch = 20, alpha = .7, color = 'darkgrey') +
        geom_boxplot(width = .4, color = zcol[1], fill = '#fde72570', size = .3, outlier.shape = 20, outlier.size = .3, position = position_nudge(.2))
    }
  }
  
  p <- p + 
    scale_color_viridis(option = 'D', discrete = TRUE, begin = .2, end = .95) +
    scale_fill_viridis(option = 'D', discrete = TRUE, begin = .2, end = .95) +
    scale_y_datetime(breaks = seq(from = min(data$EVENT_DATE), to = max(data$EVENT_DATE), by = "2 years"), date_labels = "%Y") +
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

dfdist <- acled %>% mutate(ADMIN1_ABR = fct_rev(fct_infreq(ADMIN1_ABR)))

p_disj <- generate_plot(dfdist, geom = "jitter", axis_text = FALSE)
p_diss <- generate_plot(dfdist, geom = "sina", method = "density", axis_text = FALSE)
p_disb <- generate_plot(dfdist, geom = "sina", method = "boxplot")
p_dissc <- wrap_plots(p_disj, p_diss, p_disb, ncol = 1)

## Heatmap
# df_heat_adm <- acled %>%
#   mutate(EVENT_DATE = as.Date(EVENT_DATE), MONTH = floor_date(EVENT_DATE, "month")) %>%
#   count(ADMIN1_ABR, MONTH) %>%
#   complete(ADMIN1_ABR, MONTH, fill = list(n = 0))

df_heat_adm_evn <- acled %>%
  count(CMONTH, ADMIN1_ABR) %>%
  complete(ADMIN1_ABR, CMONTH, fill = list(n = 0))

df_heat_adm_fat <- acled %>%
  group_by(ADMIN1_ABR, CMONTH) %>%
  summarise(n = sum(FATALITIES), .groups = 'drop_last') %>%
  complete(CMONTH = 1:108) %>%
  replace_na(list(n = 0))

create.heatmap <- function(data, event = TRUE) {
  p <- data %>%
    ggplot(aes(x = CMONTH, y = reorder(ADMIN1_ABR, n))) +
    geom_tile(mapping = aes(fill = n), color = '#000000', linewidth = .25) +
    scale_x_continuous(breaks = seq(2, 108, 108/36), expand = c(0, 0)) +
    theme(legend.position = 'top', legend.justification = 'right', 
          legend.key.height = unit(0.15, "cm"),
          legend.key.width = unit(1, "cm"), legend.title = element_text(size = 6),
          legend.text = element_text(size = 6), legend.ticks = element_blank(),
          panel.border = element_blank(), axis.text = element_text(size = 6),
          axis.ticks.length = unit(1, 'mm')) +
    scale_y_discrete(name = NULL, position = "right") +
    labs(x = 'Number of Month (Continuous)')
  
  if (event) {
    p <- p + 
      scale_fill_viridis(
        option = 'D', trans = 'log10', begin = .2, end = 1,
        breaks = round(10^seq(log10(1), log10(max(df_heat_adm_evn$n)), length.out = 4)),
        name = paste(l$frq, ' / ', l$bln),
        guide = guide_colorbar(direction = "horizontal"),
        na.value = "#440154")
  } else {
    p <- p +
      scale_fill_viridis(
        option = 'C', trans = 'log10', begin = .2, end = 1,
        # breaks = round(exp(seq(log(1), log(max(df_heat_adm_fat$n)), length.out = 4))), 
        breaks = round(10^seq(log10(1), log10(max(df_heat_adm_fat$n)), length.out = 4)),
        name = paste(l$frq, ' / ', l$bln),
        guide = guide_colorbar(direction = "horizontal"),
        na.value = "#440154")
  }
  
  return(p)
}

p_heat_evn <- create.heatmap(df_heat_adm_evn, event = TRUE)
p_heat_fat <- create.heatmap(df_heat_adm_fat, event = FALSE)


# bar per type
nn <- 7
df_frq_adm2 <- acled %>% count(ADMIN2) %>% arrange(desc(n)) %>% top_n(n = nn) %>%
  set_names(c('adm', 'freq'))
df_frq_adm3 <- acled %>% count(ADMIN3) %>% arrange(desc(n)) %>% na.omit() %>%
  top_n(n = nn) %>% set_names(c('adm', 'freq'))
df_fat_adm2 <- acled %>% group_by(ADMIN2) %>% summarize(fat = sum(FATALITIES)) %>%
  arrange(desc(fat)) %>% na.omit() %>% top_n(n = nn) %>% set_names(c('adm', 'freq'))
df_fat_adm3 <- acled %>% group_by(ADMIN3) %>% summarize(fat = sum(FATALITIES)) %>%
  arrange(desc(fat)) %>% na.omit() %>% top_n(n = nn) %>% set_names(c('adm', 'freq'))

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

pa1 <- p.adm(df_frq_adm2, 'Events', 'Regency/City')
pa2 <- p.adm(df_frq_adm3, 'Events', 'District')
pb1 <- p.adm(df_fat_adm2, 'Fatalities', 'Regency/City', event = FALSE)
pb2 <- p.adm(df_fat_adm3, 'Fatalities', 'District', event = FALSE)

p_adm_evt_fat <- pa1 + pa2 + pb1 + pb2 + plot_layout(nrow = 2)

# cusw1 = plot_layout(width = c(5, .15, 5))
# # cusw2 = plot_layout(width = c(5, 1.2, 5))
# cusw2 = plot_layout(width = c(5, 1.2, 5))
# combine_1 =  (pa1 + space + pa2 + cusw1) & plot_annotation(title = "Events") & 
#   theme(plot.title = element_text(hjust = .5))
# combine_2 =  (pb1 + space + pb2 + cusw2) & plot_annotation(title = "Fatalities") & 
#   theme(plot.title = element_text(hjust = .5))
# 
# # type n and fat
# p_adm_evt_fat <- wrap_elements(combine_1) / wrap_elements(combine_2)
# 
# wrap_plots(pa1,pa2,pb1,pb2)

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

# map type
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

# stacked bar
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
  # geom_bar(stat = 'identity', aes(fill = event)) +
  geom_col(position = 'fill', width = .6) +
  scale_x_continuous(breaks = seq(1, 12, 1), expand = expansion(mult = c(.02, .02))) +
  scale_y_continuous(
    breaks = seq(.0, 1, .5),
    expand = expansion(mult = c(.05, .05))) +
  scale_fill_zata() +
  # theme(axis.text.x = element_text(vjust = .4), axis.title = element_blank()) +
  guides(fill = guide_legend(nrow = 1)) +
  facet_wrap(~year, nrow = 3, ncol = 3) +
  labs(x = "Number of Month", y = "Proportion", fill = NULL)


# admin event and fatalities rank per type
etypes <- c(l$bat, l$ervl, l$prt, l$rts, l$devl, l$vacl)
scan.evn <- function(x) {
  acled %>% filter(EVENT_TYPE == {{ x }}) %>%  count(ADMINID, sort = TRUE) %>% 
    rename(total = n) %>% top_n(5, wt = total) %>% arrange(desc(total))}
ls.evn.adm <- lapply(etypes, function(etypes) {
  scan.evn(etypes) %>% mutate(type = etypes)})

#---

scan.fat <- function(x) {
  acled %>% filter(EVENT_TYPE == {{ x }}) %>% group_by(ADMINID) %>%
    summarize(total = sum(FATALITIES)) %>% filter(total > 0) %>%
    top_n(5, wt = total) %>%arrange(desc(total))}

ls.fat.adm <- lapply(etypes, function(etypes) {
  scan.fat(etypes) %>% mutate(type = etypes)})

#--

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

## map fatal per type
df_fat <- acled %>% group_by(ADMIN1, EVENT_TYPE) %>% 
  summarise(FATAL = sum(FATALITIES), .groups = 'drop') %>%
  mutate(FATAL = replace(FATAL, FATAL == 0, NA)) %>%
  complete(ADMIN1, EVENT_TYPE, fill = list(FATAL = NA))

df_adm_fat <- prvnc %>%
  left_join(df_fat, by = c('PROVINSI' = 'ADMIN1'))

p_cho_adm_fat <- ggplot() +
  geom_sf(data = df_adm_fat, aes(fill = FATAL), lwd = NA) +
  scale_fill_viridis_c(
    option = 'C', trans = 'log1p', na.value = pal.zata.grey[4], direction = 1, 
    breaks = c(1, max(df_adm_fat$FATAL, na.rm = TRUE)), begin = .05, end = .95, 
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

## Wordcloud
stop.words <- c(
  stopwords('en'),
  'january', 'february', 'march', 'april', 'may', 'june', 'july',
  'august', 'september', 'october', 'november', 'december',
  'province', 'held', 'city', 'district', 'regency', 'north',
  'west', 'south', 'east', 'front'
)

create.wc <- function(month, min) {
  note <- acled %>% filter(CMONTH == month) %>% dplyr::select(NOTES)
  text <- paste(note, colapse = " ")                     # Pra-pemrosesan teks
  corpus <- Corpus(VectorSource(text))                   # Membuat Corpus dari teks
  corpus <- tm_map(corpus, content_transformer(tolower)) # Ubah ke huruf kecil
  corpus <- tm_map(corpus, removePunctuation)            # Hapus tanda baca
  corpus <- tm_map(corpus, removeNumbers)                # Hapus angka
  corpus <- tm_map(corpus, removeWords, stop.words)      # Hapus stop words bahasa Inggris
  tdm <- TermDocumentMatrix(corpus)                      # Buat Term Document Matrix
  m <- as.matrix(tdm)                                    # Ubah TDM ke dalam bentuk matriks
  word_freqs <- sort(rowSums(m), decreasing = TRUE)      # Hitung frekuensi masing-masing kata
  wordcloud(words = names(word_freqs), freq = word_freqs, min.freq = min,
            max.words = max(word_freqs), random.order = FALSE, colors = brewer.pal(8, "Dark2"), 
            rot.per = .35, scale = c(2, .4))
}

## density2d
dfden <- acled %>% group_by(CMONTH, EVENT_TYPE_SRT) %>% summarise(n = n(), .groups = 'drop_last')

create.den2d <- function(fill = FALSE, point = FALSE) {
  p <- dfden %>%
    ggplot(aes(x = CMONTH, y = n)) +
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

pden2d <- create.den2d(point = TRUE) + space + create.den2d(fill = TRUE, point = FALSE) + 
  layw2 + plot_layout(axis_titles = 'collect') +
  plot_annotation(
    title = NULL,
    caption = 'Logarithmic scaled y axis',
    theme = theme(plot.title = element_text(size = 9))
  )

plot.c12 <- function(p1, p2) {
  p <- p1 + space + p2 + layw2
  return(p)
}

plot.c22 <- function(p1, p2, p3, p4) {
  p <- (p1 + space + p2 + layw2) / space / (p3 + space + p4 + layw2) + layh2
  return(p)
}

##---
dfsubtype <- data.frame(table(acled$EVENT_TYPE, acled$SUB_EVENT_TYPE)) %>% filter(Freq != 0) %>% rename(type = Var1, subtype = Var2)

mtpe <- c(
  'Violence against civilians' = 'VAC', 'Explosions/Remote violence' = 'ERV', 
  'Battles' = 'Battles', 'Strategic developments' = 'Str.Dev.', 'Protests' = 'Protests',
  'Riots' = 'Riots'
)
dfsubtype <- dfsubtype %>% mutate(typ = recode(as.character(type), !!!mtpe))
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
dfsubtype <- dfsubtype %>% mutate(Sub = recode(as.character(subtype), !!!subbr))
dfsubtype$Sub <- factor(dfsubtype$Sub, levels = dfsubtype$Sub[order(dfsubtype$type)])

p_subtype <- dfsubtype %>%
  ggplot(aes(x = Sub, y = Freq, fill = as.factor(type))) +
  geom_bar(stat = 'identity', position = 'dodge', width = .6) +
  geom_text(aes(y = Freq, label = Freq), position = 'identity',
            size = 2.5, hjust = -.3, vjust = .3, angle = 90) +
  scale_y_continuous(trans = 'sqrt', expand = expansion(mult = c(.01, .5))) +
  scale_fill_zata() +
  theme(legend.position = 'right', panel.border = element_blank(), 
        panel.grid.major.y = element_blank(), axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), axis.title.y = element_blank(),
        # axis.text.x = element_text(angle = -25, vjust = 1, hjust = 0, size = 7),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = .2),
        plot.margin = unit(c(0,60,0,60), 'pt')) +
  guides(fill = guide_legend(title = "Event Types", size = 5)) + 
  xlab('Sub Event Types')

## Aktor

actor_interactions <- acled %>%
  group_by(INTER1, INTER2) %>%
  summarize(freq = n(), .groups = 'drop') %>%
  filter(INTER1 != "n/a" & INTER2 != "n/a") %>%
  arrange(desc(freq))

# Create a tbl_graph
graph_data <- actor_interactions %>%
  as_tbl_graph(directed = TRUE, node_key = "Actor", edge = c("INTER1", "INTER2"))

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

ptil <- ggplot(actor_interactions, aes(INTER1, INTER2, fill = freq)) + 
  geom_tile() + scale_fill_viridis(discrete = F)

##---
df_cas_act <- acled %>%
  count(INTER1, INTER2, wt = FATALITIES) %>%
  mutate(actors = ifelse(is.na(INTER2), INTER1, INTER2), total = n) %>%
  group_by(actors) %>%
  summarize(total = sum(total)) %>%
  arrange(desc(total)) %>%
  filter(total > 0) 

df_act <- acled %>%
  count(INTER1) %>%
  rename(actors = INTER1, total = n) %>%
  filter(!is.na(actors)) %>%
  bind_rows(acled %>%
              count(INTER2) %>%
              rename(actors = INTER2, total = n) %>%
              filter(!is.na(actors))) %>%
  group_by(actors) %>%
  summarize(total = sum(total)) %>%
  arrange(desc(total))


create.plactr <- function(df, opt, tit) {
  p <- df %>% ggplot(aes(x = reorder(actors, total), y = total)) +
    geom_col(aes(fill = total), width = .6) +
    geom_text(aes(label = total), vjust = .4, hjust = -.3, size = 2.5) +
    scale_y_continuous(trans = 'sqrt', expand = expansion(mult = c(.02, .25))) +
    scale_fill_viridis_c(option = opt, trans = 'log', begin = .05, end = .95) +
    theme(legend.position = 'none', panel.border = element_blank(),
          panel.grid.major.x = element_blank(), axis.text.x = element_blank(),
          axis.ticks.x = element_blank(), axis.title = element_blank()) +
    labs(title = tit) +
    coord_flip()
  return(p)
}

p_actor <- create.plactr(df_act, 'D', 'Actor Occurance')
p_actor_fat <- create.plactr(df_cas_act, 'C', 'Actors Contributions to Fatalities')
p_actor <- p_actor + space + p_actor_fat + layw2




