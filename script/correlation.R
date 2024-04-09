# auto spatial --------------------------------------------------------------------------------

## Permutasi moran
spatial.moran.mc <- function(data, variable) {
  nb <- poly2nb(data, queen = T)
  lw <- nb2listw(nb, style = "W", zero.policy = T)

  moran_test <- moran.test(data[[variable]], listw = lw)
  mcmoran <- moran.mc(data[[variable]], lw, nsim = 999)

  result <- list(
    moran_test = moran_test,
    mcmoran = mcmoran
  )
  return(list(result = result, nb = nb, lw = lw))
}

df_f_mod <- df_prv_geo %>%
  filter(TYPE == "FATALITIES") %>%
  mutate(n = replace(n, is.na(n), 0))

result_evt <- spatial.moran.mc(df_sf_evn, "n")
result_fat <- spatial.moran.mc(df_f_mod, "n")

x_evt <- seq(-.4, .6, by = .2)
x_fat <- c(-.25, 0, .25, .5, .75)

plot_mcmoran <- function(res, br, col, title = "") {
  plot_data <- data.frame(Moran_I = c(res$result$mcmoran$statistic, res$result$mcmoran$res))
  moran_statistic <- res$result$mcmoran$statistic
  density_data <- density(plot_data$Moran_I)
  shade_start_index <- which(density_data$x >= moran_statistic)[1]
  moran_statistic_2 <- min(plot_data$Moran_I)
  shade_start_index_2 <- 1

  p <- ggplot() +
    geom_area(
      data = data.frame(
        x = density_data$x[shade_start_index_2:length(density_data$x)],
        y = density_data$y[shade_start_index_2:length(density_data$x)]
      ),
      aes(x = x, y = y), fill = "#fde725"
    ) +
    geom_area(
      data = data.frame(
        x = density_data$x[shade_start_index:length(density_data$x)],
        y = density_data$y[shade_start_index:length(density_data$x)]
      ),
      aes(x = x, y = y), fill = col
    ) +
    geom_vline(xintercept = moran_statistic, linetype = "longdash", color = "#2e0595", lwd = 0.5) +
    ggplot2::annotate(
      geom = "text",
      x = moran_statistic + 0.03,
      y = 1, label = paste(round(moran_statistic, 2)),
      size = 2.5, hjust = 0, vjust = 0, fontface = "italic", color = "#2e0595"
    ) +
    labs(x = "Moran's I", y = "Density", title = title) +
    scale_x_continuous(breaks = br) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.01)))

  return(p)
}

p_moran_evn <- plot_mcmoran(result_evt, x_evt, "#2c728e", "Events")
p_moran_fat <- plot_mcmoran(result_fat, x_fat, "#d8576b", "Fatalities")
p_moran <- p_moran_evn + space + p_moran_fat + layw2

## Plot moran

plot.moran <- function(df_spatial, variable, listw, title) {
  x <- df_spatial[[variable]]
  w <- lag.listw(listw, x, zero.policy = T)
  xwx.lm <- lm(w ~ x)
  infl.xwx <- influence.measures(xwx.lm)
  is.inf <- which(apply(infl.xwx$is.inf, 1, any))
  labels <- as.character(df_spatial$PROVINSI)
  plot_data <- data.frame(x = x, wx = w, labels = labels)

  p <- ggplot(plot_data, aes(x = x, y = wx)) +
    geom_point(shape = 16, size = 2, color = "#FC4E07", alpha = .5) +
    geom_abline(slope = coef(xwx.lm)[2], intercept = coef(xwx.lm)[1]) +
    geom_hline(yintercept = mean(w), linetype = "dashed") +
    geom_vline(xintercept = mean(x), linetype = "dashed") +
    geom_point(data = plot_data[is.inf, ], aes(x = x, y = wx), shape = 8, size = 2) +
    geom_text_repel(
      data = plot_data[is.inf, ], aes(x = x, y = wx, label = labels),
      size = 2.5, box.padding = .8
    ) +
    scale_y_continuous(expand = expansion(mult = c(.07, .2))) +
    labs(x = variable, y = "Spatially lagged", title = title) +
    theme(axis.title = element_blank())
  return(p)
}

# df_sf_evn dan df_sf_fat sebagai dasar df_f_mod didefinisikan di distribution
p_mpe <- plot.moran(df_sf_evn, "n", result_evt$lw, "Events")
p_mpf <- plot.moran(df_f_mod, "n", result_fat$lw, "Fatalities")
p_moranp <- p_mpe + space + p_mpf + layw2

# event type correlation ----------------------------------------------------------------------


df_cofa <- df_monthly_num[, c(4:9)] %>%
  set_names(c("Battles", "ERV", "Protests", "Riots", "Str.Dev.", "VAC"))

df_cor <- round(cor(df_cofa), 1)
p.mat <- cor_pmat(df_cofa)

p_cor_con2 <- ggcorrplot(
  df_cor,
  tl.cex = 8.5, hc.order = TRUE, type = "lower", outline.color = "white", lab = TRUE,
  lab_size = 2.5, ggtheme = NULL, colors = c(zcol[2], "white", zcol[1]), method = "circle",
  legend.title = NULL, p.mat = p.mat
) +
  theme(
    legend.position = "right", axis.text.x = element_text(angle = 0, hjust = .5),
    legend.key.height = unit(10, "mm"), legend.key.width = unit(3, "mm")
  )

# admin and event types -----------------------------------------------------------------------

p_dis_evn <- acled %>%
  ggplot(aes(x = ADMIN1_ABR, y = fct_rev(EVENT_TYPE_SRT))) +
  geom_jitter(aes(color = EVENT_TYPE_SRT), size = .3, show.legend = FALSE) +
  scale_color_zata() +
  theme(axis.text.x = element_text(hjust = .5)) +
  guides(color = guide_legend(nrow = 1)) +
  labs(x = NULL, y = NULL)

# actor 1 vs actor 2 --------------------------------------------------------------------------

custom_order <- c(
  "Civilians", "Identity Militias", "Other Forces", "Political Militias",
  "Protesters", "Rebel Groups", "Rioters", "State Forces", "Sole Action"
)

df_cor_actrs <- acled %>%
  dplyr::select(EVENT_TYPE, ACT1, ACT2) %>%
  mutate(ACT2 = ifelse(is.na(ACT2), "Sole Action", ACT2))

df_cor_actrs$ACT2 <- factor(df_cor_actrs$ACT2, levels = custom_order)

p_cor_actrs <- ggplot(df_cor_actrs) +
  aes(x = ACT1, y = ACT2, fill = EVENT_TYPE) +
  geom_tile(color = "#ffffff", lwd = 1.5) +
  scale_fill_zata() +
  theme(
    legend.title = element_text(size = 8),
    legend.position = "right",
    panel.border = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
  ) +
  labs(x = "Actor 1", y = "Actor 2", fill = "Event Type") +
  coord_flip()

# admin vs interaction fatalities -------------------------------------------------------------

df_inter_fat_adm <- acled %>%
  group_by(ADMIN1_ABR, INTERACTION) %>%
  summarise(FATAL = sum(FATALITIES), .groups = "drop") %>%
  complete(ADMIN1_ABR, INTERACTION, fill = list(FATAL = NA))

p_adm_inter_fat <- ggplot(df_inter_fat_adm) +
  aes(x = ADMIN1_ABR, y = INTERACTION, fill = FATAL) +
  geom_tile(color = "black", lwd = .7) +
  scale_fill_viridis(
    option = "C", trans = "log10", begin = .3, end = 1,
    breaks = round(10^seq(log10(1), log10(max(df_inter_fat_adm$FATAL, na.rm = TRUE)), length.out = 4)),
    name = "Fatalities/Province",
    guide = guide_colorbar(direction = "horizontal"),
    na.value = "#440154"
  ) +
  theme(
    legend.position = "top", legend.justification = "right",
    legend.key.height = unit(1, "mm"),
    legend.key.width = unit(8, "mm"), legend.title = element_text(size = 7),
    legend.text = element_text(size = 6), legend.ticks = element_blank(),
    panel.border = element_blank(), axis.text = element_text(size = 7),
    axis.ticks.length = unit(1, "mm"), legend.margin = margin(0, 8, 0, 0)
  )

df_inter_evn_adm <- acled %>%
  group_by(ADMIN1_ABR, INTERACTION) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(ADMIN1_ABR, INTERACTION, fill = list(n = NA))

p_adm_inter_evn <- ggplot(df_inter_evn_adm) +
  aes(x = ADMIN1_ABR, y = INTERACTION, fill = n) +
  geom_tile(color = "black", lwd = .7) +
  scale_fill_viridis(
    option = "D", trans = "log10", begin = .3, end = 1,
    breaks = round(10^seq(log10(1), log10(max(df_inter_evn_adm$n, na.rm = TRUE)), length.out = 4)),
    name = "Fatalities/Province",
    guide = guide_colorbar(direction = "horizontal"),
    na.value = "#440154"
  ) +
  theme(
    legend.position = "top", legend.justification = "right",
    legend.key.height = unit(1, "mm"),
    legend.key.width = unit(8, "mm"), legend.title = element_text(size = 7),
    legend.text = element_text(size = 6), legend.ticks = element_blank(),
    panel.border = element_blank(), axis.text = element_text(size = 7),
    axis.ticks.length = unit(1, "mm"), legend.margin = margin(0, 8, 0, 0)
  )

# fatalities act1 / admin ---------------------------------------------------------------------

df_actrs_adm_fat <- acled %>%
  group_by(ACT1, ACT2) %>%
  summarise(FATAL = sum(FATALITIES), .groups = "drop") %>%
  mutate(ACT2 = ifelse(is.na(ACT2), "Sole Action", ACT2)) %>%
  complete(ACT1, ACT2, fill = list(FATAL = NA))

d <- ggplot(df_actrs_adm_fat) +
  aes(x = ACT1, y = ACT2, fill = FATAL) +
  geom_tile(color = "black", lwd = 1) +
  scale_fill_viridis(
    option = "C", trans = "log10", begin = .3, end = 1,
    breaks = round(10^seq(log10(1), log10(max(df_actrs_adm_fat$FATAL, na.rm = TRUE)), length.out = 4)),
    name = "Fatalities/Province",
    guide = guide_colorbar(direction = "horizontal"),
    na.value = "#440154"
  )

# Task:
#   1. complete rows in df_actrs_adm_fat, fill it with NA
#   2. use df_inter_fat_adm process as reference
#   3. use viridis in p_adm_inter_fat as reference for plotting
#   4. ngopi dulu
