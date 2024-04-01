##----------------------------------------------------------------------------------- MORAN

spatial.moran.mc <- function(data, variable, data_geo) {
  nb <- poly2nb(data_geo, queen = T)
  lw <- nb2listw(nb, style = "W", zero.policy = T)
  
  moran_test <- moran.test(data[[variable]], listw = lw)
  mcmoran <- moran.mc(data[[variable]], lw, nsim = 999)
  
  result <- list(
    moran_test = moran_test,
    mcmoran = mcmoran
  )
  return(result)
}

result_evt <- spatial.moran.mc(df_geo_adm, "n", df_geo_adm)
result_fat <- spatial.moran.mc(df_geo_fat, "n", df_geo_fat)

x_evt <- seq(-.4, .6, by = .2)
x_fat <- c(-.25, 0, .25, .5, .75)

plot_mcmoran <- function(result, br, col, title = "") {
  plot_data <- data.frame(Moran_I = c(result$mcmoran$statistic, result$mcmoran$res))
  moran_statistic <- result$mcmoran$statistic
  density_data <- density(plot_data$Moran_I)
  shade_start_index <- which(density_data$x >= moran_statistic)[1]
  moran_statistic_2 <- min(plot_data$Moran_I)
  shade_start_index_2 <- 1
  
  p <- ggplot() +
    geom_area(data = data.frame(
      x = density_data$x[shade_start_index_2:length(density_data$x)],
      y = density_data$y[shade_start_index_2:length(density_data$x)]),
      aes(x = x, y = y), fill = "#fde725") +
    geom_area(data = data.frame(
      x = density_data$x[shade_start_index:length(density_data$x)],
      y = density_data$y[shade_start_index:length(density_data$x)]),
      aes(x = x, y = y), fill = col) +
    geom_vline(xintercept = moran_statistic, linetype = "longdash", color = "#2e0595", lwd = 0.5) +
    ggplot2::annotate(geom = "text", 
                      x = moran_statistic + 0.03, 
                      y = 1, label = paste(round(moran_statistic, 2)), 
                      size = 2.5, hjust = 0, vjust = 0, fontface = "italic", color = "#2e0595") +
    labs(x = "Moran's I", y = "Density", title = title) +
    scale_x_continuous(breaks = br) +
    scale_y_continuous(expand = expansion(mult = c(0,0.01)))
  
  print(p)
}

p_moran_evn <- plot_mcmoran(result_evt, x_evt, "#2c728e", "Events")
p_moran_fat <- plot_mcmoran(result_fat, x_fat, "#d8576b", "Fatalities")
p_moran <- p_moran_evn + space + p_moran_fat + layw2

## -------------------------------------------------------------------------------
