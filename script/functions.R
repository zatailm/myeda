# Fungsi: accuracy
accuracy <- function(actual, predicted) {
  mean(pmin(abs(actual - predicted), abs(actual - predicted + 1)) / abs(actual))
}
# Fungsi: cb2d
cb2d <- function(cp, sd = as.Date("2015-01-03")) {
  y <- as.integer(cp)
  d <- as.integer((cp - y) * 365)
  sd + years(y - 2015) + days(d)
}
# Fungsi: com.plot
com.plot <- function(data, x, y1, y2, abr = TRUE) {
  p <- data %>%
    ggplot(aes(x = {{ x }})) +
    geom_col(aes(y = sqrt({{ y1 }}), fill = "Events"), position = "identity", width = 0.6) +
    geom_col(aes(y = -sqrt({{ y2 }}), fill = "Fatalities"), position = "identity", width = 0.6) +
    geom_text(aes(y = sqrt({{ y1 }}) + 0.1, label = {{ y1 }}), position = "identity", size = 2.5, hjust = -0.3, vjust = 0.35, angle = 90) +
    geom_text(aes(y = -sqrt({{ y2 }}) - 0.1, label = {{ y2 }}),
      position = "identity", size = 2.5, hjust = 1.3, vjust = 0.35,
      angle = 90
    ) +
    scale_y_continuous(
        expand = expansion(mult = c(0.25, 0.35))
    ) +
    theme(
      axis.text.y = element_blank(), axis.ticks.y = element_blank(),
      axis.title.y = element_blank(), panel.border = element_blank(),
      panel.grid.major.y = element_blank()
    ) +
    scale_fill_manual(values = c(
      Events = "#3e4a89",
      Fatalities = "#fca636"
    ), guide = guide_legend(title = NULL)) +
    labs(
      x = l$prab,
      caption = "Square root scaled bar"
    )
  if (abr) {
    return(p)
  } else {
    p <- p + theme(axis.text.x = element_text(
      angle = 90, vjust = 0.35,
      hjust = 1
    ))
  }
  return(p)
}
# Fungsi: compare
compare <- function(data, clean = TRUE) {
  p <- ggplot(data = data, aes(x = Metric, y = Count, fill = Metric)) +
    geom_bar(stat = "identity", width = 0.7) +
    geom_text(aes(label = Count),
      hjust = -0.2, size = 2.7
    ) +
    scale_y_continuous(
      trans = "log1p",
      expand = expansion(mult = c(0, 0.2))
    ) +
    scale_fill_zata() +
    theme(
      legend.position = "none",
      plot.title.position = "plot", axis.text.x = element_blank(), axis.line.x = element_blank(),
      panel.grid.major.x = element_blank()
    ) +
    coord_flip()
  if (clean) {
    p <- p + theme(axis.text.y = element_blank()) + labs(
      x = NULL, y = NULL,
      title = "Dataset Characteristics (Processed)", caption = "Logarithmic scaled bar"
    )
  } else {
    p <- p + labs(x = NULL, y = NULL, title = "Dataset Characteristics (Pre-processed)")
  }
  return(p)
}
# Fungsi: create.den2d
create.den2d <- function(data, x, y, fill = FALSE, point = FALSE) {
  p <- data %>%
    ggplot(aes(x = {{ x }}, y = {{ y }})) +
    scale_x_continuous(breaks = seq(1, 107, 7)) +
    scale_y_continuous(trans = "log1p") +
    theme(
      axis.title.y = element_blank(), axis.text.y = element_blank(),
      axis.ticks.y = element_blank(), panel.grid.major.y = element_blank()
    ) +
    xlab("Number of Month (Continuous)")
  if (fill) {
    p <- p + geom_density_2d_filled(show.legend = "none") +
      scale_fill_viridis_d(option = "inferno")
  } else if (point) {
    p <- p + geom_point(size = 0.5, alpha = 0.5, color = zcol[6]) +
      geom_density_2d(color = zcol[1])
  } else {
    p <- p + geom_density_2d(color = zcol[1])
  }
  return(p)
}
# Fungsi: create.ef
create.ef <- function(df, x, y, tit, d = TRUE) {
  p <- df %>%
    filter(total > 0) %>%
    ggplot(aes(x = reorder({{ x }}, {{ y }}), y = {{ y }}, fill = {{ y }})) +
    geom_col(width = 0.6) +
    geom_text(aes(label = {{ y }}), hjust = -0.5, vjust = 0.3, size = 2.5) +
    scale_y_continuous(expand = expansion(mult = c(
      0.02,
      0.3
    ))) +
    theme(
      legend.position = "none", plot.margin = ggplot2::margin(
        5,
        0, 0, 0, "pt"
      ), plot.title = element_text(size = 8), panel.border = element_blank(),
      axis.text.x = element_blank(), axis.ticks.x = element_blank(),
      panel.grid.major.x = element_blank(), axis.title = element_blank()
    ) +
    labs(title = tit) +
    coord_flip()
  if (d) {
    p <- p + scale_fill_viridis_c(
      option = "D", trans = "sqrt", begin = 0.05,
      end = 0.95
    )
  } else {
    p <- p + scale_fill_viridis_c(
      option = "C", trans = "sqrt", begin = 0.05,
      end = 0.95
    )
  }
}
# Fungsi: create.heatmap
create.heatmap <- function(data, xdat, ydat, value, viridis, numeric = FALSE, pass.scale = TRUE) {
  if (!all(c(xdat, ydat, value) %in% names(data))) {
    stop("Columns not found in data!")
  }
  p <- data %>%
    ggplot(aes(x = !!sym(xdat), y = reorder(!!sym(ydat), !!sym(value)))) +
    geom_tile(aes(fill = !!sym(value)), color = "#000000", linewidth = 0.25) +
    scale_y_discrete(position = "right") +
    scale_fill_viridis(
      option = viridis,
      trans = "log10", begin = 0.2, end = 1, breaks = round(10^seq(log10(1),
        log10(max(data[[value]], na.rm = TRUE)),
        length.out = 4
      )), name = paste(
        l$frq,
        " / ", l$bln
      ), guide = guide_colorbar(direction = "horizontal"), na.value = "#440154"
    ) +
    theme(
      legend.position = "top", legend.justification = "right", legend.key.height = unit(
        1.5,
        "mm"
      ), legend.key.width = unit(10, "mm"), legend.title = element_text(size = 6),
      legend.text = element_text(size = 6), legend.ticks = element_blank(),
      panel.border = element_blank(), axis.text = element_text(size = 6),
      axis.ticks.length = unit(1, "mm")
    )
  if (numeric) {
    p <- p + scale_x_continuous(breaks = seq(0, 108, 4), expand = c(0, 0))
  } else {
    if (pass.scale) {
      return(p)
    } else {
      p <- p + scale_x_date(
        breaks = seq(as.Date(min(data[[xdat]])) + years(1),
          as.Date(max(data[[xdat]])),
          by = "1 year"
        ), date_labels = "%Y",
        expand = c(0, 0)
      )
    }
  }
  return(p)
}
# Fungsi: create.plactr
create.plactr <- function(df, x, y, opt, tit) {
  p <- df %>%
    ggplot(aes(x = reorder({{ x }}, {{ y }}), y = {{ y }})) +
    geom_col(aes(fill = {{ y }}), width = 0.6) +
    geom_text(aes(label = {{ y }}), vjust = 0.4, hjust = -0.3, size = 2.5) +
    scale_y_continuous(
      trans = "sqrt",
      expand = expansion(mult = c(0.02, 0.25))
    ) +
    scale_fill_viridis_c(
      option = opt,
      trans = "log", begin = 0.05, end = 0.95
    ) +
    theme(
      legend.position = "none",
      panel.border = element_blank(), panel.grid.major.x = element_blank(),
      axis.text.x = element_blank(), axis.ticks.x = element_blank(), axis.title = element_blank()
    ) +
    labs(title = tit) +
    coord_flip()
  return(p)
}
# Fungsi: create.wc
create.wc <- function(data, src.in, src, words, rem.words = stop.words, min, max) {
  note <- data %>%
    filter({{ src.in }} == src) %>%
    dplyr::select({{ words }})
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
  wordcloud(
    words = names(word_freqs), freq = word_freqs, min.freq = min, max.words = max,
    random.order = FALSE, colors = brewer.pal(8, "Dark2"), rot.per = 0.35, scale = c(2, 0.4)
  )
}
# Fungsi: cv
cv <- function(model, data, k = 5) {
  n <- length(data)
  fs <- ceiling(n / k)
  mse_cv <- rep(0, k)
  for (i in 1:k) {
    start <- (i - 1) * fs + 1
    end <- min(i * fs, n)
    ti <- c(1:(start - 1), (end + 1):n)
    td <- data[ti]
    vd <- data[start:end]
    mf <- Arima(td, model = model)
    vf <- forecast(mf, h = length(vd))$mean
    mse_cv[i] <- mse(vd, vf)
  }
  return(mean(mse_cv))
}
# Fungsi: do.scan
do.scan <- function(data) {
  column_types <- sapply(data, function(col) {
    if (is.factor(col) || is.character(col)) {
      "discrete"
    } else if (is.numeric(col)) {
      "continuous"
    } else {
      "other"
    }
  })
  num_discrete_columns <- sum(column_types == "discrete")
  num_continuous_columns <- sum(column_types == "continuous")
  missing_columns <- sum(colSums(is.na(data)) > 0)
  complete_rows <- sum(complete.cases(data))
  missing_observations <- sum(is.na(data))
  num_columns_in_dataset <- ncol(data)
  num_rows_in_dataset <- nrow(data)
  results_data <- data.frame(Metric = c(
    "Discrete Columns", "Continuous Columns",
    "Missing Columns", "Complete Rows", "Missing Observations", "Number of Columns",
    "Number of Rows"
  ), Count = c(
    num_discrete_columns, num_continuous_columns, missing_columns,
    complete_rows, missing_observations, num_columns_in_dataset, num_rows_in_dataset
  ))
  return(results_data)
}
# Fungsi: extract_acf_data
extract_acf_data <- function(x) {
  data <- as.data.frame.table(x$acf)[-1]
  data$lag <- as.numeric(x$lag)
  return(data)
}
# Fungsi: gl
gl <- function(db) {
  sapply(db, function(d) {
    d <- as.Date(d)
    y <- year(d)
    m <- month(d)
    w <- week(d) - week(floor_date(d, "month")) + 1
    paste("Y:", y, ", M:", m, ", W:", w)
  })
}
# Fungsi: load_mypkg
load_mypkg <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = TRUE)
  }
  lapply(pkg, function(x) {
    suppressMessages(suppressPackageStartupMessages(library(x, character.only = TRUE)))
  })
}
# Fungsi: map.loc
map.loc <- function(layera, layerb, show_legend = TRUE, wrap = FALSE) {
  p <- ggplot() +
    geom_sf(data = layera) +
    geom_sf(
      data = layerb, aes(color = EVENT_TYPE),
      size = 1, alpha = 0.2, show.legend = show_legend
    ) +
    theme_void() +
    theme(
      legend.position = ifelse(show_legend, "right", "none"),
      legend.title = element_text(size = 8)
    ) +
    scale_color_manual(
      name = "Event Type",
      values = pal.zata
    )
  if (wrap) {
    p <- p + facet_wrap(~EVENT_TYPE, ncol = 3)
  }
  return(p)
}
# Fungsi: map.plot
map.plot <- function(data, title, breaks, labels, option) {
  ggplot() +
    geom_sf(data = data, aes(fill = n), lwd = NA) +
    scale_fill_viridis_c(
      option = option,
      begin = 0.05, end = 0.95, trans = "log10", direction = 1,
      breaks = breaks, labels = labels, na.value = pal.zata.grey[4]
    ) +
    scale_x_continuous(expand = expansion(mult = c(0.03, 0.03))) +
    theme_void(10) +
    theme(
      legend.position = "bottom", legend.text = element_text(size = 7),
      legend.key.height = unit(3, "pt"), legend.key.width = unit(
        10,
        "pt"
      ), plot.title = element_text(hjust = 0.5, size = 8),
      plot.margin = ggplot2::margin(0, 0, 0, 0, "pt"), legend.justification = "right",
      legend.title = element_blank(), legend.margin = ggplot2::margin(
        0,
        20, 0, 0
      )
    ) +
    labs(title = title)
}
# Fungsi: mape
mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual)) * 100
}
# Fungsi: mse
mse <- function(actual, predicted) {
  mean((actual - predicted)^2)
}
# Fungsi: p.adm
p.adm <- function(df, tit, xlab, event = TRUE) {
  p <- df %>%
    ggplot(aes(x = fct_reorder(adm, freq), y = freq)) +
    geom_col(aes(fill = freq),
      width = 0.6, show.legend = F
    ) +
    geom_text(aes(label = freq),
      hjust = -0.3,
      vjust = 0.5, size = 2.5
    ) +
    scale_y_continuous(expand = expansion(mult = c(
      0.01,
      0.2
    ))) +
    theme(
      axis.ticks.x = element_blank(), axis.text.x = element_blank(),
      panel.grid.major.x = element_blank(), panel.border = element_blank()
    ) +
    labs(
      title = tit,
      x = xlab, y = NULL
    ) +
    coord_flip()
  if (event) {
    p <- p + scale_fill_viridis_c(option = "D", trans = "sqrt", begin = 0.2, end = 0.95)
  } else {
    p <- p + scale_fill_viridis_c(option = "C", trans = "log2", begin = 0.2, end = 0.95)
  }
  return(p)
}
# Fungsi: p.autocor
p.autocor <- function(est, data, title) {
  ci <- qnorm((1 + 0.95) / 2) / sqrt(est$n.used)
  p <- ggplot(data = data, aes(x = lag, xend = lag, y = 0, yend = Freq)) +
    geom_segment(color = zcol[1]) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = c(
      -ci,
      ci
    ), linetype = "dashed", color = zcol[2]) +
    scale_x_continuous(breaks = seq(
      0,
      1.5, 0.5
    )) +
    labs(y = title)
  return(p)
}
# Fungsi: p.hist
p.hist <- function(df, xtit) {
  center.dis <- density(df$value)$x[which.max(density(df$value)$y)]
  p <- ggplot(df, aes(x = value)) +
    geom_density(color = "black", fill = zcol[3], alpha = 0.5) +
    geom_vline(xintercept = center.dis, color = zcol[1], linetype = "longdash") +
    labs(
      x = xtit,
      y = "Density"
    )
  list(c = center.dis, p = p)
}
# Fungsi: pboxsea
pboxsea <- function(data, time) {
  if (identical(data, df_wek_evn)) {
    p <- ggplot(data = data, aes(x = factor({{ time }}), y = value)) +
      geom_boxplot(lwd = 0.3, color = my_cols[1]) +
      labs(title = NULL, x = colnames(data)[1], y = colnames(data)[2]) +
      scale_x_discrete(labels = function(x) {
        ifelse(as.numeric(x) %% 4 ==
          0, x, "")
      })
  } else if (identical(data, df_yer_evn)) {
    p <- ggplot(data = data, aes(x = factor({{ time }}), y = value)) +
      geom_boxplot(lwd = 0.3, color = my_cols[1]) +
      labs(title = NULL, x = colnames(data)[1], y = colnames(data)[2])
  } else {
    p <- ggplot(data = data, aes(x = {{ time }}, y = value)) +
      geom_boxplot(lwd = 0.3, color = zcol[1]) +
      labs(
        title = NULL,
        x = NULL, y = NULL
      )
  }
  return(p)
}
# Fungsi: plot.for
plot.for <- function(model, sub) {
  p <- ggplot() +
    geom_ribbon(aes(x = index(model$mean), ymin = model$lower[
      ,
      "95%"
    ], ymax = model$upper[, "95%"]), fill = zcol[6], alpha = 0.5) +
    geom_ribbon(aes(
      x = index(model$mean), ymin = model$lower[, "80%"],
      ymax = model$upper[, "80%"]
    ), fill = zcol[6], alpha = 0.5) +
    geom_line(aes(x = index(model$mean), y = model$mean),
      color = zcol[1],
      lwd = 0.7
    ) +
    geom_line(
      data = wcts, aes(x = index(wcts), y = wcts),
      color = zcol[2], lwd = 0.3
    ) +
    scale_x_continuous(breaks = seq(
      2015,
      2026, 2
    )) +
    scale_y_continuous(breaks = seq(1, 3, 1)) +
    labs(
      title = sub,
      x = NULL, y = NULL
    )
  p
}
# Fungsi: plot.moran
plot.moran <- function(df_spatial, variable, listw, title) {
  x <- df_spatial[[variable]]
  w <- lag.listw(listw, x, zero.policy = T)
  xwx.lm <- lm(w ~ x)
  infl.xwx <- influence.measures(xwx.lm)
  is.inf <- which(apply(infl.xwx$is.inf, 1, any))
  labels <- as.character(df_spatial$PROVINSI)
  plot_data <- data.frame(x = x, wx = w, labels = labels)
  p <- ggplot(plot_data, aes(x = x, y = wx)) +
    geom_point(
      shape = 16, size = 2,
      color = "#FC4E07", alpha = 0.5
    ) +
    geom_abline(
      slope = coef(xwx.lm)[2],
      intercept = coef(xwx.lm)[1]
    ) +
    geom_hline(yintercept = mean(w), linetype = "dashed") +
    geom_vline(xintercept = mean(x), linetype = "dashed") +
    geom_point(data = plot_data[is.inf, ], aes(x = x, y = wx), shape = 8, size = 2) +
    geom_text_repel(data = plot_data[is.inf, ], aes(x = x, y = wx, label = labels), size = 2.5, box.padding = 0.8) +
    scale_y_continuous(expand = expansion(mult = c(0.07, 0.2))) +
    labs(
      x = variable,
      y = "Spatially lagged", title = title
    ) +
    theme(axis.title = element_blank())
  return(p)
}
# Fungsi: plot_mcmoran
plot_mcmoran <- function(res, br, col, title = "") {
  plot_data <- data.frame(Moran_I = c(res$result$mcmoran$statistic, res$result$mcmoran$res))
  moran_statistic <- res$result$mcmoran$statistic
  density_data <- density(plot_data$Moran_I)
  shade_start_index <- which(density_data$x >= moran_statistic)[1]
  moran_statistic_2 <- min(plot_data$Moran_I)
  shade_start_index_2 <- 1
  p <- ggplot() +
    geom_area(data = data.frame(
      x = density_data$x[shade_start_index_2:length(density_data$x)],
      y = density_data$y[shade_start_index_2:length(density_data$x)]
    ), aes(x = x, y = y), fill = "#fde725") +
    geom_area(
      data = data.frame(x = density_data$x[shade_start_index:length(density_data$x)], y = density_data$y[shade_start_index:length(density_data$x)]),
      aes(x = x, y = y), fill = col
    ) +
    geom_vline(
      xintercept = moran_statistic, linetype = "longdash",
      color = "#2e0595", lwd = 0.5
    ) +
    ggplot2::annotate(
      geom = "text", x = moran_statistic + 0.03,
      y = 1, label = paste(round(moran_statistic, 2)), size = 2.5, hjust = 0, vjust = 0, fontface = "italic",
      color = "#2e0595"
    ) +
    labs(x = "Moran's I", y = "Density", title = title) +
    scale_x_continuous(breaks = br) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.01)))
  return(p)
}
# Fungsi: prep.texts
prep.texts <- function(data, src.in, src, words, lab, rem.words) {
  notes_list <- lapply(lab, function(src) {
    data %>%
      filter({{ src.in }} == src) %>%
      dplyr::select({{ words }}) %>%
      mutate(labels = src)
  })
  corp.list <- lapply(notes_list, function(x) VCorpus(VectorSource(toString(x))))
  corp.all <- corp.list[[1]]
  for (i in 2:length(src)) {
    corp.all <- c(corp.all, corp.list[[i]])
  }
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
  index <- as.logical(sapply(rownames(doc.tm.clean.mat), function(x) (nchar(x) > 3)))
  result <- doc.tm.clean.mat[index, ]
  return(result)
}
# Fungsi: ptrend
ptrend <- function(value, cap, point = TRUE) {
  if (point) {
    p <- ggplot(data = df_datweek, aes(x = year, y = event)) +
      geom_point(
        color = zcol[6],
        size = 0.7, alpha = 0.5
      ) +
      geom_line(aes(y = value), color = zcol[1])
  } else {
    p <- ggplot(data = df_datweek, aes(x = year, y = event)) +
      geom_line(aes(y = value),
        color = zcol[1]
      )
  }
  p + scale_x_continuous(breaks = seq(2015, 2023, 2)) + labs(x = NULL, y = NULL, caption = cap)
}
# Fungsi: pts
pts <- function(data, tit, cap, ylab) {
  ggplot(data = data, aes(x = index(data), y = data)) +
    geom_line(lwd = 0.3, color = zcol[1]) +
    scale_x_continuous(breaks = seq(2015, 2023, 2)) +
    labs(
      title = tit, subtitle = NULL,
      caption = cap, x = NULL, y = ylab
    )
}
# Fungsi: rb
rb <- function(d, t) {
  beast_res <- beast(d, quite = TRUE, print.progress = FALSE, print.options = FALSE)
  beast_df <- data.frame(
    time = beast_res$time, data = d, trend = beast_res$trend$Y,
    cpoc = beast_res$trend$cpOccPr
  )
  breakdata <- data.frame(cp = beast_res$trend$cp, cpPr = beast_res$trend$cpPr) %>%
    filter(!is.na(cp))
  cpmode <- round(beast_res$trend$ncp)
  sel_cp <- breakdata[1:cpmode, ]
  cp <- sel_cp$cp
  beast_df$isbreak <- "No"
  closest_indices <- sapply(cp, function(x) which.min(abs(beast_df$time - x)))
  beast_df$isbreak[closest_indices] <- "Yes"
  beast_df$type <- t
  dat_break <- cb2d(cp)
  datbreak_label <- gl(dat_break)
  list(breakdata = breakdata, datbreak = dat_break, data = beast_df)
}
# Fungsi: rmse
rmse <- function(actual, predicted) {
  sqrt(mse(actual, predicted))
}

# Fungsi: scan.break
scan.break <- function(x) {
  p <- ggplot(data = x, aes(x = time)) +
    geom_point(aes(y = data),
      color = zcol[6],
      alpha = 0.5, size = 0.7
    ) +
    geom_line(aes(y = trend), lwd = 0.7) +
    geom_area(aes(y = cpoc *
      1.5), fill = zcol[2], alpha = 0.5) +
    geom_vline(data = filter(x, isbreak ==
      "Yes"), aes(xintercept = time), linetype = "longdash", lwd = 0.7, color = zcol[1]) +
    scale_x_continuous(breaks = seq(2015, 2023, 2)) +
    scale_y_continuous(sec.axis = sec_axis(~ . *
      0.5, name = "Probability")) +
    theme(
      panel.spacing = unit(1, "lines"),
      legend.position = "none"
    ) +
    xlab("Time") +
    ylab("Value")
  p
}
# Fungsi: scan.evn
scan.evn <- function(x) {
  acled %>%
    filter(EVENT_TYPE == {{ x }}) %>%
    count(ADMINID, sort = TRUE) %>%
    rename(total = n) %>%
    top_n(5, wt = total) %>%
    arrange(desc(total))
}
# Fungsi: scan.fat
scan.fat <- function(x) {
  acled %>%
    filter(EVENT_TYPE == {{ x }}) %>%
    group_by(ADMINID) %>%
    summarize(total = sum(FATALITIES)) %>%
    filter(total > 0) %>%
    top_n(5, wt = total) %>%
    arrange(desc(total))
}
# Fungsi: scat.plot
scat.plot <- function(data, geom = "jitter", method = "density", axis_text = TRUE) {
  if (geom == "jitter") {
    p <- ggplot(data, aes(x = factor(ADMIN1_ABR), y = EVENT_DATE)) +
      geom_jitter(aes(color = ADMIN1_ABR, fill = ADMIN1_ABR),
        pch = 20,
        position = position_jitter(0.2), cex = 1.2
      )
  } else if (geom == "sina") {
    if (method == "density") {
      p <- ggplot(data, aes(x = factor(ADMIN1_ABR), y = EVENT_DATE)) +
        geom_sina(
          method = "density", aes(color = ADMIN1_ABR, fill = ADMIN1_ABR),
          size = 0.3, pch = 20
        ) +
        geom_violin(
          color = zcol[1], fill = "#ffffff00",
          linewidth = 0.3
        )
    } else if (method == "boxplot") {
      p <- ggplot(data, aes(x = factor(ADMIN1_ABR), y = EVENT_DATE)) +
        geom_sina(
          method = "density", aes(color = ADMIN1_ABR, fill = ADMIN1_ABR),
          size = 0.3, pch = 20, alpha = 0.7, color = "darkgrey"
        ) +
        geom_boxplot(
          width = 0.4, color = zcol[1], fill = "#fde72570",
          size = 0.3, outlier.shape = 20, outlier.size = 0.3, position = position_nudge(0.2)
        )
    }
  }
  p <- p + scale_color_viridis(
    option = "D", discrete = TRUE, begin = 0.2,
    end = 0.95
  ) + scale_fill_viridis(
    option = "D", discrete = TRUE, begin = 0.2,
    end = 0.95
  ) + scale_y_datetime(breaks = seq(
    from = min(data$EVENT_DATE),
    to = max(data$EVENT_DATE), by = "2 years"
  ), date_labels = "%Y") +
    theme(legend.position = "none", panel.border = element_blank())
  if (!axis_text) {
    p <- p + theme(axis.text.x = element_blank()) + theme(axis.title = element_blank())
  } else {
    p <- p + theme(axis.title.y = element_blank()) + labs(x = "Province (Abbreviation)")
  }
  return(p)
}
# Fungsi: scn.ano
scn.ano <- function(df) {
  df %>%
    time_decompose(val, method = "stl") %>%
    anomalize(remainder, method = "iqr") %>%
    time_recompose()
}
# Fungsi: spatial.moran.mc
spatial.moran.mc <- function(data, variable) {
  nb <- poly2nb(data, queen = T)
  lw <- nb2listw(nb, style = "W", zero.policy = T)
  moran_test <- moran.test(data[[variable]], listw = lw)
  mcmoran <- moran.mc(data[[variable]], lw, nsim = 999)
  result <- list(moran_test = moran_test, mcmoran = mcmoran)
  return(list(result = result, nb = nb, lw = lw))
}
