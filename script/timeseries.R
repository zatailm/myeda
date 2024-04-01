##--------------------------------------------------------------------------------------- CONFIG

ren <- c("EBAT" = "Battles",
         "EERV" = "Explosions/Remote Violence",
         "EPRT" = "Protests",
         "ERTS" = "Riots",
         "ESTR" = "Strategic Developments",
         "EVAC" = "Violence Against Civilians")

ts_data <- tsm

##------------------------------------------------------------------------------ TIMESERIES PLOT
tday <- daily.ts[,1]
twek <- tsx
pts <- function(x, y, z, v) {
  ggplot(data = x, aes(x = index(x), y = y)) +
    geom_line(lwd = .3, color = zcol[1]) +
    scale_x_continuous(breaks = seq(2015, 2023, 2)) +
    labs(title = NULL, subtitle = NULL, caption = z,  x = NULL, y = v)
}

pday <- pts(tday, tday, 'Time step: Daily, Method of aggregation: Sums', 'Event (Sums)')
pwek <- pts(twek, twek, 'Time step: Weekly, Method of aggregation: Means', 'Event (Log1p)')

ptdisp <- pday + space + pwek + layw2

##------------------------------------------------------------------------- SPECTRUM, ACF & PACF

spec_analysis <- spec.pgram(tsx, spans = 10, kernel = "daniell", taper = 0.1, pad = 0, 
                            fast = TRUE, demean = FALSE, detrend = FALSE, plot = FALSE)
spec_data <- data.frame(frequency = spec_analysis$freq, spectrum = spec_analysis$spec)
p_spec <- ggplot(spec_data, aes(x = frequency, y = spectrum)) +
  geom_line(lwd = .3, color = zcol[2]) +
  scale_y_continuous(trans = "log", breaks = trans_breaks("log", function(x) exp(x)),
                     labels = trans_format("log", math_format(10^.x))) +
  scale_x_continuous(breaks = seq(0, 25, 5)) +
  geom_vline(aes(xintercept = 1/7), color = zcol[1], linetype = 'longdash') +
  labs(x = "Frequency", y = "Amplitude", title = NULL) 

extract_acf_data <- function(x) {
  data <- as.data.frame.table(x$acf)[-1]
  data$lag <- as.numeric(x$lag)
  return(data)
}

acf_est <- acf(tsx, plot = FALSE)
acf_data <- extract_acf_data(acf_est)
pacf_est <- pacf(tsx, plot = FALSE)
pacf_data <- extract_acf_data(pacf_est)

p.autocor <- function(est, data, title) {
  ci <- qnorm((1 + .95) / 2) / sqrt(est$n.used)
  p <- ggplot(data = data, aes(x = lag, xend = lag, y = 0, yend = Freq)) +
    geom_segment(color = zcol[1]) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = c(-ci, ci), linetype = "dashed", color = zcol[2]) +
    labs(y = title)
  return(p)
}

p_acf <- p.autocor(acf_est, acf_data, 'ACF') + labs(x = NULL)
p_pacf <- p.autocor(pacf_est, pacf_data, 'PACF') + labs(x = 'Lag')

psap <- p_spec + space + (p_acf / space / p_pacf + layh2) + layw2

##----------------------------------------------------------------------------- BOX, HIST & DENS 

phd <- ggplot(tsx, aes(tsx)) + labs(x = 'Event')
dtsx <- data.frame(year = as.integer(time(tsx)), val = as.vector(tsx))
pbx <- ggplot(dtsx, aes(x = factor(year), y = val)) + geom_boxplot(color = zcol[1]) +
  labs(x = 'Year', y = 'Event')

p.hist <- function(df, xtit) {
  center.dis <- density(df$value)$x[which.max(density(df$value)$y)]
  ggplot(df, aes(x = value)) +
    geom_density(color = 'black', fill = zcol[3], alpha = .5) +
    geom_vline(xintercept = center.dis, color = zcol[1], linetype = "longdash") +
    labs(x = xtit, y = 'Density') -> p
  list(c = center.dis, p = p)}

df.hist <- lapply(tsw, function(x) {data.frame(value = x)})
df.hist <- df.hist[1:8]

xtit <- c('Event', 'Fatalities', 'Battles', 'Explosions/Remote Violences', 'Protests',
          'Riots', 'Strategic Developments', 'Violence Against Civilians')

plotsh <- lapply(seq_along(df.hist), function(i) p.hist(df.hist[[i]], xtit[i]))

pbhd <- (pbx + space + 
           ((phd + geom_histogram(color = 'white', fill = zcol[2]) + ylab('Histogram')) / 
              space / (plotsh[[1]]$p) + plot_layout(height = c(5, .5, 5)))) + 
  plot_layout(width = c(5, .25, 3.5))

##------------------------------------------------------------------------------------ DECOMPOSE

dodec <- list(
  decompose = function(x) decompose(x, type = 'multiplicative'),
  stl = function(x) stl(x, s.window = 'periodic'),
  mstl = function(x) mstl(x)
)

dec <- lapply(dodec, function(f) f(tsx))

dec_dec <- dec$decompose
dec_stl <- dec$stl$time.series
dec_mstl <- dec$mstl

dataw <- data.frame(time = time(tsx), event = tsx)
dataw[['pwise']] <- predict(segmented(lm(event ~ time, data = dataw), seg.Z = ~ time))
dataw[['loess']] <- predict(loess(event ~ time, data = dataw, span = 0.75))
dataw[['deco']]  <- dec$decompose$trend
dataw[['stl']]   <- dec$stl$time.series[, 'trend']
dataw[['mstl']]  <- dec$mstl[, 'Trend']

ptrend <- function(value, cap, point = TRUE) {
  if (point) {
    p <- ggplot(data = dataw, aes(x = time, y = event)) +
      geom_point(color = zcol[6], alpha = .5) + 
      geom_line(aes(y = value), color = zcol[1], lwd = .7) +
      scale_x_continuous(breaks = seq(2015, 2023, 2)) +
      labs(x = NULL, y = NULL, caption = cap)
  } else {
    p <- ggplot(data = dataw, aes(x = time, y = event)) +
      geom_line(aes(y = value), color = zcol[1], lwd = .7) +
      scale_x_continuous(breaks = seq(2015, 2023, 2)) +
      labs(x = NULL, y = NULL, caption = cap)
  }
  return(p)
}

pdex <- ptrend(dataw[, 'event'], 'Observed')
pmva <- ptrend(dataw[, 'deco'], 'Moving Average')
pstl <- ptrend(dataw[, 'stl'], 'STL (LOESS)')
ploe <- ptrend(dataw[, 'loess'], 'LOESS (0.75)')
ppws <- ptrend(dataw[, 'pwise'], 'Piecewise Linear')

p1 <- pdex + space + pmva + plot_layout(width = c(4.4, .2, 2))
p2 <- pstl + space + ploe + space + ppws + plot_layout(width = c(2, .2, 2, .2, 2))
pmtren <- p1 / space / p2 + layh2 
pseasn <- ptrend(dec$mstl[, 'Seasonal52'], 'Seasonal', point = FALSE)
prmain <- ptrend(dec$stl$time.series[, 'remainder'], 'Remainder', point = FALSE)

#

tsb_df <- data.frame(Year = time(tsb), coredata(tsb[, 3:8]))
smth.df <- tsb_df %>% pivot_longer(cols = -Year, names_to = "Var", values_to = "Value")

ptrent <- ggplot(smth.df, aes(x = Year, y = Value, color = Var)) +
  geom_point(color = zcol[6]) +
  geom_smooth(method = 'loess', formula = y ~ x, span = .25, color = 'black', 
              fill = zcol[6], alpha =  0.2, lwd = .7, se = F) +
  geom_smooth(method = 'lm', formula = y ~ x, span = .25, color = zcol[1], 
              fill = zcol[1], alpha =  0.2, lwd = .7, linetype = "dashed", se = F) +
  scale_x_continuous(breaks = seq(2015, 2023, 2)) +
  # labs(title = NULL, x = NULL, y = NULL) +
  facet_wrap(~ Var, scales = "free_y", labeller = labeller(Var = ren, x = 'Year',
                                                           y = 'Value')) + 
  theme(panel.spacing = unit(1, "lines"), legend.position = 'none')

psea <- ggseasonplot(tsm, year.labels = F, polar = T) + 
  labs(color = NULL, x = NULL, y = NULL, title = NULL) + scalecolzt() +
  theme(legend.position = 'right', legend.margin = margin(t = 0, r = 0, b = 0, l = 0))
ptm <- ggmonthplot(tsm) + labs(x = NULL, y = NULL)
ptlag <- gglagchull(tsm) + labs(x = NULL, y = NULL)

dfy <- data.frame(Year = rep(2015:2023, each = ceiling(467/9), length.out = 467), Value = tsx)
dfm <- data.frame(Month = factor(rep(month.abb, length.out = length(tsm)), levels = month.abb),
                  Value = c(tsm))
dfw <- data.frame(Week = rep(1:51, each = ceiling(467/51), length.out = 467), Value = tsx)

pboxsea <- function(data, time) {
  if (identical(data, dfw)) {
    p <- ggplot(data = data, aes(x = factor({{time}}), y = Value)) +
      geom_boxplot(lwd = 0.3, color = my_cols[1]) +
      labs(title = NULL, x = colnames(data)[1], y = colnames(data)[2]) +
      scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0, x, ""))
  } else if (identical(data, dfy)) {
    p <- ggplot(data = data, aes(x = factor({{time}}), y = Value)) +
      geom_boxplot(lwd = 0.3, color = my_cols[1]) +
      labs(title = NULL, x = colnames(data)[1], y = colnames(data)[2])
  } else {
    p <- ggplot(data = data, aes(x = {{time}}, y = Value)) +
      geom_boxplot(lwd = 0.3, color = zcol[1]) +
      labs(title = NULL, x = colnames(data)[1], y = NULL)
  }
  return(p)
}

pbseay <- pboxsea(dfy, dfy[,1])
pbseam <- pboxsea(dfm, dfm[,1])
pbseaw <- pboxsea(dfw, dfw[,1])
pbs <- ptm / pbseam
pseason <- (psea + plot_layout(guides = 'keep') & theme(legend.position = 'right')) | pbs + layw2

##---------------------------------------------------------------------------------- BREAKPOINTS

cb2d <- function(cp, sd = as.Date("2015-01-03")) {
  y <- as.integer(cp)
  d <- as.integer((cp - y) * 365)
  sd + years(y - 2015) + days(d)
}

gl <- function(db) {
  sapply(db, function(d) {
    d <- as.Date(d)
    y <- year(d)
    m <- month(d)
    w <- week(d) - week(floor_date(d, "month")) + 1
    paste("Y:", y, ", M:", m, ", W:", w)
  })
}

rb <- function(d, t) {
  set.seed(123)
  beast_res <- beast(d, quite = TRUE, print.progress = FALSE, print.options = FALSE)
  beast_df <- data.frame(
    time = beast_res$time,
    data = d,
    trend = beast_res$trend$Y,
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

tlist <- list()
for (i in 3:8) {tlist[[i]] <- rb(tsb[,i], colnames(tsb)[i])$data}
tbeast <- bind_rows(tlist)

mlist <- list()
for (i in 1:2) {mlist[[i]] <- rb(tsw[,i], colnames(tsw)[i])$data}
mbeast <- bind_rows(mlist)

scan.break <- function(x) {
  p <- ggplot(data = x, aes(x = time)) +
    geom_point(aes(y = data), color = zcol[6], alpha = .5) +
    geom_line(aes(y = trend), lwd= .7) +
    geom_area(aes(y = cpoc * 1.5), fill = zcol[2], alpha = .5) +
    geom_vline(data = filter(x, isbreak == 'Yes'), aes(xintercept = time),
               linetype = 'longdash', lwd = .7, color = zcol[1]) +
    scale_x_continuous(breaks = seq(2015, 2023, 2)) +
    scale_y_continuous(sec.axis = sec_axis(~ . * .5, name = 'Probability')) +
    theme(panel.spacing = unit(1,'lines'), legend.position = 'none') +
    xlab('Time') + ylab('Value')
  p
}

pmbhere <- scan.break(mbeast) + 
  facet_wrap(~ type, scales = 'free_y', labeller = labeller(type = c(
    'EVENT' = 'Events', 'FATAL' = 'Fatalities')))
ptbhere <- scan.break(tbeast) + 
  facet_wrap(~ type, scales = 'free_y', labeller = labeller(type = ren))

#

breakm_df <- mbeast %>% filter(type == 'EVENT')
datel <- mbeast %>% filter(type == 'EVENT') %>% filter(isbreak == 'Yes')

sdat <- as.Date("2015-01-03")
year <- as.integer(datel$time)
day <- as.integer((datel$time - year) * 365)
bdate <- sdat + lubridate::years(year - 2015) + lubridate::days(day)

hasil <- c()
for (i in bdate) {
  i <- as.Date(i)
  y <- lubridate::year(i)
  m <- lubridate::month(i)
  w <- lubridate::week(i) - lubridate::week(floor_date(i, "month")) + 1
  hasil <- c(hasil, paste(y, '-', m, '-', "W", w))
  
}
hasil <- gsub(" ", "", hasil)
datel$week_str = hasil

pstrbrk <- ggplot(breakm_df, aes(x = time)) +
  geom_point(aes(y = data), color = zcol[6], alpha = .5) +
  geom_line(aes(y = trend), color = 'black', lwd = .7) +
  geom_vline(data = datel, aes(xintercept = time), linetype = "longdash", 
             color = zcol[1], lwd = .7) +
  geom_area(aes(y = cpoc * 5.5), fill = zcol[2], alpha = .5) +
  scale_x_continuous(breaks = seq(2015, 2023, 2)) +
  scale_y_continuous(sec.axis = sec_axis(~ . * .5, name = 'Probability')) +
  ggplot2::annotate(geom = "label", x = datel$time[1], y =  max(breakm_df$data)-.5,
                    label = paste(hasil[1]), vjust = .5, hjust = .5,
                    color = zcol[1], size = 2.5, fill = 'white') +
  ggplot2::annotate(geom = "label", x = datel$time[2], y = max(breakm_df$data)-.5,
                    label = paste(hasil[2]), vjust = .5, hjust = .5,
                    color = zcol[1], size = 2.5, fill = 'white') +
  ggplot2::annotate(geom = "label", x = datel$time[3], y = max(breakm_df$data)-.5,
                    label = paste(hasil[3]), vjust = .5, hjust = .5,
                    color = zcol[1], size = 2.5, fill = 'white') +
  labs(title = NULL, x = 'Time', y = 'Value') 

mod <- 'ABBBC'
pmainstrbrk <- space + pstrbrk + space + plot_layout(design = mod)

##------------------------------------------------------------------------- ANOMALIES & OUTLIERS

wdf <- as_tibble(data.frame(Date = weekly.df$Date, Events = tsx))
ano_dec <- wdf %>%
  time_decompose(Events, method = "stl") %>%
  anomalize(remainder, method = "iqr") %>%
  time_recompose()

anomaly_points <- ano_dec %>% filter(anomaly == "Yes")
anodate <- unique(anomaly_points$Date)

est_ano <- c()
for (date in anodate) {
  date <- as.Date(date)
  year <- lubridate::year(date)
  month <- lubridate::month(date)
  week <- lubridate::week(date) - lubridate::week(floor_date(date, "month")) + 1
  est_ano <- c(est_ano, paste(year,'-', month, '-', "W", week))
}
est_ano <- gsub(" ", "", est_ano)

anly <- as_tibble(data.frame(Date = as.Date(anodate), here = est_ano))
anodf <- ano_dec %>% left_join(anly, by = "Date") %>% rename(date = Date)

pano <- ggplot(anodf, aes(x = date, y = observed)) +
  geom_line(color = zcol[6]) +
  geom_ribbon(aes(ymin = recomposed_l1, ymax = recomposed_l2), fill = zcol[6], alpha = 0.1) +
  geom_point(data = filter(anodf, anomaly == "Yes"), aes(color = anomaly), color = zcol[1]) +
  scale_x_date(breaks = seq(as.Date('2015-01-03'), as.Date('2023-12-31'), by = '2 years'), 
               date_labels = '%Y') +
  geom_text_repel(data = subset(anodf, anomaly == 'Yes'), aes(label = here), color = zcol[1], 
                  size = 2.7, box.padding = .7) +
  labs(caption = 'Anomalies')

#

smthdf <- data.frame(lapply(weekly.df[, -1], log1p))
smthdf$Date <- weekly.df$Date
smthdf <- pivot_longer(smthdf, cols = -Date, names_to = "var", values_to = "val")

df1 <- filter(smthdf, var == 'EBAT')
df2 <- filter(smthdf, var == 'EERV')
df3 <- filter(smthdf, var == 'EPRT')
df4 <- filter(smthdf, var == 'ERTS')
df5 <- filter(smthdf, var == 'ESTR')
df6 <- filter(smthdf, var == 'EVAC')

scn.ano <- function(df) {
  df %>%
    time_decompose(val, method = 'stl') %>%
    anomalize(remainder, method = 'iqr') %>%
    time_recompose()
}

anotypdf <- bind_rows(
  scn.ano(df1), scn.ano(df2), scn.ano(df3), scn.ano(df4), scn.ano(df5), scn.ano(df6)
)

anotypdf$type <- rep(c("EBAT", "EERV", "EPRT", "ERTS", "ESTR", "EVAC"), each = nrow(df1))
anomaly_data <- anotypdf %>% filter(anomaly == "Yes")

panot <- anotypdf %>%
  ggplot(aes(x = Date, y = observed)) +
  geom_line(color = zcol[6]) +
  facet_wrap(~type, scales = "free_y", labeller = labeller(type = ren)) +
  geom_point(data = anomaly_data, aes(x = Date, y = observed), color = zcol[1]) +
  geom_ribbon(aes(ymin = recomposed_l1, ymax = recomposed_l2), fill = zcol[6], alpha = 0.1) +
  scale_x_date(breaks = seq(as.Date('2015-01-03'), as.Date('2023-12-31'), by = '2 years'), 
               date_labels = '%Y') +
  labs(title = NULL, x = "Date", y = "Value") +
  theme(panel.spacing = unit(1, "lines"), legend.position = 'none')

#

outliers <- tso(tsx)
odf <- data.frame(
  time = time(outliers$y),
  date = time(outliers$y),
  obs = outliers$y,
  obadj = outliers$yadj,
  effect = outliers$effect,
  outliers = ifelse(time(outliers$y) %in% outliers$times, TRUE, FALSE)
)

otl <- data.frame(time = outliers$outliers$time, type = outliers$outliers$type)
otl$time <- as.character(otl$time)
oy <- as.integer(sub(":.*", "", otl$time))
ow <- as.integer(sub(".*:", "", otl$time))
otl$time <- oy + (ow - 1) / 52
odf$time <- as.character(odf$time)
otl$time <- as.character(otl$time)
odf <- odf %>% left_join(otl, by = "time")

pout <- ggplot(odf, aes(x = date, y = obs)) +
  geom_line(color = zcol[11]) +
  geom_line(aes(y = obadj), color = zcol[6]) +
  geom_point(data = subset(odf, outliers == TRUE), color = zcol[1]) +
  geom_text_repel(data = subset(odf, outliers == TRUE), aes(label = type), color = zcol[1], 
                  size = 2.7, box.padding = .6) +
  scale_x_continuous(breaks = seq(2015, 2023, 2)) +
  labs(caption = 'Outliers')

panout <- pano + space + pout + layw2 & xlab(NULL) & ylab(NULL)

##---------------------------------------------------------------------------------- FORECASTING

## Prep data

ts_outliers <- tsoutliers(tsx)
tsxc <- tsclean(tsx)
tsmc <- tsclean(tsm)
tsdc <- tsclean(daily.ts[,1])

# Inisiasi parameter terbaik

data <- tsxc
best_values <- c(p_ar = 0, q_ma = 0, p_arma = 0, q_arma = 0, p_arima = 0, d_arima = 0)
best_aics <- c(aic_ar = Inf, aic_ma = Inf, aic_arma = Inf, aic_arima = Inf)

for (p in 1:3) {
  model_ar <- arima(data, order = c(p, 0, 0))
  aic_ar <- AIC(model_ar)
  if (aic_ar < best_aics["aic_ar"]) {
    best_aics["aic_ar"] <- aic_ar
    best_values["p_ar"] <- p
  }
  
  for (q in 1:3) {
    model_ma <- arima(data, order = c(0, 0, q))
    aic_ma <- AIC(model_ma)
    if (aic_ma < best_aics["aic_ma"]) {
      best_aics["aic_ma"] <- aic_ma
      best_values["q_ma"] <- q
    }
    
    model_arma <- arima(data, order = c(p, 0, q))
    aic_arma <- AIC(model_arma)
    if (aic_arma < best_aics["aic_arma"]) {
      best_aics["aic_arma"] <- aic_arma
      best_values["p_arma"] <- p
      best_values["q_arma"] <- q
    }
    
    for (d in 0:1) {
      model_arima <- arima(data, order = c(p, d, q))
      aic_arima <- AIC(model_arima)
      if (aic_arima < best_aics["aic_arima"]) {
        best_aics["aic_arima"] <- aic_arima
        best_values["p_arima"] <- p
        best_values["d_arima"] <- d
        best_values["q_arima"] <- q
      }
    }
  }
}

result_matrix <- matrix(
  c(paste("p =", best_values["p_ar"]),best_aics["aic_ar"], 
    paste("q =", best_values["q_ma"]),best_aics["aic_ma"], 
    paste("p =", best_values["p_arma"],", q =", best_values["q_arma"]), best_aics["aic_arma"],
    paste("p =", best_values["p_arima"], ", d =",best_values["d_arima"], ", q =", best_values["q_arima"]),
    best_aics["aic_arima"]), 
  ncol = 2, byrow = TRUE)

colnames(result_matrix) <- c("best value", "AIC")
rownames(result_matrix) <- c("AR", "MA", "ARMA", "ARIMA")

result_matrix

## Evaluasi model

mse <- function(actual, predicted) { mean((actual - predicted)^2) }
rmse <- function(actual, predicted) { sqrt(mse(actual, predicted)) }
mape <- function(actual, predicted) { mean(abs((actual - predicted) / actual)) * 100 }
accuracy <- function(actual, predicted) { 
  mean(pmin(abs(actual - predicted), abs(actual - predicted + 1)) / abs(actual)) }

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

data <- tsxc
dl <- length(data)
tl <- round(0.8 * dl)
td <- data[1:tl] # train
tsd <- data[(tl + 1):dl] # test

ar <- arima(td, order = c(3, 0, 0))
ma <- arima(td, order = c(0, 0, 3))
arma <- arima(td, order = c(3, 0, 2), method = 'CSS')
ari <- arima(td, order = c(1, 1, 2))
auto_ari <- auto.arima(td)

f <- lapply(list(ar, ma, arma, ari, auto_ari), function(model) {
  forecast(model, h = length(tsd))$mean
})

ev <- data.frame(
  Model = c("AR", "MA", "ARMA", "ARIMA", "Auto ARIMA"), 
  MSE = sapply(f, function(forecast) mse(tsd, forecast)), 
  RMSE = sapply(f, function(forecast) rmse(tsd, forecast)), 
  'MAPE (%)' = sapply(f, function(forecast) mape(tsd, forecast)), 
  Accuracy = sapply(f, function(forecast) accuracy(tsd, forecast)),
  'CV MSE' = c(cv(ar, td), cv(ma, td), cv(arma, td), cv(ari, td), cv(auto_ari, td))
)
colnames(ev) <- gsub("\\.", " ", colnames(ev))
colnames(ev)[colnames(ev) == "MAPE    "] <- paste("MAPE (%)", sep = "")
ev[] <- lapply(ev, function(x) if(is.numeric(x)) round(x, 3) else x)

# print(ev)

# Analisis residu
residuals_autoarima <- residuals(auto_ari)
autoarimares <- plot(residuals_autoarima)


## Forecasting

data <- tsxc

model_ar <- arima(data, order = c(3, 0, 0))
model_ma <- arima(data, order = c(0, 0, 3))
model_arma <- arima(data, order = c(3, 0, 2), method = 'CSS')
model_arima <- arima(data, order = c(1, 1, 2))
model_autoarima <- auto.arima(data)

forecast_ar <- forecast(model_ar, h = 105)
forecast_ma <- forecast(model_ma, h = 105)
forecast_arma <- forecast(model_arma, h = 105)
forecast_arima <- forecast(model_arima, h = 105)
forecast_autoarima <- forecast(model_autoarima, h = 105)

## Visualisai forecasting

plot.for <- function(model, sub) {
  p <- ggplot() +
    geom_ribbon(aes(x = index(model$mean), ymin = model$lower[, '95%'],
                    ymax = model$upper[, '95%']), fill = zcol[6], alpha = .5) +
    geom_ribbon(aes(x = index(model$mean), ymin = model$lower[, '80%'],
                    ymax = model$upper[, '80%']), fill = zcol[6], alpha = .5) +
    geom_line(aes(x = index(model$mean), y = model$mean), color = zcol[1], lwd = .7) +
    geom_line(data = data, aes(x = index(data), y = data), color = zcol[2], lwd = .3) +
    scale_x_continuous(breaks = seq(2015, 2026, 2)) +
    scale_y_continuous(breaks = seq(1, 3, 1)) +
    labs(title = sub, x = NULL, y = NULL)
  
  p
}

par     <- plot.for(forecast_ar, 'AR (3,0,0)')
pam     <- plot.for(forecast_ma, 'MA (0,0,3)')
parma   <- plot.for(forecast_arma, "ARMA (3,0,2) method CSS")
parima  <- plot.for(forecast_arima, 'ARIMA (1,1,2)')
paarima <- plot.for(forecast_autoarima, 'Auto ARIMA (3,1,0)(1,1,0)[52] with Drift')

ptr <- (parima + space + parma + layw2) / space / (par + space + pam + layw2) + layh2
ptrall <- paarima / space / ptr + layh2
