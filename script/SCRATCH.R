model_string <- "
model {
  # Prior distributions
  mu ~ dnorm(0, 0.001)    # Prior for the mean
  tau ~ dgamma(0.001, 0.001)  # Prior for the precision (inverse of variance)
  
  # Likelihood
  for (i in 1:N) {
    konflik[i] ~ dnorm(mu, tau)  # Likelihood function
  }
}
"

library(rjags)

# Definisi data
data_list <- list(N = length(konflik_per_minggu), konflik = konflik_per_minggu)

# Buat model
model <- jags.model(textConnection(model_string), data = data_list, n.chains = 3)

# Jalankan sampler
samples <- coda.samples(model, variable.names = c("mu"), n.iter = 5000)

summary(samples)

library(ggplot2)

# Hitung rata-rata posterior dari sampel
posterior_mean <- mean(samples[[1]])

# Buat garis tren berdasarkan rata-rata posterior
trend_line <- posterior_mean * seq_along(konflik_per_minggu)

# Plot hasil
ggplot(data = NULL, aes(x = seq_along(konflik_per_minggu), y = konflik_per_minggu)) +
  geom_line(color = "blue") +
  geom_line(aes(y = trend_line), color = "red", linetype = "dashed") +
  labs(title = "Time Trend of Conflict Count",
       x = "Week",
       y = "Conflict Count") +
  theme_minimal()


# Load necessary libraries
library(ggplot2)
library(lubridate)

# Sample data
# Assuming you have a weekly time series object named 'ts_data'
ts_data <- tsx
# Sample data
# Assuming you have a weekly time series object named 'ts_data'

# Convert weekly data to monthly data
ts_data_monthly <- ts(apply(matrix(ts_data, nrow = 4), 2, mean), frequency = 12)




ts_data_monthly <- df_monthly[,1]
# Decompose the time series
decomposed <- decompose(ts_data_monthly)

# Trim the decomposed components to match the length of the original time series
n <- length(ts_data_monthly)
decomposed_trend <- decomposed$trend[1:n]
decomposed_seasonal <- decomposed$seasonal[1:n]
decomposed_random <- decomposed$random[1:n]

# Create a data frame with the decomposed components
decomposed_df <- data.frame(
  Date = time(ts_data_monthly),
  Original = ts_data_monthly,
  Trend = decomposed_trend,
  Seasonal = decomposed_seasonal,
  Random = decomposed_random
)

# Convert Date column to a proper date format
decomposed_df$Date <- as.Date(decomposed_df$Date)
# decomposed_df$Date <- format(as.Date(decomposed_df$Date), "%y-%m-%d")

# Plot
p.seamon <- ggplot(decomposed_df, aes(x = Date)) +
  geom_line(aes(y = Original, color = "Original"), size = 1) +
  geom_line(aes(y = Trend, color = "Trend"), linetype = "dashed") +
  geom_line(aes(y = Seasonal, color = "Seasonal"), linetype = "dotted") +
  labs(
    title = "Seasonal Trend Plot",
    y = "Value",
    color = "Component"
  ) +
  scale_color_manual(
    values = c("Original" = "black", "Trend" = "red", "Seasonal" = "blue"),
    labels = c("Original", "Trend", "Seasonal")
  ) +
  theme_minimal()


ts_data_monthly <- df_monthly[, 1]
decomposed <- decompose(ts_data_monthly)

n <- length(ts_data_monthly)
decomposed_trend <- decomposed$trend[1:n]
decomposed_seasonal <- decomposed$seasonal[1:n]
decomposed_random <- decomposed$random[1:n]

decomposed_df <- data.frame(
  Date = time(ts_data_monthly), 
  Original = ts_data_monthly,
  Trend = decomposed_trend, 
  Seasonal = decomposed_seasonal, 
  Random = decomposed_random)

decomposed_df$Date <- as.Date(decomposed_df$Date)

p.seamon <- ggplot(decomposed_df, aes(x = Date)) + 
  geom_line(aes(y = Original,color = "Original"), size = 1) + 
  geom_line(aes(y = Trend, color = "Trend"),linetype = "dashed") + 
  geom_line(aes(y = Seasonal, color = "Seasonal"),linetype = "dotted") + 
  labs(title = "Seasonal Trend Plot", y = "Value",color = "Component") + 
  scale_color_manual(values = c(Original = "black",Trend = "red", Seasonal = "blue"), 
                     labels = c("Original", "Trend", "Seasonal")) + theme_minimal()





lag.max <- round(min(max(10 * log10(length(tsx)), 3 * frequency(tsx)), length(tsx) / 3))

acf_est <- acf(tsx, lag.max = lag.max, plot = FALSE)
aacf <- `dimnames<-`(acf_est$acf, list(NULL, acf_est$snames, acf_est$snames))
alag <- `dimnames<-`(acf_est$lag, list(NULL, acf_est$snames, acf_est$snames))
adata <- as.data.frame.table(aacf)[-1]
adata$lag <- as.numeric(alag)
if (acf_est$type == "correlation" & is.null(acf_est$ccf)) {
  adata <- adata[adata$lag != 0, ]
}

pacf_est <- pacf(tsx, lag.max = lag.max, plot = FALSE)
pacf <- `dimnames<-`(pacf_est$acf, list(NULL, pacf_est$snames, pacf_est$snames))
plag <- `dimnames<-`(pacf_est$lag, list(NULL, pacf_est$snames, pacf_est$snames))
pdata <- as.data.frame.table(pacf)[-1]
pdata$lag <- as.numeric(plag)
if (pacf_est$type == "correlation" & is.null(pacf_est$ccf)) {
  pdata <- pdata[pdata$lag != 0, ]
}

p.autocor <- function(est, data, title) {
  ci <- qnorm((1 + .95) / 2) / sqrt(est$n.used)
  p <- ggplot(data = data, aes(x = lag, xend = lag, y = 0, yend = Freq)) +
    geom_segment(size = .2, color = "#00AFBB") +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = c(-ci, ci), linetype = "dashed", color = '#FC4E07') +
    labs(x = NULL, y = title, title = NULL)
  return(p)
}



extract_data <- function(acf_pacf_est, type) {
  data <- as.data.frame.table(
    `dimnames<-`(acf_pacf_est$acf, list(NULL, acf_pacf_est$snames, acf_pacf_est$snames)))[-1]
  data$lag <- as.numeric(
    `dimnames<-`(acf_pacf_est$lag, list(NULL, acf_pacf_est$snames, acf_pacf_est$snames)))
  if (acf_pacf_est$type == type & is.null(acf_pacf_est$ccf)) {
    data <- data[data$lag != 0, ]
  }
  data
}

lag.max <- round(min(max(10 * log10(length(tsx)), 3 * frequency(tsx)), length(tsx) / 3))

acf_est <- acf(tsx, lag.max = lag.max, plot = FALSE)
adata <- extract_data(acf_est, "correlation")

pacf_est <- pacf(tsx, lag.max = lag.max, plot = FALSE)
pdata <- extract_data(pacf_est, "correlation")

p.autocor <- function(est, data, title) {
  ci <- qnorm((1 + .95) / 2) / sqrt(est$n.used)
  p <- ggplot(data = data, aes(x = lag, xend = lag, y = 0, yend = Freq)) +
    geom_segment(size = .2, color = "#00AFBB") +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = c(-ci, ci), linetype = "dashed", color = '#FC4E07') +
    labs(x = NULL, y = title, title = NULL)
  p
}

p_acf <- p.autocor(acf_est, adata, 'ACF')
p_pacf <- p.autocor(pacf_est, pdata, 'PACF')





spec_analysis <- spec.pgram(tsx, plot = F)
max_freq <- spec_analysis$freq[which.max(spec_analysis$spec)] # frekuensi tertinggi

df_spectrum <- data.frame(frequency = spec_analysis$freq, spectrum = spec_analysis$spec)
pspec <- ggplot(df_spectrum, aes(x = frequency, y = spectrum)) +
  geom_line() +
  scale_y_continuous(trans = "log", breaks = trans_breaks("log", function(x) exp(x)),
                     labels = trans_format("log", math_format(10^.x))) +
  labs(x = "Frequency", y = "Spectrum", title = NULL) 




tsm <- df_monthly[, 1]
lambda <- BoxCox.lambda(tsm)
tsm <- BoxCox(tsm, lambda)
decodfm <- stl(tsm, s.window = "periodic", robust = TRUE)

# n <- length(tsm)
# decomposed_trend <- decomposed$trend[1:n]
# decomposed_seasonal <- decomposed$seasonal[1:n]
# decomposed_random <- decomposed$random[1:n]

deco_dfm <- data.frame(
  Date = time(tsm), 
  Observed = tsm,
  Trend = decodfm$time.series[, 2], 
  Seasonal = decodfm$time.series[, 1], 
  Random = decodfm$time.series[, 3])

# decomposed_df$Date <- as.Date(decomposed_df$Date)

p.seamon <- ggplot(deco_dfm, aes(x = Date)) + 
  geom_line(aes(y = Observed, color = "Observed"), lwd = .7) + 
  geom_line(aes(y = Trend, color = "Trend"),linetype = "solid", lwd = 1) + 
  geom_line(aes(y = Seasonal, color = "Seasonal"),linetype = "solid", lwd = .6) + 
  labs(title = NULL, x = 'Year', y = "Value", color = NULL) + 
  scale_color_manual(values = c(Observed = "#00AFBB",Trend = "#FC4E07", Seasonal = "#000000"), 
                     labels = c("Observed", "Trend", "Seasonal")) 
# + 
#   scale_x_date(breaks = seq(as.Date("2015-01-01"), as.Date("2023-01-01"), 
#                             by = "2 years"), date_labels = "%Y")


tbt <- tbats(tsx, use.box.cox = TRUE, use.trend = TRUE, use.damped.trend = TRUE)
comptbats <- tbats.components(tbt)

ptbats <- function(value, ylab) {
  p <- ggplot(data = comptbats, aes(x = time(comptbats), y = value)) +
    geom_line(color = '#00AFBB') + theme(axis.title.x = element_blank()) +
    scale_x_continuous(breaks = seq(2015, 2023, 2)) +
    labs(y = ylab)
  return(p)
}

ptbtobs <- ptbats(comptbats[,1], 'Observed')
ptbtlvl <- ptbats(comptbats[,2], 'Level')
ptbtslp <- ptbats(comptbats[,3], 'Slope')
ptbtsea <- ptbats(comptbats[,4], 'Season')




# Menghitung frekuensi data
frequency_table <- table(tsm)
print(frequency_table)



library(tseries)

# Menggunakan tes Dickey-Fuller
result <- adf.test(tsm, alternative = "stationary")
print(result)



# Menghitung Fourier Transform
fft_result <- fft(tsm)

# Menghitung frekuensi
freq <- seq(0, 1, length.out = length(tsm))

# Plot Fourier Transform
plot(freq, abs(fft_result), type = "l", main = "Fourier Transform", xlab = "Frequency", ylab = "Amplitude")

# Menghitung amplitudo
amplitude <- abs(fft_result)

# Plot Fourier Transform dengan amplitudo
plot(freq, amplitude, type = "l", main = "Fourier Transform", xlab = "Frequency", ylab = "Amplitude")


ggseasonplot(tsm)
ggseasonplot(tsm, year.labels=TRUE, continuous=TRUE)
psea <- ggseasonplot(tsm, col=rainbow(9), year.labels=TRUE)








"#00AFBB", "#E7B800", "#FC4E07"




d <- tbats(tsx)
co <- forecast(d)
plot(co$fitted)

names(co)



nextodd <- function(x){
  x <- round(x)
  if(x%%2==0) x <- x+1
  as.integer(x)
}


swin = 52
pedic = 52

nextodd(ceiling((1.5*pedic) / (1-(1.5/swin))))













plot.tsoutliers <- function(x, 
                            args.lines.y = list(col = "gray80"), args.lines.yadj = list(col = "blue"),
                            args.lines.effects = list(type = "s", col = "red"),   
                            args.points = list(col = "gray80", bg = "red", pch = 21), plot.points = TRUE, 
                            args.x.axis = list(at = pretty(time(x$y)), tcl = -0.5, lwd = 0, lwd.ticks = 1),
                            args.y.axis = list(at = pretty(x$y), tcl = -0.5, lwd = 0, lwd.ticks = 1),
                            args.effects.axis = list(at = pretty(x$effects), tcl = -0.5, lwd = 0, lwd.ticks = 1),
                            ...)
{

  fargs.linesy <- formals(plot.tsoutliers)$args.lines.y
  efargs.linesy <- eval(fargs.linesy)
  if (!identical(args.lines.y, efargs.linesy))
  {
    args.lines.y <- c(args.lines.y, efargs.linesy)
    id <- which(duplicated(names(args.lines.y)))
    if (length(id) > 0)
      args.lines.y <- args.lines.y[-id]
  }
  
  fargs.linesyadj <- formals(plot.tsoutliers)$args.lines.yadj
  efargs.linesyadj <- eval(fargs.linesyadj)
  if (!identical(args.lines.yadj, efargs.linesyadj))
  {
    args.lines.yadj <- c(args.lines.yadj, efargs.linesyadj)
    id <- which(duplicated(names(args.lines.yadj)))
    if (length(id) > 0)
      args.lines.yadj <- args.lines.yadj[-id]
  }
  
  fargs.linesef <- formals(plot.tsoutliers)$args.lines.effects
  efargs.linesef <- eval(fargs.linesef)
  if (!identical(args.lines.effects, efargs.linesef))
  {
    args.lines.effects <- c(args.lines.effects, efargs.linesef)
    id <- which(duplicated(names(args.lines.effects)))
    if (length(id) > 0)
      args.lines.effects <- args.lines.effects[-id]
  }
  
  fargs.points <- formals(plot.tsoutliers)$args.points
  efargs.points <- eval(fargs.points)
  if (!identical(args.points, efargs.points))
  {
    args.points <- c(args.points, efargs.points)
    id <- which(duplicated(names(args.points)))
    if (length(id) > 0)
      args.points <- args.points[-id]
  }
  
  fargs.xaxis <- formals(plot.tsoutliers)$args.x.axis
  efargs.xaxis <- eval(fargs.xaxis)
  if (!identical(args.x.axis, efargs.xaxis))
  {
    args.x.axis <- c(args.x.axis, efargs.xaxis)
    id <- which(duplicated(names(args.x.axis)))
    if (length(id) > 0)
      args.x.axis <- args.x.axis[-id]
  }
  if (is.null(args.x.axis$labels))
    args.x.axis$labels <- args.x.axis$at
  args.x.axis$side <- 1
  
  fargs.yaxis <- formals(plot.tsoutliers)$args.y.axis
  efargs.yaxis <- eval(fargs.yaxis)
  if (!identical(args.y.axis, efargs.yaxis))
  {
    args.y.axis <- c(args.y.axis, efargs.yaxis)
    id <- which(duplicated(names(args.y.axis)))
    if (length(id) > 0)
      args.y.axis <- args.y.axis[-id]
  }
  if (is.null(args.y.axis$labels))
    args.y.axis$labels <- args.y.axis$at
  args.y.axis$side <- 2
  
  fargs.eaxis <- formals(plot.tsoutliers)$args.effects.axis
  efargs.eaxis <- eval(fargs.eaxis)
  if (!identical(args.effects.axis, efargs.eaxis))
  {
    args.effects.axis <- c(args.effects.axis, efargs.eaxis)
    id <- which(duplicated(names(args.effects.axis)))
    if (length(id) > 0)
      args.effects.axis <- args.effects.axis[-id]
  }
  if (is.null(args.effects.axis$labels))
    args.effects.axis$labels <- args.effects.axis$at
  if (is.null(args.effects.axis$side))
    args.effects.axis$side <- 4
  
  if (nrow(x$outliers) == 0)
  {
    cat(paste(sQuote("x"), "does not contain outliers to display\n"))
    return()
  }
  
  oldpar <- par(mar = c(0, 3, 0, 2.1), oma =  c(3, 0, 3, 0), mfcol = c(2, 1), ...)
  on.exit(par(oldpar))
  
  plot(cbind(x$y, x$yadj), plot.type ="single", 
       type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  mtext(side = 3, text = "Original and adjusted series", adj = 0)
  
  do.call("lines", args = c(list(x = x$y), args.lines.y))
  do.call("lines", args = c(list(x = x$yadj), args.lines.yadj))
  do.call("axis", args = args.y.axis)
  
  if (plot.points)
  {
    do.call("points", args = c(list(x = x$times, y = x$y[x$outliers[,"ind"]]), 
                               args.points))
  }
  plot(x$effects, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "u")
  do.call("lines", args = c(list(x = x$effects), args.lines.effects))
  mtext(side = 3, text = "Outlier effects", adj = 0, line = -1)
  do.call("axis", args = args.effects.axis)
  do.call("axis", args = args.x.axis)
}


plot_tsoutliers_y <- function(x, plot_points = TRUE) {
  # Membuat data frame dari x
  df <- data.frame(time = time(x$y), y = x$y)
  
  # Membuat plot untuk y
  p_y <- ggplot(df, aes(x = time, y = y)) +
    geom_line(color = "gray80") +
    geom_line(aes(y = y + x$yadj), color = "blue")
  
  # Menambahkan points untuk outlier jika plot_points = TRUE
  if (plot.points) {
    outlier_df <- data.frame(time = time(x$times), value = x$y[x$outliers[,"ind"]])
    plot2 <- plot2 +
      geom_point(data = outlier_df, aes(x = time, y = value), **args.points)
  }
  
  return(p_y)
}

plot_tsoutliers_y(outliers)





library(ggplot2)

plot.tsoutliers <- function({{x}}, 
                            args.lines.y = list(col = "gray80"), args.lines.yadj = list(col = "blue"),
                            args.lines.effects = list(type = "s", col = "red"),   
                            args.points = list(col = "gray80", fill = "red", shape = 21), plot.points = TRUE, 
                            args.x.axis = list(at = pretty(time(x$y)), tcl = -0.5, lwd = 0, lwd.ticks = 1),
                            args.y.axis = list(at = pretty(x$y), tcl = -0.5, lwd = 0, lwd.ticks = 1),
                            args.effects.axis = list(at = pretty(x$effects), tcl = -0.5, lwd = 0, lwd.ticks = 1),
                            ...)
{
  # Plot Original and Adjusted Series
  original_df <- data.frame(time = time(x$y), value = x$y, type = "Original")
  adjusted_df <- data.frame(time = time(x$yadj), value = x$yadj, type = "Adjusted")
  data_df <- rbind(original_df, adjusted_df)
  
  plot1 <- ggplot(data_df, aes(x = time, y = value, color = type)) +
    geom_line() +
    scale_color_manual(values = c("Original" = args.lines.y$col, "Adjusted" = args.lines.yadj$col)) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_text(size = 12),
          legend.position = "none") +
    labs(y = "Value", title = "Original and Adjusted Series")
  
  # Plot Outlier Effects
  plot2 <- ggplot(data.frame(time = time(x$effects), value = x$effects), aes(x = time, y = value)) +
    geom_line(color = args.lines.effects$col, linetype = args.lines.effects$type) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10),
          axis.title.x = element_text(size = 12),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_text(size = 12)) +
    labs(y = "Effect", title = "Outlier Effects")
  
  # Combine plots
  if (plot.points) {
    outlier_df <- data.frame(time = time(x$times), value = x$y[x$outliers[,"ind"]])
    plot2 <- plot2 +
      geom_point(data = outlier_df, aes(x = time, y = value), **args.points)
  }
  
  plot_grid(plot1, plot2, ncol = 1, align = "v")
}

# Example usage:
plot.tsoutliers(x = tsx)

library(ggplot2)

plot_original_adjusted <- function(x, args.lines.y = list(col = "gray80"), args.lines.yadj = list(col = "blue"), ...) {
  original_df <- data.frame(time = time(x$y), value = x$y, type = "Original")
  adjusted_df <- data.frame(time = time(x$yadj), value = x$yadj, type = "Adjusted")
  data_df <- rbind(original_df, adjusted_df)
  
  ggplot(data_df, aes(x = time, y = value, color = type)) +
    geom_line() +
    scale_color_manual(values = c("Original" = args.lines.y$col, "Adjusted" = args.lines.yadj$col)) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_text(size = 12),
          legend.position = "none") +
    labs(y = "Value", title = "Original and Adjusted Series") +
    ...
}

plot_outlier_effects <- function(x, args.lines.effects = list(type = "s", col = "red"), args.points = list(col = "gray80", fill = "red", shape = 21), plot.points = TRUE, ...) {
  plot2 <- ggplot(data.frame(time = time(x$effects), value = x$effects), aes(x = time, y = value)) +
    geom_line(color = args.lines.effects$col, linetype = args.lines.effects$type) +
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10),
          axis.title.x = element_text(size = 12),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_text(size = 12)) +
    labs(y = "Effect", title = "Outlier Effects") +
    ...
  
  if (plot.points) {
    outlier_df <- data.frame(time = time(x$times), value = x$y[x$outliers[,"ind"]])
    plot2 <- plot2 +
      geom_point(data = outlier_df, aes(x = time, y = value), **args.points)
  }
  
  plot2
}

# Contoh penggunaan:
# plot_original_adjusted(tsx)
# plot_outlier_effects(tsx)


# Data berpotensi memiliki pola musiman kompleks, atau lemah, atau bahkan tidak memiliki pola musiman, data sepertinya memiliki pola cyclical, dan tentunya tren. Oleh karena itu, model yang sepertinya tepat untuk membuat forecasting adalah model sederhana seperti Autoaggresion (AR) atau Moving Average (MA), atau Autoaggresion Moving Average (ARMA), gabungan keduanya. Model ini dapat dijalankan dengan mengatur parameter AR dan MA secara manual atau dengan otomatis. Pengaturan parameter secara manual membutuhkan serangkaian pengujian yang banyak sampai mendapatkan hasil terbaik dengan nilai-nilai parameter yang seimbang. Oleh karena itu, penentuan parameter dilakukan dengan otomatis dengan pendekatan Automatic Autoaggresion Moving Average (Auto ARIMA). Untuk memastikan hasil forcasting dapat diandalkan dan akurat, data terlebih dahulu dibersihkan dari outlier dengan melakukan interpolasi nilai pada outlier yang ditemukan.

ts_outliers <- tsoutliers(tsx)
tsxc <- tsclean(tsx)
tsmc <- tsclean(tsm)
tsdc <- tsclean(ts_daily[,1])

data <- tsxc
fit_ARIMA <- function(data, p, d, q) {
  model <- arima(data, order = c(p, d, q))
  return(AIC(model))
}
best_AR <- NULL
best_AIC_AR <- Inf
for (p in 0:3) {
  AIC_value <- fit_ARIMA(data, p, 0, 0)
  if (AIC_value < best_AIC_AR) {
    best_AR <- p
    best_AIC_AR <- AIC_value
  }
}
fit_MA <- function(data, p, d, q) {
  model <- arima(data, order = c(p, d, q))
  return(AIC(model))
}
best_MA <- NULL
best_AIC_MA <- Inf
for (q in 0:3) {
  AIC_value <- fit_ARIMA(data, 0, 0, q)
  if (AIC_value < best_AIC_MA) {
    best_MA <- q
    best_AIC_MA <- AIC_value
  }
}
print(paste("Best AR value:", best_AR))
print(paste("Best MA value:", best_MA))
print(paste("Best AIC MA value:", best_AIC_AR))
print(paste("Best AIC MA value:", best_AIC_MA))


ar <- arima(data, order = c(3, 0, 0))
forecast(ar, h = 104) %>% autoplot()

ma <- arima(data, order = c(0, 0, 3))
forecast(ma, h = 104) %>% autoplot()

arma <- arima(data, order = c(3, 0, 3))
forecast(arma, h = 104) %>% autoplot()


# Menentukan nilai parameter model ARIMA terbaik dan nilai AIC
fit_ARIMA <- function(data, p, d, q) {
  model <- arima(data, order = c(p, d, q))
  return(AIC(model))
}
best_p <- NULL
best_d <- NULL
best_q <- NULL
best_AIC <- Inf
for (p in 0:3) {
  for (d in 1:1) {
    for (q in 0:3) {
      if (p == 0 & q == 0)
        next
      AIC_value <- fit_ARIMA(data, p, d, q)
      if (AIC_value < best_AIC) {
        best_p <- p
        best_d <- d
        best_q <- q
        best_AIC <- AIC_value
      }
    }
  }
}
print("Best ARIMA model:")
print(paste("Best p value:", best_p))
print(paste("Best d value:", best_d))
print(paste("Best q value:", best_q))
print(paste("Best AIC value:", best_AIC))


arima <- arima(data, order = c(1, 1, 2))
forecast(arima, h = 104) %>% autoplot()

aarima <- auto.arima(data) 
f.aarima <- forecast(aarima, h = 104)
plot(f.aarima)

print(paste(aarima$arma))
print(paste('AIC:', aarima$aic))


# Definisi model ARIMA
model_ar <- arima(data, order = c(3, 0, 0))
model_ma <- arima(data, order = c(0, 0, 3))
model_arima <- arima(data, order = c(1, 1, 2))
model_autoarima <- auto.arima(data)

# Fungsi untuk menghitung MSE
calculate_mse <- function(actual, predicted) {
  mean((actual - predicted)^2)
}

# Menguji akurasi model dengan data yang sama
# Anda harus menggunakan data yang berbeda untuk evaluasi yang sebenarnya
mse_ar <- calculate_mse(data, fitted(model_ar))
mse_ma <- calculate_mse(data, fitted(model_ma))
mse_arima <- calculate_mse(data, fitted(model_arima))
mse_autoarima <- calculate_mse(data, fitted(model_autoarima))

# Menampilkan hasil
print("Mean Squared Error (MSE) untuk setiap model:")
print(paste("AR model:", mse_ar))
print(paste("MA model:", mse_ma))
print(paste("ARIMA model:", mse_arima))
print(paste("Auto ARIMA model:", mse_autoarima))

######


data_length <- length(data)

train_length <- round(0.8 * data_length)

train_data <- data[1:train_length]
test_data <- data[(train_length + 1):data_length]

model_ar <- arima(train_data, order = c(3, 0, 0))
model_ma <- arima(train_data, order = c(0, 0, 3))
model_arima <- arima(train_data, order = c(1, 1, 2))
model_autoarima <- auto.arima(train_data)

calculate_mse <- function(actual, predicted) {
  mean((actual - predicted)^2)
}

calculate_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

calculate_mape <- function(actual, predicted) {
  mean(abs((actual - predicted)/actual)) * 100
}

calculate_accuracy <- function(actual, predicted) {
  mean(min(abs(actual - predicted), abs(actual - predicted + 1)))/mean(abs(actual))
}

calculate_aic <- function(model) {
  AIC(model)
}

calculate_bic <- function(model) {
  BIC(model)
}

perform_cross_validation <- function(model, data, k = 5) {
  n <- length(data)
  fold_size <- ceiling(n/k)
  mse_cv <- numeric(k)
  
  for (i in 1:k) {
    start <- (i - 1) * fold_size + 1
    end <- min(i * fold_size, n)
    train_indices <- c(1:(start - 1), (end + 1):n)
    train_data <- data[train_indices]
    val_data <- data[start:end]
    
    model_fit <- Arima(train_data, model = model)
    
    val_forecast <- forecast(model_fit, h = length(val_data))$mean
    mse_cv[i] <- mean((val_data - val_forecast)^2)
  }
  
  return(mean(mse_cv))
}


mse_ar <- calculate_mse(test_data, forecast(model_ar, h = length(test_data))$mean)
mse_ma <- calculate_mse(test_data, forecast(model_ma, h = length(test_data))$mean)
mse_arima <- calculate_mse(test_data, forecast(model_arima, h = length(test_data))$mean)
mse_autoarima <- calculate_mse(test_data, forecast(model_autoarima, h = length(test_data))$mean)

rmse_ar <- calculate_rmse(test_data, forecast(model_ar, h = length(test_data))$mean)
rmse_ma <- calculate_rmse(test_data, forecast(model_ma, h = length(test_data))$mean)
rmse_arima <- calculate_rmse(test_data, forecast(model_arima, h = length(test_data))$mean)
rmse_autoarima <- calculate_rmse(test_data, forecast(model_autoarima, h = length(test_data))$mean)

mape_ar <- calculate_mape(test_data, forecast(model_ar, h = length(test_data))$mean)
mape_ma <- calculate_mape(test_data, forecast(model_ma, h = length(test_data))$mean)
mape_arima <- calculate_mape(test_data, forecast(model_arima, h = length(test_data))$mean)
mape_autoarima <- calculate_mape(test_data, forecast(model_autoarima, h = length(test_data))$mean)

accuracy_ar <- calculate_accuracy(test_data, forecast(model_ar, h = length(test_data))$mean)
accuracy_ma <- calculate_accuracy(test_data, forecast(model_ma, h = length(test_data))$mean)
accuracy_arima <- calculate_accuracy(test_data, forecast(model_arima, h = length(test_data))$mean)
accuracy_autoarima <- calculate_accuracy(test_data, forecast(model_autoarima,
                                                             h = length(test_data))$mean)

aic_ar <- calculate_aic(model_ar)
aic_ma <- calculate_aic(model_ma)
aic_arima <- calculate_aic(model_arima)
aic_autoarima <- calculate_aic(model_autoarima)

bic_ar <- calculate_bic(model_ar)
bic_ma <- calculate_bic(model_ma)
bic_arima <- calculate_bic(model_arima)
bic_autoarima <- calculate_bic(model_autoarima)

cv_ar <- perform_cross_validation(model_ar, train_data)
cv_ma <- perform_cross_validation(model_ma, train_data)
cv_arima <- perform_cross_validation(model_arima, train_data)
cv_autoarima <- perform_cross_validation(model_autoarima, train_data)


evaluations <- data.frame(
  Model = c("AR", "MA", "ARIMA", "Auto ARIMA"),
  MSE = c(mse_ar, mse_ma, mse_arima, mse_autoarima),
  RMSE = c(rmse_ar, rmse_ma, rmse_arima, rmse_autoarima),
  MAPE = c(mape_ar, mape_ma, mape_arima, mape_autoarima),
  Forecast_Accuracy = c(accuracy_ar, accuracy_ma, accuracy_arima, accuracy_autoarima),
  AIC = c(aic_ar, aic_ma, aic_arima, aic_autoarima),
  BIC = c(bic_ar, bic_ma, bic_arima, bic_autoarima),
  Cross_Validation = c(cv_ar, cv_ma, cv_arima, cv_autoarima)
)

print(evaluations)



# atribut arma dari model m4, nilai yang ditampilkan adalah 3 1 0 0 1 1 0. Ini menggambarkan parameter ARIMA yang digunakan oleh model:
#   
#   3: Orde parameter AR (p)
# 1: Orde parameter diferensiasi (d)
# 0: Orde parameter MA (q)
# 0: Orde parameter musiman AR (P)
# 1: Orde parameter musiman diferensiasi (D)
# 1: Orde parameter musiman MA (Q)
# 0: Periode musiman (S)
# 
# Jadi, dalam kasus ini, model m4 adalah model ARIMA dengan parameter (3,1,0)(1,1,0)_0.
# 
# 
data <- tsxc

model_ar <- arima(data, order = c(3, 0, 0))
model_ma <- arima(data, order = c(0, 0, 3))
model_arima <- arima(data, order = c(1, 1, 2))
model_autoarima <- auto.arima(data)

forecast_ar <- forecast(model_ar, h = 104)
forecast_ar <- forecast(model_ma, h = 104)
forecast_ar <- forecast(model_arima, h = 104)
forecast_autoarima <- forecast(model_autoarima, h = 104)


forecast_df <- data.frame(
  Date = index(forecast_autoarima$mean),
  Forecast = forecast_autoarima$mean,
  Lower = forecast_autoarima$lower,
  Upper = forecast_autoarima$upper
)

farima <- ggplot() +
  geom_line(data = forecast_df, aes(x = Date, y = Forecast, color = "Forecast"), 
            linetype = "solid", lwd = .5) +
  geom_ribbon(data = forecast_df, 
              aes(x = Date, ymin = Lower.80., ymax = Upper.80., fill = "Interval"), 
              alpha = 0.3) +
  geom_line(data = tsxc, aes(x = time(tsxc), y = tsxc, color = 'Observed'), lwd = .5) +
  labs(title = "Forecasting with ARIMA", x = "Year", y = "Events") +
  scale_color_manual(name = NULL, values = c("Forecast" = zcol[1], 'Observed' = zcol[6])) +
  scale_fill_manual(name = NULL, values = zcol[6]) +
  scale_x_continuous(breaks = seq(2015, 2026, 2)) +
  theme(legend.position = 'top', legend.justification = 'right')

##################################################

visualize_forecast <- function(forecast_data, title) {
  ggplot() +
    geom_line(aes(x = index(forecast_data$mean), y = forecast_data$mean, color = "Actual")) +
    geom_line(aes(x = index(forecast_data$lower), y = forecast_data$lower, color = "Lower 95% CI"), linetype = "dashed") +
    geom_line(aes(x = index(forecast_data$upper), y = forecast_data$upper, color = "Upper 95% CI"), linetype = "dashed") +
    labs(title = title, x = "Time", y = "Value") +
    scale_color_manual(values = c("Actual" = "blue", "Lower 95% CI" = "red", "Upper 95% CI" = "red")) +
    theme_minimal()
}

# Visualisasi forecast untuk setiap model
visualize_forecast(forecast_ar, "Forecast AR Model")
visualize_forecast(forecast_ma, "Forecast MA Model")
visualize_forecast(forecast_arima, "Forecast ARIMA Model")
visualize_forecast(forecast_autoarima, "Forecast Auto ARIMA Model")

####################################################

data_length <- length(data)
train_length <- round(0.8 * data_length)
train_data <- data[1:train_length]
test_data <- data[(train_length + 1):data_length]

model_ar <- arima(train_data, order = c(3, 0, 0))
model_ma <- arima(train_data, order = c(0, 0, 3))
model_arima <- arima(train_data, order = c(1, 1, 2))
model_autoarima <- auto.arima(train_data)

calculate_mse <- function(actual, predicted) {
  mean((actual - predicted)^2)
}

calculate_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

calculate_mape <- function(actual, predicted) {
  mean(abs((actual - predicted)/actual)) * 100
}

calculate_accuracy <- function(actual, predicted) {
  mean(min(abs(actual - predicted), abs(actual - predicted + 1)))/mean(abs(actual))
}

calculate_aic <- function(model) {
  AIC(model)
}

calculate_bic <- function(model) {
  BIC(model)
}

perform_cross_validation <- function(model, data, k = 5) {
  n <- length(data)
  fold_size <- ceiling(n/k)
  mse_cv <- rep(0, k)
  
  for (i in 1:k) {
    start <- (i - 1) * fold_size + 1
    end <- min(i * fold_size, n)
    train_indices <- c(1:(start - 1), (end + 1):n)
    train_data <- data[train_indices]
    val_data <- data[start:end]
    
    model_fit <- Arima(train_data, model = model)
    
    val_forecast <- forecast(model_fit, h = length(val_data))$mean
    mse_cv[i] <- mean((val_data - val_forecast)^2)
  }
  
  return(mean(mse_cv))
}


forecasts <- lapply(list(model_ar, model_ma, model_arima, model_autoarima), function(model) {
  forecast(model, h = length(test_data))$mean
})

evaluations <- data.frame(
  Model = c("AR", "MA", "ARIMA", "Auto ARIMA"),
  MSE = sapply(forecasts, function(forecast) calculate_mse(test_data, forecast)),
  RMSE = sapply(forecasts, function(forecast) calculate_rmse(test_data, forecast)),
  MAPE = sapply(forecasts, function(forecast) calculate_mape(test_data, forecast)),
  Forecast_Accuracy = sapply(forecasts, function(forecast) calculate_accuracy(test_data, forecast)),
  AIC = c(calculate_aic(model_ar), calculate_aic(model_ma), calculate_aic(model_arima), calculate_aic(model_autoarima)),
  BIC = c(calculate_bic(model_ar), calculate_bic(model_ma), calculate_bic(model_arima), calculate_bic(model_autoarima)),
  Cross_Validation = c(perform_cross_validation(model_ar, train_data), perform_cross_validation(model_ma, train_data), perform_cross_validation(model_arima, train_data), perform_cross_validation(model_autoarima, train_data))
)

print(evaluations)

##################################################

data_length <- length(data)
train_length <- round(0.8 * data_length)
train_data <- data[1:train_length]
test_data <- data[(train_length + 1):data_length]
model_ar <- arima(train_data, order = c(3, 0, 0))
model_ma <- arima(train_data, order = c(0, 0, 3))
model_arima <- arima(train_data, order = c(1, 1, 2))
model_autoarima <- auto.arima(train_data)
calculate_mse <- function(actual, predicted) {
  mean((actual - predicted)^2)
}
calculate_rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}
calculate_mape <- function(actual, predicted) {
  mean(abs((actual - predicted)/actual)) * 100
}
calculate_accuracy <- function(actual, predicted) {
  mean(min(abs(actual - predicted), abs(actual - predicted + 1)))/mean(abs(actual))
}
calculate_aic <- function(model) {
  AIC(model)
}
calculate_bic <- function(model) {
  BIC(model)
}
perform_cross_validation <- function(model, data, k = 5) {
  n <- length(data)
  fold_size <- ceiling(n/k)
  mse_cv <- rep(0, k)
  for (i in 1:k) {
    start <- (i - 1) * fold_size + 1
    end <- min(i * fold_size, n)
    train_indices <- c(1:(start - 1), (end + 1):n)
    train_data <- data[train_indices]
    val_data <- data[start:end]
    model_fit <- Arima(train_data, model = model)
    val_forecast <- forecast(model_fit, h = length(val_data))$mean
    mse_cv[i] <- mean((val_data - val_forecast)^2)
  }
  return(mean(mse_cv))
}
forecasts <- lapply(list(model_ar, model_ma, model_arima, model_autoarima),
                    function(model) {
                      forecast(model, h = length(test_data))$mean
                    })
evaluations <- data.frame(Model = c("AR", "MA", "ARIMA", "Auto ARIMA"),
                          MSE = sapply(forecasts, function(forecast) calculate_mse(test_data,
                                                                                   forecast)), RMSE = sapply(forecasts, function(forecast) calculate_rmse(test_data,
                                                                                                                                                          forecast)), MAPE = sapply(forecasts, function(forecast) calculate_mape(test_data,
                                                                                                                                                                                                                                 forecast)), Forecast_Accuracy = sapply(forecasts, function(forecast) calculate_accuracy(test_data,
                                                                                                                                                                                                                                                                                                                         forecast)), AIC = c(calculate_aic(model_ar), calculate_aic(model_ma),
                                                                                                                                                                                                                                                                                                                                             calculate_aic(model_arima), calculate_aic(model_autoarima)), BIC = c(calculate_bic(model_ar),
                                                                                                                                                                                                                                                                                                                                                                                                                  calculate_bic(model_ma), calculate_bic(model_arima), calculate_bic(model_autoarima)),
                          Cross_Validation = c(perform_cross_validation(model_ar, train_data),
                                               perform_cross_validation(model_ma, train_data), perform_cross_validation(model_arima,
                                                                                                                        train_data), perform_cross_validation(model_autoarima, train_data)))
print(evaluations)

###############################################

es <- es(data, model = 'ZZZ', lags = c(52))
pr <- forecast(es, h = 105, interval = 'semiparametric')
plot(pr)

model_es <- es(data, model = 'ZZZ')
forec_es <- forecast(model_es, h = 105)
plot(forec_es)


# Menghitung forecast untuk model ES
forecast_es <- forecast(es_model, h = length(tsd))$mean

# Menghitung metrik evaluasi untuk model ES
mse_es <- mse(tsd, forecast_es)
rmse_es <- rmse(tsd, forecast_es)
mape_es <- mape(tsd, forecast_es)

# Menambahkan model ES ke dalam dataframe evaluasi
df_evaluate_models <- data.frame(
  Model = c("AR", "MA", "ARMA", "ARIMA", "Auto ARIMA", "ES"), 
  MSE = c(
    sapply(f, function(forecast) mse(tsd, forecast)), mse_es
  ), 
  RMSE = c(
    sapply(f, function(forecast) rmse(tsd, forecast)), rmse_es
  ), 
  MAPE = c(
    sapply(f, function(forecast) mape(tsd, forecast)), mape_es
  ), 
  'Forecast Accuracy' = c(
    sapply(f, function(forecast) accuracy(tsd, forecast)), NA
  ),
  'CV MSE' = c(
    cv(ar, td), cv(ma, td), cv(arma, td), 
    cv(ari, td), cv(auto_ari, td), NA
  )
)

# Menampilkan hasil evaluasi
print(df_evaluate_models)


###############################################################


mod_stl <- stl(data, s.window = 'periodic')
for_stl <- forecast(mod_stl, h = 105)
plot(for_stl)

for_ets <- forecast(ets(tsmc), h = 24)
plot(for_ets)

mod_arima <- auto.arima(data, seasonal = TRUE)
for_arima <- forecast(mod_arima, h = 105)
plot(for_arima)


library(rugarch)


total_data <- length(data)
n_roll <- total_data - 100 # decrease n_roll to total_data - 50

sarima_spec <- ugarchspec(mean.model = list(armaOrder = c(1, 1)), variance.model = list(garchOrder = c(1, 1)))

sarima_garch_fit <- ugarchfit(data = data, spec = sarima_spec)

forecast_sarima_garch <- ugarchforecast(sarima_garch_fit, n.ahead = 105, n.roll = n_roll)

# Plot hasil forecasting
plot(forecast_sarima_garch)

#

spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0)))
spec <- ugarchspec()
fit <- ugarchfit(data = data, spec = sarspec, out.sample = 367)
forecast <- ugarchforecast(fit, n.ahead = 105, n.roll = 367)
plot(forecast, which = 2)

# extract the date of the last observation in the original data
last_date <- index(data)[length(data)]

# generate the forecast
forecast <- ugarchforecast(params, data = data, n.ahead = 105, n.roll = 367)

# set the T0 date in the forecast output
forecast@model$T0 <- last_date

# extract the forecasted conditional standard deviations
sigmaFor <- forecast@forecast$sigmaFor

# add the last observation of the original data to the forecasted values
sigmaFor <- sigmaFor + tail(data, 1)

# print the forecasted values
print(sigmaFor)


# Create a GARCH model specification

spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0)))

fit <- ugarchfit(spec = spec, data = data, start = 1, out.sample = 367)

forecast <- ugarchforecast(fit, n.ahead = 105, n.roll = 367)

# Print the forecast

print(forecast)
plot(forecast)

##############################################################


ts_outliers <- tsoutliers(tsx)
tsxc <- tsclean(tsx)
tsmc <- tsclean(tsm)
tsdc <- tsclean(ts_daily[,1])

tsxcc <- tso(tsxc)
plot(tsxcc)

##------------------------------------------------------

fit.ets <- ets(tsm)
for.ets <- forecast(fit.ets, h = 24) 
plot(for.ets)


fit.arima <- auto.arima(tsxc, seasonal = TRUE)
for.arima <- forecast(fit.arima, h = 104)
plot(for.arima)

fit.holtw <- HoltWinters(tsm)
for.holtw <- forecast(fit.holtw, h = 24)
plot(for.holtw)

fit.tbats <- bats(tsx, use.box.cox = TRUE, 
                  use.trend = TRUE, 
                  use.damped.trend = TRUE)
for.tbats <- forecast(fit.tbats, h = 104)
plot(for.tbats)

acc.ets <- accuracy(for.ets)
acc.arima <- accuracy(for.arima)
acc.holtw <- accuracy(for.holtw)
acc.tbats <- accuracy(for.tbats)


c <- decompose(tsm, type = 'additive')
plot(c)


tsdf <- df_weekly
cols_to_transform <- names(tsdf)[-1] 
transformed.df <- tsdf
transformed.df[, cols_to_transform] <- lapply(tsdf[, cols_to_transform], log1p)

library(feasts)
as_tsibble(tsm) %>%
  model(classical_decomposition(value, type = 'additive')) %>%
  components() %>% autoplot()

tsdisplay(tsm)



# The first letter denotes the error type ("A", "M" or "Z"); the second letter denotes the trend type ("N","A","M" or "Z"); and the third letter denotes the season type ("N","A","M" or "Z"). In all cases, "N"=none, "A"=additive, "M"=multiplicative and "Z"=automatically selected. So, for example, "ANN" is simple exponential smoothing with additive errors, "MAM" is multiplicative Holt-Winters' method with multiplicative errors, and so on.


ann <- ets(tsmc, model = 'ANN')
ana <- ets(tsmc, model = 'ANA')
aan <- ets(tsmc, model = 'AAN')
aaa <- ets(tsmc, model = 'AAA')
mam <- ets(tsmc, model = 'MAM')
mna <- ets(tsmc, model = 'MNA')
zzz <- ets(tsmc, model = 'ZZZ')

m.stats <- function(m.n, m.s) { 
  aic <- round(m.s$aic, 2)
  aicc <- round(m.s$aicc, 2)
  bic <- round(m.s$bic, 2)
  return(c(m.n, aic,aicc,bic))
}

m.e <- data.frame(matrix(ncol=4)) 
names(m.e)[1:4] <- c("Model", "aic", "aicc", "bic")

m.e[1,] <- m.stats("ANN", ann)
m.e[2,] <- m.stats("ANA", ana)
m.e[3,] <- m.stats("AAN", aan) 
m.e[4,] <- m.stats("AAA", aaa)
m.e[5,] <- m.stats("MAM", mam)
m.e[6,] <- m.stats("MNA", mna) # best
m.e[7,] <- m.stats("ZZZ", zzz)

m.e
# 

plot(mna)
etsaan <- forecast(zzz, method = c('ets'), h = 24)
plot(etsaan)


tsxc %>% auto.arima() %>% forecast(h = 104) %>% autoplot()
tsm %>% ets() %>% forecast(h = 24) %>% autoplot()
tsxc %>% tbats(use.box.cox = FALSE, 
               use.trend = TRUE, 
               use.damped.trend = TRUE) %>%
  forecast(h = 104) %>% autoplot()

library(smooth)
es <- es(tsx, 'XXX', 52)

for.es <- forecast(es, h = 104)
plot(for.es)

##########################################################

# Dalam analisis data time series menggunakan model TBATS (Trigonometric seasonality, Box-Cox transformation, ARMAerrors, Trend and Seasonal components), empat komponen utama yang dihasilkan memberikan wawasan yang berharga tentang dinamika data. Komponen ini, yaitu observed, level, slope, dan season, mencerminkan aspek-aspek penting dari data, seperti tren utama, perubahan dalam tingkat data, perubahan dalam kecepatan perubahan data, dan variasi yang berulang sepanjang waktu.
# 
# Komponen observed menunjukkan data asli yang kita analisis, mencakup tren utama dan variasi sesaon-al. Ini memberikan gambaran umum tentang bagaimana data berubah sepanjang waktu, termasuk bagaimana pola sesaonal mempengaruhi data. Dalam konteks data konflik, komponen observed dapat menunjukkan peningkatan atau penurunan keseluruhan peristiwa yang terjadi selama periode pengamatan, serta bagai-mana jumlah konflik berubah sepanjang musim.
# 
# Sementara itu, grafik remainder atau residual yang berfungsi untuk menunjukkan sisa-sisa kesalahan prediksi terhadap model data yang diamati. Tujuan utamanya adalah untuk memastikan bahwa tidak ada pola atau struktur yang tersembunyi dalam sisa-sisa tersebut. Grafik menunjukkan bahwa tidak terdapat pola yang jelas dan cenderung acak, mengindikasikan bahwa tidak ada pola yang tersisa, dengan kata lain semua pola telah teridientifikasi dengan baik.
# 
# Komponen level mencerminkan perubahan dalam tingkat data sepanjang waktu, tanpa mempertimbangkan tren atau variasi sesaonal. Ini dapat menunjukkan perubahan dalam tingkat keseluruhan data, seperti peningkatan atau penurunan yang tidak terkait dengan musim atau pola siklus. Level dapat menunjukkan perubahan dalam tingkat keseluruhan konflik yang tidak terkait dengan musim tertentu.
# 
# Komponen slope mencerminkan perubahan dalam tren data sepanjang waktu, menunjukkan perubahan dalam kecepatan perubahan data, yang mungkin disebabkan oleh perubahan dalam kondisi tertentu yang mempengaruhi data. Hasil analisis slope menunjukkan tren dimanis dalam perubahan data sepanjang periode waktu. Pada awal periode, terdapat penurunan signifikan dalam kecepatan perubahan data, yang menunjukkan bahwa tren data mungkin sedang menurun. Namun, seiring berjalannya waktu, terdapat beberapa titik di mana kecepatan perubahan data meningkat, menunjukkan adanya peningkatan dalam tren data. Beberapa model dapat merepresentasikan garis tren yang menunjukkan perubahan data sepanjang periode waktu, dari model yang paling sederhana sampai model smoothing sebagaimana berikut.
# 
# {r eval=FALSE, include=FALSE}
# pwsen <- sens.slope(pwpred)
# loessdsen <- sens.slope(lopredd)     # Sen's Slope (LOESS)
# loesssen <- sens.slope(lopred)
# 
# Uji Theil Sen (Sen's Slope) untuk ketiga model di atas menunjukkan hasil yang signifikan secara statistik, ditunjukkan dengan p-value yang kecil (p-value: < 2.2e-16) meskipun nilai sen's slope untuk masing-masing model berbeda (Piecewise = 0.0034, LOESS 1 = 0.0029, LOESS 2 = 0.028). Sementara itu, interval kepercayaan 95% untuk masing-masing model sangat sempit (Piecewise = 0.003436199 - 0.003436199, LOESS 1 = 0.002822707-0.002912969, LOESS 2 = 0.002689167-0.002890815) menunjukkan tingkat kepercayaan yang tinggi dalam perkiraan sen's slope.
# 
# Komponen season mencerminkan variasi yang berulang sepanjang waktu, seperti peningkatan atau penurunan yang terjadi secara berkala, seperti musiman atau periode tertentu. Ini memberikan wawasan tentang bagaimana data berubah sepanjang waktu berdasarkan musim atau periode tertentu. Dalam analisis jumlah konflik, season dapat menunjukkan bagaimana jumlah konflik berubah sepanjang periode.
# 
# <!--# Seasonal boxplot -->
# 
# Dengan memahami dan menganalisis komponen-komponen ini, kita dapat mendapatkan pemahaman yang lebih mendalam tentang dinamika data time series, yang pada akhirnya dapat membantu dalam pengambilan keputusan, perencanaan, dan prediksi.
# 
# Analisis Tren Jenis Konflik
#

tsdf <- df_weekly
cols_to_transform <- names(tsdf)[-1] 
transformed.df <- tsdf
transformed.df[, cols_to_transform] <- lapply(tsdf[, cols_to_transform], log1p)
dfp <- data.frame(ds = as.Date(transformed.df$Date), y = transformed.df$EVENT)

m <- prophet(dfp)
future <- make_future_dataframe(m, periods = 730)
forecast <- predict(m, future)
plot(m, forecast)
prophet_plot_components(m, forecast)


ggplot(m$history, aes(x = ds, y = y_scaled)) + geom_line()



lam = BoxCox.lambda(dfp$y, method = "loglik")
dfp$y = BoxCox(dfp$y, lam)
df.m <- melt(dfp, measure.vars=c("Date", "y"))#

ggplot(dfp, aes(index(dfp), y)) + geom_line()


cols_to_transform <- names(df_weekly)[-1]

transformed.df <- df_weekly
transformed.df[, cols_to_transform] <- lapply(df_weekly[, cols_to_transform], function(x) log1p(log1p(x)))

ggplot(transformed.df, aes(x = Date, y = EVENT)) + geom_line()


x <- dfp$y
# Jalankan uji Anderson-Darling
ad_test <- ad.test(x)
pval <- ad_test$p.value
if (pval < 0.05) {
  cat("Data tidak terdistribusi secara normal (p-value < 0.05)")
} else {
  cat("Data terdistribusi secara normal (p-value >= 0.05)")
}

# Jalankan uji Kolmogorov-Smirnov
x <- unique(x)
ks_test <- ks.test(x, pnorm, mean = mean(x), sd = sd(x))
pval <- ks_test$p.value
# Buat kesimpulan
if (pval < 0.05) {
  cat("Data tidak terdistribusi secara normal (p-value < 0.05)")
} else {
  cat("Data terdistribusi secara normal (p-value >= 0.05)")
}



# Lakukan analisis spektrum-----------------------------------------------
spectrum_result <- spectrum(tsx)

magnitude <- spectrum_result$spec

dominant_frequency <- spectrum_result$freq[which.max(magnitude)]
cat("Frekuensi dominan:", dominant_frequency)

threshold <- 0.5 * max(magnitude)
seasonal_frequencies <- spectrum_result$freq[magnitude > threshold]
cat("Frekuensi seasonal:", seasonal_frequencies)

# analisis lanjutan Fourier------------------------------------------------------
# Melakukan transformasi Fourier
fourier_result <- fft(tsx)

# Menghitung magnitudo spektrum
magnitude <- Mod(fourier_result)
plot(magnitude)

# Mencari frekuensi dengan magnitudo tertinggi
dominant_frequency <- which.max(magnitude)

# Mencari frekuensi yang berulang setiap tahun (frekuensi 1)
yearly_frequency <- which(magnitude > 0.75) 
# Menampilkan frekuensi dominan dan frekuensi yang berulang setiap tahun
cat("Frekuensi dominan:", dominant_frequency)
cat("Frekuensi yang berulang setiap tahun:", yearly_frequency)



# Menghitung MSE-------------------------------------------------
model <- auto.arima(konflik_ts)
forecasts <- forecast(model, h = 10)
mse <- mean((konflik_ts - forecasts$mean)^2)
cat("MSE:", mse)



fourier_result <- fft(tsx)
magnitude <- Mod(fourier_result)
dominant_frequency <- which.max(magnitude)
y_frequency <- which(magnitude > 0.75) 



pacf_est <- pacf(tsx, lag.max = 20)
plot(pacf_est, main = "Partial Autocorrelation Function (PACF) for tsx")
significant_lags <- which(abs(pacf_est) > 1.96)
significant_lags_below_10 <- significant_lags[significant_lags <= 10]
significant_lags_below_10
order_AR <- max(significant_lags_below_10)
order_AR

# ARMA

best_aic <- Inf
best_order <- c(0, 0)
for (p in 0:3) {
  for (q in 0:3) {
    arma_model <- arima(tsx, order = c(p, 0, q))
    aic <- AIC(arma_model)
    if (aic < best_aic) {
      best_aic <- aic
      best_order <- c(p, 0, q)
    }
  }
}
best_order

arma_model <- arima(tsx, order = c(0, 1, 3), method = 'CSS')
summary(arma_model)
a <- acf(arma_model$residuals)
b <- pacf(arma_model$residuals)

forecast_values <- forecast(arma_model, h = 108)
print(forecast_values)

n <- length(tsx)
train_data <- tsx[1:(n * 0.8)] %>% log1p()
test_data <- tsx[(n * 0.8 + 1):n] %>% log1p()
arma_model_train <- arima(train_data, order = c(0, 1, 3), method = 'CSS')
forecast_values_test <- forecast(arma_model_train, h = length(test_data))
accuracy(forecast_values_test, test_data)

forecast_values <- forecast(arma_model_train, h = 104)
plot(forecast_values)

# Cross Validation
library(forecast)
library(caret)
n <- length(tsx)
train_data <- tsx[1:(n * 0.8)] %>% log1p()
test_data <- tsx[(n * 0.8 + 1):n] %>% log1p()
evaluate_model <- function(p, d, q) {
  train_data_transformed <- train_data
  test_data_transformed <- test_data
  arma_model <- arima(train_data_transformed, order = c(p, d, q), method = "CSS")
  forecast_values_test <- forecast(arma_model, h = length(test_data_transformed))
  eval_metrics <- accuracy(forecast_values_test, test_data_transformed)
  return(eval_metrics[2, "RMSE"])
}
set.seed(123)
best_rmse <- Inf
best_order <- c(0, 0, 0)
p_range <- 0:3
d_range <- 0:1
q_range <- 0:3
for (p in p_range) {
  for (d in d_range) {
    for (q in q_range) {
      rmse <- evaluate_model(p, d, q)
      if (rmse < best_rmse) {
        best_rmse <- rmse
        best_order <- c(p, d, q)
      }
    }
  }
}
cat("Best order:", best_order, "\n")

#

k <- 5
n <- length(tsx)
subset_size <- floor(n/k)
rmse_values <- numeric(k)
mae_values <- numeric(k)
mape_values <- numeric(k)
for (i in 1:k) {
  test_indices <- ((i - 1) * subset_size + 1):(i * subset_size)
  train_indices <- setdiff(1:n, test_indices)
  train_data <- tsx[train_indices] %>%
    log1p()
  test_data <- tsx[test_indices] %>%
    log1p()
  arma_model_train <- arima(train_data, order = c(0, 1, 3), method = "CSS")
  forecast_values_test <- forecast(arma_model_train, h = length(test_data))
  accuracy_values <- accuracy(forecast_values_test, test_data)
  rmse_values[i] <- accuracy_values[2, "RMSE"]
  mae_values[i] <- accuracy_values[2, "MAE"]
  mape_values[i] <- accuracy_values[2, "MAPE"]
}
mean_rmse <- mean(rmse_values)
mean_mae <- mean(mae_values)
mean_mape <- mean(mape_values)
cat("Mean RMSE:", mean_rmse, "\n")
cat("Mean MAE:", mean_mae, "\n")
cat("Mean MAPE:", mean_mape, "\n")


##----------------------------------------------------------
dftrain <- data.frame(ds = time(train_data), y = train_data)
dftest <- data.frame(ds = time(test_data), y = test_data)

xgb_matrix_train <- xgb.DMatrix(data = as.matrix(dftrain[,"ds"]), label = dftrain$y)
xgb_matrix_test <- xgb.DMatrix(data = as.matrix(dftest[,"ds"]), label = dftest$y)

params <- list(objective = "reg:squarederror", nrounds = 100, max_depth = 6, eta = 0.3)
xgb_model_train <- xgboost(data = xgb_matrix_train, params = params, nrounds = params$nrounds)
xgb_test_predictions <- predict(xgb_model_train, xgb_matrix_test)
accuracy(xgb_test_predictions, dftest$y)


# SARIMA
sarima_model <- auto.arima(train_data, seasonal = TRUE)
forecast_values <- forecast(sarima_model, h = length(test_data))
accuracy(forecast_values, test_data)

forecast_values <- forecast(sarima_model, h = 104)
plot(forecast_values)


# ETS
# Fitting ETS model
ets_model <- ets(train_data)
# Make forecasts
forecast_values <- forecast(ets_model, h = length(test_data))
# Evaluate accuracy
accuracy(forecast_values, test_data)

forecast_values <- forecast(sarima_model, h = 104)
plot(forecast_values)



tbt <- tbats(tsx, use.box.cox = TRUE, use.trend = TRUE, use.damped.trend = TRUE)
forecast_values <- forecast(tbt, h = 104)
accuracy(forecast_values, test_data)

forecast_values <- forecast(sarima_model, h = 104)
plot(forecast_values)


# Membuat prediksi pada data uji
forecast_values_test <- forecast(arma_model, h = length(test_data))

# Evaluasi performa model
accuracy(forecast_values_test, test_data)

best_rmse <- Inf
best_order <- c(0, 0)  # Inisialisasi parameter terbaik
for (p in 1:3) {
  for (q in 1:3) {
    arma_model <- arima(train_data, order = c(p, 0, q))
    forecast_values <- forecast(arma_model, h = length(test_data))
    rmse <- accuracy(forecast_values, test_data)[2, "RMSE"]  # Mendapatkan RMSE pada data pengujian
    if (rmse < best_rmse) {
      best_rmse <- rmse
      best_order <- c(p, q)
    }
  }
}
best_order  # Menampilkan parameter terbaik

arma_model <- arima(tsx, order = c(3, 0, 1), method = 'CSS')

forecast_values <- forecast(arma_model, h = 108)
print(forecast_values)
n <- length(tsx)
train_data <- tsx[1:(n * 0.8)] %>% log()
test_data <- tsx[(n * 0.8 + 1):n] %>% log()
arma_model_train <- arima(train_data, order = c(3, 0, 1), method = 'CSS')
forecast_values_test <- forecast(arma_model_train, h = length(test_data))
accuracy(forecast_values_test, test_data)


transformed_train_data <- log(train_data)
transformed_test_data <- log(test_data)

###########################################################

##-------------------------------------------------------- MENCARI AOAKAH DATA MEMILIKI POLA MUSIMAN
library(forecast)
seasonality_test <- function(data) {
  lomb_scargle_test <- function(series) {
    spec <- spec.pgram(series)
    max_freq <- spec$freq[which.max(spec$spec)]
    return(1/max_freq)
  }
  period <- lomb_scargle_test(data)
  if (period > 1) {
    cat("Data memiliki musiman dengan periode sekitar", round(period),
        "\n")
  }
  else {
    cat("Tidak cukup bukti untuk menyatakan adanya musiman dalam data\n")
  }
}

arima_decomposition <- function(data) {
  arima_model <- auto.arima(data)
  decomposition <- decompose(arima_model)
  return(decomposition)
}

ets_decomposition <- function(data) {
  ets_model <- ets(data)
  decomposition <- decompose(ets_model)
  return(decomposition)
}

seasonality_test(tsx)
decomposition_arima <- arima_decomposition(tsx)
decomposition_ets <- ets_decomposition(tsx)

##--------------------------------------------------------------------------------------------- TERDETEKSI 3 POLA DALAM TSX
seasonality_test <- function(data) {
  lomb_scargle_test <- function(series) {
    spec <- spec.pgram(series)
    max_freq <- spec$freq[which.max(spec$spec)]
    return(1/max_freq)
  }
  period <- lomb_scargle_test(data)
  if (period > 1) {
    cat("Data memiliki musiman dengan periode sekitar", round(period),
        "\n")
  }
  else {
    cat("Tidak cukup bukti untuk menyatakan adanya musiman dalam data\n")
  }
  return(period)
}

nperiod <- seasonality_test(tsx)

# Fungsi untuk dekomposisi decompose()
seasonal_decomposition <- function(data, period) {
  ts_data <- ts(data, frequency = period)
  decomposed <- decompose(ts_data, "multiplicative")
  plot(decomposed)
  return(list(decomposed, ts_data))
}
decomposition_result <- seasonal_decomposition(tsx, period = nperiod)

# Fungsi untuk dekomposisi STL
seasonal_decomposition_stl <- function(data, period) {
  ts_data <- ts(data, frequency = period)
  decomposed <- stl(ts_data, s.window = "periodic")
  plot(decomposed)
  return(list(decomposed, ts_data))
}
decomposition_result_stl <- seasonal_decomposition_stl(tsx, period = nperiod)





# Modelisasi dengan tbats, menggunakan 3 pola musiman
model <- tbats(tsx, seasonal.periods = 3)

# Prediksi
forecast <- forecast(model, h = 104) # Misalnya, prediksi untuk 12 bulan ke depan

# Evaluasi model (misalnya, menggunakan MAE)
accuracy(forecast, tsx)

plot(forecast)




seasonal_freq <- c(3, 52)
msts_data <- msts(tsx, seasonal.periods = seasonal_freq)
decomposed_result <- mstl(msts_data)
# autoplot(decomposed_result)

train_data <- window(tsx, end = c(2022, 46))
test_data <- window(tsx, start = c(2023, 1))
decomposed_result <- mstl(train_data)


model <- tbats(decomposed_result[, "Trend"], seasonal.periods = c(3))
forecast_result <- forecast(model, h = length(test_data))
accuracy(forecast_result, test_data)
autoplot(forecast_result) + autolayer(test_data, series = "Observed")

decomposed_result <- stlm(train_data, s.window = "periodic")
model <- tbats(decomposed_result[, "Trend"], seasonal.periods = c(3))
forecast_result <- forecast(model, h = length(test_data))
accuracy(forecast_result, test_data)
autoplot(forecast_result) + autolayer(test_data, series = "Observed")



# Dekomposisi data pelatihan menggunakan stlm
decomposed_result <- stlm(train_data, s.window="periodic")

# Membuat model forecasting menggunakan tbats dengan komponen trend dari stlm
model <- tbats(decomposed_result$time.series[, "trend"], seasonal.periods = c(3))

# Membuat prediksi pada set pengujian
forecast_result <- forecast(model, h=length(test_data))

# Evaluasi model menggunakan MAE
accuracy(forecast_result, test_data)

# Visualisasi hasil prediksi dan data aktual
autoplot(forecast_result) + autolayer(test_data, series="Observed")


##########

model_and_predict <- function(data, period, horizon, future_horizon) {
  decomposed <- stl(data, s.window = "periodic")
  
  trend <- decomposed$time.series[, "trend"]
  seasonal <- decomposed$time.series[, "seasonal"]
  residual <- decomposed$time.series[, "remainder"]
  
  trend_model <- lm(trend ~ time(trend))
  seasonal_model <- lm(seasonal ~ time(seasonal))
  residual_model <- arima(residual, order = c(1, 0, 1))
  
  trend_pred <- predict(trend_model, newdata = data.frame(time = time(trend) + horizon))
  seasonal_pred <- predict(seasonal_model, newdata = data.frame(time = time(seasonal) + horizon))
  residual_pred <- predict(residual_model, n.ahead = future_horizon)$pred
  
  # Memperpanjang prediksi residu menjadi jangka waktu yang diinginkan
  residual_pred <- c(rep(NA, length(trend_pred) - length(residual_pred)), residual_pred)
  
  predicted_values <- trend_pred + seasonal_pred + residual_pred
  
  # Plot data asli dan prediksi
  plot(data)
  lines(ts(predicted_values, start = start(data), frequency = frequency(data)), col = "red")
  legend("topright", legend = "Predicted", col = "red", lty = 1)
  
  return(predicted_values)
}

# Memanggil fungsi untuk memprediksi 2 tahun ke depan
predicted_values <- model_and_predict(tsx, period = 3, horizon = 12, future_horizon = 104)  # 2 tahun = 104 minggu



library(forecast)

model_and_predict <- function(data, future_horizon) {
  decomposed <- stl(data, s.window = "periodic")
  
  trend <- decomposed$time.series[, "trend"]
  seasonal <- decomposed$time.series[, "seasonal"]
  residual <- decomposed$time.series[, "remainder"]
  
  # Menggunakan auto.arima untuk mendapatkan model ARIMA
  trend_model <- auto.arima(trend)
  seasonal_model <- auto.arima(seasonal)
  residual_model <- auto.arima(residual)
  
  # Melakukan prediksi menggunakan model ARIMA
  trend_pred <- forecast(trend_model, h = future_horizon)$mean
  seasonal_pred <- forecast(seasonal_model, h = future_horizon)$mean
  residual_pred <- forecast(residual_model, h = future_horizon)$mean
  
  # Menggabungkan prediksi dari semua komponen
  predicted_values <- trend_pred + seasonal_pred + residual_pred
  
  # Plot data asli dan prediksi
  plot(data)
  lines(ts(predicted_values, start = start(data), frequency = frequency(data)), col = "red")
  legend("topright", legend = "Predicted", col = "red", lty = 1)
  
  return(predicted_values)
}

# Memanggil fungsi untuk memprediksi 2 tahun ke depan
predicted_values <- model_and_predict(tsx, future_horizon = 104)  # 2 tahun = 104 minggu


# MENCARI NILALI T.WINDOW DAN S.WINDOW OPTIMAL UNTUK STL

library(forecast)

# Fungsi untuk melakukan validasi silang dengan model ARIMA
cross_validation_arima <- function(data, order_values, folds = 5) {
  n <- length(data)
  fold_size <- floor(n / folds)
  errors <- array(NA, dim = c(length(order_values), folds))
  
  for (i in 1:length(order_values)) {
    for (k in 1:folds) {
      start_idx <- (k - 1) * fold_size + 1
      end_idx <- min(k * fold_size, n)
      train_data <- data[-(start_idx:end_idx)]
      test_data <- data[start_idx:end_idx]
      
      # Fit ARIMA model
      arima_order <- order_values[i]
      arima_model <- arima(train_data, order = arima_order)
      arima_forecast <- forecast(arima_model, h = length(test_data))
      errors[i, k] <- sqrt(mean((arima_forecast$mean - test_data)^2))
    }
  }
  
  return(errors)
}

# Data Anda (gantilah dengan data Anda sendiri)
tsx <- tsm

# Nilai yang akan diuji
order_values <- list(c(1, 0, 1), c(1, 1, 1), c(1, 0, 2), c(1, 1, 2), c(2, 0, 1), c(2, 1, 1))  # Contoh kombinasi nilai order untuk ARIMA

# Lakukan validasi silang
errors_arima <- cross_validation_arima(tsx, order_values)

# Cari kombinasi order dengan kesalahan terendah
min_error_arima <- min(errors_arima)
best_order_idx <- which(errors_arima == min_error_arima, arr.ind = TRUE)
best_order <- order_values[[best_order_idx[1]]]

cat("Best order for ARIMA:", best_order, "\n")
cat("Lowest RMSE for ARIMA:", min_error_arima, "\n")


###########################################################

# Choose the number of folds for cross-validation (e.g., 5 or 10)
num_folds <- 5

# Step 3: Create indices for cross-validation
# Use the createFolds() function from the caret package to create indices for cross-validation
# This function splits the data into 'num_folds' folds
library(caret)
set.seed(123) # Set seed for reproducibility
cv_indices <- createFolds(df_datweek$event, k = num_folds)

# Step 4: Perform cross-validation
# Iterate over the folds and fit LOESS models on the training data
# Evaluate the performance of each model on the corresponding validation data
for (i in 1:num_folds) {
  # Split data into training and validation sets
  train_data <- df_datweek[-cv_indices[[i]], ] # Training data
  valid_data <- df_datweek[cv_indices[[i]], ]  # Validation data
  
  # Fit LOESS model on training data
  loess_model <- loess(df_datweek$event ~ df_datweek$time, data = train_data)
  
  # Make predictions on validation data
  predicted_values <- predict(loess_model, newdata = valid_data$time)
  
  # Evaluate performance (e.g., calculate mean squared error)
  mse <- mean((valid_data$event - predicted_values)^2)
  
  # Print or store performance metrics as needed
  print(paste("Fold", i, "- Mean Squared Error:", mse))
}


# 
# This appears to be the autocorrelation values of a time series data named 'tsx' at different lags. Let's analyze it:
# 
# - **Autocorrelation**: Autocorrelation measures the relationship between a variable and a lagged version of itself. Here, it seems we're examining autocorrelation at various lags for the 'tsx' series.
# 
# - **Lags**: The numbers from 0 to 156 represent the lags at which autocorrelation is calculated.
# 
# - **Autocorrelation Values**: The values range from 1.000 to 0.031, indicating the strength and direction of the autocorrelation at each lag.
# 
# - **Interpretation**:
#   - At lag 0, the autocorrelation is 1.000, which is expected since the variable is perfectly correlated with itself at lag 0.
# - As the lag increases, the autocorrelation gradually decreases, indicating that the current value of the series is less correlated with values further in the past.
# - Autocorrelation values above 0.5 or below -0.5 are typically considered significant, suggesting a substantial correlation.
# 
# - **Trend**:
#   - There seems to be a gradual decline in autocorrelation values as the lag increases, indicating a weakening correlation between the 'tsx' series and its lagged values as time progresses.
# 
# - **Further Analysis**:
#   - These autocorrelation values can be used to identify the presence of serial correlation in the time series data and to determine the appropriate lag order for autoregressive models.
# 
# Overall, this analysis provides insights into the temporal dependency structure of the 'tsx' series.

#########################################################

df <- data.frame(tsb)
tm <- data.frame(date = time(tsb))

ts_data <- cbind(tm, df)
ts_data <- ts_data[, -c(2, 3, 10:15)]
ts_wide <- ts_data
ts_data_long <- pivot_longer(ts_data,
                             cols = -date,
                             names_to = 'var',
                             values_to = 'value'
)
ts_data <- ts_data_long

ts_data <- spread(ts_data, key = var, value = value)
ts_data_ts <- ts(ts_data[, -1], start = c(2015), frequency = 12)

decomposed_stl <- lapply(ts_data_ts, function(x) {
  stl_result <- stl(x, s.window = "periodic", robust = TRUE)
  return(list(seasonal = stl_result$time.series[, "seasonal"], 
              trend = stl_result$time.series[,"trend"], 
              remainder = stl_result$time.series[, "remainder"]))
})

for (i in seq_along(decomposed_stl)) {
  cat("Variable:", names(decomposed_stl)[i], "\n")
  print(decomposed_stl[[i]])
  cat("\n")
}


plot_data <- lapply(decomposed_stl, function(decomposed) {
  df <- data.frame(date = time(ts_data_ts), seasonal = decomposed$seasonal,
                   trend = decomposed$trend, remainder = decomposed$remainder)
  df_melted <- melt(df, id.vars = "date")
  return(df_melted)
})

plots <- lapply(seq_along(plot_data), function(i) {
  ggplot(plot_data[[i]], aes(x = date, y = value, color = variable)) +
    geom_line() + labs(title = paste("Decomposed Components for", names(decomposed_stl)[i]),
                       x = "Date", y = "Value") + theme_minimal()
})

# for (i in seq_along(plots)) {
#   print(plots[[i]])
# }

plots[[4]]

##--------------------------------------------------------------------------------------------------------------------------------!!!!!!!!!!!!!!!!

# Load necessary libraries
library(forecast)
library(tseries)

# Asumsikan tsx sudah didefinisikan
# tsx <- ts(data, start = c(2000, 1), frequency = 12)

# 1. Analisis Autokorelasi
acf(tsx)
pacf(tsx)

# 2. Model ARIMA
# Pilih model ARIMA yang paling sesuai berdasarkan plot autokorelasi
# Misalnya, jika ada autokorelasi signifikan pada lag 12, kita bisa mencoba model ARIMA(12, 1, 0)
fit <- auto.arima(tsx, seasonal = FALSE)
summary(fit)

# 3. Analisis Trend
# Gunakan tes Mann-Kendall
library(Kendall)
mk.test(tsx)

# 4. Analisis Pola
# Gunakan Fourier Transform
fft(tsx)

# 5. Model Non-Musimanitas
# Misalnya, model ARIMA tanpa komponen musimanitas
fit_non_seasonal <- auto.arima(tsx, seasonal = FALSE)
summary(fit_non_seasonal)

# 6. Validasi Model
accuracy(fit)


# 7. Analisis Autokorelasi untuk Model yang Dipilih
acf(fitted(fit))
pacf(fitted(fit))

##-------------------------------------------------------------------------------------------------
# Load library
library(forecast)

# Bagi data menjadi data pelatihan dan pengujian
train_ratio <- 0.7
train_size <- floor(train_ratio * length(tsx))
train_data <- tsx[1:train_size]
test_data <- tsx[(train_size + 1):length(tsx)]

# Buat model ARIMA
arima_model <- auto.arima(train_data)

# Buat peramalan
arima_forecast <- forecast(arima_model, h = length(test_data))

# Evaluasi kinerja model
rmse_arima <- sqrt(mean((arima_forecast$mean - test_data)^2))
cat("RMSE ARIMA on test data:", rmse_arima, "\n")


# Buat model ETS
ets_model <- ets(train_data)

# Buat peramalan
ets_forecast <- forecast(ets_model, h = length(test_data))

# Evaluasi kinerja model
rmse_ets <- sqrt(mean((ets_forecast$mean - test_data)^2))
cat("RMSE ETS on test data:", rmse_ets, "\n")


# Buat model Exponential Smoothing
ses_model <- ses(train_data)

# Buat peramalan dengan horizon yang sesuai
horizon <- min(400, length(test_data))  # Misalnya, atur horizon maksimal menjadi 12
ses_forecast <- forecast(ses_model, h = horizon)

# Evaluasi kinerja model
rmse_ses <- sqrt(mean((ses_forecast$mean[1:horizon] - test_data[1:horizon])^2))
cat("RMSE Exponential Smoothing on test data:", rmse_ses, "\n")





# Memuat paket mstl
install.packages("mstl")
library(mstl)

# Dekomposisi menggunakan MSTL
decomposition <- mstl(tsx)

# Plot komponen musiman
plot(decomposition[, "Seasonal12"], main = "Komponen Musiman")

# Plot data asli dan tren
plot(decomposition[, "Data"], type = "l", ylab = "Data Asli", main = "Data Asli dan Tren")
lines(decomposition[, "Trend"], col = "red")

# Analisis Pola Musiman Tahunan
# Menghitung rata-rata musiman untuk setiap bulan
monthly_seasonal_means <- aggregate(x = decomposition[, "Seasonal12"], by = list(Bulan = cycle(decomposition[, "Seasonal12"])), FUN = mean)
names(monthly_seasonal_means) <- c("Bulan", "Rata-rata Musiman")
plot(monthly_seasonal_means, type = "b", xlab = "Bulan", ylab = "Rata-rata Musiman", main = "Rata-rata Musiman per Bulan")

# Analisis Pola Musiman Kuartalan
# Menghitung rata-rata musiman untuk setiap kuartal
quarterly_seasonal_means <- aggregate(x = decomposition[, "Seasonal12"], by = list(Kuartal = (cycle(decomposition[, "Seasonal12"]) - 1) %/% 3 + 1), FUN = mean)
plot(quarterly_seasonal_means, type = "b", xlab = "Kuartal", ylab = "Rata-rata Musiman", main = "Rata-rata Musiman per Kuartal")


# Analisis Pola Musiman Bulanan
# Melihat plot musiman bulanan
month <- cycle(tsm)
boxplot(decomposition[, "Seasonal12"] ~ month, xlab = "Bulan", ylab = "Musiman", main = "Pola Musiman Bulanan")

# Bandingkan antara Musim
# Anda dapat membandingkan pola musiman tahunan, kuartalan, dan bulanan menggunakan plot yang relevan

# Analisis Anomali
# Perhatikan apakah ada anomali atau peristiwa tidak terduga dalam data yang mempengaruhi pola musiman

# Pemodelan dan Peramalan
# Setelah Anda memahami pola musiman dengan baik, Anda dapat menggunakan informasi tersebut untuk membangun model peramalan yang lebih baik





## HASIL PENGAMATAN GRAFIK MUSIMAN DATA TSM, TERAMATI SETIDAKNYA 2 POLA, YAKNI POLA 1 SEKITAR 2015-2018 DAN POLA 2 SEKITAR 2018-2023


# Pemisahan Data - Pola 1
data_pola1 <- window(tsx, start = c(2015), end = c(2017))

# Dekomposisi Data - Pola 1
decomposition_pola1 <- mstl(data_pola1)

# Plot komponen musiman - Pola 1
plot(decomposition_pola1[,3], main = "Komponen Musiman - Pola 1")

# Analisis Musiman - Pola 1
# Lakukan analisis lebih lanjut terhadap komponen musiman


# Pemisahan Data - Pola 2
data_pola2 <- window(tsm, start = c(2018), end = c(2023))

# Dekomposisi Data - Pola 2
decomposition_pola2 <- mstl(data_pola2)

# Plot komponen musiman - Pola 2
plot(decomposition_pola2[,3], main = "Komponen Musiman - Pola 2")

# Analisis Musiman - Pola 2
# Lakukan analisis lebih lanjut terhadap komponen musiman




# Menggunakan acf() tanpa plot
acf_result <- acf(tsx)
# Nilai kritis untuk tingkat signifikansi 0.05
critical_value <- qnorm(1 - 0.05/2)
# Mencari nilai lag di mana autokorelasi melebihi nilai kritis
significant_lags_acf <- which(acf_result$acf > critical_value)
print(significant_lags_acf)

# Menggunakan pacf() tanpa plot
pacf_result <- pacf(tsx)
# Nilai kritis untuk tingkat signifikansi 0.05
critical_value <- qnorm(1 - 0.05/2)
# Mencari nilai lag di mana autokorelasi parsial melebihi nilai kritis
significant_lags_pacf <- which(pacf_result$acf > critical_value)
print(significant_lags_pacf)


########################################################

# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

# Load data
df <- data.frame(tsb)
tm <- data.frame(date = time(tsb))

ts_data <- cbind(tm, df)
ts_data <- ts_data[, -c(2, 3, 10:15)]
ts_wide <- ts_data
ts_data_long <- pivot_longer(ts_data,
                             cols = -date,
                             names_to = 'var',
                             values_to = 'value'
)

ts_data <- ts_data_long

# View data structure
str(ts_data)

# Plot time series data
ggplot(ts_data, aes(x = date, y = value)) +
  geom_line() +
  labs(title = "Time Series Data",
       x = "Date",
       y = "Value")

# Plot distribution of data
ggplot(ts_data, aes(x = value)) +
  geom_histogram(binwidth = 100, fill = "blue", color = "black") +
  labs(title = "Distribution of Data",
       x = "Value",
       y = "Frequency")

# Create seasonal plot
ggplot(ts_data, aes(x = date, y = value)) +
  geom_line() +
  facet_wrap(~ format(date, "%Y"), ncol = 4) +
  labs(title = "Seasonal Plot",
       x = "Date",
       y = "Value")

# Create trend plot
ggplot(ts_data, aes(x = date, y = value)) +
  geom_line() +
  geom_smooth(method = "loess") +
  labs(title = "Trend Plot",
       x = "Date",
       y = "Value")

# Create boxplot
ggplot(ts_data, aes(x = "", y = value)) +
  geom_boxplot() +
  labs(title = "Outlier Boxplot",
       x = "",
       y = "Value")

# Create correlation plot
ts_cor <- ts_wide[, -1]
correlation_matrix <- cor(ts_cor)
# Melt the correlation matrix
correlation_melted <- melt(correlation_df)

# Plot heatmap using ggplot2
ggplot(correlation_melted, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  labs(x = "Variable", y = "Variable") +
  coord_fixed()

##------------------------------------------------------------------------------------------- Decompose START
# Assuming your data is already in a time series format, if not, convert it
# Convert to time series object
# 
# Convert ts_data into a time series object
ts_data_ts <- ts_data %>%
  spread(key = var, value = value)

# Extracting only the numeric columns
ts_data_ts <- ts_data_ts[, -1]

# Convert to time series
ts_data_ts <- ts(ts_data_ts, start = c(2015), frequency = 12)



# Decompose each variable individually using stl
decomposed_stl <- lapply(ts_data_ts, function(x) {
  stl_result <- stl(x, s.window = "periodic", robust = TRUE)
  return(list(seasonal = stl_result$time.series[, "seasonal"],
              trend = stl_result$time.series[, "trend"],
              remainder = stl_result$time.series[, "remainder"]))
})

# Print the decomposed components for each variable
for (i in seq_along(decomposed_stl)) {
  cat("Variable:", names(decomposed_stl)[i], "\n")
  print(decomposed_stl[[i]])
  cat("\n")
}


# Convert the decomposed components to data frames for plotting
plot_data <- lapply(decomposed_stl, function(decomposed) {
  df <- data.frame(
    date = time(ts_data_ts),
    seasonal = decomposed$seasonal,
    trend = decomposed$trend,
    remainder = decomposed$remainder
  )
  # Melt the data for easier plotting
  df_melted <- melt(df, id.vars = "date")
  return(df_melted)
})

# Plot each decomposed component for each variable
plots <- lapply(seq_along(plot_data), function(i) {
  ggplot(plot_data[[i]], aes(x = date, y = value, color = variable)) +
    geom_line() +
    labs(title = paste("Decomposed Components for", names(decomposed_stl)[i]),
         x = "Date", y = "Value") +
    theme_minimal()
})

# Print the plots
for (i in seq_along(plots)) {
  print(plots[[i]])
}

##------------------------------------------------------------------------------------------- Decompose END


# Check for autocorrelation
acf_plot <- acf(ts_data$value, lag.max = 20)
plot(acf_plot)

# Check for white noise
Box.test(ts_data$value, lag = 10, type = "Ljung-Box")


##########################################################

library(forecast)

timedf <- df_weekly[, 1]
datadf <- df_weekly[, 2]
datats <- ts_weekly[, 1]

autoarima <- forecast::auto.arima(datats)
autoplot(autoarima)

arima <- forecast::Arima(datats, order = c(2,2,3))
autoplot(arima)

y <- df_monthly[, 1]
linearts <- forecast::tslm(ts_weekly[, 1] ~ trend + season)
plot(forecast(linearts, h=100))

bclambda <- forecast::BoxCox.lambda(datats)
datatsBC <- forecast::BoxCox(datats, bclambda)
plot(datatsBC)

tsoo <- tsoutliers::tso(datats)
plot(tsoo)

tsd <- forecast::tsdisplay(datats)
plot.new()

deaths.model  <- auto.arima(datats, xreg=fourier(datats,K=5), seasonal=FALSE)
deaths.fcast <- forecast(deaths.model, xreg=fourier(datats, K=5, h=36))
autoplot(deaths.fcast) + xlab("Year")


p <- ggplot(aes(x=time(ts_weekly[,1]), y=ts_weekly[,1]), data=ts_weekly[,1])
p <- p + geom_line()
p + geom_forecast()


fit <- bats(datats)
plot(forecast(fit))

gghistogram(datats, add.kde=TRUE)

ggmonthplot(df_monthly[,1])

fit <- ets(df_monthly[,1])
plot(forecast(fit))

fit <- stlf(datats)
plot(forecast(fit))

findfrequency(df_monthly[,1])


fit <- tslm(df_monthly[, 3:8] ~ trend)
fcast <- forecast(fit, h=104)
plot(fcast)
autoplot(fcast)


fit <- auto.arima(df_monthly[,3:8])

ggtsbreaks <- function(x) {
  # Make x axis contain only whole numbers (e.g., years)
  return(unique(round(pretty(floor(x[1]):ceiling(x[2])))))
}
ggtsbreaks(datats)



# Misalkan 'data' adalah data waktu Anda
model <- auto.arima(datats)

# Mendeteksi breakpoints
breakpoints <- breakpoints(model)

# Menampilkan breakpoints
print(breakpoints)

###########################################################

# Data berpotensi memiliki pola musiman kompleks, atau lemah, atau bahkan tidak memiliki pola musiman, data sepertinya memiliki pola cyclical, dan tentunya tren. Oleh karena itu, model yang sepertinya tepat untuk membuat forecasting adalah model sederhana seperti Autoaggresion (AR) atau Moving Average (MA), atau Autoaggresion Moving Average (ARMA), gabungan keduanya. Model ini dapat dijalankan dengan mengatur parameter AR dan MA secara manual atau dengan otomatis. Pengaturan parameter secara manual membutuhkan serangkaian pengujian yang banyak sampai mendapatkan hasil terbaik dengan nilai-nilai parameter yang seimbang. Oleh karena itu, penentuan parameter dilakukan dengan otomatis dengan pendekatan Automatic Autoaggresion Moving Average (Auto ARIMA). Untuk memastikan hasil forcasting dapat diandalkan dan akurat, data terlebih dahulu dibersihkan dari outlier dengan melakukan interpolasi nilai pada outlier yang ditemukan.

## Prep data

ts_outliers <- tsoutliers(tsx)
tsxc <- tsclean(tsx)
tsmc <- tsclean(tsm)
tsdc <- tsclean(ts_daily[,1])

## Mencari nilai parameter terbaik

data <- tsxc
fit_ARIMA <- function(data, p, d, q) {
  model <- arima(data, order = c(p, d, q))
  return(AIC(model))
}

best_AR <- NULL
best_AIC_AR <- Inf
for (p in 0:3) {
  AIC_value <- fit_ARIMA(data, p, 0, 0)
  if (AIC_value < best_AIC_AR) {
    best_AR <- p
    best_AIC_AR <- AIC_value
  }
}

fit_MA <- function(data, p, d, q) {
  model <- arima(data, order = c(p, d, q))
  return(AIC(model))
}

best_MA <- NULL
best_AIC_MA <- Inf
for (q in 0:3) {
  AIC_value <- fit_ARIMA(data, 0, 0, q)
  if (AIC_value < best_AIC_MA) {
    best_MA <- q
    best_AIC_MA <- AIC_value
  }
}

result_arma <- data.frame(
  Parameter = c("AR", "MA", "AIC AR", "AIC MA"),
  Value = c(best_AR, best_MA, best_AIC_AR, best_AIC_MA)
)

fit_ARIMA <- function(data, p, d, q) {
  model <- arima(data, order = c(p, d, q))
  return(AIC(model))
}
best_p <- NULL
best_d <- NULL
best_q <- NULL
best_AIC <- Inf
for (p in 0:3) {
  for (d in 1:1) {
    for (q in 0:3) {
      if (p == 0 & q == 0)
        next
      AIC_value <- fit_ARIMA(data, p, d, q)
      if (AIC_value < best_AIC) {
        best_p <- p
        best_d <- d
        best_q <- q
        best_AIC <- AIC_value
      }
    }
  }
}

result_arima <- data.frame(
  Parameter = c("p", "d", "q", "AIC"),
  Value = c(best_p, best_d, best_q, best_AIC)
)

## Evaluasi model

data_length <- length(data)
train_length <- round(0.8 * data_length)
train_data <- data[1:train_length]
test_data <- data[(train_length + 1):data_length]

model_ar <- arima(train_data, order = c(3, 0, 0))
model_ma <- arima(train_data, order = c(0, 0, 3))
model_arima <- arima(train_data, order = c(2, 1, 2))
model_autoarima <- auto.arima(train_data)

calculate_mse <- function(actual, predicted) {mean((actual - predicted)^2)}
calculate_rmse <- function(actual, predicted) {sqrt(mean((actual - predicted)^2))}
calculate_mape <- function(actual, predicted) {mean(abs((actual - predicted)/actual)) * 100}
calculate_accuracy <- function(actual, predicted) {
  mean(min(abs(actual - predicted), abs(actual - predicted +1)))/mean(abs(actual))}
calculate_aic <- function(model) {AIC(model)}
calculate_bic <- function(model) {BIC(model)}

perform_cross_validation <- function(model, data, k = 5) {
  n <- length(data)
  fold_size <- ceiling(n/k)
  mse_cv <- rep(0, k)
  for (i in 1:k) {
    start <- (i - 1) * fold_size + 1
    end <- min(i * fold_size, n)
    train_indices <- c(1:(start - 1), (end + 1):n)
    train_data <- data[train_indices]
    val_data <- data[start:end]
    model_fit <- Arima(train_data, model = model)
    val_forecast <- forecast(model_fit, h = length(val_data))$mean
    mse_cv[i] <- mean((val_data - val_forecast)^2)
  }
  return(mean(mse_cv))
}

forecasts <- lapply(
  list(model_ar, model_ma, model_arima,
       model_autoarima), function(model) {
         forecast(model, h = length(test_data))$mean
       })

evaluations <- data.frame(
  Model = c("AR","MA", "ARIMA", "Auto ARIMA"), 
  MSE = sapply(forecasts,function(forecast) calculate_mse(test_data,forecast)), 
  RMSE = sapply(forecasts,function(forecast) calculate_rmse(test_data, forecast)), 
  MAPE = sapply(forecasts, function(forecast) calculate_mape(test_data, forecast)), 
  Forecast_Accuracy = sapply(forecasts, function(forecast) calculate_accuracy(test_data,forecast)), 
  AIC = c(calculate_aic(model_ar), calculate_aic(model_ma), calculate_aic(model_arima), 
          calculate_aic(model_autoarima)),
  BIC = c(calculate_bic(model_ar),calculate_bic(model_ma), calculate_bic(model_arima),
          calculate_bic(model_autoarima)),
  Cross_Validation = c(perform_cross_validation(model_ar,train_data), 
                       perform_cross_validation(model_ma, train_data), 
                       perform_cross_validation(model_arima, train_data), 
                       perform_cross_validation(model_autoarima,train_data)))
print(evaluations)


## Forecasting

data <- tsxc

model_ar <- arima(data, order = c(3, 0, 0))
model_ma <- arima(data, order = c(0, 0, 3))
model_arima <- arima(data, order = c(1, 1, 2))
model_autoarima <- auto.arima(data)

forecast_ar <- forecast(model_ar, h = 105)
forecast_ma <- forecast(model_ma, h = 105)
forecast_arima <- forecast(model_arima, h = 105)
forecast_autoarima <- forecast(model_autoarima, h = 105)

## Visualisai forecasting

plot.for <- function(model, sub) {
  p <- ggplot() +
    geom_ribbon(aes(x = index(model$mean), ymin = model$lower[, '95%'],
                    ymax = model$upper[, '95%']), fill = zcol[6], alpha = .3) +
    geom_ribbon(aes(x = index(model$mean), ymin = model$lower[, '80%'],
                    ymax = model$upper[, '80%']), fill = zcol[1], alpha = .3) +
    geom_line(aes(x = index(model$mean), y = model$mean), color = zcol[1], lwd = .3) +
    geom_line(data = data, aes(x = index(data), y = data), color = zcol[6], lwd = .3) +
    scale_x_continuous(breaks = seq(2015, 2026, 2)) +
    labs(title = sub, x = NULL, y = NULL)
  p
}

par <- plot.for(forecast_ar, 'AR (3,0,0)')
pam <- plot.for(forecast_ma, 'MA (0,0,3)')
parima <- plot.for(forecast_arima, 'ARIMA (1,1,2)')
paarima <- plot.for(forecast_autoarima, 'Auto ARIMA (3,1,0)(1,1,0)_0')

paarima / space / (par + space + pam + space + parima + plot_layout(width = c(5, .5, 5, .5, 5))) + plot_layout(height = c(5, .5, 5))

##############################################################

## ARMA

best_aic <- Inf
best_order <- c(0, 0)
for (p in 0:3) {
  for (q in 0:3) {
    arma_model <- arima(tsx, order = c(p, 0, q))
    aic <- AIC(arma_model)
    if (aic < best_aic) {
      best_aic <- aic
      best_order <- c(p, 0, q)
    }
  }
}
best_order

n <- length(tsx)
train_data <- tsx[1:(n * 0.8)] %>% log1p()
test_data <- tsx[(n * 0.8 + 1):n] %>% log1p()
evaluate_model <- function(p, d, q) {
  train_data_transformed <- train_data
  test_data_transformed <- test_data
  arma_model <- arima(train_data_transformed, order = c(p, d, q), method = "CSS")
  forecast_values_test <- forecast(arma_model, h = length(test_data_transformed))
  eval_metrics <- accuracy(forecast_values_test, test_data_transformed)
  return(eval_metrics[2, "RMSE"])
}

set.seed(123)
best_rmse <- Inf
best_order <- c(0, 0, 0)
p_range <- 0:3
d_range <- 0:1
q_range <- 0:3
for (p in p_range) {
  for (d in d_range) {
    for (q in q_range) {
      rmse <- evaluate_model(p, d, q)
      if (rmse < best_rmse) {
        best_rmse <- rmse
        best_order <- c(p, d, q)
      }
    }
  }
}
cat("Best order:", best_order, "\n")

#

arma_model <- arima(tsx, order = c(0, 1, 3), method = 'CSS')
forecast_arma <- forecast(arma_model, h = 104)

n <- length(tsx)
train_data <- tsx[1:(n * 0.8)] %>% log1p()
test_data <- tsx[(n * 0.8 + 1):n] %>% log1p()
arma_model_train <- arima(train_data, order = c(0, 1, 3), method = 'CSS')
forecast_values_test <- forecast(arma_model_train, h = length(test_data))
acc <- accuracy(forecast_values_test, test_data)

arima_model_train <- auto.arima(train_data)
forecast_values_test <- forecast(arima_model_train, h = length(test_data))
accuracy(forecast_values_test, test_data)



forecast_df_arma <- data.frame(
  Date = index(forecast_arma$mean),
  Forecast = forecast_arma$mean,
  Lower = forecast_arma$lower,
  Upper = forecast_arma$upper
)

parma <- ggplot() +
  geom_line(data = forecast_df_arma, aes(x = Date, y = Forecast, color = "Forecast"), 
            linetype = "solid", lwd = .5) +
  geom_ribbon(data = forecast_df_arma, 
              aes(x = Date, ymin = Lower.80., ymax = Upper.80., fill = "Interval"), 
              alpha = 0.3) +
  geom_line(data = tsx, aes(x = time(tsx), y = tsx, color = 'Observed'), lwd = .5) +
  labs(title = "Forecasting with ARIMA", x = "Year", y = "Events") +
  scale_color_manual(name = NULL, values = c("Forecast" = zcol[1], 'Observed' = zcol[6])) +
  scale_fill_manual(name = NULL, values = zcol[6]) +
  scale_x_continuous(breaks = seq(2015, 2026, 2)) +
  theme(legend.position = 'top', legend.justification = 'right')

# AUTO ARIMA

arima_model <- auto.arima(tsx, seasonal = TRUE)
forecast_result <- forecast(arima_model, h = 104)

forecast_df <- data.frame(
  Date = index(forecast_result$mean),
  Forecast = forecast_result$mean,
  Lower = forecast_result$lower,
  Upper = forecast_result$upper
)

parima <- ggplot() +
  geom_line(data = forecast_df, aes(x = Date, y = Forecast, color = "Forecast"), 
            linetype = "solid", lwd = .5) +
  geom_ribbon(data = forecast_df, 
              aes(x = Date, ymin = Lower.80., ymax = Upper.80., fill = "Interval"), 
              alpha = 0.3) +
  geom_line(data = tsx, aes(x = time(tsx), y = tsx, color = 'Observed'), lwd = .5) +
  labs(title = "Forecasting with ARIMA", x = "Year", y = "Events") +
  scale_color_manual(name = NULL, values = c("Forecast" = zcol[1], 'Observed' = zcol[6])) +
  scale_fill_manual(name = NULL, values = zcol[6]) +
  scale_x_continuous(breaks = seq(2015, 2026, 2)) +
  theme(legend.position = 'top', legend.justification = 'right')

# Cross Validation
n <- length(tsx)
train_data <- tsx[1:(n * 0.8)] %>% log1p()
test_data <- tsx[(n * 0.8 + 1):n] %>% log1p()
evaluate_model <- function(p, d, q) {
  train_data_transformed <- train_data
  test_data_transformed <- test_data
  arma_model <- arima(train_data_transformed, order = c(p, d, q), method = "CSS")
  forecast_values_test <- forecast(arma_model, h = length(test_data_transformed))
  eval_metrics <- accuracy(forecast_values_test, test_data_transformed)
  return(eval_metrics[2, "RMSE"])
}



## PRPOPHET
## 
## 
# set.seed(123) # Untuk memastikan hasil yang sama setiap kali dijalankan
# 
# train_index <- createDataPartition(dfp$y, p = 0.8, list = FALSE)
# 
# train_data <- dfp[train_index, ]
# 
# test_data <- dfp[-train_index, ]
# 
# prophet_model_train <- prophet(train_data)
# future <- make_future_dataframe(prophet_model_train, periods = length(test_data))
# forecast_values_test <- forecast(prophet_model_train, h = length(test_data))
# 
# acc <- accuracy(forecast_values_test, test_data)

## 
## 

tsdf <- df_weekly
cols_to_transform <- names(tsdf)[-1] 
transformed.df <- tsdf
transformed.df[, cols_to_transform] <- lapply(tsdf[, cols_to_transform], log1p)
dfp <- data.frame(ds = as.Date(transformed.df$Date), y = transformed.df$EVENT)

m <- prophet(dfp)
future <- make_future_dataframe(m, periods = 730)
forecast <- predict(m, future)

df_for_plotting <- function(m, fcst) {
  # Make sure there is no y in fcst
  fcst$y <- NULL
  df <- m$history %>%
    dplyr::select(ds, y) %>%
    dplyr::full_join(fcst, by = "ds") %>%
    dplyr::arrange(ds)
  return(df)
}

plot.prophet <- function(x, fcst, uncertainty = TRUE, plot_cap = TRUE,
                         xlabel = 'ds', ylabel = 'y', ...) {
  df <- df_for_plotting(x, fcst)
  gg <- ggplot(df, aes(x = ds, y = y)) +
    labs(x = xlabel, y = ylabel)
  if (exists('cap', where = df) && plot_cap) {
    gg <- gg + geom_line(
      aes(y = cap), linetype = 'dashed', na.rm = TRUE)
  }
  if (x$logistic.floor && exists('floor', where = df) && plot_cap) {
    gg <- gg + geom_line(
      aes(y = floor), linetype = 'dashed', na.rm = TRUE)
  }
  if (uncertainty && x$uncertainty.samples && exists('yhat_lower', where = df)) {
    gg <- gg +
      geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper),
                  alpha = 0.2,
                  fill = zcol[3],
                  na.rm = TRUE)
  }
  gg <- gg +
    geom_point(na.rm=TRUE, color = zcol[6]) +
    geom_line(aes(y = yhat), color = zcol[3],
              na.rm = TRUE) +
    labs(x = NULL, y = NULL, caption = 'Prophet Model')
  return(gg)
}

ppropht <- plot.prophet(m, forecast)

###################################################################
# Split data into training and testing sets
set.seed(123) # Set seed for reproducibility
train_index <- createDataPartition(dfp$y, p = 0.8, list = FALSE)
train_data <- dfp[train_index, ]
test_data <- dfp[-train_index, ]

# Fit prophet model
prophet_model <- prophet(train_data)

# Make future predictions
future_forecast <- make_future_dataframe(prophet_model, periods = nrow(test_data))
forecast_prophet <- predict(prophet_model, future_forecast)

# Calculate accuracy
accuracy_prophet <- accuracy(forecast_prophet$yhat, test_data$y)

# Print accuracy
print(accuracy_prophet)

# Calculate accuracy for training set
accuracy_prophet_train <- accuracy(forecast_prophet$yhat[1:nrow(train_data)], train_data$y)

# Print accuracy for training set
print(accuracy_prophet_train)




arma_model_train <- arima(train_data, order = c(0, 1, 3),
                          seasonal = list(order = c(0, 1, 12),
                                          method = 'CSS'))
forecast_values_test <- forecast(arma_model_train,
                                 h = length(test_data))
acc <- accuracy(forecast_values_test, test_data)



arma_model <- arima(train_data, order = c(0, 1, 3), 
                    seasonal = list(order = c(0, 1, 12), 
                                    method = 'CSS'))
forecast_arma <- forecast(arma_model, h = 104)
plot(forecast_arma)


median_val <- median(train_data)
train_data_filtered <- train_data[train_data < median_val * 1.5, ]







arma_model_train <- arima(train_data, order = c(0, 1, 3), seasonal = list(order = c(0, 1, 12), period = 12, model = "additive", method = 'CSS'))
forecast_values_test <- forecast(arma_model_train, h = length(test_data))
acc <- accuracy(forecast_values_test, test_data)










##---------------------------------------------------

lambda <- BoxCox.lambda(tsx, method = 'loglik')
data <- BoxCox(tsx, lambda)

n <- length(data)
train_data <- data[1:(n * 0.8)]
test_data <- data[(n * 0.8 + 1):n]

arima_a <- arima(train_data, 
                 order = c(0, 1, 3),
                 method = 'CSS')
forcst_a <- forecast(arima_a, h = length(test_data))
acc_a <- accuracy(forcst_a, test_data)

arima_b <- arima(train_data,
                 order = c(0, 0, 0),
                 seasonal = list(order = c(1, 0, 6),
                                 period = 6),
                 method = 'CSS')
forcst_b <- forecast(arima_b, h = length(test_data))
acc_b <- accuracy(forcst_b, test_data)

arima_fin <- arima(tsx,
                   order = c(0, 0, 0),
                   seasonal = list(order = c(1, 0, 6),
                                   period = 6),
                   method = 'CSS')
forecast_fin <- forecast(arima_fin, h = 104)
acc_c <- accuracy(forecast_fin)
plot(forecast_fin)

autoarima <- auto.arima(train_data, seasonal = TRUE)
forecast_autoarima <- forecast(autoarima, h = length(test_data))
acc_d <- accuracy(forecast_autoarima, test_data)

##---------------------------------------------------


evaluate_model <- function(p, d, q) {
  train_data_transformed <- train_data
  test_data_transformed <- test_data
  arma_model <- arima(train_data_transformed, 
                      order = c(p, d, q), 
                      seasonal = list(order = c(0, 1, 12),
                                      period = 12),
                      method = "CSS")
  forecast_values_test <- forecast(arma_model, h = length(test_data_transformed))
  eval_metrics <- accuracy(forecast_values_test, test_data_transformed)
  return(eval_metrics[2, "RMSE"])
}

set.seed(123)
best_rmse <- Inf
best_order <- c(0, 0, 0)
p_range <- 0:3
d_range <- 0:1
q_range <- 0:3
for (p in p_range) {
  for (d in d_range) {
    for (q in q_range) {
      rmse <- evaluate_model(p, d, q)
      if (rmse < best_rmse) {
        best_rmse <- rmse
        best_order <- c(p, d, q)
      }
    }
  }
}
cat("Best order:", best_order, "\n")




# Load the time series data

order_grid <- c(0, 1, 2)
period_grid <- c(12)

best_aic <- Inf
best_model <- NULL

for (order in order_grid) {
  for (period in period_grid) {
    model <- arima(train_data, 
                   order = c(order, 1, 3), 
                   seasonal = list(order = c(0, 1, period),
                                   period = period))
    aic <- AIC(model)
    if (aic < best_aic) {
      best_aic <- aic
      best_model <- model
    }
  }
}

# Print the best
print(best_model)





evaluate_model <- function(p, d, q) {
  train_data_transformed <- train_data
  test_data_transformed <- test_data
  arma_model <- arima(train_data_transformed, order = c(p, d, q), method = "CSS")
  forecast_values_test <- forecast(arma_model, h = length(test_data_transformed))
  eval_metrics <- accuracy(forecast_values_test, test_data_transformed)
  rmse <- eval_metrics[2, "RMSE"]
  mape <- eval_metrics[2, "MAPE"]
  return(list(RMSE = rmse, MAPE = mape))
}
for (p in p_range) {
  for (d in d_range) {
    for (q in q_range) {
      metrics <- evaluate_model(p, d, q)
      rmse <- metrics$RMSE
      mape <- metrics$MAPE
      if (rmse < best_rmse) {
        best_rmse <- rmse
        best_mape <- mape
        best_order <- c(p, d, q)
      }
    }
  }
}
cat("Best order:", best_order, "\n") # 013
cat("Best RMSE:", best_rmse, "\n")
cat("Best MAPE:", best_mape, "\n")


evaluate_model <- function(p, d, q, x, y, z) {
  train_data_transformed <- train_data
  test_data_transformed <- test_data
  arma_model <- arima(train_data_transformed, 
                      order = c(p, d, q), 
                      seasonal = list(order = c(x, y, z),
                                      period = g),
                      method = "CSS")
  forecast_values_test <- forecast(arma_model, h = length(test_data_transformed))
  eval_metrics <- accuracy(forecast_values_test, test_data_transformed)
  rmse <- eval_metrics[2, "RMSE"]
  mape <- eval_metrics[2, "MAPE"]
  return(list(RMSE = rmse, MAPE = mape))
}

set.seed(123)
best_rmse <- Inf
best_order <- c(0, 0, 0)
best_seasonal <- c(0, 0, 0)
p_range <- 0:3
d_range <- 0:1
q_range <- 0:3
x_range <- 0:1  # Parameter musiman x
y_range <- 0:1  # Parameter musiman y
z_range <- c(4, 6, 12, 24, 52)  # Periode musiman z
g_range <- c(4, 6, 12, 24, 52)  # Rentang nilai untuk periode
for (p in p_range) {
  for (d in d_range) {
    for (q in q_range) {
      for (x in x_range) {
        for (y in y_range) {
          for (z in z_range) {
            metrics <- evaluate_model(p, d, q, x, y, z)
            rmse <- metrics$RMSE
            mape <- metrics$MAPE
            if (rmse < best_rmse) {
              best_rmse <- rmse
              best_mape <- mape
              best_order <- c(p, d, q)
              best_seasonal <- c(x, y, z)
            }
          }
        }
      }
    }
  }
}
cat("Best order:", best_order, "\n")
cat("Best seasonal:", best_seasonal, "\n")
cat("Best RMSE:", best_rmse, "\n")
cat("Best MAPE:", best_mape, "\n")
# > cat("Best order:", best_order, "\n")
# Best order: 0 0 0 
# > cat("Best seasonal:", best_seasonal, "\n")
# Best seasonal: 1 0 6 
# > cat("Best RMSE:", best_rmse, "\n")
# Best RMSE: 0.1108357 
# > cat("Best MAPE:", best_mape, "\n")
# Best MAPE: 6.761925 


############################################################







