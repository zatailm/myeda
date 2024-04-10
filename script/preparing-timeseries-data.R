
# calculate frequencies -----------------------------------------------------------------------

df_timeseries <- acled[, c('EVENT_DATE', 'EVENT_TYPE', 'FATALITIES')] %>%
  rename(DATE = EVENT_DATE) %>%
  group_by(DATE) %>%
  summarise(
    EVENT = n(),  
    FATAL = sum(FATALITIES, na.rm = TRUE),  
    EBAT = sum(EVENT_TYPE == 'Battles', na.rm = TRUE),
    EERV = sum(EVENT_TYPE == 'Explosions/Remote violence', na.rm = TRUE),
    EPRT = sum(EVENT_TYPE == 'Protests', na.rm = TRUE),
    ERTS = sum(EVENT_TYPE == 'Riots', na.rm = TRUE),
    ESTR = sum(EVENT_TYPE == 'Strategic developments', na.rm = TRUE),
    EVAC = sum(EVENT_TYPE == 'Violence against civilians', na.rm = TRUE),
    FBAT = sum(FATALITIES * (EVENT_TYPE == 'Battles'), na.rm = TRUE),
    FERV = sum(FATALITIES * (EVENT_TYPE == 'Explosions/Remote violence'), na.rm = TRUE),
    FPRT = sum(FATALITIES * (EVENT_TYPE == 'Protests'), na.rm = TRUE),
    FRTS = sum(FATALITIES * (EVENT_TYPE == 'Riots'), na.rm = TRUE),
    FSTR = sum(FATALITIES * (EVENT_TYPE == 'Strategic developments'), na.rm = TRUE),
    FVAC = sum(FATALITIES * (EVENT_TYPE == 'Violence against civilians'), na.rm = TRUE),
    .groups = 'drop_last') %>%
  mutate(DATE = as.Date(DATE))

# NOTE: Pengisian baris yang hilang dan interpolasi tidak diperlukan karena analisis akan menggunakan data mingguan, bukan harian.

# convert to xts object -----------------------------------------------------------------------

xts_timeseries  <- xts(df_timeseries[, -1], order.by = df_timeseries$DATE)
xts_daily     <- apply.daily(xts_timeseries, FUN = colMeans)
xts_weekly    <- apply.weekly(xts_timeseries, FUN = colMeans) 
xts_monthly   <- apply.monthly(xts_timeseries, FUN = colMeans)
xts_quarterly <- apply.quarterly(xts_timeseries, FUN = colMeans)
xts_yearly    <- apply.yearly(xts_timeseries, FUN = colMeans)

# convert to data frame -----------------------------------------------------------------------

df_daily      <- data.frame(Date = index(xts_daily), coredata(xts_daily))
df_weekly     <- data.frame(Date = index(xts_weekly), coredata(xts_weekly))
df_monthly    <- data.frame(Date = index(xts_monthly), coredata(xts_monthly))
df_quarterly  <- data.frame(Date = index(xts_quarterly), coredata(xts_quarterly))
df_yearly     <- data.frame(Date = index(xts_yearly), coredata(xts_yearly))

# convert to ts object ------------------------------------------------------------------------

ts_daily      <- ts(xts_daily, start = c(2015, 1), end = c(2023, 365), frequency = 365)
ts_weekly     <- ts(xts_weekly, start = c(2015, 1), frequency = 52)
ts_monthly    <- ts(xts_monthly, start = c(2015, 1), end = c(2023, 12), frequency = 12)
ts_quarterly  <- ts(xts_quarterly, start = c(2015, 1), end = c(2023, 4), frequency = 4)
ts_yearly     <- ts(xts_yearly, start = c(2015, 1), end = c(2023, 1), frequency = 1)

ts_daily_log      <- do.call(ts.union, lapply(ts_daily, function(x) log1p(x)))
ts_weekly_log     <- do.call(ts.union, lapply(ts_weekly, function(x) log1p(x)))
ts_monthly_log    <- do.call(ts.union, lapply(ts_monthly, function(x) log1p(x)))
ts_quarterly_log  <- do.call(ts.union, lapply(ts_quarterly, function(x) log1p(x)))
ts_yearly_log     <- do.call(ts.union, lapply(ts_yearly, function(x) log1p(x)))

# assigning for lazy me :D --------------------------------------------------------------------

tsd <- ts_daily[,1]
tsx <- ts_weekly_log[,1]
tsm <- ts_monthly_log[,1]
tsw <- ts_weekly_log
tsb <- ts_monthly_log
