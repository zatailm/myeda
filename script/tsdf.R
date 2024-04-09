# Menghitung frekuensi
dtdum <- acled[, c('EVENT_DATE', 'EVENT_TYPE', 'FATALITIES')] %>%
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

# Membuat data frame xts dan transfoemasi
dtdumxts  <- xts(dtdum[, -1], order.by = dtdum$DATE)
daily     <- apply.daily(dtdumxts, FUN = colMeans)
weekly    <- apply.weekly(dtdumxts, FUN = colMeans) 
monthly   <- apply.monthly(dtdumxts, FUN = colMeans)
quarterly <- apply.quarterly(dtdumxts, FUN = colMeans)
yearly    <- apply.yearly(dtdumxts, FUN = colMeans)

# Ubah ke data frame untuk analisis tertentu
df_daily      <- data.frame(Date = index(daily), coredata(daily))
df_weekly     <- data.frame(Date = index(weekly), coredata(weekly))
monthly.df    <- data.frame(Date = index(monthly), coredata(monthly))
df_quarterly  <- data.frame(Date = index(quarterly), coredata(quarterly))
df_yearly     <- data.frame(Date = index(yearly), coredata(yearly))

# Konversi ke objek ts
ts_daily      <- ts(daily, start = c(2015, 1), end = c(2023, 365), frequency = 365)
ts_weekly     <- ts(weekly, start = c(2015, 1), frequency = 52)
df_monthly    <- ts(monthly, start = c(2015, 1), end = c(2023, 12), frequency = 12)
quarterly.ts  <- ts(quarterly, start = c(2015, 1), end = c(2023, 4), frequency = 4)
yearly.ts     <- ts(yearly, start = c(2015, 1), end = c(2023, 1), frequency = 1)

ts_daily.log    <- do.call(ts.union, lapply(ts_daily, function(x) log1p(x)))
ts_weekly.log   <- do.call(ts.union, lapply(ts_weekly, function(x) log1p(x)))
df_monthly.log  <- do.call(ts.union, lapply(df_monthly, function(x) log1p(x)))
quarterly.ts.log  <- do.call(ts.union, lapply(quarterly.ts, function(x) log1p(x)))
yearly.ts.log  <- do.call(ts.union, lapply(yearly.ts, function(x) log1p(x)))

weekly_event  <- df_weekly[, c('Date', 'n.EVENT')] %>% 
  rename(Events = n.EVENT) %>% as_tbl_time(index = Date)
ts_weekly.event <- ts_weekly[, 'EVENT']
