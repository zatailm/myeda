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
daily.df      <- data.frame(Date = index(daily), coredata(daily))
weekly.df     <- data.frame(Date = index(weekly), coredata(weekly))
monthly.df    <- data.frame(Date = index(monthly), coredata(monthly))
quarterly.df  <- data.frame(Date = index(quarterly), coredata(quarterly))
yearly.df     <- data.frame(Date = index(yearly), coredata(yearly))

# Konversi ke objek ts
daily.ts      <- ts(daily, start = c(2015, 1), end = c(2023, 365), frequency = 365)
weekly.ts     <- ts(weekly, start = c(2015, 1), frequency = 52)
monthly.ts    <- ts(monthly, start = c(2015, 1), end = c(2023, 12), frequency = 12)
quarterly.ts  <- ts(quarterly, start = c(2015, 1), end = c(2023, 4), frequency = 4)
yearly.ts     <- ts(yearly, start = c(2015, 1), end = c(2023, 1), frequency = 1)

daily.ts.log    <- do.call(ts.union, lapply(daily.ts, function(x) log1p(x)))
weekly.ts.log   <- do.call(ts.union, lapply(weekly.ts, function(x) log1p(x)))
monthly.ts.log  <- do.call(ts.union, lapply(monthly.ts, function(x) log1p(x)))
quarterly.ts.log  <- do.call(ts.union, lapply(quarterly.ts, function(x) log1p(x)))
yearly.ts.log  <- do.call(ts.union, lapply(yearly.ts, function(x) log1p(x)))

# weekly.df.log <- do.call(lapply)
tsd <- daily.ts[,1]
tsx <- weekly.ts.log[,1]
tsm <- monthly.ts.log[,1]
tsw <- weekly.ts.log
tsb <- monthly.ts.log
