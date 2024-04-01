data_evt_fat <- acled %>%
  mutate(
    EVENT_DATE = as.Date(EVENT_DATE), 
    MONTH = floor_date(EVENT_DATE, "month")) %>%
  group_by(EVENT_DATE, YEAR, MONTH) %>%
  summarize(
    ETOT = n(),
    FTOT = sum(FATALITIES),
    EBAT = sum(EVENT_TYPE == 'Battles'),
    EERV = sum(EVENT_TYPE == 'Explosions/Remote violence'),
    EPRT = sum(EVENT_TYPE == 'Protests'),
    ERTS = sum(EVENT_TYPE == 'Riots'),
    ESTR = sum(EVENT_TYPE == 'Strategic developments'),
    EVAC = sum(EVENT_TYPE == 'Violence against civilians'),
    FBAT = sum(FATALITIES * (EVENT_TYPE == 'Battles')),
    FERV = sum(FATALITIES * (EVENT_TYPE == 'Explosions/Remote violence')),
    FPRT = sum(FATALITIES * (EVENT_TYPE == 'Protests')),
    FRTS = sum(FATALITIES * (EVENT_TYPE == 'Riots')),
    FSTR = sum(FATALITIES * (EVENT_TYPE == 'Strategic developments')),
    FVAC = sum(FATALITIES * (EVENT_TYPE == 'Violence against civilians')),
    .groups = "drop_last"
  ) %>%
  arrange(MONTH)

data_monthly_type <- acled %>%
  mutate(EVENT_DATE = as.Date(EVENT_DATE), 
         MONTH = floor_date(EVENT_DATE, 'month')) %>%
  group_by(MONTH, EVENT_TYPE) %>%
  summarize(
    ETOT = n(), 
    FTOT = sum(FATALITIES),
    EBAT = sum(EVENT_TYPE == 'Battles'),
    EERV = sum(EVENT_TYPE == 'Explosions/Remote violence'),
    EPRT = sum(EVENT_TYPE == 'Protests'),
    ERTS = sum(EVENT_TYPE == 'Riots'),
    ESTR = sum(EVENT_TYPE == 'Strategic developments'),
    EVAC = sum(EVENT_TYPE == 'Violence against civilians'),
    FBAT = sum(FATALITIES * (EVENT_TYPE == 'Battles')),
    FERV = sum(FATALITIES * (EVENT_TYPE == 'Explosions/Remote violence')),
    FPRT = sum(FATALITIES * (EVENT_TYPE == 'Protests')),
    FRTS = sum(FATALITIES * (EVENT_TYPE == 'Riots')),
    FSTR = sum(FATALITIES * (EVENT_TYPE == 'Strategic developments')),
    FVAC = sum(FATALITIES * (EVENT_TYPE == 'Violence against civilians')),
    .groups = 'drop_last') %>%
  arrange(MONTH)

data_monthly <- acled %>%
  mutate(EVENT_DATE = as.Date(EVENT_DATE),
         MONTH = floor_date(EVENT_DATE, "month")) %>%
  group_by(MONTH) %>%
  summarize(
    ETOT = n(),
    FTOT = sum(FATALITIES),
    EBAT = sum(EVENT_TYPE == 'Battles'),
    EERV = sum(EVENT_TYPE == 'Explosions/Remote violence'),
    EPRT = sum(EVENT_TYPE == 'Protests'),
    ERTS = sum(EVENT_TYPE == 'Riots'),
    ESTR = sum(EVENT_TYPE == 'Strategic developments'),
    EVAC = sum(EVENT_TYPE == 'Violence against civilians'),
    FBAT = sum(FATALITIES * (EVENT_TYPE == 'Battles')),
    FERV = sum(FATALITIES * (EVENT_TYPE == 'Explosions/Remote violence')),
    FPRT = sum(FATALITIES * (EVENT_TYPE == 'Protests')),
    FRTS = sum(FATALITIES * (EVENT_TYPE == 'Riots')),
    FSTR = sum(FATALITIES * (EVENT_TYPE == 'Strategic developments')),
    FVAC = sum(FATALITIES * (EVENT_TYPE == 'Violence against civilians'))
  ) %>%
  arrange(MONTH)
