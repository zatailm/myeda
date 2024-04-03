##----load_data---------------------------------------------
acled <- read_excel('data/IDN2015-2023.xlsx')
dataraw <- acled
acled <- acled[,-c(1,4,5,9,12,15:18,25:27,30,31)]
idn1 <- readRDS('data/IDN38.rds')
# idn1 <- st_read('data/38.shp')
prvnc <- st_as_sf(idn1)

##----list---------------------------------------------
inter <- c(
  '1' = 'State Forces', '2' = 'Rebel Groups', 
  '3' = 'Political Militias', '4' = 'Identity Militias', '5' = 'Rioters',
  '6' = 'Protesters', '7' = 'Civilians', '8' = 'External/Other Forces')
interaction <- c(
  '10' = 'Sole State Forces Action', '11' = 'State Forces Vs State Forces',
  '12' = 'State Forces Vs Rebels', '13' = 'State Forces Vs Political Militia',
  '14' = 'State Forces Vs Identity Militia', '15' = 'State Forces Vs Rioters',
  '16' = 'State Forces Vs Protesters', '17' = 'State Forces Vs Civilians',
  '18' = 'Military Vs External/Other Forces', '20' = 'Sole Rebel Action ',
  '22' = 'Rebels Vs Rebels', '23' = 'Rebels Vs Political Miliita',
  '24' = 'Rebels Vs Identity Militia', '25' = 'Rebels Vs Rioters',
  '26' = 'Rebels Vs Protesters', '27' = 'Rebels Vs Civilians',
  '28' = 'Rebels Vs Others', '30' = 'Sole Political Militia Action',
  '33' = 'Political Militia Vs Political Militia',
  '34' = 'Political Militia Vs Identity Militia',
  '35' = 'Political Militia Vs Rioters', '36' = 'Political Militia Vs Protesters',
  '37' = 'Political Militia Vs Civilians', '38' = 'Political Militia Vs Others',
  '40' = 'Sole Identity Militia Action', '44' = 'Identity Militia Vs Identity Militia',
  '45' = 'Identity Militia Vs Rioters', '46' = 'Identity Militia Vs Protesters',
  '47' = 'Identity Militia Vs Civilians', '48' = 'Identity Militia Vs Other',
  '50' = 'Sole Rioter Action', '55' = 'Rioters Vs Rioters',
  '56' = 'Rioters Vs Protesters', '57' = 'Rioters Vs Civilians',
  '58' = 'Rioters Vs Others', '60' = 'Sole Protester Action',
  '66' = 'Protesters Vs Protesters', '67' = 'Protesters Vs Civilians',
  '68' = 'Protesters Vs Other', '70' = 'Sole Civilians',
  '77' = 'Civilians Vs Civilians', '78' = 'Other Actor Vs Civilians',
  '80' = 'Sole Other Action', '88' = 'Others Vs Others')
adm_id <- c(
  'Aceh'='Aceh', 'Bali'='Bali', 'Bangka-Belitung'='Babel', 'Banten'='Banten',
  'Bengkulu'='Bengkulu', 'Central Java'='Jateng', 'Central Kalimantan'='Kalteng',
  'Central Papua'='Papteng', 'Central Sulawesi'='Sulteng', 'East Java'='Jatim',
  'East Kalimantan'='Kaltim', 'East Nusa Tenggara'='NTT', 'Gorontalo'='Gorontalo',
  'Highland Papua'='Papeg', 'Jakarta'='Jakarta', 'Jambi'='Jambi', 'Lampung'='Lampung',
  'Maluku'='Maluku', 'North Kalimantan'='Kaltara', 'North Maluku'='Malut',
  'North Sulawesi'='Sulut', 'North Sumatra'='Sumut', 'Papua'='Papua', 'Riau'='Riau', 
  'Riau Islands'='Kepri', 'Southeast Sulawesi'='Sultra', 'South Kalimantan'='Kalsel', 
  'South Papua'='Pasel', 'South Sulawesi'='Sulsel', 'South Sumatra'='Sumsel', 
  'Southwest Papua'='PBD', 'West Java'='Jabar', 'West Kalimantan'='Kalbar', 
  'West Nusa Tenggara'='NTB', 'West Papua'='Pabar', 'West Sulawesi'='Sulbar', 
  'West Sumatra'='Sumbar', 'Yogyakarta'='DIY')
adm_abr <- c(
  'Aceh'='AC', 'Bali'='BA', 'Bangka-Belitung'='BB', 'Banten'='BT',
  'Bengkulu'='BE', 'Central Java'='JT', 'Central Kalimantan'='KT',
  'Central Papua'='PT', 'Central Sulawesi'='ST', 'East Java'='JI',
  'East Kalimantan'='KI', 'East Nusa Tenggara'='NT', 'Gorontalo'='GO',
  'Highland Papua'='PE', 'Jakarta'='JK', 'Jambi'='JA', 'Lampung'='LA',
  'Maluku'='MA', 'North Kalimantan'='KU', 'North Maluku'='MU',
  'North Sulawesi'='SA', 'North Sumatra'='SU', 'Papua'='PA',
  'Riau'='RI', 'Riau Islands'='KR', 'Southeast Sulawesi'='SG',
  'South Kalimantan'='KS', 'South Papua'='PS', 'South Sulawesi'='SN',
  'South Sumatra'='SS', 'Southwest Papua'='PD', 'West Java'='JB',
  'West Kalimantan'='KB', 'West Nusa Tenggara'='NB', 'West Papua'='PB',
  'West Sulawesi'='SR', 'West Sumatra'='SB', 'Yogyakarta'='YO')
evt_abr <- c(
  'Battles' = 'Battles', 'Explosions/Remote violence' = 'ERV', 'Protests'='Protests', 
  'Riots'='Riots','Strategic developments' = 'Str.Dev.', 
  'Violence against civilians' = 'VAC')
l <- list(
  pro = 'Admin 1', kab = 'Admin 2', kec = 'Admin 3', 
  tot = 'Number of Total', frq = 'Frequency', frk = 'Freq.', tip = 'Event Types', 
  int = 'Type of Interactions', bln = 'Month', knf = 'Event Types', krb = 'Fatalities', 
  akt = 'Actors', ak1 = 'Actor 1', ak2 = 'Actor 2', thn = 'Year', 
  bat = 'Battles', prt = 'Protests', erv = 'ERV', rts = 'Riots',
  dev = 'Str.Dev.', vac = 'VAC', ervl = 'Explosions/Remote violence', 
  devl = 'Strategic developments', vacl = 'Violence against civilians', 
  nn = '', jml = 'Number of Events', pdt = 'Density', dis = 'Distribution', 
  vrb = 'Variables', kor = 'Correlation', prs = 'Percent', stp = 'Sub Event Types', 
  evt = 'Events', prab = 'Province (Abbreviation)')
l <- l[c(
  'pro', 'kab', 'kec', 'tot', 'frq', 'frk', 'tip', 'int', 'bln', 'knf', 'krb', 
  'akt', 'ak1', 'ak2', 'thn', 'bat', 'prt', 'erv', 'rts', 'dev', 'vac', 'ervl', 
  'devl', 'vacl', 'nn', 'jml', 'pdt', 'dis', 'vrb', 'kor', 'prs', 'stp', 'evt',
  'prab')]

##----preprocessing--------------------------------------
acled <- acled %>%
  mutate(across(c(ADMIN2, ADMIN3, ACTOR2), ~ ifelse(is.na(.), NA, .))) %>%
  mutate(across(c(INTER1, INTER2), ~ ifelse(. == 0, NA, as.character(.)))) %>%
  mutate(across(c(ACTOR1, ACTOR2), ~ str_replace_all(., ' \\s*\\([^\\)]+\\)', ''))) %>%
  mutate(across(c(INTER1, INTER2), ~ recode(as.character(.), !!!inter))) %>%
  mutate(INTERACTION = recode(as.character(INTERACTION), !!!interaction)) %>%
  mutate(ADMIN1_ABR = recode(as.character(ADMIN1), !!!adm_abr)) %>%
  mutate(EVENT_TYPE_SRT = recode(as.character(EVENT_TYPE), !!!evt_abr)) %>%
  mutate(LONGITUDE = as.numeric(LONGITUDE), LATITUDE = as.numeric(LATITUDE),
         FATALITIES = as.integer(FATALITIES)) %>%
  mutate(CMONTH = (year(ymd(EVENT_DATE)) - min(year(ymd(EVENT_DATE)))) * 12 + 
           month(ymd(EVENT_DATE)) - min(month(ymd(EVENT_DATE))) + 1) %>%
  mutate(ADMINID = recode(as.character(ADMIN1), !!!adm_id)) %>%
  mutate(MONTH = month(EVENT_DATE))
