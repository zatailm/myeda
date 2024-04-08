
layout_matrix <- matrix(c(1:6), nrow = 2, byrow = TRUE)

layout(mat = layout_matrix,
       heights = c(.25, 3),     # Heights of the two rows
       widths = c(3, 3, 3))     # Widths of the three columns

# layout.show(12)

uninterest_words <- c(
  stopwords('en'),
  'january', 'february', 'march', 'april', 'may', 'june',
  'july', 'august', 'september', 'october', 'november', 'december',
  'province', 'held', 'north', 'south', 'west', 'east', 'front',
  'district'
)

par(mar=rep(0, 4))
plot.new()
text(x = 0.5, y = 0.5, "September 2019")
par(mar=rep(0, 4))
plot.new()
text(x = 0.5, y = 0.5, "October 2020")
par(mar=rep(0, 4))
plot.new()
text(x = 0.5, y = 0.5, "September 2022")

create.wc(
  data = acled,
  src.in = format(EVENT_DATE, '%b-%y'),
  src = 'Sep-19',
  words = NOTES,
  rem.words = uninterest_words,
  min = 30,
  max = 2000
)
create.wc(
  data = acled,
  src.in = format(EVENT_DATE, '%b-%y'),
  src = 'Oct-20',
  words = NOTES,
  rem.words = uninterest_words,
  min = 50,
  max = 2000
)
create.wc(
  data = acled,
  src.in = format(EVENT_DATE, '%b-%y'),
  src = 'Sep-22',
  words = NOTES,
  rem.words = uninterest_words,
  min = 50,
  max = 2000
)

# NOTE: use this if prefer using cmonth numeric (may relate to heatmap in distribution)
# create.wc(
#   data = acled,
#   src.in = CMONTH,
#   src = 57,
#   words = NOTES,
#   rem.words = uninterest_words,
#   min = 30,
#   max = 2000
# )
# create.wc(
#   data = acled,
#   src.in = CMONTH,
#   src = 70,
#   words = NOTES,
#   rem.words = uninterest_words,
#   min = 50,
#   max = 2000
# )
# create.wc(
#   data = acled,
#   src.in = CMONTH,
#   src = 93,
#   words = NOTES,
#   rem.words = uninterest_words,
#   min = 50,
#   max = 2000
# )
