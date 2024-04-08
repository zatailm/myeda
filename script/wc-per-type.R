
layout_matrix <- matrix(c(1:12), nrow = 4, byrow = TRUE)

layout(mat = layout_matrix,
       heights = c(.25, 3, .25, 3), # Heights of the four rows
       widths = c(3, 3, 3))         # Widths of the three columns

# layout.show(12)

rem_this <- c(
  stopwords('en'),
  'province', 'district', 'city', 'village', 'held',
  'north', 'east', 'south', 'west', 'regency',
  'front', 'one', 'two', 'three', 'four', 'five',
  'six', 'seven', 'eight', 'nine', 'ten', 'many',
  'thousands', 'hundreds', 'may', 'coded', 'arounds'
)

par(mar=rep(0, 4))
plot.new()
text(x = 0.5, y = 0.5, "Battles")
par(mar=rep(0, 4))
plot.new()
text(x = 0.5, y = 0.5, "ERV")
par(mar=rep(0, 4))
plot.new()
text(x = 0.5, y = 0.5, "Protests")

create.wc(
  data = acled,
  src.in = EVENT_TYPE_SRT,
  src = 'Battles',
  words = NOTES,
  rem.words = rem_this,
  min = 50,
  max = 2000
)
create.wc(
  data = acled,
  src.in = EVENT_TYPE_SRT,
  src = 'ERV',
  words = NOTES,
  rem.words = rem_this,
  min = 5,
  max = 30
)
create.wc(
  data = acled,
  src.in = EVENT_TYPE_SRT,
  src = 'Protests',
  words = NOTES,
  rem.words = rem_this,
  min = 800,
  max = 5000
)

par(mar=rep(0, 4))
plot.new()
text(x = 0.5, y = 0.5, "Riots")
par(mar=rep(0, 4))
plot.new()
text(x = 0.5, y = 0.5, "Str.Dev.")
par(mar=rep(0, 4))
plot.new()
text(x = 0.5, y = 0.5, "VAC")

create.wc(
  data = acled,
  src.in = EVENT_TYPE_SRT,
  src = 'Riots',
  words = NOTES,
  rem.words = rem_this,
  min = 200,
  max = 2000
)
create.wc(
  data = acled,
  src.in = EVENT_TYPE_SRT,
  src = 'Str.Dev.',
  words = NOTES,
  rem.words = rem_this,
  min = 50,
  max = 2000
)
create.wc(
  data = acled,
  src.in = EVENT_TYPE_SRT,
  src = 'VAC',
  words = NOTES,
  rem.words = rem_this,
  min = 80,
  max = 2000
)
