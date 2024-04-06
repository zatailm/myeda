
layout_matrix <- matrix(c(1:4), nrow = 2, byrow = TRUE)

layout(mat = layout_matrix,
       heights = c(.5, 3), 
       widths = c(3, 3))

# layout.show(12)

par(mar=rep(0, 4))
plot.new()
text(x = 0.5, y = 0.5, "Comparison")
par(mar=rep(0, 4))
plot.new()
text(x = 0.5, y = 0.5, "Commonality")

yearlist <- c('2015', '2016', '2017', '2018', '2019', '2020', '2021', '2022', '2023')

word_torem <- c(
  stopwords('en')
)

notes_year <- prep.texts(
  data = acled,
  src.in = YEAR,
  src = yearlist,
  words = NOTES,
  lab = yearlist,
  rem.words = word_torem
)

set.seed(1234)
comparison.cloud(notes_year, max.words = 5000, random.order = FALSE, 
                 scale = c(2, .4), title.size = 1)

set.seed(1234)
commonality.cloud(notes_year, min.words = 5000, random.order = FALSE)
