
layout_matrix <- matrix(c(1:4), nrow = 2, byrow = TRUE)

layout(mat = layout_matrix,
       heights = c(.25, 3), 
       widths = c(3, 3))

# layout.show(12)

par(mar=rep(0, 4))
plot.new()
text(x = 0.5, y = 0.5, "Comparison")
par(mar=rep(0, 4))
plot.new()
text(x = 0.5, y = 0.5, "Commonality")

typelist <- c("Battles", "ERV", "Protests", "Riots", "Str.Dev.", "VAC")

word_torem <- c(
  stopwords('en')
)

notes_type <- prep.texts(
  data = acled,
  src.in = EVENT_TYPE_SRT,
  src = typelist,
  words = NOTES,
  lab = typelist,
  rem.words = word_torem
)

set.seed(1234)
comparison.cloud(notes_type, max.words = 5000, random.order = FALSE, 
                 scale = c(2, .4), title.size = 1)

set.seed(1234)
commonality.cloud(notes_type, min.words = 5000, random.order = FALSE)
