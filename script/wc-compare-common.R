
layout_matrix <- matrix(c(1:4), nrow = 2, byrow = TRUE)

layout(mat = layout_matrix,
       heights = c(.5, 3), # Heights of the four rows
       widths = c(3, 3))     # Widths of the three columns

# layout.show(12)

par(mar=rep(0, 4))
plot.new()
text(x = 0.5, y = 0.5, "Comparison")
par(mar=rep(0, 4))
plot.new()
text(x = 0.5, y = 0.5, "Commonality")

set.seed(1234)
comparison.cloud(document.tm.clean.mat.s, max.words = 2000, random.order = FALSE, 
                 scale = c(2, .4), title.size = 1.2)
set.seed(1234)
commonality.cloud(document.tm.clean.mat.s, min.words = 2000, random.order = FALSE)
