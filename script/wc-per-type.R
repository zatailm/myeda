
layout_matrix <- matrix(c(1:12), nrow = 4, byrow = TRUE)

layout(mat = layout_matrix,
       heights = c(.5, 3, .5, 3), # Heights of the four rows
       widths = c(3, 3, 3))     # Widths of the three columns

# layout.show(12)
par(mar=rep(0, 4))
plot.new()
text(x = 0.5, y = 0.5, "Battles")
par(mar=rep(0, 4))
plot.new()
text(x = 0.5, y = 0.5, "ERV")
par(mar=rep(0, 4))
plot.new()
text(x = 0.5, y = 0.5, "Protests")

create.wc(acled, EVENT_TYPE_SRT, 'Battles', NOTES, 50)
create.wc(acled, EVENT_TYPE_SRT, 'ERV', NOTES, 1)
create.wc(acled, EVENT_TYPE_SRT, 'Protests', NOTES, 800)

par(mar=rep(0, 4))
plot.new()
text(x = 0.5, y = 0.5, "Riots")
par(mar=rep(0, 4))
plot.new()
text(x = 0.5, y = 0.5, "Str.Dev.")
par(mar=rep(0, 4))
plot.new()
text(x = 0.5, y = 0.5, "VAC")

create.wc(acled, EVENT_TYPE_SRT, 'Riots', NOTES, 200)
create.wc(acled, EVENT_TYPE_SRT, 'Str.Dev.', NOTES, 50)
create.wc(acled, EVENT_TYPE_SRT, 'VAC', NOTES, 80)
