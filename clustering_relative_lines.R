library(mclust)

source('DrawHighlightedLines.R')
source('DrawLineKanji.R')


mod5 <- densityMclust(lines[ , 1:3])
summary(mod5)
plot(mod5, what = "density")
# plot(mod5, what = "density", type = "level")
# plot(mod5, what = "density", type = "persp")




top.percentile <- quantile(mod$density, c(0.9))
top.percentile.indices <- which(lines > top.percentile, arr.ind = T)

filtered.matrix <-lines[top.percentile.indices, ]

BIC <- mclustBIC(filtered.matrix)
plot(BIC)
summary(BIC)
mod1 <- Mclust(filtered.matrix, x = BIC)
summary(mod1, parameter = T)

plot(mod1, what = "classification")


mod1$classification

cluster1 <- lines[which(mod1$classification == 1), ]

unicode <- all.lines.in.kanji[cluster1[1, 4], ]$unicode 
all.lines.in.kanji[cluster1[1, 4], ]
all.lines.in.kanji[cluster1[1, 5], ]

cluster1[1, 4]

DrawHighlightedLines(unicode, all.lines.in.kanji, c(cluster1[1, 4], cluster1[1, 5]))

DrawLineKanji(unicode, all.lines.in.kanji)
