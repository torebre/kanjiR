library(MASS)
library(ks)
library(colorspace)
library(RColorBrewer)
library(rgl)
library(misc3d)
library(alphashape3d)


predictions <- predict(kernel.density.estimate, x = filtered.matrix)

max(predictions)
min(predictions)

truehist(predictions)

# TODO Find clustering


# kernel mean shift clustering
filtered.matrix.normalized <- cbind(filtered.matrix[ , 1] / max(filtered.matrix[ , 1]), 
                                    filtered.matrix[ , 2] / max(filtered.matrix[ , 2]),
                                    filtered.matrix[ , 3] / max(filtered.matrix[ , 3]))
evaluation.points <- cbind(seq(0, max(filtered.matrix.normalized[, 1]), length.out = 5),
                           seq(0, max(filtered.matrix.normalized[, 2]), length.out = 5),
                           seq(0, max(filtered.matrix.normalized[, 3]), length.out = 5))


unique.rows <- unique(filtered.matrix.normalized[ , 1:3])
bandwidth.matrix <- Hlscv(x = unique.rows)
# bandwidth.matrix <- Hns(x = filtered.matrix.normalized)
start.matrix <- rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))
bandwidth.matrix <- Hpi(x = filtered.matrix.normalized, pilot = 'dscalar', Hstart = start.matrix, bgridsize = c(0.0001, 0.0001, 0.0001), binned = T)
#filtered.matrix.kms <- kms(x = filtered.matrix.normalized, H = bandwidth.matrix, y = evaluation.points, verbose = T)
filtered.matrix.kms <- kms(x = filtered.matrix, verbose = T)

xlab <- "x"
ylab <- "y"
zlab <- "z"
# xlim <- c(0,  max(filtered.matrix.normalized[, 1]))
# ylim <- c(0,  max(filtered.matrix.normalized[, 2]))
# zlim <- c(0,  max(filtered.matrix.normalized[, 3]))

xlim <- c(0,  max(filtered.matrix[, 1]))
ylim <- c(0,  max(filtered.matrix[, 2]))
zlim <- c(0,  max(filtered.matrix[, 3]))


plot(filtered.matrix.kms)
clear3d()
plot(filtered.matrix.kms, col=pt.col((1:filtered.matrix.kms$nclust)*2), splom=FALSE, size=8, axes=FALSE, alpha=(filtered.matrix.kms$label+1)^1.5/40, asp=1, xlim=xlim, ylim=ylim, zlim=zlim, xlab=xlab, ylab=ylab, zlab="")
hsct.rgl(zlab=zlab)
box3d()


density.experiment <- kde(x = filtered.matrix.normalized, H = bandwidth.matrix)
clear3d()
plot(density.experiment, xlim=xlim, ylim=ylim, zlim=zlim, xlab="", ylab="", zlab="", axes=FALSE)
hsct.rgl(xlab=xlab, ylab=ylab, zlab=zlab)


kernel.feature.significance <- kfs(filtered.matrix)
plot(kernel.feature.significance)


max.prediction <- which(predictions == max(predictions))

filtered.matrix[max.prediction, ]

max.line <- filtered.lines[max.prediction, ]

kanji.test <- kanji.line.data[which(kanji.line.data[ , 1] == max.line[6]), ]

DrawHighlightedLines(max.line[6], kanji.test, c(max.line[7], max.line[8]))

DrawLineKanji(max.line[6], kanji.line.data)

lines.from <- filtered.lines[which(filtered.lines[ , 6] == 32993 & filtered.lines[ , 7] == max.line[7]), ]
lines.to <- filtered.lines[which(filtered.lines[ , 8] == max.line[8]), ]

lines.from[which(lines.from[ , 8] == 11), ]

# Using the max line as an example
# max.line involving 8 and 10
max.line
# Looking at 8
line.from.numbers <- which(filtered.lines[ , 6] == max.line[6] & filtered.lines[ , 7] == max.line[7])
lines.from <- filtered.lines[line.from.numbers, ]

lines.from.short <- filtered.matrix[line.from.numbers, ]
predict(kernel.density.estimate, x = lines.from.short)


