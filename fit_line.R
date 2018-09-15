line.matrix <- CreateLineMatrix(lines)

min.x <- 1
min.y <- 1
max.x <- 1
max.y <- 1

for (i in 1:length(lines)) {
  min.x <- min(min.x, min(lines[[i]][, 1]))
  min.y <- min(min.y, min(lines[[i]][, 2]))
  
  max.x <- max(max.x, max(lines[[i]][, 1]))
  max.y <- max(max.y, max(lines[[i]][, 2]))
}


x.offset <- -min.x
y.offset <- -min.y


top.percentile <- quantile(line.matrix, c(0.999))
top.percentile.matrix.indices <- which(line.matrix < top.percentile, arr.ind = T)
filtered.matrix <- line.matrix
filtered.matrix[top.percentile.matrix.indices] <- 0

palette <- c(rgb(1, 1, 1), rainbow(length(unique(filtered.matrix))))
image(filtered.matrix, col = palette)

line.scores <- matrix(NA, nrow = length(lines), ncol = 1)
for(i in 1:length(lines)) {
  shifted.line <- lines[[i]]
  shifted.line[ , 1] <- shifted.line[ , 1] + x.offset + 1
  shifted.line[ , 2] <- shifted.line[ , 2] + y.offset + 1
  
  line.scores[i] <- sum(filtered.matrix[shifted.line])
}

max.indices <- which(line.scores == max(line.scores))

best.fits <- position.data[max.indices]

unicode <- position.data[best.fits[1]]

lines.in.kanji <- position.data[which(unicode == position.data[ , 1]), ]


DrawLineKanji(unicode, kanji.line.data)

best.fit.line <- position.data[best.fits[1], ]

DrawHighlightedLines(unicode, kanji.line.data, best.fit.line[7:8] + 1)


lines.for.unicode <- which(position.data[ , 1] == unicode)
score.data <- line.scores[lines.for.unicode]


DrawIdentifiedLinesInKanji(unicode, kanji.line.data, score.data, position.data)
