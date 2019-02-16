library(MASS)


predictions <- predict(kernel.density.estimate, x = filtered.matrix)

max(predictions)
min(predictions)

truehist(predictions)

# TODO Find clustering


max.prediction <- which(predictions == max(predictions))

filtered.matrix[max.prediction, ]

max.line <- filtered.lines[max.prediction, ]

kanji.test <- kanji.line.data[which(kanji.line.data[ , 1] == max.line[6]), ]

DrawHighlightedLines(max.line[6], kanji.test, c(max.line[7], max.line[8]))

DrawLineKanji(max.line[6], kanji.line.data)

lines.from <- filtered.lines[which(filtered.lines[ , 6] == 32993 & filtered.lines[ , 7] == max.line[7]), ]
lines.to <- filtered.lines[which(filtered.lines[ , 8] == max.line[8]), ]

lines.from[which(lines.from[ , 8] == 11), ]
