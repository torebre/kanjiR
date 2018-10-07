all.lines.in.kanji <-kanji.line.data[which(kanji.line.data[, 1] == 26397), ]

dim(lines.in.kanji)

par(mfrow=c(2, 3))

lines <- list()
counter <- 1
for(i in 1:dim(all.lines.in.kanji)[1]) {
  line1.original <- all.lines.in.kanji[i, ]
  
  if(line1.original$length < 10) {
    next
  }
  
  lines.draw <- DrawLineRelativeToInputLine(line1.original, all.lines.in.kanji, c(i))

  for(j in 1:length(lines.draw)) {
    lines[[counter]] <- lines.draw[[j]]
    counter <- counter + 1
  }
  
  # print(paste('Highlighted line: ',i))
  # print(lines.draw[[i]])
  # DrawLines(lines.draw, highlighted.lines =  c(i), transpose = T, x.offset = 70, y.offset = 50, min.x = 0, min.y = 0, max.x = 100, max.y = 100)
}



GenerateLineHeatMap(lines)

line.matrix <- CreateLineMatrix(lines)


# for(i in 1:10) {
#   line.matrix[1, i] <- 0
# }

image(line.matrix)



for(i in 1:6) { #length(lines)) {
  DrawLines(lines[[i]])
}


par(mfrow=c(1, 1))
DrawLineKanji(26397, kanji.line.data)

DrawLinesInKanji(all.lines.in.kanji[1:8, ])

test.line <- matrix(nrow = 1, ncol = 6)
test.line[1, ] <- c(1, 0, 2, 10, 0, 0)
test.line.data <- as.data.frame(test.line)
colnames(test.line.data) <- c('unicode', 'line_number', 'angle', 'length', 'start_x', 'start_y')

lines.draw <- DrawLineRelativeToInputLine(test.line.data, all.lines.in.kanji)
DrawLines(lines.draw, transpose = T)
