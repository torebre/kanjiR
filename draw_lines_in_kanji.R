source('DrawLineRelativeToInputLine.R')

all.lines.in.kanji <-
  kanji.line.data[which(kanji.line.data[, 1] == 26397), ]

dim(lines.in.kanji)

par(mfrow=c(3, 4))
for(i in 1:12) {
  line1.original <- all.lines.in.kanji[i, ]
  lines.draw <- DrawLineRelativeToInputLine(line1.original, all.lines.in.kanji)
  
  DrawLines(lines.draw)
}

par(mfrow=c(1, 1))
DrawLineKanji(26397, kanji.line.data)




square <- matrix(nrow = 4, ncol = 6)
square[1, ] <- c(1, 0, 0, 10, 0, 0)
square[2, ] <- c(1, 1, -pi/2, 10, 10, 0)
square[3, ] <- c(1, 2, -pi, 10, 10, -10)
square[4, ] <- c(1, 3, pi/2, 10, 0, -10)

square.data <- as.data.frame(square)
colnames(square.data) <- c('unicode', 'line_number', 'angle', 'length', 'start_x', 'start_y')
lines.draw <- DrawLineRelativeToInputLine(square.data[1, ], square.data)
DrawLines(lines.draw)
