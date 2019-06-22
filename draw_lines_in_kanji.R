source('DrawLineRelativeToInputLine.R')

all.lines.in.kanji <-kanji.line.data[which(kanji.line.data[, 1] == 26397), ]

dim(lines.in.kanji)

long.lines <- which(all.lines.in.kanji$length > 5)

par(mfrow=c(2, 3))
for(i in 11:15) {
  line1.original <- all.lines.in.kanji[i, ]
  lines.draw <- DrawLineRelativeToInputLine(line1.original, all.lines.in.kanji)
  
  DrawLines(lines.draw)
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

square <- matrix(nrow = 5, ncol = 6)
square[1, ] <- c(1, 0,pi/4, 10, 0, 0)
square[2, ] <- c(1, 1, 0, 10, -10, -10)
square[3, ] <- c(1, 1, -pi/2, 10, 0, 0)
# square[3, ] <- c(1, 2, -pi, 10, 0, -10)
square[4, ] <- c(1, 3, pi/2, 10, 0, -10)
square[5, ] <- c(1, 4, pi/5, 5, 0, 0)

square.data <- as.data.frame(square)
colnames(square.data) <- c('unicode', 'line_number', 'angle', 'length', 'start_x', 'start_y')

DrawLinesInKanji(square)

lines.draw <- DrawLineRelativeToInputLine(square.data[1, ], square.data)
DrawLines(lines.draw, transpose = T)
