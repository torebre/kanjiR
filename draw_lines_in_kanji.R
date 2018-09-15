all.lines.in.kanji <-
  kanji.line.data[which(kanji.line.data[, 1] == 26397), ]

line1.original <- all.lines.in.kanji[37, ]

lines.draw <- list()
counter <- 1

for(i in 1:dim(all.lines.in.kanji)[1]) {
  line2 <- all.lines.in.kanji[i, ]
  
  
  if (round(line1.original$length) == 0) {
    from.length <- 1
  } else {
    from.length <- line1.original$length
  }
  
  if (round(line2$length) == 0) {
    to.length <- 1
  } else {
    to.length <- line2$length
  }
  
  relative.length <- from.length / to.length
  angle.diff <- line1.original$angle - line2$angle
  row.diff <- line1.original$start_x - line2$start_x
  start.pair.distance <-
    sqrt(row.diff ^ 2 + (line1.original$start_y - line2$start_y) ^
           2)
  if (row.diff == 0) {
    start.pair.angle.diff <- 0
  } else {
    start.pair.angle.diff <-
      atan((line1.original$start_y - line2$start_y) / row.diff)
  }
  
  second.line <-
    GenerateSecondLine(relative.length,
                       angle.diff,
                       start.pair.distance,
                       start.pair.angle.diff)
  lines.draw[[counter]] <- second.line  
  counter <- counter + 1
}


DrawLines(lines.draw)
DrawLineKanji(26397, kanji.line.data)

