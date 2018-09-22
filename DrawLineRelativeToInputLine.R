DrawLineRelativeToInputLine <- function(input.line, all.lines.in.kanji) {
  lines.draw <- list()
  counter <- 1
  
  for(i in 1:dim(all.lines.in.kanji)[1]) {
    line2 <- all.lines.in.kanji[i, ]
    
    
    if (round(input.line$length) == 0) {
      from.length <- 1
    } else {
      from.length <- input.line$length
    }
    
    if (round(line2$length) == 0) {
      to.length <- 1
    } else {
      to.length <- line2$length
    }
    
    relative.length <- from.length / to.length
    angle.diff <- input.line$angle - line2$angle
    row.diff <- input.line$start_x - line2$start_x
    start.pair.distance <-
      sqrt(row.diff ^ 2 + (input.line$start_y - line2$start_y) ^
             2)
    if (row.diff == 0) {
      start.pair.angle.diff <- 0
    } else {
      start.pair.angle.diff <-
        atan((input.line$start_y - line2$start_y) / row.diff)
    }
    
    second.line <-
      GenerateSecondLine(relative.length,
                         angle.diff,
                         start.pair.distance,
                         start.pair.angle.diff)
    lines.draw[[counter]] <- second.line  
    counter <- counter + 1
  }
  
  lines.draw
}