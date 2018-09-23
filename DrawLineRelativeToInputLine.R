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
    
    relative.length <- from.length / to.length<
    angle.diff <- input.line$angle - line2$angle
    row.diff <- line2$start_y - input.line$start_y
    column.diff <- line2$start_x - input.line$start_x
    start.pair.distance <-
      sqrt(row.diff ^ 2 + column.diff ^ 2)
    # if (row.diff == 0) {
    #   start.pair.angle.diff <- 0
    # } else {
    #   start.pair.angle.diff <-
    #     atan(column.diff / row.diff)
    # }
    start.pair.angle.diff <- atan2(row.diff, column.diff)
    
    second.line <-
      GenerateSecondLine(relative.length,
                         line2$angle - input.line$angle,
                         start.pair.distance,
                         start.pair.angle.diff - input.line$angle) 
    lines.draw[[counter]] <- second.line  
    counter <- counter + 1
  }
  
  lines.draw
}