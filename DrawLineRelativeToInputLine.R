DrawLineRelativeToInputLine <- function(input.line, all.lines.in.kanji, skip = c()) {
  lines.draw <- list()
  counter <- 1
  
  for(i in 1:dim(all.lines.in.kanji)[1]) {
    if(i %in% skip) {
      next
    }
    
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
    row.diff <- line2$start_y - input.line$start_y
    column.diff <- line2$start_x - input.line$start_x
    start.pair.distance <- sqrt(row.diff ^ 2 + column.diff ^ 2)
    start.pair.angle.diff <- atan2(row.diff, column.diff)
    
    second.line <-
      GenerateSecondLine(input.line$length, 
                         relative.length,
                         line2$angle - input.line$angle,
                         start.pair.distance,
                         start.pair.angle.diff + input.line$angle)
    lines.draw[[counter]] <- second.line  
    counter <- counter + 1
  }
  
  lines.draw
}