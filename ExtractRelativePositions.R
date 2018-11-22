ExtractRelativePositions <- function(input.line, all.lines.in.kanji, skip = c(), line.number.input) {
  result <- matrix(nrow = dim(all.lines.in.kanji)[1] - length(skip), ncol = 8)
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
    
    line2.stop.x <- line2$start_x * cos(line2$angle)
    line2.stop.y <- line2$start_y * sin(line2$angle)
    
    switched.length <- sqrt((input.line$start_x - line2.stop.x)^2 + (input.line$start_y - line2.stop.y)^2)
    
    if(switched.length < start.pair.distance) {
      start.pair.distance <- switched.length
      start.pair.angle.diff <- pi/2 + atan2(row.diff, column.diff)
    }
    else {
      start.pair.angle.diff <- atan2(row.diff, column.diff)  
    }
    
    second.line.angle <- line2$angle - input.line$angle
    start.pair.second.line.angle <- start.pair.angle.diff + input.line$angle
    
    if(start.pair.second.line.angle < 0) {
      start.pair.second.line.angle <- start.pair.second.line.angle + 2*pi
    }
    
    result[counter, 1] <- abs(row.diff) / input.line$length
    result[counter, 2] <- abs(column.diff) / input.line$length
    result[counter, 3] <- start.pair.second.line.angle
    result[counter, 4] <- line.number.input
    result[counter, 5] <- as.integer(rownames(all.lines.in.kanji[i, ]))
    result[counter, 6] <- input.line$unicode
    result[counter, 7] <- input.line$line_number
    result[counter, 8] <- line2$line_number
    
    counter <- counter + 1
  }
  
  result
}