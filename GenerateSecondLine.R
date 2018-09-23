GenerateSecondLine <-
  function(relative.length,
           second.line.angle,
           start.pair.difference,
           start.pair.second.line.angle) {
    line1.length <- 10
    line2.length <- line1.length / relative.length
    line1.angle <- 0
    
    line1.start.x <- 0
    line1.start.y <- 0
    
    line1.end.x <- line1.length
    line1.end.y <- 0
    
    line2.start.x <-
      round(start.pair.difference * cos(start.pair.second.line.angle) / relative.length)
    line2.start.y <-
      round(start.pair.difference * sin(start.pair.second.line.angle) / relative.length)
    
    line2.end.x <-
      round(line2.start.x + line2.length * cos(second.line.angle))
    line2.end.y <-
      round(line2.start.y + line2.length * sin(second.line.angle))
    
    CreateLine(line2.start.x, line2.start.y, line2.end.x, line2.end.y)
  }
