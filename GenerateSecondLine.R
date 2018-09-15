GenerateSecondLine <- function(relative.length, angle.diff, start.pair.difference, start.pair.angle.diff) {
  line1.length <- 10
  line2.length <- line1.length / relative.length
  line2.angle <- -angle.diff
  line1.angle <- 0
  
  line1.start.x <- 0
  line1.start.y <- 0
  
  line1.end.x <- line1.length
  line1.end.y <- 0
  
  line2.start.x <- round(start.pair.difference * cos(start.pair.angle.diff))
  line2.start.y <- round(start.pair.difference * sin(start.pair.angle.diff))
  
  line2.end.x <- round(line2.start.x + line2.length * cos(angle.diff))
  line2.end.y <- round(line2.start.y + line2.length * sin(angle.diff))
  
  CreateLine(line2.start.x, line2.start.y, line2.end.x, line2.end.y)
}