source('DrawLines.R')

number.of.rows <- 64
number.of.columns <- 64


generated.matrix <- matrix(nrow = number.of.rows, ncol = number.of.columns)


lines <- sapply(seq(1, 10), function(x) {
  start.x <- sample(number.of.rows, 1)
  start.y <- sample(number.of.columns, 1)
  
  stop.x <- sample(number.of.rows, 1)
  stop.y <- sample(number.of.columns, 1)
  
  x.delta <- stop.x - start.x
  y.delta <- stop.y - start.y
  
  angle <- atan2(y.delta, x.delta)
  line.length <- sqrt(x.delta^2 + y.delta^2)
  
  c(angle, line.length, start.x, start.y, stop.x, stop.y)
})


lines <- t(lines)

line.matrices <- sapply(seq(1, 10), function(x) {
  return(CreateLine(lines[x, 3], lines[x, 4], lines[x, 5], lines[x, 6]))
})

DrawLines(line.matrices)
