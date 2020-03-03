source('DrawLines.R')
source('AddRectangle.R')

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

lines <- rbind(t(lines), AddRectangle(number.of.rows, number.of.columns))

line.matrices <- apply(lines, 1, function(x) {
  return(CreateLine(x[3], x[4], x[5], x[6]))
})

DrawLines(line.matrices)


# test.line <- AddRectangle(number.of.rows, number.of.columns)
# test.line.matrices <- apply(test.line, 1, function(x) {
#   return(CreateLine(x[3], x[4], x[5], x[6]))
# })
# DrawLines(test.line.matrices)
