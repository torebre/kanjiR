AddRectangle <- function(number.of.rows, number.of.columns) {
  start.x <- sample(number.of.columns - 1, 1)
  available.space <- number.of.columns - start.x
  line.length <- sample(available.space, 1)
  
  start.y <- sample(number.of.rows - 1, 1)
  available.space.y <- number.of.rows - start.y
  line.length.y <- sample(available.space.y, 1)
  
  rbind(c(0, line.length, start.x, start.y, start.x + line.length, start.y),
  c(7/4 * pi, line.length.y, start.x, start.y, start.x, start.y + line.length.y),
  c(0, line.length, start.x, start.y  + line.length.y, start.x + line.length, start.y + line.length.y),
  c(1/2 * pi, line.length.y, start.x + line.length, start.y  + line.length.y, start.x + line.length, start.y))
}