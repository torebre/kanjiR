source("CreateLine.R")

DrawLineRelative <- function(lines.to.draw) {
  lines <- list()
  lines[[1]] <- CreateLine(0, 0, 0, 10)
  counter <- 2
  
  for (i in 1:dim(lines.to.draw)[1]) {
    row.diff <- lines.to.draw[i, 1]
    column.diff <- lines.to.draw[i, 2]
    angle <- lines.to.draw[i, 3]
    
    lines[[counter]] <-
      CreateLine(
        ceiling(row.diff),
        ceiling(column.diff),
        ceiling(row.diff + 10 * cos(angle)),
        ceiling(column.diff + 10 * sin(angle))
      )
    counter <- counter + 1
  }
  
  lines
}