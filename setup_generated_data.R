source('DrawLines.R')
source('AddRectangle.R')
source('CreateLine.R')

number.of.rows <- 64
number.of.columns <- 64


GenerateTrainingSample <- function() {
  lines <- sapply(seq(1, 10), function(x) {
    start.x <- sample(number.of.rows, 1)
    start.y <- sample(number.of.columns, 1)
    
    stop.x <- sample(number.of.rows, 1)
    stop.y <- sample(number.of.columns, 1)
    
    x.delta <- stop.x - start.x
    y.delta <- stop.y - start.y
    
    angle <- atan2(y.delta, x.delta)
    line.length <- sqrt(x.delta ^ 2 + y.delta ^ 2)
    
    c(angle, line.length, start.x, start.y, stop.x, stop.y)
  })
  
  return(rbind(t(lines), AddRectangle(number.of.rows, number.of.columns)))
}

CreateMatrixLines <- function() {
  apply(lines, 1, function(x) {
    return(CreateLine(x[3], x[4], x[5], x[6]))
  })
}

DrawLines(line.matrices)


# test.line <- AddRectangle(number.of.rows, number.of.columns)
# test.line.matrices <- apply(test.line, 1, function(x) {
#   return(CreateLine(x[3], x[4], x[5], x[6]))
# })
# DrawLines(test.line.matrices)

training.data <- lapply(1:100, function(x) {
  index <- x
  training.sample <- GenerateTrainingSample()
  
  transformed.sample <- t(sapply(1:dim(training.sample)[1], function(row) {
    c(index, row, training.sample[row, 1], training.sample[row, 2], training.sample[row, 3], training.sample[row, 4])
  }))
  
  colnames(transformed.sample) <- c("unicode", "line_number", "angle", "length", "start_x", "start_y")
  
  transformed.sample
})

training.data.transformed <- do.call(rbind, lapply(training.data, function(x) {
  x
}))

training.data.unlisted[1:20, ]
write.csv(training.data.transformed, "training_data.csv", row.names = F)
