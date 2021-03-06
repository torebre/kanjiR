CreateLineMatrix <-
  function(lines) {
    min.x <- 0
    min.y <- 0
    max.x <- 10
    max.y <- 10
    
    for (i in 1:length(lines)) {
      min.x <- min(min.x, min(lines[[i]][, 1]))
      min.y <- min(min.y, min(lines[[i]][, 2]))
      
      max.x <- max(max.x, max(lines[[i]][, 1]))
      max.y <- max(max.y, max(lines[[i]][, 2]))
    }
    
    x.offset <- -min.x
    y.offset <- -min.y
    
    image.matrix <-
      matrix(0, nrow = max.x - min.x + 1, ncol = max.y - min.y + 1)
    
    counter <- 1
    
    
    for (i in 1:length(lines)) {
      current.line <- lines[[i]]
      for (j in 1:dim(current.line)[1]) {
        row <- current.line[j, 1] + x.offset + 1
        column <- current.line[j, 2] + y.offset + 1
        image.matrix[row, column] <- image.matrix[row, column] + 1
      }
    }
    
    image.matrix
  }