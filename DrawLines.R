DrawLines <- function(lines,
                      x.offset = NULL,
                      y.offset = NULL,
                      transpose = F) {
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
  
  if (is.null(x.offset)) {
    x.offset <- -min.x
  }
  if (is.null(y.offset)) {
    y.offset <- -min.y
  }
  
  image.matrix <-
    matrix(0, nrow = max.x - min.x + 1, ncol = max.y - min.y + 1)
  
  counter <- 1
  
  palette <- c(rgb(1, 1, 1), rainbow(length(lines)))
  
  for (i in 1:length(lines)) {
    current.line <- lines[[i]]
    for (j in 1:dim(current.line)[1]) {
      row <- current.line[j, 1] + x.offset + 1
      row <- dim(image.matrix)[1] - row
      column <- current.line[j, 2] + y.offset + 1
      image.matrix[row, column] <- counter
    }
    counter <- counter + 1
  }
  
  if(transpose) {
    image.matrix <- t(image.matrix)
  }
  image(image.matrix, col = palette, xaxt = 'n', yaxt = 'n', ann= F)
  
  # min.x <- 0
  # min.y <- 0
  # max.x <- 10
  # max.y <- 10
  # 
  # for(i in 1:length(lines)) {
  #   min.x <- min(min.x, min(lines[[i]][ , 1]))
  #   min.y <- min(min.y, min(lines[[i]][ , 2]))
  #   
  #   max.x <- max(max.x, max(lines[[i]][ , 1]))
  #   max.y <- max(max.y, max(lines[[i]][ , 2]))
  # }
  # 
  # 
  # image.matrix <- matrix(0, nrow = abs(max.x - min.x), ncol = abs(max.y - min.y))
  # x.offset <- -min.x
  # y.offset <- -min.y
  # 
  # counter <- 1
  # palette <- c(rgb(1, 1, 1), rainbow(length(lines)))
  # 
  # for(i in 1:length(lines)) {
  #   current.line <- lines[[i]]
  #   for(j in 1:dim(current.line)[1]) {
  #     row <- current.line[j, 1] + x.offset
  #     row <- dim(image.matrix)[1] - row
  #     column <- current.line[j, 2] + y.offset
  #     image.matrix[row, column] <- counter
  #   }
  #   counter <- counter + 1
  # }
  # 
  # image(t(image.matrix), col = palette)
}