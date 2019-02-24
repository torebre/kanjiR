DrawLineKanjiWithCorners <- function(unicode, kanji.data, corner.pairs) {
  kanji <- which(kanji.data[, 1] == unicode)
  lines <- list()
  
  for(i in 1:length(kanji)) {
    line.data <-  kanji.data[kanji[i], ]
    
    x.start <- unlist(line.data[5])
    y.start <- unlist(line.data[6])
    x.offset <- round(unlist(line.data[4] * sin(line.data[3])))
    y.offset <- round(unlist(line.data[4] * cos(line.data[3])))
    
    # print(cat("Line:", i, ". x.offset: ", x.offset, ". y.offset: ", y.offset))
    
    lines[[i]] <- CreateLine(x.start, y.start, x.start + x.offset, y.start + y.offset)
    
    # print(cat("Line: ", i, "Line number: ", unlist(line.data[2]), "Start: ", x.start, y.start, "Stop:", x.start + x.offset, y.start + y.offset))
    # print(lines[[i]])
  }
  
  min.x <- 0
  min.y <- 0
  max.x <- 10
  max.y <- 10
  
  number.of.pixels <- 0
  for(i in 1:length(lines)) {
    min.x <- min(min.x, min(lines[[i]][ , 1]))
    min.y <- min(min.y, min(lines[[i]][ , 2]))
    
    max.x <- max(max.x, max(lines[[i]][ , 1]))
    max.y <- max(max.y, max(lines[[i]][ , 2]))
    
    number.of.pixels <- number.of.pixels + length(lines[[i]])
  }
  
  image.matrix <- matrix(0, nrow = abs(max.x - min.x), ncol = abs(max.y - min.y))
  x.offset <- -min.x
  y.offset <- -min.y
  
  distinct.clusters = length(unique(corner.pairs[ , 3]))
  
  counter <- 1
  palette <- rainbow(distinct.clusters)
  
  label.positions <- matrix(nrow = length(lines), ncol = 2)
  
  # plot(x = NULL, y = NULL, xlim = c(0, nrow = abs(max.x - min.x)), ylim = c(0 ,abs(max.y - min.y)))
  plot(x = NULL, y = NULL, xlim = c(0 ,abs(max.y - min.y)), ylim = c(0, nrow = abs(max.x - min.x)))
  
  for(i in 1:dim(corner.pairs)[1]) {
      current.line <- lines[[corner.pairs[i, 2] + 1]]
      # label.positions[i , ] <- c(dim(image.matrix)[1] - current.line[1, 1]  - x.offset, current.line[1, 2] + y.offset)
      
      first.row <- dim(image.matrix)[1] - current.line[1, 1] - x.offset
      first.column <- current.line[1, 2] + y.offset
      
      last.pixel = dim(current.line)[1]
      last.row = dim(image.matrix)[1] - current.line[last.pixel, 1] - x.offset 
      last.column = current.line[last.pixel, 2] + y.offset
      
      segments(first.column, first.row, last.column, last.row, col = palette[corner.pairs[i, 1]])
    
  
      current.line <- lines[[corner.pairs[i, 3] + 1]]
      # label.positions[i , ] <- c(dim(image.matrix)[1] - current.line[1, 1]  - x.offset, current.line[1, 2] + y.offset)
      
      first.row <- dim(image.matrix)[1] - current.line[1, 1] - x.offset
      first.column <- current.line[1, 2] + y.offset
      
      last.pixel = dim(current.line)[1]
      last.row = dim(image.matrix)[1] - current.line[last.pixel, 1] - x.offset 
      last.column = current.line[last.pixel, 2] + y.offset
      
      segments(first.column, first.row, last.column, last.row, col = palette[corner.pairs[i, 1]])
  }
  
  
  # for(i in 1:length(lines)) {
  #   current.line <- lines[[i]]
  #   label.positions[i , ] <- c(dim(image.matrix)[1] - current.line[1, 1]  - x.offset, current.line[1, 2] + y.offset)
  #   
  #   first.row <- dim(image.matrix)[1] - current.line[1, 1] - x.offset
  #   first.column <- current.line[1, 2] + y.offset
  #   
  #   last.pixel = dim(current.line)[1]
  #   last.row = dim(image.matrix)[1] - current.line[last.pixel, 1] - x.offset 
  #   last.column = current.line[last.pixel, 2] + y.offset
  #   
  #   which(corner.pairs[ , 1] == 5 & corner.pairs[ , 2] == 300)
  #   
  #   segments(first.column, first.row, last.column, last.row)
  # }
  
  # image(t(image.matrix), col = palette)
  # matrix.dim <- dim(image.matrix)
  # image(x = 1:matrix.dim[2], y = 1:matrix.dim[1], z = t(image.matrix))
  
  for(i in 1:length(lines)) {
    text(x = label.positions[i, 2], y = label.positions[i, 1], labels = i - 1, adj = 0.5)
  }
}