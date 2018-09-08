DrawHighlightedLines <- function(unicode, kanji.data, hightlight.lines) {
  kanji <- which(kanji.data[, 1] == unicode)
  lines <- list()
  
  for(i in 1:length(kanji)) {
    line.data <-  kanji.data[kanji[i], ]
    
    x.start <- unlist(line.data[5])
    y.start <- unlist(line.data[6])
    x.offset <- round(unlist(line.data[4] * sin(line.data[3])))
    y.offset <- round(unlist(line.data[4] * cos(line.data[3])))
    
    print(cat("Line:", i, ". x.offset: ", x.offset, ". y.offset: ", y.offset))
    
    lines[[i]] <- CreateLine(x.start, y.start, x.start + x.offset, y.start + y.offset)
    
    print(cat("Line: ", i, "Line number: ", unlist(line.data[2]), "Start: ", x.start, y.start, "Stop:", x.start + x.offset, y.start + y.offset))
    print(lines[[i]])
  }
  
  min.x <- 0
  min.y <- 0
  max.x <- 10
  max.y <- 10
  
  for(i in 1:length(lines)) {
    min.x <- min(min.x, min(lines[[i]][ , 1]))
    min.y <- min(min.y, min(lines[[i]][ , 2]))
    
    max.x <- max(max.x, max(lines[[i]][ , 1]))
    max.y <- max(max.y, max(lines[[i]][ , 2]))
  }
  
  
  image.matrix <- matrix(0, nrow = abs(max.x - min.x), ncol = abs(max.y - min.y))
  x.offset <- -min.x
  y.offset <- -min.y
  
  counter <- 1
  palette <- c(rgb(1, 1, 1), rgb(1, 1, 0), rgb(1, 0, 0))
  
  for(i in 1:length(lines)) {
    current.line <- lines[[i]]
    if(i %in% hightlight.lines) {
      level <- 2
    }
    else {
      level <- 1
    }
    
    for(j in 1:dim(current.line)[1]) {
      row <- current.line[j, 1] + x.offset
      row <- dim(image.matrix)[1] - row
      column <- current.line[j, 2] + y.offset
      image.matrix[row, column] <- level
    }
  }
  
  image(t(image.matrix), col = palette)
}