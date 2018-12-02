source("CreateLine.R")
source("PointToLine.R")

ExtractClosestLinesToLine2 <-
  function(current.start.x,
           current.start.y,
           current.stop.x,
           current.stop.y,
           number.of.lines.to.include,
           kanji.data) {
    number.of.lines <- dim(kanji.data)[1]
    input.line <- CreateLine(current.start.x, current.start.y, current.stop.x, current.stop.y)
    
    lines <- list()
    
    for(i in 1:dim(kanji.data)[1]) {
      line.data <-  kanji.data[i, ]
      
      x.start <- unlist(line.data[5])
      y.start <- unlist(line.data[6])
      x.offset <- round(unlist(line.data[4] * sin(line.data[3])))
      y.offset <- round(unlist(line.data[4] * cos(line.data[3])))
      
      lines[[i]] <- CreateLine(x.start, y.start, x.start + x.offset, y.start + y.offset)
    }
    
    min.max.points <- sapply(lines, function(x) {
      c(min(x[ , 1]), min(x[ , 2]), max(x[ , 1]), max(x[ , 2]))
    })
    
    min.max <- c(min(min.max.points[1 , ]), min(min.max.points[2 , ]), max(min.max.points[3 , ]), max(min.max.points[4 , ]))
    
    if(min.max[1] < 1) {
      x.offset <- abs(min.max[1]) + 1
    }
    else {
      x.offset <- 0
    }
    
    if(min.max[2] < 1) {
      y.offset <- abs(min.max[2]) + 1
    }
    else {
      y.offset <- 0
    }
    
    
    matrix.dim <- max(c(x.offset + min.max[3], y.offset + min.max[4]))
    image.matrix <- matrix(nrow = matrix.dim, ncol = matrix.dim)
    
    for (i in 1:length(lines)) {
      current.line <- lines[[i]]
      for (j in 1:dim(current.line)[1]) {
        row <- current.line[j, 1] + x.offset
        column <- current.line[j, 2] + y.offset
        
        print(dim(image.matrix))
        print(paste("Row:", row, "Column:", column))
        
        image.matrix[row, column] <- -i
      }
    }
    
    image(t(image.matrix), axes = F)
    
  }