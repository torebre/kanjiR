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
    input.line <-
      CreateLine(current.start.x,
                 current.start.y,
                 current.stop.x,
                 current.stop.y)
    
    lines <- list()
    
    for (i in 1:dim(kanji.data)[1]) {
      line.data <-  kanji.data[i, ]
      
      x.start <- unlist(line.data[5])
      y.start <- unlist(line.data[6])
      x.offset <- round(unlist(line.data[4] * sin(line.data[3])))
      y.offset <- round(unlist(line.data[4] * cos(line.data[3])))
      
      lines[[i]] <-
        CreateLine(x.start, y.start, x.start + x.offset, y.start + y.offset)
    }
    
    min.max.points <- sapply(lines, function(x) {
      c(min(x[, 1]), min(x[, 2]), max(x[, 1]), max(x[, 2]))
    })
    
    min.max <-
      c(min(c(min.max.points[1 , ], min(input.line[, 1]))),
        min(c(min.max.points[2 , ], min(input.line[, 2]))),
        max(c(min.max.points[3 , ], max(input.line[, 1]))),
        max(c(min.max.points[4 , ], max(input.line[, 2]))))
    
    if (min.max[1] < 1) {
      x.offset <- abs(min.max[1]) + 1
    }
    else {
      x.offset <- 0
    }
    
    if (min.max[2] < 1) {
      y.offset <- abs(min.max[2]) + 1
    }
    else {
      y.offset <- 0
    }
    
    matrix.dim <-
      max(c(x.offset + min.max[3], y.offset + min.max[4])) + 2
    image.matrix <- matrix(data = NA, nrow = matrix.dim, ncol = matrix.dim)
    
    for (i in 1:length(lines)) {
      current.line <- lines[[i]]
      for (j in 1:dim(current.line)[1]) {
        row <- current.line[j, 1] + x.offset - 1
        column <- current.line[j, 2] + y.offset - 1
        image.matrix[row, column] <- -i
      }
    }
    
    # image(t(image.matrix), main = "Before", axes = F)
    
    for (i in 1:dim(input.line)[1]) {
      row <- input.line[i , 1] + x.offset - 1
      column <- input.line[i, 2] + y.offset - 1
      image.matrix[row, column] <- 0
    }
    
    # image(t(image.matrix), main = "After", axes = F)
    
    closest.lines <- matrix(nrow = number.of.lines.to.include, ncol = 1)
    closest.lines.counter <- 1
    
    # TODO Find a suitable upper bound for iterations
    for (counter in 1:50) {
      temp.matrix <- image.matrix
      indices.to.examine <- which(temp.matrix == counter - 1, arr.ind = T)
      
      if(length(indices.to.examine) == 0) {
        # No hits found
        break
      }
      
      for(i in 1:dim(indices.to.examine)[1]) {
        # North  
        if (indices.to.examine[i , 1] > 1) {
          if(is.na(temp.matrix[indices.to.examine[i, 1] - 1, indices.to.examine[i , 2]])) {
            image.matrix[indices.to.examine[i, 1] - 1, indices.to.examine[i , 2]] <- counter  
          }
          else if(temp.matrix[indices.to.examine[i, 1] - 1, indices.to.examine[i , 2]] < 0) {
            if(!kanji.data[-temp.matrix[indices.to.examine[i, 1] - 1, indices.to.examine[i , 2]], 2] %in% closest.lines) {
              closest.lines[[closest.lines.counter]] <- kanji.data[-temp.matrix[indices.to.examine[i, 1] - 1, indices.to.examine[i , 2]], 2]
              if(closest.lines.counter >= number.of.lines.to.include) {
                return(closest.lines)
              }
              closest.lines.counter <- closest.lines.counter + 1
            }
            image.matrix[indices.to.examine[i, 1] - 1, indices.to.examine[i , 2]] <- counter
          }
        }
        
        # North-east
        if (indices.to.examine[i , 1] > 1 && indices.to.examine[i , 2] < matrix.dim) {
          if(is.na(temp.matrix[indices.to.examine[i, 1] - 1, indices.to.examine[i , 2] + 1])) {
            image.matrix[indices.to.examine[i, 1] - 1, indices.to.examine[i , 2] + 1] <- counter  
          }
          else if(temp.matrix[indices.to.examine[i, 1] - 1, indices.to.examine[i , 2] + 1] < 0) {
            if(!kanji.data[-temp.matrix[indices.to.examine[i, 1] - 1, indices.to.examine[i , 2] + 1], 2] %in% closest.lines) {
              closest.lines[[closest.lines.counter]] <- kanji.data[-temp.matrix[indices.to.examine[i, 1] - 1, indices.to.examine[i , 2] + 1], 2]
              if(closest.lines.counter >= number.of.lines.to.include) {
                return(closest.lines)
              }
              closest.lines.counter <- closest.lines.counter + 1
            }
            image.matrix[indices.to.examine[i, 1] - 1, indices.to.examine[i , 2] + 1] <- counter
          }
        }
        
        # East
        if (indices.to.examine[i, 2] < matrix.dim) {
          if(is.na(temp.matrix[indices.to.examine[i, 1], indices.to.examine[i , 2] + 1])) {
            image.matrix[indices.to.examine[i, 1], indices.to.examine[i , 2] + 1] <- counter  
          }
          else if(temp.matrix[indices.to.examine[i, 1], indices.to.examine[i , 2] + 1] < 0) {
            if(!kanji.data[-temp.matrix[indices.to.examine[i, 1], indices.to.examine[i , 2] + 1], 2] %in% closest.lines) {
              closest.lines[[closest.lines.counter]] <- kanji.data[-temp.matrix[indices.to.examine[i, 1], indices.to.examine[i , 2] + 1], 2]
              if(closest.lines.counter >= number.of.lines.to.include) {
                return(closest.lines)
              }
              closest.lines.counter <- closest.lines.counter + 1
            }
            image.matrix[indices.to.examine[i, 1], indices.to.examine[i , 2] + 1] <- counter
          }
        }
        
        # South-east
        if (indices.to.examine[i , 1] < matrix.dim && indices.to.examine[i , 2] < matrix.dim) {
          if(is.na(temp.matrix[indices.to.examine[i, 1] + 1, indices.to.examine[i , 2] + 1])) {
            image.matrix[indices.to.examine[i, 1] + 1, indices.to.examine[i , 2] + 1] <- counter  
          }
          else if(temp.matrix[indices.to.examine[i, 1] + 1, indices.to.examine[i , 2] + 1] < 0) {
            if(!kanji.data[-temp.matrix[indices.to.examine[i, 1] + 1, indices.to.examine[i , 2] + 1], 2] %in% closest.lines) {
              closest.lines[[closest.lines.counter]] <- kanji.data[-temp.matrix[indices.to.examine[i, 1] + 1, indices.to.examine[i , 2] + 1], 2]
              if(closest.lines.counter >= number.of.lines.to.include) {
                return(closest.lines)
              }
              closest.lines.counter <- closest.lines.counter + 1
            }
            image.matrix[indices.to.examine[i, 1] + 1, indices.to.examine[i , 2] + 1] <- counter
          }
        }
        
        # South
        if (indices.to.examine[i , 1] < matrix.dim) {
          if(is.na(temp.matrix[indices.to.examine[i, 1] + 1, indices.to.examine[i , 2]])) {
            image.matrix[indices.to.examine[i, 1] + 1, indices.to.examine[i , 2]] <- counter  
          }
          else if(temp.matrix[indices.to.examine[i, 1] + 1, indices.to.examine[i , 2]] < 0) {
            if(!kanji.data[-temp.matrix[indices.to.examine[i, 1] + 1, indices.to.examine[i , 2]], 2] %in% closest.lines) {
              closest.lines[[closest.lines.counter]] <- kanji.data[-temp.matrix[indices.to.examine[i, 1] + 1, indices.to.examine[i , 2]], 2]
              if(closest.lines.counter >= number.of.lines.to.include) {
                return(closest.lines)
              }
              closest.lines.counter <- closest.lines.counter + 1
            }
            image.matrix[indices.to.examine[i, 1] + 1, indices.to.examine[i , 2]] <- counter
          }
        }
        
        # South-west
        if (indices.to.examine[i , 1] < matrix.dim && indices.to.examine[i, 2] > 1) {
          if(is.na(temp.matrix[indices.to.examine[i, 1] + 1, indices.to.examine[i , 2] - 1])) {
            image.matrix[indices.to.examine[i, 1] + 1, indices.to.examine[i , 2] - 1] <- counter  
          }
          else if(temp.matrix[indices.to.examine[i, 1] + 1, indices.to.examine[i , 2] - 1] < 0) {
            if(!kanji.data[-temp.matrix[indices.to.examine[i, 1] + 1, indices.to.examine[i , 2] - 1], 2] %in% closest.lines) {
              closest.lines[[closest.lines.counter]] <- kanji.data[-temp.matrix[indices.to.examine[i, 1] + 1, indices.to.examine[i , 2] - 1], 2]
              if(closest.lines.counter >= number.of.lines.to.include) {
                return(closest.lines)
              }
              closest.lines.counter <- closest.lines.counter + 1
            }
            image.matrix[indices.to.examine[i, 1] + 1, indices.to.examine[i , 2] - 1] <- counter
          }
        }
        
        # West
        if (indices.to.examine[i, 2] > 1) {
          if(is.na(temp.matrix[indices.to.examine[i, 1], indices.to.examine[i , 2] - 1])) {
            image.matrix[indices.to.examine[i, 1], indices.to.examine[i , 2] - 1] <- counter  
          }
          else if(temp.matrix[indices.to.examine[i, 1], indices.to.examine[i , 2] - 1] < 0) {
            if(!kanji.data[-temp.matrix[indices.to.examine[i, 1], indices.to.examine[i , 2] - 1], 2] %in% closest.lines) {
              closest.lines[[closest.lines.counter]] <- kanji.data[-temp.matrix[indices.to.examine[i, 1], indices.to.examine[i , 2] - 1], 2]
              if(closest.lines.counter >= number.of.lines.to.include) {
                return(closest.lines)
              }
              closest.lines.counter <- closest.lines.counter + 1
            }
            image.matrix[indices.to.examine[i, 1], indices.to.examine[i , 2] - 1] <- counter
          }
        }
        
        # North-west
        if (indices.to.examine[i, 1] > 1 && indices.to.examine[i, 2] > 1) {
          if(is.na(temp.matrix[indices.to.examine[i, 1] - 1, indices.to.examine[i , 2] - 1])) {
            image.matrix[indices.to.examine[i, 1] - 1, indices.to.examine[i , 2] - 1] <- counter  
          }
          else if(temp.matrix[indices.to.examine[i, 1] - 1, indices.to.examine[i , 2] - 1] < 0) {
            if(!kanji.data[-temp.matrix[indices.to.examine[i, 1] - 1, indices.to.examine[i , 2] - 1], 2] %in% closest.lines) {
              closest.lines[[closest.lines.counter]] <- kanji.data[-temp.matrix[indices.to.examine[i, 1] - 1, indices.to.examine[i , 2] - 1], 2]
              if(closest.lines.counter >= number.of.lines.to.include) {
                return(closest.lines)
              }
              closest.lines.counter <- closest.lines.counter + 1
            }
            image.matrix[indices.to.examine[i, 1] - 1, indices.to.examine[i , 2] - 1] <- counter
          }
        }
      }
      
      # image.temp <- image.matrix
      # for(i in which(image.temp < 0)) {
      #   row <- i %% matrix.dim
      #   column <- i %/% matrix.dim
      #   image.temp[row, column + 1] <- -1
      # }
      # image(t(image.temp), axes = F)
      
    }
    
    closest.lines
  }