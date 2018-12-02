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
    image.matrix <- matrix(nrow = matrix.dim, ncol = matrix.dim)
    
    for (i in 1:length(lines)) {
      current.line <- lines[[i]]
      for (j in 1:dim(current.line)[1]) {
        row <- current.line[j, 1] + x.offset - 1
        column <- current.line[j, 2] + y.offset - 1
        image.matrix[row, column] <- -i
      }
    }
    
    for (i in 1:dim(input.line)[1]) {
      row <- input.line[i , 1] + x.offset - 1
      column <- input.line[i, 2] + y.offset - 1
      image.matrix[row, column] <- 0
    }
    
    closest.lines <- matrix(nrow = number.of.lines.to.include, ncol = 1)
    closest.lines.counter <- 1
    
    # TODO Find a suitable upper bound for iterations
    for (counter in 1:10) {
      mat.pad <- rbind(NA, cbind(NA, image.matrix, NA), NA)
      
      ind <- 2:(matrix.dim + 1) # row/column indices of the "middle"
      neigh <- rbind(
        N  = as.vector(mat.pad[ind - 1, ind]),
        NE = as.vector(mat.pad[ind - 1, ind + 1]),
        E  = as.vector(mat.pad[ind    , ind + 1]),
        SE = as.vector(mat.pad[ind + 1, ind + 1]),
        S  = as.vector(mat.pad[ind + 1, ind]),
        SW = as.vector(mat.pad[ind + 1, ind - 1]),
        W  = as.vector(mat.pad[ind    , ind - 1]),
        NW = as.vector(mat.pad[ind - 1, ind - 1])
      )
      
      indices.to.examine <- which(image.matrix == counter - 1, arr.ind = T)
      
      
      for(i in 1:dim(indices.to.examine)[1]) {
        neighbour <-
          neigh[, (matrix.dim - 1) * indices.to.examine[i , 2] + indices.to.examine[i , 1]]
      
        # North  
        if (indices.to.examine[i , 1] > 1) {
          if(is.na(neighbour[1])) {
            image.matrix[indices.to.examine[i, 1] - 1, indices.to.examine[i , 2]] <- counter  
          }
          else if(neighbour[1] < 0) {
            image.matrix[indices.to.examine[i, 1] - 1, indices.to.examine[i , 2]] <- counter
            if(!abs(neighbour[1]) %in% closest.lines) {
              closest.lines[[closest.lines.counter]] <- kanji.data[neighbour[1], 2]
              if(closest.lines.counter >= number.of.lines.to.include) {
                break
              }
              closest.lines.counter <- closest.lines.counter + 1
            }
          }
        }
        
        # North-east
        if (indices.to.examine[i , 1] > 1 && indices.to.examine[i , 2] < matrix.dim) {
          if(is.na(neighbour[2])) {
            image.matrix[indices.to.examine[i, 1] - 1, indices.to.examine[i , 2] + 1] <- counter  
          }
          else if(neighbour[2] < 0) {
            image.matrix[indices.to.examine[i, 1] - 1, indices.to.examine[i , 2] + 1] <- counter
            if(!abs(neighbour[2]) %in% closest.lines) {
              closest.lines[[closest.lines.counter]] <- abs(neighbour[2])
              if(closest.lines.counter >= number.of.lines.to.include) {
                break
              }
              closest.lines.counter <- closest.lines.counter + 1
            }
          }
        }
        
        # East
        if (indices.to.examine[i , 2] < matrix.dim) {
          if(is.na(neighbour[3])) {
            image.matrix[indices.to.examine[i, 1], indices.to.examine[i , 2] + 1] <- counter  
          }
          else if(neighbour[3] < 0) {
            image.matrix[indices.to.examine[i, 1], indices.to.examine[i , 2] + 1] <- counter
            if(!abs(neighbour[3]) %in% closest.lines) {
              closest.lines[[closest.lines.counter]] <- abs(neighbour[3])
              if(closest.lines.counter >= number.of.lines.to.include) {
                break
              }
              closest.lines.counter <- closest.lines.counter + 1
            }
          }
        }
        
        # South-east
        if (indices.to.examine[i , 1] < matrix.dim && indices.to.examine[i , 2] < matrix.dim) {
          if(is.na(neighbour[4])) {
            image.matrix[indices.to.examine[i, 1] + 1, indices.to.examine[i , 2] + 1] <- counter  
          }
          else if(neighbour[4] < 0) {
            image.matrix[indices.to.examine[i, 1] + 1, indices.to.examine[i , 2] + 1] <- counter
            if(!abs(neighbour[4]) %in% closest.lines) {
              closest.lines[[closest.lines.counter]] <- abs(neighbour[4])
              if(closest.lines.counter >= number.of.lines.to.include) {
                break
              }
              closest.lines.counter <- closest.lines.counter + 1
            }
          }
        }
        
        # South
        if (indices.to.examine[i , 1] < matrix.dim) {
          if(is.na(neighbour[5])) {
            image.matrix[indices.to.examine[i, 1] + 1, indices.to.examine[i , 2]] <- counter  
          }
          else if(neighbour[5] < 0) {
            image.matrix[indices.to.examine[i, 1] + 1, indices.to.examine[i , 2]] <- counter
            if(!abs(neighbour[5]) %in% closest.lines) {
              closest.lines[[closest.lines.counter]] <- abs(neighbour[5])
              if(closest.lines.counter >= number.of.lines.to.include) {
                break
              }
              closest.lines.counter <- closest.lines.counter + 1
            }
          }
        }
        
        # South-west
        if (indices.to.examine[i , 1] < matrix.dim && indices.to.examine[i, 2] > 1) {
          if(is.na(neighbour[6])) {
            image.matrix[indices.to.examine[i, 1] + 1, indices.to.examine[i , 2] - 1] <- counter  
          }
          else if(neighbour[6] < 0) {
            image.matrix[indices.to.examine[i, 1] + 1, indices.to.examine[i , 2] - 1] <- counter
            if(!abs(neighbour[6]) %in% closest.lines) {
              closest.lines[[closest.lines.counter]] <- abs(neighbour[6])
              if(closest.lines.counter >= number.of.lines.to.include) {
                break
              }
              closest.lines.counter <- closest.lines.counter + 1
            }
          }
        }
        
        # West
        if (indices.to.examine[i, 2] > 1) {
          if(is.na(neighbour[7])) {
            image.matrix[indices.to.examine[i, 1], indices.to.examine[i , 2] - 1] <- counter  
          }
          else if(neighbour[7] < 0) {
            image.matrix[indices.to.examine[i, 1], indices.to.examine[i , 2] - 1] <- counter
            if(!abs(neighbour[7]) %in% closest.lines) {
              closest.lines[[closest.lines.counter]] <- abs(neighbour[7])
              if(closest.lines.counter >= number.of.lines.to.include) {
                break
              }
              closest.lines.counter <- closest.lines.counter + 1
            }
          }
        }
        
        # North-west
        if (indices.to.examine[i, 1] > 1 && indices.to.examine[i, 2] > 1) {
          if(is.na(neighbour[8])) {
            image.matrix[indices.to.examine[i, 1] - 1, indices.to.examine[i , 2] - 1] <- counter  
          }
          else if(neighbour[8] < 0) {
            image.matrix[indices.to.examine[i, 1] - 1, indices.to.examine[i , 2] - 1] <- counter
            if(!abs(neighbour[8]) %in% closest.lines) {
              closest.lines[[closest.lines.counter]] <- abs(neighbour[8])
              if(closest.lines.counter >= number.of.lines.to.include) {
                break
              }
              closest.lines.counter <- closest.lines.counter + 1
            }
          }
        }
        
        
        
      }
      
      
      print("Test23")
      
      image.temp <- image.matrix
      for(i in which(image.temp < 0)) {
        row <- i %% matrix.dim
        column <- i %/% matrix.dim
        
        # print(paste(row, column))
        
        image.temp[row, column] <- -1
      }
      
      # image.temp
      
      image(t(image.temp), axes = F)
      
    }
    
    
    
    
    sapply(closest.lines, function(x) {
      kanji.data[x, 2]
    })
    
  }