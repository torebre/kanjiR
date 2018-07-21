


CreateLine <- function(start.x, start.y, stop.x, stop.y) {
  if(start.x == stop.y) {
    # Horizontal line
    rows <- abs(stop.y - start.y)
    result <- matrix(nrow = rows, ncol = 2)
    if(start.y < stop.y) {
      result[ ,1] <- rep(start.x, rows)
      result[ ,2] <- start.y:(stop.y - 1)
    }
    else {
      result[ ,1] <- rep(start.x, rows)
      result[ ,2] <- start.y:(stop.y - 1)
    }
    
    return(result)
  }
  
  if(start.y == stop.y) {
    # Vertical line
    rows <- abs(stop.x - start.x)
    result <- matrix(nrow = rows, ncol = 2)
    if(start.x < stop.x) {
      result[ ,1] <- rep(start.y, rows)
      result[ ,2] <- start.y:(stop.x - 1)
    }
    else {
      result[ ,1] <- rep(start.y, rows)
      result[ ,2] <- start.y:(stop.x - 1)
    }
    
    return(result)
  }
  
  swap <- start.x < start.y
  
  first.translate <- abs(min(0, min(start.x, stop.x)))
  second.translate <- abs(min(0, min(start.y, stop.y)))
  
  
  if(swap) {
    start.x.translate <- stop.x + first.translate
    start.y.translate <- stop.y + second.translate
    stop.x.translate <- start.x + first.translate
    stop.y.translate <- start.y + second.translate
  }
  else {
    start.x.translate <- start.x + first.translate
    start.y.translate <- start.y + second.translate
    stop.x.translate <-  stop.x + first.translate
    stop.y.translate <-  stop.y + second.translate
  }
  
  x.delta <- stop.x.translate - start.x.translate
  y.delta <- stop.y.translate - start.y.translate
  delta.error <- abs(y.delta / x.delta)
  if(y.delta) {
    sign.y.delta <- -1
  }
  else {
    sign.y.delta <- 1
  }

  error <- 0
  y <- start.y.translate
  
  new.y = y
  
  temp.result <- matrix(nrow = 2 * (abs(start.x - stop.x) + abs(start.y - stop.y)), ncol = 2)
  counter <- 1
  
  for(x in start.x.translate:stop.x.translate) {
    if(y != new.y) {
      if(sign.y.delta < 0) {
        for(inc.y in new.y:y) {
          temp.result[counter, 1] <- x
          temp.result[counter, 2] <- inc.y
        }
      }
      else {
        for(inc.y in y:new.y) {
          temp.result[counter, 1] <- x
          temp.result[counter, 2] <- inc.y
        }
      }
    }
    else {
      temp.result[counter, 1] <- x
      temp.result[counter, 2] <- y
      
    }
    counter <- counter + 1
    y <- new.y
    
    error <- delta.error
    
    while(error >= 0.5) {
      new.y <- new.y + sign.y.delta
      error <- error - 1
    }
  }
  
  temp.result <- temp.result[which(!(temp[, 1] %in% NA)), ]
  temp.result[ , 1] <- temp.result[ , 1] - first.translate
  temp.result[ , 2] <- temp.result[ , 2] - second.translate
  
  if(swap) {
    temp.result[ , 1] <- rev(temp.result[ , 1])
    temp.result[ , 2] <- rev(temp.result[ , 2])
  }
  
  return(temp.result)
}

