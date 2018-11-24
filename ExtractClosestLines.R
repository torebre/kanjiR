source("CreateLine.R")
source("PointToLine.R")


ExtractClosestLines <- function(kanji.unicode.to.process, number.of.lines.to.include, kanji.line.data) {
  kanji.data <- kanji.line.data[which(kanji.line.data[, 1] == kanji.unicode.to.process), ]
  number.of.lines <- dim(kanji.data)[1]
  result <- matrix(nrow = number.of.lines, ncol = number.of.lines.to.include)
  
  for(i in 1:number.of.lines) {
    current.start.x <- kanji.data[i, 5]
    current.start.y <- kanji.data[i, 6]
    
    current.stop.x <- current.start.x + ceiling(kanji.data[i, 4] * cos(kanji.data[i, 3]))
    current.stop.y <- current.start.y + ceiling(kanji.data[i, 4] * sin(kanji.data[i, 3]))
    
    # line <- CreateLine(start.x, start.y, stop.x, stop.y)
    
    distances <- matrix(nrow = number.of.lines, ncol = 1)
    for(j in 1:number.of.lines) {
      if(i == j) {
        distances[j] <- .Machine$integer.max
        next
      }
      
      start.x <- kanji.data[j, 5]
      start.y <- kanji.data[j, 6]
      
      stop.x <- start.x + ceiling(kanji.data[j, 4] * cos(kanji.data[j, 3]))
      stop.y <- start.y + ceiling(kanji.data[j, 4] * sin(kanji.data[j, 3]))
      
      # print(paste("Test23:",start.x, start.y, stop.x, stop.y))
      
      line <- CreateLine(start.x, start.y, stop.x, stop.y)
      
      min.distance <- .Machine$integer.max
      for(k in 1:nrow(line)) {
        
        # print(paste('k:', k, 'lines:', nrow(line)))
        
        distance <- PointToLine(current.start.x, current.start.y, current.stop.x, current.stop.y, 
                                line[k, 1], line[k, 2])
        
        if(distance < min.distance) {
          min.distance <- distance
        }
        
        # print(paste('Distance:', distance))
      }
      
      # print(paste("Distance ", j, ":", distance))
      
      distances[j] <- min.distance
    }
    
    cutoff.distance <- sort(distances)[number.of.lines.to.include]
    closest.elements <- which(distances <= cutoff.distance)
    
    result[i, ] <- closest.elements[1:number.of.lines.to.include]
  }
  
  result
}