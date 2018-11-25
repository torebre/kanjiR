source("CreateLine.R")
source("PointToLine.R")

ExtractClosestLinesToLine <-
  function(current.start.x,
           current.start.y,
           current.stop.x,
           current.stop.y,
           number.of.lines.to.include,
           kanji.data) {
    number.of.lines <- dim(kanji.data)[1]
    
    distances <- c(nrow = number.of.lines)
    for (j in 1:number.of.lines) {
      start.x <- kanji.data[j, 5]
      start.y <- kanji.data[j, 6]
      
      stop.x <-
        start.x + ceiling(kanji.data[j, 4] * cos(kanji.data[j, 3]))
      stop.y <-
        start.y + ceiling(kanji.data[j, 4] * sin(kanji.data[j, 3]))
      
      # print(paste("Test23:",start.x, start.y, stop.x, stop.y))
      
      line <- CreateLine(start.x, start.y, stop.x, stop.y)
      
      min.distance <- .Machine$integer.max
      for (k in 1:nrow(line)) {
        # print(paste('k:', k, 'lines:', nrow(line)))
        
        distance <-
          PointToLine(
            current.start.x,
            current.start.y,
            current.stop.x,
            current.stop.y,
            line[k, 1],
            line[k, 2]
          )
        
        if (distance < min.distance) {
          min.distance <- distance
        }
        
        # print(paste('Distance:', distance))
      }
      
      # print(paste("Distance ", j, ":", distance))
      
      distances[j] <- min.distance
    }
    
    cutoff.distance <- sort(distances)[number.of.lines.to.include]
    closest.elements <- which(distances <= cutoff.distance)
    
    closest.elements[1:number.of.lines.to.include]
  }