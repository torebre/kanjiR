source("CreateLine.R")
source("PointToLine.R")


ExtractClosestLinesToPoint <-
  function(point.x,
           point.y,
           number.of.lines.to.include,
           kanji.line.data) {
    number.of.lines <- dim(kanji.data.line.data)[1]
    distances <- matrix(nrow = number.of.lines, ncol = 1)
    
    for (j in 1:number.of.lines) {
      start.x <- kanji.line.data[j, 5]
      start.y <- kanji.line.data[j, 6]
      
      stop.x <-
        start.x + ceiling(kanji.line.data[j, 4] * cos(kanji.line.data[j, 3]))
      stop.y <-
        start.y + ceiling(kanji.line.data[j, 4] * sin(kanji.line.data[j, 3]))
      
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
      }
      distances[j] <- min.distance
    }
    
    cutoff.distance <- sort(distances)[number.of.lines.to.include]
    
    which(distances <= cutoff.distance)[1:number.of.lines.to.include]
  }