DrawLinesInKanji <- function(kanji.data) {
  lines <- list()
  
  for(i in 1:dim(kanji.data)[1]) {
    line.data <-  kanji.data[i, ]
    
    x.start <- unlist(line.data[5])
    y.start <- unlist(line.data[6])
    x.offset <- round(unlist(line.data[4] * sin(line.data[3])))
    y.offset <- round(unlist(line.data[4] * cos(line.data[3])))
    
    print(cat("Line:", i, ". x.offset: ", x.offset, ". y.offset: ", y.offset))
    
    lines[[i]] <- CreateLine(x.start, y.start, x.start + x.offset, y.start + y.offset)
    
    print(cat("Line: ", i, "Line number: ", unlist(line.data[2]), "Start: ", x.start, y.start, "Stop:", x.start + x.offset, y.start + y.offset))
    #print(lines[[i]])
  }
  
  DrawLines(lines, transpose = T)
}