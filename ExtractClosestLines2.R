ExtractClosestLines2 <- function(kanji.unicode.to.process, number.of.lines.to.include, kanji.line.data) {
  kanji.data <- kanji.line.data[which(kanji.line.data[, 1] == kanji.unicode.to.process), ]
  number.of.lines <- dim(kanji.data)[1]
  result <- matrix(nrow = number.of.lines, ncol = number.of.lines.to.include)
  
  for(i in 1:number.of.lines) {
    current.start.x <- kanji.data[i, 5]
    current.start.y <- kanji.data[i, 6]
    
    current.stop.x <- current.start.x + ceiling(kanji.data[i, 4] * sin(kanji.data[i, 3]))
    current.stop.y <- current.start.y + ceiling(kanji.data[i, 4] * cos(kanji.data[i, 3]))
    
    # line <- CreateLine(start.x, start.y, stop.x, stop.y)
    
    closest.lines.2 <- ExtractClosestLinesToLine2(current.start.x, current.start.y, current.stop.x, current.stop.y, number.of.lines.to.include, kanji.data[-i, ])
    result[i, ] <- closest.lines.2
  }
  
  result
}