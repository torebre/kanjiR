ExtractRelativePositions <- function(input.line, all.lines.in.kanji, skip = c(), line.number.input) {
  result <- matrix(nrow = dim(all.lines.in.kanji)[1] - length(skip), ncol = 5)
  counter <- 1
  
  
  for(i in 1:dim(all.lines.in.kanji)[1]) {
    if(i %in% skip) {
      next
    }
    
    line2 <- all.lines.in.kanji[i, ]
    
    if (round(input.line$length) == 0) {
      from.length <- 1
    } else {
      from.length <- input.line$length
    }
    
    if (round(line2$length) == 0) {
      to.length <- 1
    } else {
      to.length <- line2$length
    }
    
    relative.length <- from.length / to.length
    row.diff <- line2$start_y - input.line$start_y
    column.diff <- line2$start_x - input.line$start_x
    start.pair.distance <- sqrt(row.diff ^ 2 + column.diff ^ 2)
    start.pair.angle.diff <- atan2(row.diff, column.diff)
    
    second.line.angle <- line2$angle - input.line$angle
    start.pair.second.line.angle <- start.pair.angle.diff + input.line$angle
    
    result[counter, 1] <- abs(row.diff) / input.line$length
    result[counter, 2] <- abs(column.diff) / input.line$length
    result[counter, 3] <- start.pair.second.line.angle
    result[counter, 4] <- line.number.input
    result[counter, 5] <- as.integer(rownames(all.lines.in.kanji[i, ]))
    
    counter <- counter + 1
  }
  
  result
}

# file.name <- '/home/student/workspace/testEncodings/kanji_data_full.csv'
# kanji.line.data <- read.table(file.name, header = T, sep = ",")

kanji.unicodes <- unique(kanji.line.data[ , 1])[1:10]




number.of.rows <- 0
for(i in 1:length(kanji.unicodes)) {
  all.lines.in.kanji <- kanji.line.data[which(kanji.line.data[, 1] == kanji.unicodes[i] & kanji.line.data[ , 4] > 5), ]
  line.count <- dim(all.lines.in.kanji)[1]
  
  for(j in 1:line.count) {
    line1.original <- all.lines.in.kanji[j, ]
    
    # if(line1.original$length < 10) {
    #   next
    # }
    
    number.of.rows <- number.of.rows + line.count - 1
  }
}


lines <- matrix(nrow = number.of.rows, ncol = 5)
counter <- 1
for(i in 1:length(kanji.unicodes)) {
  # all.lines.in.kanji <- kanji.line.data[which(kanji.line.data[, 1] == kanji.unicodes[i]), ]
  all.lines.in.kanji <- kanji.line.data[which(kanji.line.data[, 1] == kanji.unicodes[i] & kanji.line.data[ , 4] > 5), ]
  
  for(j in 1:dim(all.lines.in.kanji)[1]) {
    line1.original <- all.lines.in.kanji[j, ]
    
    # if(line1.original$length < 10) {
    #   next
    # }
    
    lines.draw <- ExtractRelativePositions(line1.original, all.lines.in.kanji, c(j), 
                                           as.integer(rownames(all.lines.in.kanji[j, ])))
    
    
    for(j in 1:dim(lines.draw)[1]) {
      lines[counter, ] <- lines.draw[j, ]
      counter <- counter + 1
    }
  }
}
