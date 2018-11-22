source('ExtractRelativePositions.R')

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


lines <- matrix(nrow = number.of.rows, ncol = 8)
counter <- 1
for(i in 1:length(kanji.unicodes)) {
  # Only use lines that have a length greater than 5
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
