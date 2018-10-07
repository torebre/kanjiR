source('CreateLine.R')
source('DrawLineRelativeToInputLine.R')
source('GenerateSecondLine.R')
source('GenerateLineHeatMap.R')
source('CreateLineMatrix.R')

file.name <- '/home/student/workspace/testEncodings/kanji_data_full.csv'
kanji.line.data <- read.table(file.name, header = T, sep = ",")

kanji.unicodes <- unique(kanji.line.data[ , 1])

lines <- list()
counter <- 1
for(i in 1:length(kanji.unicodes)) {
  all.lines.in.kanji <- kanji.line.data[which(kanji.line.data[, 1] == kanji.unicodes[i]), ]

  for(j in 1:dim(all.lines.in.kanji)[1]) {
    line1.original <- all.lines.in.kanji[j, ]
    
    if(line1.original$length < 10) {
      next
    }
    
    lines.draw <- DrawLineRelativeToInputLine(line1.original, all.lines.in.kanji, skip = c(j))
    for(j in 1:length(lines.draw)) {
      lines[[counter]] <- lines.draw[[j]]
      counter <- counter + 1
    }
  }
}

GenerateLineHeatMap(lines)
line.matrix <- CreateLineMatrix(lines)
truehist(line.matrix, prob = F)
image(line.matrix)
