library("MASS")

source('CreateLine.R')
source('DrawLineRelativeToInputLine.R')
source('GenerateSecondLine.R')
source('GenerateLineHeatMap.R')
source('CreateLineMatrix.R')
source('DrawLines.R')


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




top.percentile <- quantile(line.matrix, c(0.99))
top.percentile.matrix.indices <- which(line.matrix < top.percentile, arr.ind = T)
filtered.matrix <- line.matrix
filtered.matrix[top.percentile.matrix.indices] <- 0

palette <- c(rgb(1, 1, 1), rainbow(length(unique(filtered.matrix))))
image(filtered.matrix, col = palette)



min.x <- 1
min.y <- 1
max.x <- 1
max.y <- 1

for (i in 1:length(lines)) {
  min.x <- min(min.x, min(lines[[i]][, 1]))
  min.y <- min(min.y, min(lines[[i]][, 2]))
  
  max.x <- max(max.x, max(lines[[i]][, 1]))
  max.y <- max(max.y, max(lines[[i]][, 2]))
}


x.offset <- -min.x
y.offset <- -min.y

line.scores <- matrix(NA, nrow = length(lines), ncol = 1)
for(i in 1:length(lines)) {
  shifted.line <- lines[[i]]
  shifted.line[ , 1] <- shifted.line[ , 1] + x.offset + 1
  shifted.line[ , 2] <- shifted.line[ , 2] + y.offset + 1
  
  line.scores[i] <- sum(filtered.matrix[shifted.line])
}

max.line.index <- which(line.scores==max(line.scores))

lines[[max.line.index]]

DrawLines(list(lines[[max.line.index]]))