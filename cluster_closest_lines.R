library(scatterplot3d)
library(mclust)

source('DrawHighlightedLines.R')
source('DrawLineKanji.R')
source("ExtractClosestLines.R")
source("ExtractRelativePositions.R")
source("ExtractClosestLinesToLine.R")



file.name <- '/home/student/workspace/testEncodings/kanji_data_full.csv'
kanji.line.data <- read.table(file.name, header = T, sep = ",")
kanji.unicodes <- unique(kanji.line.data[ , 1])[1:10]

number.of.rows <- 0
for(i in 1:length(kanji.unicodes)) {
  all.lines.in.kanji <- kanji.line.data[which(kanji.line.data[, 1] == kanji.unicodes[i] & kanji.line.data[ , 4] > 5), ]
  line.count <- dim(all.lines.in.kanji)[1]
  
  for(j in 1:line.count) {
    line1.original <- all.lines.in.kanji[j, ]
    number.of.rows <- number.of.rows + line.count - 1
  }
}


lines <- matrix(nrow = number.of.rows, ncol = 8)
counter <- 1
for(i in 1:length(kanji.unicodes)) {
  # Only use lines that have a length greater than 5
  all.lines.in.kanji <- kanji.line.data[which(kanji.line.data[, 1] == kanji.unicodes[i] & kanji.line.data[ , 4] > 5), ]
  closest.lines <- ExtractClosestLines(kanji.unicodes[i], 3, all.lines.in.kanji)
  
  for(j in 1:dim(closest.lines)[1]) {
    closest.lines.in.kanji <- all.lines.in.kanji[closest.lines[j, ], ]
    line1.original <- all.lines.in.kanji[j, ]
    
    # print(paste("Original line: ", line1.original))
    # print(closest.lines.in.kanji)
    
    lines.draw <- ExtractRelativePositions(line1.original, closest.lines.in.kanji, c(), 
                                           as.integer(rownames(all.lines.in.kanji[j, ])))
    
    for(j in 1:dim(lines.draw)[1]) {
      lines[counter, ] <- lines.draw[j, ]
      counter <- counter + 1
    }
  }
}


filtered.lines <- lines[which(!is.na(lines[ , 1])) ,]
filtered.matrix <-filtered.lines[, 1:3]

BIC <- mclustBIC(filtered.matrix[ , 1:3])
plot(BIC)
summary(BIC)
mod1 <- Mclust(filtered.matrix, x = BIC)
summary(mod1, parameter = T)

plot(mod1, what = "classification")

table(mod1$classification)

# Cluster 1
cluster1.lines <- filtered.lines[mod1$classification == 1, ]

for(i in 1:mod1$G) {
  cluster.lines <- filtered.lines[mod1$classification == i, ]
  cluster.draw.lines <- DrawLineRelative(cluster.lines)
  GenerateLineHeatMap(cluster.draw.lines)  
}





cluster.1.next.iteration <- matrix(nrow = 6 * dim(cluster1.lines)[1], ncol = 6)
counter <- 1
for(i in 1:dim(cluster1.lines)[1]) {
  first.line <- cluster1.lines[i, ]
  first.line.kanji <- kanji.line.data[which(kanji.line.data[ , 1] == first.line[6]), ]
  
  first.kanji.index <- which(first.line.kanji[ , 2] == first.line[7])
  second.kanji.index <- which(first.line.kanji[ , 2] == first.line[8])
  
  kanji.first.lines.removed <- first.line.kanji[-c(first.kanji.index, second.kanji.index ), ]
  
  first.line.1 <- first.line.kanji[which(first.line.kanji[ , 2] == first.line[7]), ]
  first.line.2 <-  first.line.kanji[which(first.line.kanji[ , 2] == first.line[8]), ]
  
  stop.1.x <- first.line.1$start_x + ceiling(first.line.1$length * cos(first.line.1$angle))
  stop.1.y <- first.line.1$start_y + ceiling(first.line.1$length * sin(first.line.1$angle))
  closest.lines.1 <- ExtractClosestLinesToLine(first.line.1$start_x, first.line.1$start_y, stop.1.x, stop.1.y, 3, kanji.first.lines.removed)[1:3]
  
  stop.2.x <- first.line.2$start_x  + ceiling(first.line.2$length * cos(first.line.2$angle))
  stop.2.y <- first.line.2$start_y + ceiling(first.line.2$length * sin(first.line.2$angle))
  
  closest.lines.2 <- ExtractClosestLinesToLine(first.line.2$start_x, first.line.2$start_y, stop.2.x, stop.2.y, 3, kanji.first.lines.removed)[1:3]
  
  closest.lines.indices <- unique(c(closest.lines.1, closest.lines.2))
  closest.lines <- kanji.first.lines.removed[closest.lines.indices, ]
  
  lines.draw.1 <- ExtractRelativePositions(first.line.1, closest.lines, c(), first.line.1$line_number)
  
  lines.draw.2 <- ExtractRelativePositions(first.line.2, closest.lines, c(), first.line.2$line_number)

  for(i in 1:length(closest.lines.indices)) {
    temp <- c(lines.draw.1[i , 1:3], lines.draw.2[i , 1:3])
    cluster.1.next.iteration[counter, ] <- temp
    counter <- counter + 1
  }
  
}

cluster1.next.iteration <- cluster.1.next.iteration[which(!is.na(cluster.1.next.iteration[ , 1])), ]


BIC.2 <- mclustBIC(cluster1.next.iteration)
plot(BIC.2)
summary(BIC.2)
mod2 <- Mclust(cluster1.next.iteration, x = BIC.2)
summary(mod2, parameter = T)

plot(mod2, what = "classification")

mod2$uncertainty

mod2$classification
table(mod2$classification)

