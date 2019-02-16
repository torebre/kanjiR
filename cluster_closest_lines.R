library(scatterplot3d)
library(mclust)

source('DrawHighlightedLines.R')
source('DrawLineKanji.R')
source("ExtractClosestLines2.R")
source("ExtractRelativePositions.R")
source("ExtractClosestLinesToLine2.R")
source('DrawHighlightedLines.R')
source("DrawLineRelative.R")
source("GenerateLineHeatMap.R")



file.name <- '/home/student/workspace/testEncodings/kanji_data_full.csv'
kanji.line.data <- read.table(file.name, header = T, sep = ",")
kanji.unicodes <- unique(kanji.line.data[ , 1]) #[1:10]

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
number.of.closest.lines.to.include <- 3
line.length.cutoff <- 10
for(i in 1:1000) { #length(kanji.unicodes)) {
  # Only use lines that have a length greater than line.length.cutoff
  all.lines.in.kanji <- kanji.line.data[which(kanji.line.data[, 1] == kanji.unicodes[i] & kanji.line.data[ , 4] > line.length.cutoff), ]
  number.of.lines.in.kanji <- dim(all.lines.in.kanji)[1]
  
  if(number.of.lines.in.kanji < 1) {
    # Only looking at characters with multiple lines
    next
  }
  
  print(paste("Current unicode:", kanji.unicodes[i], " i: ", i, " Number of kanji: ", length(kanji.unicodes)))
  
  for(j in 1:number.of.lines.in.kanji) {
    current.line <- all.lines.in.kanji[j, ]
  
    stop.1.x <- current.line$start_x + ceiling(current.line$length * sin(current.line$angle))
    stop.1.y <- current.line$start_y + ceiling(current.line$length * cos(current.line$angle))
    closest.lines <- ExtractClosestLinesToLine2(current.line$start_x, current.line$start_y, stop.1.x, stop.1.y, number.of.closest.lines.to.include, all.lines.in.kanji[-j, ])
    
    # TODO Figure out a way of handling this better
    if(anyNA(closest.lines)) {
      next
    }
    
    closest.lines.in.kanji <- all.lines.in.kanji[which(all.lines.in.kanji[ , 2] %in% closest.lines), ]
    lines.draw <- ExtractRelativePositions(current.line, closest.lines.in.kanji, c(), 
                                           as.integer(rownames(all.lines.in.kanji[j, ])), use.relative.distance = F)
    
    for(k in 1:dim(lines.draw)[1]) {
      lines[counter, ] <- lines.draw[k, ]
      counter <- counter + 1
    }
  }
}

filtered.lines <- lines[which(!is.na(lines[ , 1])) ,]
filtered.matrix <-filtered.lines[, 1:3]
# filtered.matrix.normalized <- apply(filtered.matrix[ , 1:3], 2, function(x) (x- min(x))/(max(x) - min(x)))

op <- par(mfrow = c(4, 4))
for(i in 1:16) {
  DrawHighlightedLines(filtered.lines[i, 6], kanji.line.data[which(kanji.line.data == filtered.lines[i, 6]) ,], filtered.lines[i, 7:8])
}
par(op)




BIC <- mclustBIC(filtered.matrix[ , 1:3])
plot(BIC)
summary(BIC)
clustered.lines.model <- Mclust(filtered.matrix, x = BIC)
summary(clustered.lines.model, parameter = T)

plot(clustered.lines.model, what = "classification")

table(clustered.lines.model$classification)

clust.combi.result <- clustCombi(clustered.lines.model)
plot(clust.combi.result)

# Generate a heat-map showing the lines in the clusters
for(i in 1:clustered.lines.model$G) {
  cluster.lines <- filtered.lines[clustered.lines.model$classification == i, ]
  # cluster.draw.lines <- DrawLineRelative(cluster.lines)
  # GenerateLineHeatMap(cluster.draw.lines)
  
  cluster.lines.uncertainty <- clustered.lines.model$uncertainty[clustered.lines.model$classification == i]
  cluster.lines.high.certainty <- cluster.lines[which(cluster.lines.uncertainty < 0.2), ]
  cluster.temp.lines <- DrawLineRelative(cluster.lines.high.certainty)
  GenerateLineHeatMap(cluster.temp.lines)
}

# Cluster 1
cluster1.lines <- filtered.lines[clustered.lines.model$classification == 1, ]

cluster.1.next.iteration <- matrix(nrow = 6 * dim(cluster1.lines)[1], ncol = 9)

cluster6.lines <- filtered.lines[mod1$classification == 6, ]
cluster.6.next.iteration <- matrix(nrow = 6 * dim(cluster6.lines)[1], ncol = 9)

counter <- 1
for(i in 1:dim(cluster6.lines)[1]) {
  first.line <- cluster6.lines[i, ]
  
  first.line.kanji <- kanji.line.data[which(kanji.line.data[ , 1] == first.line[6]), ]
  
  first.kanji.index <- which(first.line.kanji[ , 2] == first.line[7])
  second.kanji.index <- which(first.line.kanji[ , 2] == first.line[8])
  
  kanji.first.lines.removed <- first.line.kanji[-c(first.kanji.index, second.kanji.index ), ]
  
  first.line.1 <- first.line.kanji[which(first.line.kanji[ , 2] == first.line[7]), ]
  first.line.2 <-  first.line.kanji[which(first.line.kanji[ , 2] == first.line[8]), ]
  
  stop.1.x <- first.line.1$start_x + ceiling(first.line.1$length * sin(first.line.1$angle))
  stop.1.y <- first.line.1$start_y + ceiling(first.line.1$length * cos(first.line.1$angle))
  closest.lines.1 <- ExtractClosestLinesToLine(first.line.1$start_x, first.line.1$start_y, stop.1.x, stop.1.y, 3, kanji.first.lines.removed)[1:3]
  
  stop.2.x <- first.line.2$start_x  + ceiling(first.line.2$length * sin(first.line.2$angle))
  stop.2.y <- first.line.2$start_y + ceiling(first.line.2$length * cos(first.line.2$angle))
  
  # Find the lines closest to the two lines involved
  closest.lines.2 <- ExtractClosestLinesToLine2(first.line.2$start_x, first.line.2$start_y, stop.2.x, stop.2.y, 3, kanji.first.lines.removed)[1:3]
  
  closest.lines.numbers <- unique(c(closest.lines.1, closest.lines.2))
  closest.lines <- kanji.first.lines.removed[which(kanji.first.lines.removed[ , 2] %in% closest.lines.numbers), ]
  
  # TODO Are the lines compared to here rotated correctly?
  
  # Examine how the closest lines relate to the first extracted line
  lines.draw.1 <- ExtractRelativePositions(first.line.1, closest.lines, c(), first.line.1$line_number)
  
  for(i in 1:dim(lines.draw.1)[1]) {
    cluster.6.next.iteration[counter, ] <- c(lines.draw.1[i, ], first.line.2$line_number)
    counter <- counter + 1
  }
}


cluster.6.next.iteration <- cluster.6.next.iteration[which(!is.na(cluster.6.next.iteration[ , 1])), ]


BIC.2 <- mclustBIC(cluster.6.next.iteration[ , 1:3])
plot(BIC.2)
summary(BIC.2)
mod2 <- Mclust(cluster.6.next.iteration[ , 1:3], x = BIC.2)
summary(mod2, parameter = T)

plot(mod2, what = "classification")

mod2$uncertainty

mod2$classification
table(mod2$classification)






cluster.1.1 <- cluster.1.next.iteration[which(mod2$classification == 1), ]
op <- par(mfrow = c(4, 4))
for(i in 1:16) {
  DrawHighlightedLines(cluster.1.next.iteration[i, 6], kanji.line.data[which(kanji.line.data == cluster.1.next.iteration[i, 6]) ,], cluster.1.next.iteration[i, 7:9] + 1)
}
par(op)


for (i in 1:clustered.lines.model$G) {
  cluster.lines <- filtered.lines[clustered.lines.model$classification == i,]
  cluster.lines.uncertainty <- clustered.lines.model$uncertainty[clustered.lines.model$classification == i]
  cluster.lines.high.certainty <- cluster.lines[which(cluster.lines.uncertainty < 0.2), ]
  
  op <- par(mfrow = c(4, 4))
  for (j in 1:16) {
    DrawHighlightedLines(cluster.lines[j, 6], kanji.line.data[which(kanji.line.data == cluster.lines[j, 6]) , ], cluster.lines[j, 7:8])
  }
  par(op)
}


cluster.1.6 <- cluster.6.next.iteration[which(mod2$classification == 1), ]
op <- par(mfrow = c(4, 4))
for(i in 1:16) {
  DrawHighlightedLines(cluster.6.next.iteration[i, 6], kanji.line.data[which(kanji.line.data == cluster.6.next.iteration[i, 6]) ,], cluster.6.next.iteration[i, 7:9] + 1)
}
par(op)





