# See cluster_closest_lines.R


# Cluster 6 looks like it is centered around a line going slightly down
cluster6.lines <- filtered.lines[mod1$classification == 6, ]

counter <- 1
cluster.6.next.iteration <- matrix(nrow = 6 * dim(cluster6.lines)[1], ncol = 3)
for(i in 1:dim(cluster6.lines)[1]) {
  first.line <- cluster6.lines[i, ]
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
  
  # Find the lines closest to the two lines involved
  closest.lines.2 <- ExtractClosestLinesToLine(first.line.2$start_x, first.line.2$start_y, stop.2.x, stop.2.y, 3, kanji.first.lines.removed)[1:3]
  
  closest.lines.indices <- unique(c(closest.lines.1, closest.lines.2))
  closest.lines <- kanji.first.lines.removed[closest.lines.indices, ]
  
  # TODO Are the lines compared to here rotated correctly?
  
  # Examine how the closest lines relate to the first extracted line
  lines.draw.1 <- ExtractRelativePositions(first.line.1, closest.lines, c(), first.line.1$line_number)
  
  for(i in 1:length(closest.lines.indices)) {
    cluster.6.next.iteration[counter, ] <- lines.draw.1[i , 1:3]
    counter <- counter + 1
  }
  
}

cluster.6.next.iteration <- cluster.6.next.iteration[which(!is.na(cluster.6.next.iteration[ , 1])), ]


BIC.2 <- mclustBIC(cluster.6.next.iteration)
plot(BIC.2)
summary(BIC.2)
mod2 <- Mclust(cluster.6.next.iteration, x = BIC.2)
summary(mod2, parameter = T)

plot(mod2, what = "classification")

mod2$uncertainty

mod2$classification
table(mod2$classification)

