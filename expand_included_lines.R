top.scores <- quantile(line.scores, c(0.99))
top.scoring.lines <- which(line.scores > top.scores)

expanded.lines <- list()
expanded.line.counter <- 1
for (i in 1:length(top.scoring.lines)) {
  next.line <- position.data[top.scoring.lines[i],]
  current.unicode <- next.line[1]
  
  line1 <-
    position.data[which(position.data[, 7] == next.line[7] &
                          position.data[, 1] == current.unicode), ]
  
  
  all.lines.in.kanji <-
    kanji.line.data[which(kanji.line.data[, 1] == current.unicode), ]
  line1.original <- all.lines.in.kanji[line1[1 , 7] + 1, ]
  
  # line2 <-
  #   position.data[which(position.data[, 7] == next.line[8] &
  #                         position.data[, 1] == current.unicode),]
  
  lines.to.add <- list()
  counter <- 1
  for (i in next.line[8]) {
    line2 <-
      position.data[which(position.data[, 7] == i &
                            position.data[, 1] == current.unicode), ]
    
    lines.to.add[[counter]] <- i
    counter <- counter + 1
    for (j in line2[, 8]) {
      lines.to.add[[counter]] <- j
      counter <- counter + 1
    }
  }
  
  print(unique(unlist(lines.to.add)))
  
  
  for (i in 1:length(lines.to.add)) {
    line1.neighbour <- all.lines.in.kanji[line1[1 , 8] + 1, ]
    
    if (round(line1.original$length) == 0) {
      from.length <- 1
    } else {
      from.length <- line1.original$length
    }
    
    if (round(line1.neighbour$length) == 0) {
      to.length <- 1
    } else {
      to.length <- line1.neighbour$length
    }
    
    relative.length <- from.length / to.length
    angle.diff <- line1.original$angle - line1.neighbour$angle
    row.diff <- line1.original$start_x - line1.neighbour$start_x
    start.pair.distance <-
      sqrt(row.diff ^ 2 + (line1.original$start_y - line1.neighbour$start_y) ^
             2)
    if (row.diff == 0) {
      start.pair.angle.diff <- 0
    } else {
      start.pair.angle.diff <-
        atan(line1.original$start_y - line1.neighbour$start_y)
    }
    
    second.line <-
      GenerateSecondLine(relative.length,
                         angle.diff,
                         start.pair.distance,
                         start.pair.angle.diff)
    expanded.lines[[expanded.line.counter]] <- second.line
    expanded.line.counter <- expanded.line.counter + 1
  }
  
}



line.matrix <- CreateLineMatrix(expanded.lines)

min.x <- 1
min.y <- 1
max.x <- 1
max.y <- 1

for (i in 1:length(expanded.lines)) {
  min.x <- min(min.x, min(expanded.lines[[i]][, 1]))
  min.y <- min(min.y, min(expanded.lines[[i]][, 2]))
  
  max.x <- max(max.x, max(expanded.lines[[i]][, 1]))
  max.y <- max(max.y, max(expanded.lines[[i]][, 2]))
}


x.offset <- -min.x
y.offset <- -min.y


# top.percentile <- quantile(line.matrix, c(0.999))
# top.percentile.matrix.indices <- which(line.matrix < top.percentile, arr.ind =
filtered.matrix <- line.matrix
# filtered.matrix[top.percentile.matrix.indices] <- 0

palette <- c(rgb(1, 1, 1), rainbow(length(unique(filtered.matrix))))
image(filtered.matrix, col = palette)

line.scores <- matrix(NA, nrow = length(lines), ncol = 1)
for(i in 1:length(lines)) {
  shifted.line <- lines[[i]]
  shifted.line[ , 1] <- shifted.line[ , 1] + x.offset + 1
  shifted.line[ , 2] <- shifted.line[ , 2] + y.offset + 1
  
  line.scores[i] <- sum(filtered.matrix[shifted.line])
}

max.indices <- which(line.scores == max(line.scores))

best.fits <- position.data[max.indices]

unicode <- position.data[best.fits[1]]

lines.in.kanji <- position.data[which(unicode == position.data[ , 1]), ]

# DrawLineKanji(unicode, kanji.line.data)
# DrawHighlightedLines(unicode, kanji.line.data, best.fit.line[7:8] + 1)
# DrawIdentifiedLinesInKanji(unicode, kanji.line.data, score.data, position.data)

