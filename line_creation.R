source('CreateLine.R')

file.name <- '/home/student/workspace/testEncodings/kanji_data_2.csv'
dat <- read.table(file.name, header = T, sep = ",")


kanji.unicode <- unique(dat[,1])


kanji <- which(dat[, 1] %in% kanji.unicode[1])



lines <- list()

for(i in 1:length(kanji)) {
  line.data <- dat[kanji[i], ]
  
  x.start <- unlist(line.data[5])
  y.start <- unlist(line.data[6])
  x.offset <- round(unlist(line.data[4] * sin(line.data[3])))
  y.offset <- round(unlist(line.data[4] * cos(line.data[3])))
  
  print(cat("Line:", i, ". x.offset: ", x.offset, ". y.offset: ", y.offset))
  
  lines[[i]] <- CreateLine(x.start, y.start, x.start + x.offset, y.start + y.offset)
  
  print(cat("Line: ", i, "Line number: ", unlist(line.data[2]), "Start: ", x.start, y.start, "Stop:", x.start + x.offset, y.start + y.offset))
  print(lines[[i]])
}

min.x <- 0
min.y <- 0
max.x <- 10
max.y <- 10

for(i in 1:length(lines)) {
  min.x <- min(min.x, min(lines[[i]][ , 1]))
  min.y <- min(min.y, min(lines[[i]][ , 2]))
  
  max.x <- max(max.x, max(lines[[i]][ , 1]))
  max.y <- max(max.y, max(lines[[i]][ , 2]))
}


image.matrix <- matrix(0, nrow = abs(max.x - min.x), ncol = abs(max.y - min.y))
x.offset <- -min.x
y.offset <- -min.y

counter <- 1

palette <- c(rgb(1, 1, 1), rainbow(length(lines)))

for(i in 1:length(lines)) {
  current.line <- lines[[i]]
  for(j in 1:dim(current.line)[1]) {
    row <- current.line[j, 1] + x.offset
    row <- dim(image.matrix)[1] - row
    column <- current.line[j, 2] + y.offset
    image.matrix[row, column] <- counter
  }
  counter <- counter + 1
}


image(t(image.matrix), col = palette)
