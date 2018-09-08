library(scatterplot3d)
source('CreateLine.R')
source('DrawLines.R')
source('DrawLinePair.R')
source('GenerateSecondLine.R')
source('CreateLineMatrix.R')

# unicode, relative_length, angle_diff, start_pair_distance, relative_distance
#file.name <- '/home/student/workspace/testEncodings/position_data_2.csv'

# unicode, relative_length, angle_diff, start_pair_distance, relative_distance, start_pair_angle_diff
file.name <- '/home/student/workspace/testEncodings/position_data_4.csv'

number.of.fields =max(count.fields(file.name, sep = ','))

dat <- read.table(file.name, header = TRUE, 
                  sep = ",",
                  colClasses=rep("numeric", number.of.fields),
                  fill = TRUE)

lines <- list()
for(i in 1:dim(position.data)[1]) {
  lines[[i]] <- GenerateSecondLine(position.data[i, 2], position.data[i, 3], position.data[i, 4], position.data[i, 6])
}

for(i in 1:length(lines)) {
  if(length(lines[[i]]) / 2 > 10) {
    lines[[i]] <- lines[[i]][1:10, ]
  }
}


number.of.points <- sum(sapply(lines, function(x) {
  length(x[ , 1])
}))

line.points <- matrix(nrow = number.of.points, ncol = 2)
counter <- 1
for(i in 1:length(lines)) {
  points.in.line <- length(lines[[i]][ , 1])
  line.points[counter:(counter + points.in.line - 1), 1] <- lines[[i]][ , 1]
  line.points[counter:(counter + points.in.line - 1), 2] <- lines[[i]][ , 2]
  counter <- counter + points.in.line
}


line.matrix.pca <- prcomp(line.points, center = F, scale. = F)
biplot(line.matrix.pca)
line.matrix.pca$rotation


line.endpoints <- matrix(nrow = 2 * length(lines), ncol = 2)
counter <- 1
for(i in 1:(2 * length(lines))) {
  line.endpoints[counter, 1] <- lines[[i]][1]
  line.endpoints[counter, 2] <- lines[[i]][2]
  counter <- counter + 1
  
  line.endpoints[counter, 1] <- lines[[i]][1]
  line.endpoints[counter, 2] <- lines[[i]][2]
  counter <- counter + 1
}


line.matrix.pca <- prcomp(line.points[1:10000, ], center = F, scale. = F)
biplot(line.matrix.pca)
line.matrix.pca$rotation


DrawLines(lines)
GenerateLineHeatMap(lines)


line.matrix <- CreateLineMatrix(lines)

min(line.matrix)
max(line.matrix)

palette <- c(rgb(1, 1, 1), rainbow(length(lines)))
image(line.matrix, col = palette)

top.percentile <- quantile(line.matrix, c(0.99))
top.percentile.matrix.indices <- which(line.matrix < top.percentile, arr.ind = T)
filtered.matrix <- line.matrix
filtered.matrix[top.percentile.matrix.indices] <- 0

palette <- c(rgb(1, 1, 1), rainbow(length(unique(filtered.matrix))))
image(filtered.matrix, col = palette)

filtered.line.vector <- as.vector(line.matrix)

filtered.line.vector[which(filtered.line.vector < top.percentile)] <- 0

filtered.line.vector2 <- as.vector(line.matrix)
filtered.line.vector2 <- filtered.line.vector2[which(filtered.line.vector < top.percentile)]


top.percentile.matrix.indices <- which(line.matrix > top.percentile, arr.ind = T)
top.percentile.pca <- prcomp(top.percentile.matrix.indices, center = F, scale. = F)

dims <- dim(line.matrix)
filtered.line.matrix <- matrix(filtered.line.vector, dims[1], dims[2])

filtered.line.pca <- prcomp(filtered.line.matrix, center = F, scale. = F)

line.lengths <- sapply(lines, function(x) { length(x) / 2})
