library(scatterplot3d)
source('CreateLine.R')
source('DrawLines.R')
source('DrawLinePair.R')
source('GenerateSecondLine.R')

# unicode, relative_length, angle_diff, start_pair_distance, relative_distance
#file.name <- '/home/student/workspace/testEncodings/position_data_2.csv'

# unicode, relative_length, angle_diff, start_pair_distance, relative_distance, start_pair_angle_diff
file.name <- '/home/student/workspace/testEncodings/position_data_4.csv'
#dat <- readChar(fileName, file.info(file.name)$size)

number.of.fields =max(count.fields(file.name, sep = ','))

dat <- read.table(file.name, header = TRUE, 
                  sep = ",", #col.names = paste0("V",seq_len(number.of.fields)), 
                  colClasses=rep("numeric", number.of.fields),
                  fill = TRUE)

position.data.original <- data.matrix(dat)

position.data <- position.data.original


clusters <- hclust(dist(position.data[, 2:6]))
plot(clusters)


scatterplot3d(position.data[, 2], position.data[, 5], position.data[, 4])



scaled.position.data <- scale(position.data[, 2:5])
clusters.scaled.data <- hclust(dist(scaled.position.data))

plot(clusters.scaled.data)

cluster.cut <- cutree(clusters.scaled.data, h = 6)
plot(cluster.cut)


match(1, cluster.cut)

unique(cluster.cut)
cluster.1 <- which(cluster.cut %in% 1)
cluster.2 <- which(cluster.cut %in% 2)
cluster.3 <- which(cluster.cut %in% 3)
cluster.4 <- which(cluster.cut %in% 4)

relative.length <- mean(position.data[cluster.1, 2])
angle.diff <- mean(position.data[cluster.1, 3])
start.pair.difference <- mean(position.data[cluster.1, 4])
relative.start.distance <- mean(position.data[cluster.1, 5])
start.pair.angle.diff <- mean(position.data[cluster.1, 6])

DrawLinePair(relative.length, angle.diff, start.pair.difference, start.pair.angle.diff)


lines <- list()
for(i in 1:dim(position.data)[1]) {
  lines[[i]] <- GenerateSecondLine(position.data[i, 2], position.data[i, 3], position.data[i, 4], position.data[i, 6])
}

DrawLines(lines)
GenerateLineHeatMap(lines)

hist(line.matrix)
truehist(line.matrix, prob = F)


mean(position.data[cluster.2, 2])
mean(position.data[cluster.2, 3])
mean(position.data[cluster.2, 4])
mean(position.data[cluster.2, 5])

relative.length.2 <- mean(position.data[cluster.2, 2])
angle.diff.2 <- mean(position.data[cluster.2, 3])
start.pair.difference.2 <- mean(position.data[cluster.2, 4])
relative.start.distance.2 <- mean(position.data[cluster.2, 5])
start.pair.angle.diff.2 <- mean(position.data[cluster.2, 6])

DrawLinePair(relative.length.2, angle.diff.2, start.pair.difference.2, start.pair.angle.diff.2)


mean(position.data[cluster.3, 2])
mean(position.data[cluster.3, 3])
mean(position.data[cluster.3, 4])
mean(position.data[cluster.3, 5])



mean(position.data[cluster.4, 2])
mean(position.data[cluster.4, 3])
mean(position.data[cluster.4, 4])
mean(position.data[cluster.4, 5])
