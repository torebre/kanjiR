library(scatterplot3d)
source('CreateLine.R')
source('DrawLines.R')

# unicode, relative_length, angle_diff, start_pair_distance, relative_distance
#file.name <- '/home/student/workspace/testEncodings/position_data_2.csv'

# unicode, relative_length, angle_diff, start_pair_distance, relative_distance, start_pair_angle_diff
file.name <- '/home/student/workspace/testEncodings/position_data_3.csv'
#dat <- readChar(fileName, file.info(file.name)$size)

number.of.fields =max(count.fields(file.name, sep = ','))

dat <- read.table(file.name, header = FALSE, sep = ",", col.names = paste0("V",seq_len(number.of.fields)), fill = TRUE)

position.data.original <- data.matrix(dat)

position.data <- position.data.original[1:50 , ]


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


line2.length <- 10
line1.length <- line2.length * relative.length
line2.angle <- 0
line1.angle <- angle.diff

line1.start.x <- 0
line1.start.y <- 0

line1.end.x <- line1.length * cos(angle.diff)
line1.end.y <- line1.length * sin(angle.diff)

line2.start.x <- start.pair.difference * cos(start.pair.angle.diff)
line2.start.y <- start.pair.difference * sin(start.pair.angle.diff)

line2.end.x <- line2.start.x + line2.length
line2.end.y <- line2.start.y


CreateLine(line1.start.x, line1.start.y, line1.end.x, line1.end.y)

lines <- list(c(line1.start.x, line1.start.y), c(line2.start.x, line2.start.y))
DrawLines(lines)



mean(position.data[cluster.2, 2])
mean(position.data[cluster.2, 3])
mean(position.data[cluster.2, 4])
mean(position.data[cluster.2, 5])


mean(position.data[cluster.3, 2])
mean(position.data[cluster.3, 3])
mean(position.data[cluster.3, 4])
mean(position.data[cluster.3, 5])



mean(position.data[cluster.4, 2])
mean(position.data[cluster.4, 3])
mean(position.data[cluster.4, 4])
mean(position.data[cluster.4, 5])
