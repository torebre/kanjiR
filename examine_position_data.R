library(scatterplot3d)

file.name <- '/home/student/workspace/testEncodings/position_data_2.csv'
#dat <- readChar(fileName, file.info(file.name)$size)

number.of.fields =max(count.fields(file.name, sep = ','))

dat <- read.table(file.name, header = FALSE, sep = ",", col.names = paste0("V",seq_len(number.of.fields)), fill = TRUE)

position.data <- data.matrix(dat)


clusters <- hclust(dist(position.data[, 2:5]))
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


mean(position.data[cluster.1, 2])
mean(position.data[cluster.1, 3])
mean(position.data[cluster.1, 4])
mean(position.data[cluster.1, 5])


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


