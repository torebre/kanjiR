library(scatterplot3d)

file.name <- '/home/student/workspace/testEncodings/position_data.csv'
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
