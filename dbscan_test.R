library(dbscan)
library(MASS)



filtered.matrix.normalized <- apply(filtered.matrix[ , 2:3], 2, function(x) (x- min(x))/(max(x) - min(x)))

nearest.neighbour.distance <- kNNdist(filtered.matrix.normalized, k = 5)
kNNdistplot(filtered.matrix.normalized, k = 5)


clustering <- dbscan(filtered.matrix, eps = 0.1, minPts = 1000)

cluster1.dbscan <- filtered.lines[which(clustering$cluster == 1), ]
op <- par(mfrow = c(4, 4))
for(i in 500:516) {
  DrawHighlightedLines(cluster1.dbscan[i, 6], kanji.line.data[which(kanji.line.data == cluster1.dbscan[i, 6]) ,], cluster1.dbscan[i, 7:8] + 1)
}
par(op)






summary(clustering)

hullplot(filtered.matrix.normalized, clustering)

x.min <- min(filtered.matrix[ , 3])
x.max <- max(filtered.matrix[ , 3])
only.angle <- sapply(filtered.matrix[ , 3], function(x) (x- x.min/(x.max - x.min)))
angle.distance <- kNNdist(as.matrix(only.angle), k = 5)
kNNdistplot(as.matrix(only.angle), k = 5)


truehist(only.angle, prob = F)

x.min.x.offset <- min(filtered.matrix[ , 1])
x.max.x.offset <- max(filtered.matrix[ , 1])
x.offset.distance <- sapply(filtered.matrix[ , 1], function(x) (x- x.min.x.offset/(x.max.x.offset - x.min.x.offset)))
x.offset.knn <- kNNdist(as.matrix(x.offset.distance), k = 5)
kNNdistplot(as.matrix(x.offset.distance), k = 5)

truehist(x.offset.distance, prob = F)


y.min.y.offset <- min(filtered.matrix[ , 2])
y.max.y.offset <- max(filtered.matrix[ , 2])
y.offset.distance <- sapply(filtered.matrix[ , 2], function(x) (x- x.min.x.offset/(x.max.x.offset - x.min.x.offset)))
y.offset.knn <- kNNdist(as.matrix(y.offset.distance), k = 5)
kNNdistplot(as.matrix(y.offset.distance), k = 5)

truehist(y.offset.distance, prob = F)
