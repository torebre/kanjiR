library(dbscan)


# filtered.matrix.normalized <- apply(filtered.matrix[ , 1:3], 2, function(x) (x- min(x))/(max(x) - min(x)))

clustering <- dbscan(filtered.matrix, eps = 0.1, minPts = 100)


summary(clustering)

hullplot(filtered.matrix.normalized, clustering)
