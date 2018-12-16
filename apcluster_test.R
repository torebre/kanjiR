library(apcluster)
library(scatterplot3d)
library(plotly)
library(rgl)
library(ks)


ap.result <- apcluster(negDistMat(r=3), filtered.matrix[1:20000, ])

summary(ap.result)

ap.result

plot(ap.result, filtered.matrix[1:20000, ])
heatmap(ap.result)



scatterplot3d(filtered.matrix[ , 1], filtered.matrix[ , 2], filtered.matrix[ , 3])


rgl.open()
rgl.points(filtered.matrix[ , 1], filtered.matrix[ , 2], filtered.matrix[ , 3])


kernel.density.estimate <- kde(filtered.matrix)

plot(kernel.density.estimate)
