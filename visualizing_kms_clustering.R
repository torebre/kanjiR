filtered.matrix.kms <- kms(x = filtered.matrix, verbose = T)

xlab <- "x"
ylab <- "y"
zlab <- "z"
xlim <- c(0,  max(filtered.matrix[, 1]))
ylim <- c(0,  max(filtered.matrix[, 2]))
zlim <- c(0,  max(filtered.matrix[, 3]))


plot(filtered.matrix.kms)
clear3d()
plot(filtered.matrix.kms, col=pt.col((1:filtered.matrix.kms$nclust)*2), splom=FALSE, size=8, axes=FALSE, alpha=(filtered.matrix.kms$label+1)^1.5/40, asp=1, xlim=xlim, ylim=ylim, zlim=zlim, xlab=xlab, ylab=ylab, zlab="")
hsct.rgl(zlab=zlab)
box3d()


filtered.lines
filtered.matrix

kanji.number = filtered.lines[1, 6]
kanji.indices = which(filtered.lines[ , 6] == kanji.number)

cluster.labels = filtered.matrix.kms$label[kanji.indices]

filtered.matrix.kms$label
filtered.lines[kanji.indices, ]

# DrawLineKanji(kanji.number, kanji.line.data)
DrawLineKanjiWithCorners(kanji.number, kanji.line.data, cbind(cluster.labels, filtered.lines[kanji.indices , 7:8]))
