library(apcluster)
library(scatterplot3d)
library(plotly)
library(rgl)
library(ks)


ap.result <- apcluster(negDistMat(r=3), filtered.matrix[1:1000, ])

summary(ap.result)

ap.result

plot(ap.result, filtered.matrix[1:20000, ])
heatmap(ap.result)



scatterplot3d(filtered.matrix[ , 1], filtered.matrix[ , 2], filtered.matrix[ , 3])


rgl.open()
rgl.points(filtered.matrix[ , 1], filtered.matrix[ , 2], filtered.matrix[ , 3])

evaluation.points <- cbind(seq(0, max(filtered.matrix[, 1]), length.out = 100),
seq(0, max(filtered.matrix[, 2]), length.out = 100),
seq(0, max(filtered.matrix[, 3]), length.out = 100))

kernel.density.estimate <- kde(filtered.matrix, gridsize = c(50, 50, 50)) #, eval.points = filtered.matrix)



predict(kernel.density.estimate, x = c(1,2,3))
plot(kernel.density.estimate)

kernel.feature.significance <- kfs(filtered.matrix)
plot(kernel.feature.significance)


# kernel mean shift clustering
filtered.matrix.kms <- kms(x = filtered.matrix, y = evaluation.points)

## colour functions
pt.col <- function(pos=1, alpha=1, ...) {return(paste0(brewer.pal(12, "Paired"), format(as.hexmode(round(alpha*255,0)), width=2))[pos])}
seq.col2 <- function(n, alpha=1, ...) {c("transparent",tail(rev(diverge.col(2*n-1, alpha=alpha, ...)), n-1))}
seq.col3d <- function(n, alpha=1, ...){rev(sequential_hcl(n=n, h=290, power=1.1, c.=c(80,70), l=c(30,50), alpha=alpha, ...))}
diverge.col <- function(n, alpha=1, ...){cols <- paste0(brewer.pal(n, "PuOr"), as.hexmode(round(alpha*255,0))); cols[n %/% 2+1] <- grey(1, alpha=0); return(cols)}

## rgl plot parameters
library(colorspace)
library(RColorBrewer)
library(misc3d)
library(alphashape3d)

hsct.rgl <-function(xlab, ylab, zlab)
{
  par3d(userMatrix=matrix(c(0.78,0.20,-0.59,0.00,-0.63,0.22,-0.75,0.00,-0.02,0.96,0.29,0,0,0,0,1), nrow=4), zoom=1.1, windowRect=c(300,300,1000, 1000), cex=1.5)
  axes3d(edges=c("x--", "y--"), nticks=5, labels=c(0,200,400,600,800,1000))
  axes3d(edges=c("z-+"), nticks=5, labels=c(0,"",400,"",800,""))
  if (!missing(xlab)) mtext3d(xlab, edge="x--", line=2)
  if (!missing(ylab)) mtext3d(ylab, edge="y--", line=2)
  if (!missing(zlab)) mtext3d(zlab, edge="z-+", line=1)
  aspect3d(c(1,1,1))
}

xlab <- "x"
ylab <- "y"
zlab <- "z"
xlim <- c(0,  max(filtered.matrix[, 1]))
ylim <- c(0,  max(filtered.matrix[, 2]))
zlim <- c(0,  max(filtered.matrix[, 3]))

clear3d()
plot(filtered.matrix.kms, col=pt.col((1:5)*2), splom=FALSE, size=8, axes=FALSE, alpha=(filtered.matrix.kms$label+1)^1.5/40, asp=1, xlim=xlim, ylim=ylim, zlim=zlim, xlab=xlab, ylab=ylab, zlab="")
hsct.rgl(zlab=zlab)
box3d()

## significant modal regions
filtered.matrix.kfs <- kfs(filtered.matrix, verbose=TRUE)

clear3d()
plot(filtered.matrix.kfs, splom=FALSE, axes=FALSE, color=pt.col(8), asp=1, xlim=xlim, ylim=ylim, zlim=zlim, xlab=xlab, ylab=ylab, zlab="")
hsct.rgl(zlab=zlab); box3d()
for (i in 1:5) {
  plot(ashape3d(unique(filtered.matrix.kms$x[filtered.matrix.kms$label==i,]), alpha=100, pert=TRUE), transparency=0.1, edges=FALSE, lines=FALSE, clear=FALSE, col=pt.col(2*i))
}
