install.packages("TDA")

library("TDA")

test3 <- as.matrix(test2)
image(test3)

test4 <- test3[1:50, 1:50]
image(test4)

test5 <- matrix(test4, nrow = 1, ncol = 50 * 50)

test4[50, 50]

kanji.points <- which(test4 == 1)

kanji.points.x <- kanji.points %% 50
#kanji.points.y <- kanji.points - 50 * (kanji.points / 50 - 1)

kanji.points.y <- sapply(kanji.points, function(x) { x - 50 * (x %/% 50)})

kanji.points.matrix <- matrix(c(kanji.points.x, kanji.points.y), nrow = length(kanji.points.x), ncol = 2)

Xlim <- c(1, 46)
Ylim <- c(1, 46)

DiagGrid <- gridDiag(X = kanji.points.matrix, FUN = kde, h = 0.3, lim = cbind(Xlim, Ylim), by = 1,
       sublevel = FALSE, library = "Dionysus", location = TRUE,
       printProgress = FALSE)

h <- 0.3
KDE <- kde(X = X, Grid = Grid, h = h)
band <- bootstrapBand(X = X, FUN = kde, Grid = Grid, B = 100, parallel = FALSE, alpha = 0.1, h = h)
plot(DiagGrid[["diagram"]], band = 2 * band[["width"]], main = "KDE Diagram")



# Example
X <- circleUnif(400)
Xlim <- c(-1.6, 1.6)
Ylim <- c(-1.7, 1.7)
by <- 0.065
Xseq <- seq(Xlim[1], Xlim[2], by = by)
Yseq <- seq(Ylim[1], Ylim[2], by = by)
Grid <- expand.grid(Xseq, Yseq)

DiagGrid <- gridDiag(
X = X, FUN = kde, h = 0.3, lim = cbind(Xlim, Ylim), by = by,
sublevel = FALSE, library = "Dionysus", location = TRUE,
printProgress = FALSE)

h <- 0.3
KDE <- kde(X = X, Grid = Grid, h = h)
band <- bootstrapBand(X = X, FUN = kde, Grid = Grid, B = 100, parallel = FALSE, alpha = 0.1, h = h)

plot(DiagGrid[["diagram"]], band = 2 * band[["width"]], main = "KDE Diagram")
