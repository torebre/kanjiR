library(mclust)


matrix.dims <- dim(line.matrix)
matrix.values <- matrix(nrow=sum(line.matrix != 0), ncol = 3)
counter <- 1

for(row in 1:matrix.dims[1]) {
  for(column in 1:matrix.dims[2]) {
    if(line.matrix[row, column] == 0) {
      next
    }
    matrix.values[counter, ] <- c(row, column, line.matrix[row, column])
    counter <- counter + 1
  }
}

mod5 <- densityMclust(matrix.values)
summary(mod5)
plot(mod5, what = "density")
plot(mod5, what = "density", type = "level")
plot(mod5, what = "density", type = "persp")
