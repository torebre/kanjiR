library(MASS)

#data = load("/home/student/encodedkanji.txt")

data = read.table("/home/student/encodedkanji.txt", sep=",")

a <- matrix(unlist(data[200, ]), nrow = 100, ncol = 100)
image(a)


unlist(data[1, ])
data[2, ]

a[1, 1]
