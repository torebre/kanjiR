library(MASS)

#data = load("/home/student/encodedkanji.txt")

data = read.table("/home/student/encodedkanji.txt", sep=",")

a <- matrix(unlist(data[200, ]), nrow = 100, ncol = 100)
image(a)


unlist(data[1, ])
data[2, ]

a[1, 1]

test1 <- c(1:5, "6,7", "8,9,10")
tf <- tempfile()
writeLines(test1, tf)
unlink(tf)



test <- read.table('test_input.csv', header = T, sep = ",", colClasses = c('integer', 'matrix'))

test[1, 2]


test2 <- read.csv('/home/student/workspace/testEncodings/kanji_output/26379.dat', header = F, sep = ",")
test3 <- as.matrix(test2)
