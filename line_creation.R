file.name <- '/home/student/workspace/testEncodings/kanji_data_2.csv'
dat <- read.table(file.name, header = T, sep = ",")


kanji.unicode <- unique(dat[,1])


which(dat[, 1] %in% kanji.unicode[1])



mat <- matrix(c(1,2,1,0,0,4,1,0,1), nrow=3, byrow=TRUE)
image(mat)



source('CreateLine.R')
temp <- CreateLine(1, 2, 5, 6)


temp.matrix <- matrix(0, nrow = 10, ncol = 10)

for(row in 1:dim(temp)[1]) {
  temp.matrix[temp[row, 1], temp[row, 2]] <- 1
}