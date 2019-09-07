library(MASS)
source('DrawHighlightedLines.R')

distance.data <- read.csv("/home/student/workspace/testEncodings/sub_image_distances.csv", stringsAsFactors = F)

hist(distance.data[ , 2])

sort.order <- order(distance.data[, 2])

distance.data.sorted <- distance.data[sort.order, ]
distance.data.sorted[1:10, ]

truehist(distance.data[ , 2])



segments.file.name <- '/home/student/workspace/testEncodings/kanji_data_segments.csv'
kanji.segments.data <- read.table(segments.file.name, header = T, sep = ",")

kanji.segments.data[1:100, ]



# 35114-33897-137_21-53_51    14
kanji1 <- kanji.segments.data[ , 1]

kanji.subimages.all.lines <- which(kanji.segments.data$unicode == 35114)
kanji.subimages.all.lines2 <- which(kanji.segments.data$unicode == 33897)

kanji.segments.data[kanji.subimages.all.lines[1:10] , ]

kanji.subimage <- which(kanji.segments.data$unicode == 35114 & kanji.segments.data$segment == 9)
kanji.subimage2 <- which(kanji.segments.data$unicode == 33897 & kanji.segments.data$segment == 33)

kanji.subimage.linedata <- kanji.segments.data[kanji.subimage, ]
kanji.subimage.linedata2 <- kanji.segments.data[kanji.subimage2, ]

DrawHighlightedLines(35114, kanji.subimage.linedata)
DrawHighlightedLines(33897, kanji.subimage.linedata2)


library(splitstackshape)
#install.packages("splitstackshape")

test.read <- concat.split(data = distance.data.sorted, split.col = 1, sep = "-", drop = T)

distance.data.sorted[1, ]
kanji.subimages.all.lines.3 <- which(kanji.segments.data$unicode == test.read[1, 2][[1]])
kanji.subimages.all.lines.4 <- which(kanji.segments.data$unicode == test.read[1, 3])[[1]]

kanji.subimages.all.lines.3

segments.3 <- intersect(which(kanji.segments.data$unicode == test.read[1, 2][[1]]), which(kanji.segments.data$segment == test.read[1, 4][[1]]))
segments.4 <- intersect(which(kanji.segments.data$unicode == test.read[1, 3][[1]]), which(kanji.segments.data$segment == test.read[1, 5][[1]]))


distance.segment.data <- data.frame(a = integer(), b = integer(), c = integer())
for(i in 1:1000) { #dim(distance.data.sorted)[1]) {
  distance <- distance.data.sorted[i, 2]
  segments.5 <- intersect(which(kanji.segments.data$unicode == test.read[i, 2][[1]]), which(kanji.segments.data$segment == test.read[i, 4][[1]]))
  segments.6 <- intersect(which(kanji.segments.data$unicode == test.read[i, 3][[1]]), which(kanji.segments.data$segment == test.read[i, 5][[1]]))
  
  distance.segment.data <- rbind(distance.segment.data, data.frame(a = distance, b = length(segments.5), c = length(segments.6)))
}
