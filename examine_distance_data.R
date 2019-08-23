library(MASS)
source('DrawHighlightedLines.R')

distance.data <- read.csv("/home/student/workspace/testEncodings/sub_image_distances.csv")

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

kanji.segments.data[kanji.subimages.all.lines[1:10] , ]

kanji.subimage <- which(kanji.segments.data$unicode == 35114 & kanji.segments.data$segment == 3242)

kanji.subimage.linedata <- kanji.segments.data[kanji.subimage, ]

DrawHighlightedLines(35114, kanji.subimage.linedata)

