# Test find closest lines


kanji.unicodes <- unique(kanji.line.data[ , 1])

unicode.to.use <- kanji.unicodes[10]
kanji.test <- kanji.line.data[which(kanji.line.data[ , 1] == unicode.to.use), ]


DrawHighlightedLines(unicode.to.use, kanji.test, c())

DrawLineKanji(unicode.to.use, kanji.test)



test.line.index <- 7
test.line <- kanji.test[test.line.index, ]
stop.1.x <- test.line$start_x + ceiling(test.line$length * sin(test.line$angle))
stop.1.y <- test.line$start_y + ceiling(test.line$length * cos(test.line$angle))

closest.lines <- ExtractClosestLinesToLine(test.line$start_x, test.line$start_y, stop.1.x, stop.1.y, 3, kanji.test[-test.line.index, ])[1:3]

closest.lines.2 <- ExtractClosestLinesToLine2(test.line$start_x, test.line$start_y, stop.1.x, stop.1.y, 4, kanji.test[-test.line.index, ])

DrawHighlightedLines(unicode.to.use, kanji.test, c(test.line$line_number))
DrawHighlightedLines(unicode.to.use, kanji.test, c(closest.lines.2))


closest.lines.2
length(closest.lines.2)

DrawHighlightedLines(unicode.to.use, kanji.test, test.line$line_number)
DrawHighlightedLines(unicode.to.use, kanji.test, closest.lines)


first.line <- cluster6.lines[i, ]

first.line.kanji <- kanji.line.data[which(kanji.line.data[ , 1] == first.line[6]), ]

first.kanji.index <- which(first.line.kanji[ , 2] == first.line[7])
second.kanji.index <- which(first.line.kanji[ , 2] == first.line[8])

kanji.first.lines.removed <- first.line.kanji[-c(first.kanji.index, second.kanji.index ), ]

first.line.1 <- first.line.kanji[which(first.line.kanji[ , 2] == first.line[7]), ]
first.line.2 <-  first.line.kanji[which(first.line.kanji[ , 2] == first.line[8]), ]

stop.1.x <- first.line.1$start_x + ceiling(first.line.1$length * cos(first.line.1$angle))
stop.1.y <- first.line.1$start_y + ceiling(first.line.1$length * sin(first.line.1$angle))
closest.lines.1 <- ExtractClosestLinesToLine(first.line.1$start_x, first.line.1$start_y, stop.1.x, stop.1.y, 3, kanji.first.lines.removed)[1:3]

stop.2.x <- first.line.2$start_x  + ceiling(first.line.2$length * cos(first.line.2$angle))
stop.2.y <- first.line.2$start_y + ceiling(first.line.2$length * sin(first.line.2$angle))

# Find the lines closest to the two lines involved
closest.lines.2 <- ExtractClosestLinesToLine(first.line.2$start_x, first.line.2$start_y, stop.2.x, stop.2.y, 3, kanji.first.lines.removed)[1:3]




cluster.1.6 <- cluster.6.next.iteration[which(mod2$classification == 1), ]
op <- par(mfrow = c(4, 4))
for(i in 1:16) {
  DrawHighlightedLines(cluster.6.next.iteration[i, 6], kanji.line.data[which(kanji.line.data == cluster.6.next.iteration[i, 6]) ,], cluster.6.next.iteration[i, 7:9] + 1)
}
par(op)


