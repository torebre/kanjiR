source("ExtractClosestLines.R")

# source("CreateLine.R")
# source("PointToLine.R")
# 
# 
# kanji.unicode <- kanji.line.data[1, 1]
# all.lines.in.kanji <- kanji.line.data[which(kanji.line.data[, 1] == kanji.unicode), ]
# number.of.lines <- dim(all.lines.in.kanji)[1]
# 
# for(i in 8:8) { #1:number.of.lines) {
#   
#   # print(paste('i:', i))
#   
#   current.start.x <- all.lines.in.kanji[i, 5]
#   current.start.y <- all.lines.in.kanji[i, 6]
#   
#   current.stop.x <- current.start.x + ceiling(all.lines.in.kanji[i, 4] * cos(all.lines.in.kanji[i, 3]))
#   current.stop.y <- current.start.y + ceiling(all.lines.in.kanji[i, 4] * sin(all.lines.in.kanji[i, 3]))
#   
#   # line <- CreateLine(start.x, start.y, stop.x, stop.y)
#   
#   distances <- matrix(nrow = number.of.lines, ncol = 1)
#   for(j in 1:number.of.lines) {
#     if(i == j) {
#       distances[j] <- .Machine$integer.max
#       next
#     }
#     
#     # print(paste('j:', j))
#     
#     start.x <- all.lines.in.kanji[j, 5]
#     start.y <- all.lines.in.kanji[j, 6]
#     
#     stop.x <- start.x + ceiling(all.lines.in.kanji[j, 4] * cos(all.lines.in.kanji[j, 3]))
#     stop.y <- start.y + ceiling(all.lines.in.kanji[j, 4] * sin(all.lines.in.kanji[j, 3]))
#     
#     # print(paste("Test23:",start.x, start.y, stop.x, stop.y))
#     
#     line <- CreateLine(start.x, start.y, stop.x, stop.y)
#     
#     min.distance <- .Machine$integer.max
#     for(k in 1:nrow(line)) {
#       
#       # print(paste('k:', k, 'lines:', nrow(line)))
#       
#       distance <- PointToLine(current.start.x, current.start.y, current.stop.x, current.stop.y, 
#                               line[k, 1], line[k, 2])
#       
#       if(distance < min.distance) {
#         min.distance <- distance
#       }
#       
#       # print(paste('Distance:', distance))
#     }
#     
#     print(paste("Distance ", j, ":", distance))
#     
#     distances[j] <- min.distance
#   }
#   
#   cutoff.distance <- sort(distances)[5]
#   closest.elements <- which(distances < cutoff.distance)
#   
#   print("Distances:")
#   print(distances)
#   
#   print(all.lines.in.kanji[i, ])
#   
#   print(all.lines.in.kanji[closest.elements, ])
#   
#   DrawHighlightedLines(all.lines.in.kanji[i, 1], all.lines.in.kanji, c(i))
#   DrawHighlightedLines(all.lines.in.kanji[i, 1], all.lines.in.kanji, c(i, closest.elements))
#   
# }


closest.lines <- ExtractClosestLines(33897, 3, kanji.line.data)


a <- 9
temp.x <- all.lines.in.kanji[a, 5]
temp.y <- all.lines.in.kanji[a, 6]

temp.stop.x <- temp.x + ceiling(all.lines.in.kanji[a, 4] * cos(all.lines.in.kanji[a, 3]))
temp.stop.y <- temp.y + ceiling(all.lines.in.kanji[a, 4] * sin(all.lines.in.kanji[a, 3]))

temp.line <- CreateLine(temp.x, temp.y, temp.stop.x, temp.stop.y)

for(k in 1:nrow(temp.line)) {
  distance <- PointToLine(current.start.x, current.start.y, current.stop.x, current.stop.y,
                          temp.line[k, 1], temp.line[k, 2])

  print(paste("Point: ", temp.line[k, 1], temp.line[k, 2]))
  print(paste("Distance: ", distance))
}

PointToLine(current.start.x, current.start.y, current.stop.x, current.stop.y, 48, 19)

