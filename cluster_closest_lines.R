library(scatterplot3d)
library(mclust)

source('DrawHighlightedLines.R')
source('DrawLineKanji.R')


kanji.unicodes <- unique(kanji.line.data[ , 1])[1:10]

number.of.rows <- 0
for(i in 1:length(kanji.unicodes)) {
  all.lines.in.kanji <- kanji.line.data[which(kanji.line.data[, 1] == kanji.unicodes[i] & kanji.line.data[ , 4] > 5), ]
  line.count <- dim(all.lines.in.kanji)[1]
  
  for(j in 1:line.count) {
    line1.original <- all.lines.in.kanji[j, ]
    
    # if(line1.original$length < 10) {
    #   next
    # }
    
    number.of.rows <- number.of.rows + line.count - 1
  }
}


lines <- matrix(nrow = number.of.rows, ncol = 8)
counter <- 1
for(i in 1:length(kanji.unicodes)) {
  # Only use lines that have a length greater than 5
  all.lines.in.kanji <- kanji.line.data[which(kanji.line.data[, 1] == kanji.unicodes[i] & kanji.line.data[ , 4] > 5), ]
  closest.lines <- ExtractClosestLines(kanji.unicodes[i], 3, all.lines.in.kanji)
  
  for(j in 1:dim(closest.lines)[1]) {
    closest.lines.in.kanji <- all.lines.in.kanji[closest.lines[j, ], ]
    line1.original <- all.lines.in.kanji[j, ]
    
    # print(paste("Original line: ", line1.original))
    # print(closest.lines.in.kanji)
    
    lines.draw <- ExtractRelativePositions(line1.original, closest.lines.in.kanji, c(), 
                                           as.integer(rownames(all.lines.in.kanji[j, ])))
    
    for(j in 1:dim(lines.draw)[1]) {
      lines[counter, ] <- lines.draw[j, ]
      counter <- counter + 1
    }
  }
}


filtered.lines <- lines[which(!is.na(lines[ , 1])) ,]


mod5 <- densityMclust(filtered.lines[ , 1:3])
summary(mod5)
plot(mod5, what = "density")
# plot(mod5, what = "density", type = "level")
# plot(mod5, what = "density", type = "persp")


# top.percentile <- quantile(mod$density, c(0.9))
# top.percentile.indices <- which(lines > top.percentile, arr.ind = T)

# filtered.matrix <-lines[top.percentile.indices, ]
filtered.matrix <-filtered.lines[, 1:3]

BIC <- mclustBIC(filtered.matrix[ , 1:3])
plot(BIC)
summary(BIC)
mod1 <- Mclust(filtered.matrix, x = BIC)
summary(mod1, parameter = T)

plot(mod1, what = "classification")


mod1$classification

cluster1 <- lines[which(mod1$classification == 1), ]

unicode <- all.lines.in.kanji[cluster1[1, 4], ]$unicode 
all.lines.in.kanji[cluster1[1, 4], ]
all.lines.in.kanji[cluster1[1, 5], ]

cluster1[1, 4]

DrawHighlightedLines(unicode, all.lines.in.kanji, c(cluster1[1, 4], cluster1[1, 5]))

DrawLineKanji(unicode, all.lines.in.kanji)


scatterplot3d(lines[, 1], lines[, 2], lines[, 3])


# Cluster 1
cluster1.lines <- lines[mod1$classification, ]

cluster1.lines
