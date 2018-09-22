source('DrawLineRelativeToInputLine.R')

all.lines.in.kanji <-
  kanji.line.data[which(kanji.line.data[, 1] == 26397), ]

dim(lines.in.kanji)

par(mfrow=c(3, 4))
for(i in 1:12) {
  line1.original <- all.lines.in.kanji[i, ]
  lines.draw <- DrawLineRelativeToInputLine(line1.original, all.lines.in.kanji)
  
  DrawLines(lines.draw)
}

par(mfrow=c(1, 1))
DrawLineKanji(26397, kanji.line.data)
