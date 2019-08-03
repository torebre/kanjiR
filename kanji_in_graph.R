library(igraph)


DrawHighlightedLines(filtered.lines[1, 6], kanji.line.data[which(kanji.line.data == filtered.lines[1, 6]) ,], filtered.lines[1, 7:8])



kanjis.in.set <- unique(filtered.lines[ , 6])

sapply(kanjis.in.set, function(kanji.code) {
  # Find the same rows in filtered.matrix.kms. The order of the lines is the same in filtered.lines and filtered.matrix.kms$label
  lines.in.kanji <- filtered.lines[which(filtered.lines[ , 6] == kanji.code), ]
  corner.labels <- filtered.matrix.kms$label[which(filtered.lines[ , 6] == kanji.code)]
  
  ExtractAndSaveGraph(kanji.code, lines.in.kanji, corner.labels, paste('/home/student/workspace/kanjiR/kanji_graphs3/', kanji.code, '.xml', sep=''))
})




lines.in.kanji2 <- filtered.lines[which(filtered.lines[ , 6] == 26613), ]
corner.labels2 <- filtered.matrix.kms$label[which(filtered.lines[ , 6] == 26613)]

ExtractAndSaveGraph(26613, lines.in.kanji2, corner.labels2, paste('/home/student/workspace/kanjiR/test_kanji', 26613, '.xml', sep=''))
