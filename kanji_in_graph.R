library(igraph)


DrawHighlightedLines(filtered.lines[1, 6], kanji.line.data[which(kanji.line.data == filtered.lines[1, 6]) ,], filtered.lines[1, 7:8])



kanjis.in.set <- unique(filtered.lines[ , 6])

sapply(kanjis.in.set, function(kanji.code) {
  # Find the same rows in filtered.matrix.kms
  lines.in.kanji <- filtered.lines[which(filtered.lines[ , 6] == kanji.code), ]
  corner.labels <- filtered.matrix.kms$label[which(filtered.lines[ , 6] == kanji.code)]
  
  ExtractAndSaveGraph(kanji.code, lines.in.kanji, corner.labels, paste('/home/student/workspace/kanjiR/kanji_graphs/', kanji.code, '.xml', sep=''))
})
