library(igraph)

igraphdemo()

igraphdemo('community')


if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("graph", version = "3.8")

browseVignettes("graph")


browseVignettes('igraph')





adjm <- matrix(sample(0:1, 100, replace=TRUE, prob=c(0.9,0.1)), nc=10)
g1 <- graph_from_adjacency_matrix( adjm )


filtered.matrix[ , 1]


filtered.lines[1, 7:8]

DrawHighlightedLines(filtered.lines[1, 6], kanji.line.data[which(kanji.line.data == filtered.lines[1, 6]) ,], filtered.lines[1, 7:8])

lines.in.kanji <- filtered.lines[which(filtered.lines[ , 6] == 33897), ]

included.lines <- sort(unique(c(unique(lines.in.kanji[ , 7]), unique(lines.in.kanji[ 8]))))

lookup.vector <- sapply(0:max(included.lines), function(x) {
  if(x %in% included.lines) {
    return(x)
  }
  return(-1)
})

lookup.vector[lines.in.kanji[ , 7] + 1]
lookup.vector[lines.in.kanji[ , 8] + 1]
