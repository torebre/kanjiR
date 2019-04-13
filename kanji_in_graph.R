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

lookup.vector <- matrix(data = -1, nrow = max(included.lines))

lookup.vector[included.lines + 1] <- seq(1:length(included.lines))

# lookup.vector <- lapply(0:max(included.lines), function(counter, x) {
#   if(x %in% included.lines) {
#     return(counter)
#   }
#   return(-1)
# }, )

rows.in.adjacency.matrix <- lookup.vector[lines.in.kanji[ , 7] + 1]
columns.in.adjacency.matrix <- lookup.vector[lines.in.kanji[ , 8] + 1]

adjacency.matrix <- matrix(data = 0, nrow = length(included.lines), ncol = length(included.lines))

adjacency.matrix[(columns.in.adjacency.matrix - 1) * length(included.lines) + rows.in.adjacency.matrix] <- 1

adjacency.matrix[rows.in.adjacency.matrix, columns.in.adjacency.matrix] <- 1

g1 <- graph_from_adjacency_matrix( adjacency.matrix )

# Find the same rows in filtered.matrix.kms
corner.labels <- filtered.matrix.kms$label[which(filtered.lines[ , 6] == 33897)]
colors <- rainbow(length(unique(corner.labels)))

for(i in 1:length(rows.in.adjacency.matrix)) {
  edge <- E(g1, c(rows.in.adjacency.matrix[i], columns.in.adjacency.matrix[i]))
  E(g1, c(rows.in.adjacency.matrix[i], columns.in.adjacency.matrix[i]))$category <- corner.labels[i]
  E(g1, c(rows.in.adjacency.matrix[i], columns.in.adjacency.matrix[i]))$color <- colors[corner.labels[i]]
}

edge_attr_names(g1)

plot(g1)


