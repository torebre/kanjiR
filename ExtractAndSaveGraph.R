ExtractAndSaveGraph <-
  function(kanji.code,
           lines.in.kanji,
           corner.labels,
           output.file) {
    included.lines <-
      sort(unique(c(
        unique(lines.in.kanji[, 7]), unique(lines.in.kanji[8])
      )))
    
    lookup.vector <- matrix(data = -1, nrow = max(included.lines))
    lookup.vector[included.lines + 1] <- seq(1:length(included.lines))
    
    rows.in.adjacency.matrix <- lookup.vector[lines.in.kanji[, 7] + 1]
    columns.in.adjacency.matrix <-
      lookup.vector[lines.in.kanji[, 8] + 1]
    
    if(length(rows.in.adjacency.matrix) == 0 || length(columns.in.adjacency.matrix) == 0) {
      print(paste('No rows or columns found. Kanji code: ', kanji.code, sep = ''))
    }
    
    adjacency.matrix <-
      matrix(
        data = 0,
        nrow = length(included.lines),
        ncol = length(included.lines)
      )
    
    adjacency.matrix[(columns.in.adjacency.matrix - 1) * length(included.lines) + rows.in.adjacency.matrix] <-
      1
    
    adjacency.matrix[rows.in.adjacency.matrix, columns.in.adjacency.matrix] <-
      1
    
    kanji.graph <- graph_from_adjacency_matrix(adjacency.matrix)
    colors <- rainbow(length(unique(corner.labels)))
    
    for (i in 1:length(rows.in.adjacency.matrix)) {
      E(kanji.graph,
        c(rows.in.adjacency.matrix[i], columns.in.adjacency.matrix[i]))$category <-
        corner.labels[i]
      E(kanji.graph,
        c(rows.in.adjacency.matrix[i], columns.in.adjacency.matrix[i]))$color <-
        colors[corner.labels[i]]
    }
    
    set_graph_attr(kanji.graph, 'kanjiCode', kanji.code)
    write_graph(kanji.graph, output.file, 'graphml')
  }
