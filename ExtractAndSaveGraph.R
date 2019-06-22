ExtractAndSaveGraph <-
  function(kanji.code,
           lines.in.kanji,
           corner.labels,
           output.file) {
    # Extracts information about relationsships between lines in a kanji 
    # and stores it to an output file.
    # Args:
    #   kanji.code: Unicode used to identify the kanji
    #   lines.in.kanji: Information about the lines in the kanji. This is a matrix as output by ExtractRelativePositions
    #   corner.labels: A vector with information about which cluster the relation between two lines belong to
    #   output.file: File to write the data to. The is in the form of a graph ML document
    included.lines <-
      sort(unique(c(lines.in.kanji[, 7], lines.in.kanji[, 8])))
    
    # This vector is a mapping from the index in the vector to the line number in the kanji. 
    # The line numbers start from 0, and not all line numbers may be represented in the kanji. 
    # If the line number is not in the kanji the index in the vector maps to -1
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
    
    adjacency.matrix[cbind(rows.in.adjacency.matrix, columns.in.adjacency.matrix)] <-
      1
    
    kanji.graph <- graph_from_adjacency_matrix(adjacency.matrix)
    
    stopifnot(length(rows.in.adjacency.matrix) == length(corner.labels))
    
    # Iterate over each from-to pair defined by rows.in.adjacency.matrix 
    # and columns.in.adjacency.matrix. These are in turn made from lines.in.kanji 
    # and the first entry there corresponds to the first entry in corner.labels, the 
    # second entry to the second endty in corner.labels and so on
    for (i in 1:length(rows.in.adjacency.matrix)) {
      E(kanji.graph,
        c(rows.in.adjacency.matrix[i], columns.in.adjacency.matrix[i]))$category <-
        corner.labels[i]
    }
    
    set_graph_attr(kanji.graph, 'kanjiCode', kanji.code)
    write_graph(kanji.graph, output.file, 'graphml')
  }
