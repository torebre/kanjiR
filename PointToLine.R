PointToLine <- function(line.start.row, line.start.column, 
                        line.stop.row, line.stop.column, 
                        point.row, point.column) {
  
  t <- -((line.start.row - point.row) * (line.stop.row - line.start.row) + (line.start.column - point.column) * (line.stop.column - line.start.column)) / ((line.stop.row - line.start.row)^2 + (line.stop.column - line.start.column)^2)
  print(paste("t: ", t))
  
  if(t > 0 && t < 1) {
    return(abs(((line.stop.row - line.start.row) * (line.start.column - point.column)
                - (line.stop.column - line.start.column) * (line.start.row - point.row))/sqrt((line.stop.row - line.start.row)^2 + (line.stop.column - line.start.column)^2)))
    
    # return(abs((line.stop.column - line.start.column) * point.row 
    #            - (line.stop.row - line.start.row) * point.column 
    #            + line.stop.row * line.start.column 
    #            - line.stop.column * line.start.row) / sqrt((line.stop.column - line.start.column)^2 + (line.stop.row - line.start.row)^2))
  }
  
  d1 <- sqrt((line.stop.row - point.row)^2 + (line.stop.column - point.column)^2)
  d2 <- sqrt((line.start.row - point.row)^2 + (line.start.column - point.column)^2)
  
  if(d1 < d2) {
    return(d1)
  }
  return(d2)
}
