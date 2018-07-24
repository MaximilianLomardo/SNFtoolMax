displayClustersHeat <- function(W, group, xlabel = 'samples', ylabel = 'samples', main.title) {
  # Visualizes the specified clusters in an affinity matrix 
  #
  # Args:
  #   W: Affinity matrix 
  #   group: labels for each row/column in W
  #
  # Returns:
  #   NULL - Plots the image
  
  require(d3heatmap)
  require(RColorBrewer)
  
  normalize <- function(X){
    return( X / rowSums(X))
  }
  
  ind <- sort(as.vector(group),index.return=TRUE)
  ind <- ind$ix
  diag(W) <- 0
  W <- normalize(W)
  W <- W + t(W)
  
  #image(1:ncol(W),1:nrow(W),W[ind,ind],col = grey(100:0 / 100),xlab = xlabel ,ylab=ylabel);
  
  heatmap(W[ind, ind], Rowv = NA, Colv = NA, xlab = xlabel, ylab = ylabel , main = main.title)
  
}
